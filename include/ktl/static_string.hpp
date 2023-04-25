#pragma once

#include <ktl/access.hpp>

#include "detail/string_ops.hpp"

namespace ktl {
template<typename CharT, auto Capacity, typename Traits = std::char_traits<CharT>>
    requires std::is_trivial_v<CharT> && std::integral<std::decay_t<decltype(Capacity)>>
class basic_static_string;

template<typename CharT, std::integral Size, typename Traits>
    requires(!std::is_const_v<CharT>)
class fixed_string;

namespace detail {
    template<typename CharT, auto Capacity, typename TraitsT>
    class substr_proxy {
      public:
        // NOLINTNEXTLINE(*-explicit-conversions)
        constexpr substr_proxy(basic_string_view<CharT, TraitsT> substr) noexcept :
            m_substr(substr) {}

        // NOLINTNEXTLINE(*-explicit-conversions)
        constexpr operator basic_static_string<CharT, Capacity, TraitsT>() const noexcept {
            return str();
        }

        constexpr auto str() const noexcept -> basic_static_string<CharT, Capacity, TraitsT> {
            basic_static_string<CharT, Capacity, TraitsT> str;

            check_(str.assign(m_substr.begin(), m_substr.end()), "");

            return str;
        }

        constexpr auto view() const noexcept -> basic_string_view<CharT, TraitsT> {
            return m_substr;
        }

      private:
        basic_string_view<CharT, TraitsT> m_substr;
    };
}  // namespace detail

template<typename CharT, auto Capacity, typename Traits>
    requires std::is_trivial_v<CharT> && std::integral<std::decay_t<decltype(Capacity)>>
class basic_static_string:
    public detail::str::string_ops<
        CharT,
        Traits,
        detail::str::size_t<Capacity>,
        basic_static_string<CharT, Capacity, Traits>> {
  public:
    using traits_type = Traits;
    using value_type = CharT;
    using size_type = detail::str::size_t<Capacity>;
    using difference_type = isize;
    using reference = CharT&;
    using const_reference = const CharT&;
    using pointer = CharT*;
    using const_pointer = const CharT*;

  private:
    using base = detail::str::string_ops<
        CharT,
        Traits,
        detail::str::size_t<Capacity>,
        basic_static_string<CharT, Capacity, Traits>>;

  public:
    constexpr basic_static_string() : m_len {1} {
        std::construct_at(m_chars.data(), base::NUL);
        this->start_lifetime();
    }

    ~basic_static_string()
        requires(!ASAN_ENABLED)
    = default;

    constexpr ~basic_static_string()
        requires(ASAN_ENABLED)
    {
        this->end_lifetime();
    }

    template<typename CharU, auto Cap, typename TraitsT>
        requires(std::same_as<CharU, CharT> && Cap == Capacity && std::same_as<Traits, TraitsT>)
    // NOLINTNEXTLINE(*-explicit-conversions)
    constexpr basic_static_string(detail::substr_proxy<CharU, Cap, TraitsT> proxy) {
        auto view = proxy.view();
        assert(Capacity > view.length());

        ktl::uninitialized_copy_n(view.begin(), view.length(), m_chars.data());
        m_len = view.length() + 1;
        at(m_chars, m_len - 1) = base::NUL;
        this->start_lifetime();
    }

    // NOLINTNEXTLINE(*-explicit-conversions, *-avoid-c-arrays)
    constexpr basic_static_string(const CharT (&str)[Capacity]) : m_len {Capacity} {
        std::copy(std::begin(str), std::end(str), m_chars.data());
        this->start_lifetime();
    }

    constexpr basic_static_string(const basic_static_string& o) noexcept : m_len {o.m_len} {
        ktl::uninitialized_copy_n(o.begin(), m_len, get_storage().begin);
        this->start_lifetime();
    }

    constexpr basic_static_string(basic_static_string&& o) noexcept : m_len {o.m_len} {
        ktl::uninitialized_copy_n(o.begin(), m_len, get_storage().begin);
        this->start_lifetime();
    }

    constexpr auto operator=(const basic_static_string& o) noexcept -> basic_static_string& {
        if (this == &o) {
            return *this;
        }
        this->adjust_lifetime(o.m_len);
        m_len = o.m_len;
        ktl::uninitialized_copy_n(o.begin(), m_len, get_storage().begin);
        return *this;
    }

    constexpr auto operator=(basic_static_string&& o) noexcept -> basic_static_string& {
        if (this == &o) {
            return *this;
        }
        this->adjust_lifetime(o.m_len);
        m_len = o.m_len;
        ktl::uninitialized_copy_n(o.begin(), m_len, get_storage().begin);
        return *this;
    }

    friend constexpr void swap(basic_static_string& a, basic_static_string& b) noexcept {
        a.swap(b);
    }

    constexpr void swap(basic_static_string& o) noexcept {
        if (this == &o)
            return;

        this->adjust_lifetime(std::max(m_len, o.m_len));
        o.adjust_lifetime(std::max(m_len, o.m_len));

        detail::swap_range_with_len(this->m_chars.data(), m_len, o.m_chars.data(), o.m_len);

        this->adjust_lifetime(m_len);
        o.adjust_lifetime(o.m_len);
    }

    constexpr auto max_size() const noexcept -> size_type {
        return Capacity - 1;
    }

    // NOLINTNEXTLINE(*-explicit-conversions)
    constexpr auto as_fixed_string() const noexcept -> fixed_string<value_type, size_type, Traits> {
        return {this->data(), this->capacity(), this->size()};
    }

    // --------------------- Substr ---------------------

    constexpr auto substr(size_type pos = 0, size_type count = base::npos) const& noexcept
        -> expected<detail::substr_proxy<CharT, Capacity, Traits>, Error> {
        Try(ss, basic_string_view {*this}.substr(pos, count));
        return ss;
    }

    // --------------------- Substr ---------------------

    constexpr auto substr(size_type pos = 0, size_type count = base::npos) && noexcept
        -> expected<typename base::non_null_ptr, Error> {
        return base::substr_destructive(pos, count);
    }

  private:
    // Allow access to internal members. Classic CRTP.
    friend class detail::str::string_ops<
        CharT,
        Traits,
        detail::str::size_t<Capacity>,
        basic_static_string<CharT, Capacity, Traits>>;

    [[nodiscard]] constexpr auto get_storage() const noexcept
        -> detail::str::string_storage<const CharT> {
        return {
            .begin = m_chars.data(),
            .end = m_chars.data() + m_len,
            .end_cap = m_chars.data() + Capacity};
    }
    constexpr auto get_storage() noexcept -> detail::str::string_storage<CharT> {
        return {
            .begin = m_chars.data(),
            .end = m_chars.data() + m_len,
            .end_cap = m_chars.data() + Capacity};
    }

    constexpr auto grow(usize req_len) noexcept -> expected<void, Error> {
        if (req_len > Capacity) [[unlikely]] {
            Throw(error::BufferFull);
        }
        this->adjust_lifetime(req_len);
        return {};
    }
    constexpr auto grow_uninit(usize req_len) noexcept -> expected<void, Error> {
        return grow(req_len);
    }

    constexpr auto set_len(size_type new_len) noexcept {
        assert(new_len <= Capacity && "length cannot exceed capacity");
        m_len = new_len;
    }

    size_type m_len;
    alignas(ASAN_ALIGN<CharT>) std::array<CharT, Capacity> m_chars;
};

template<typename CharT, auto Capacity, typename Traits = std::char_traits<CharT>>
basic_static_string(detail::substr_proxy<CharT, Capacity, Traits>)
    -> basic_static_string<CharT, Capacity, Traits>;

template<auto Capacity>
using static_string = basic_static_string<char, Capacity>;
template<auto Capacity>
using static_u8string = basic_static_string<char8_t, Capacity>;
template<auto Capacity>
using static_u16string = basic_static_string<char16_t, Capacity>;
template<auto Capacity>
using static_u32string = basic_static_string<char32_t, Capacity>;
template<auto Capacity>
using static_wstring = basic_static_string<wchar_t, Capacity>;

template<typename CharT, auto N>
constexpr auto make_static_string(const CharT (&chars)[N]) noexcept
    -> basic_static_string<CharT, N> {
    using str_t = basic_static_string<CharT, N>;
    str_t str;

    check_(str.assign(chars, N - 1), "");
    return str;
}

template<auto VCapacity, typename CharT, auto N>
constexpr auto make_static_string(const CharT (&chars)[N]) noexcept
    -> basic_static_string<CharT, VCapacity> {
    static_assert(VCapacity >= N);

    using str_t = basic_static_string<CharT, VCapacity>;
    str_t str;

    check_(str.assign(chars, N - 1), "");

    return str;
}
}  // namespace ktl