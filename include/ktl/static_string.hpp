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
    public detail::string_ops<
        CharT,
        Traits,
        detail::size_t<Capacity>,
        basic_static_string<CharT, Capacity, Traits>> {
  public:
    using traits_type = Traits;
    using value_type = CharT;
    using size_type = detail::size_t<Capacity>;
    using difference_type = isize;
    using reference = CharT&;
    using const_reference = const CharT&;
    using pointer = CharT*;
    using const_pointer = const CharT*;

  private:
    using base = detail::string_ops<
        CharT,
        Traits,
        detail::size_t<Capacity>,
        basic_static_string<CharT, Capacity, Traits>>;

  public:
    constexpr basic_static_string() : m_len {1} {
        std::construct_at(m_chars.data(), base::NUL);
        AsanAnnotator<basic_static_string>::start_lifetime(*this);
    }

    ~basic_static_string()
        requires(!ASAN_ENABLED)
    = default;

    constexpr ~basic_static_string()
        requires(ASAN_ENABLED)
    {
        this->clear();
        AsanAnnotator<basic_static_string>::end_lifetime(*this);
    }

    template<typename CharU, auto Cap, typename TraitsT>
        requires(std::same_as<CharU, CharT> && Cap == Capacity && std::same_as<Traits, TraitsT>)
    // NOLINTNEXTLINE(*-explicit-conversions)
    constexpr basic_static_string(detail::substr_proxy<CharU, Cap, TraitsT> proxy) {
        auto view = proxy.view();
        assert(Capacity > view.length());

        uninitialized_copy_n(view.begin(), view.length(), m_chars.data());
        m_len = view.length() + 1;
        at(m_chars, m_len - 1) = base::NUL;
        AsanAnnotator<basic_static_string>::start_lifetime(*this);
    }

    // NOLINTNEXTLINE(*-explicit-conversions)
    constexpr basic_static_string(const CharT (&str)[Capacity]) : m_len {Capacity} {
        std::copy(std::begin(str), std::end(str), m_chars.data());
        AsanAnnotator<basic_static_string>::start_lifetime(*this);
    }

    constexpr basic_static_string(const basic_static_string& o) noexcept : m_len {o.m_len} {
        uninitialized_copy_n(o.begin(), m_len, get_storage().begin);
        AsanAnnotator<basic_static_string>::start_lifetime(*this);
    }

    constexpr basic_static_string(basic_static_string&& o) noexcept : m_len {o.m_len} {
        uninitialized_copy_n(o.begin(), m_len, get_storage().begin);
        AsanAnnotator<basic_static_string>::start_lifetime(*this);
    }

    // NOLINTNEXTLINE(cert-oop54-cpp)
    constexpr auto operator=(const basic_static_string& o) noexcept -> basic_static_string& {
        AsanAnnotator<basic_static_string>& asan_annotator {*this};
        m_len = o.m_len;
        asan_annotator.allow_full_access();
        uninitialized_copy_n(o.begin(), m_len, get_storage().begin);
        return *this;
    }

    constexpr auto operator=(basic_static_string&& o) noexcept -> basic_static_string& {
        AsanAnnotator<basic_static_string>& asan_annotator {*this};
        m_len = o.m_len;
        asan_annotator.allow_full_access();
        uninitialized_copy_n(o.begin(), m_len, get_storage().begin);
        return *this;
    }

    friend constexpr void swap(basic_static_string& a, basic_static_string& b) noexcept {
        a.swap(b);
    }

    constexpr void swap(basic_static_string& o) noexcept {
        detail::swap_contiguous_static_containers(*this, o);
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
    friend class detail::string_ops<
        CharT,
        Traits,
        detail::size_t<Capacity>,
        basic_static_string<CharT, Capacity, Traits>>;

    template<typename Container>
    friend constexpr void
    detail::swap_contiguous_static_containers(Container& a, Container& b) noexcept;

    [[nodiscard]] constexpr auto get_storage() const noexcept
        -> detail::string_storage<const CharT> {
        return {
            .begin = m_chars.data(),
            .end = m_chars.data() + m_len,
            .end_cap = m_chars.data() + Capacity};
    }
    constexpr auto get_storage() noexcept -> detail::string_storage<CharT> {
        return {
            .begin = m_chars.data(),
            .end = m_chars.data() + m_len,
            .end_cap = m_chars.data() + Capacity};
    }

    constexpr auto grow(usize req_len, AsanAnnotator<basic_static_string>& asan_annotator) noexcept
        -> expected<void, Error> {
        if (req_len > Capacity) [[unlikely]] {
            Throw(Error::BufferFull);
        }
        asan_annotator.allow_full_access();
        return {};
    }
    constexpr auto
    grow_uninit(usize req_len, AsanAnnotator<basic_static_string>& asan_annotator) noexcept
        -> expected<void, Error> {
        return grow(req_len, asan_annotator);
    }

    constexpr auto set_len(size_type new_len) noexcept {
        assert(new_len <= Capacity && "length cannot exceed capacity");
        m_len = new_len;
    }

    template<typename Container>
    friend class detail::RealAsanAnnotator;

    constexpr auto get_storage_for_asan_annotator() const noexcept
        -> detail::string_storage<const CharT> {
        return get_storage();
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