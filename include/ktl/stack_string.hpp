#pragma once

#include <ktl/access.hpp>

#include "detail/string_ops.hpp"

namespace ktl {
template<typename CharT, auto Capacity, typename Traits = std::char_traits<CharT>>
    requires std::is_trivial_v<CharT> && std::integral<std::decay_t<decltype(Capacity)>>
class basic_stack_string;

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
        constexpr operator basic_stack_string<CharT, Capacity, TraitsT>() const noexcept {
            return str();
        }

        constexpr auto str() const noexcept -> basic_stack_string<CharT, Capacity, TraitsT> {
            basic_stack_string<CharT, Capacity, TraitsT> str;

            [[maybe_unused]] auto res = str.assign(m_substr.begin(), m_substr.end());
            assert(res);

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
class basic_stack_string:
    public detail::string_ops<
        CharT,
        Traits,
        detail::size_t<Capacity>,
        basic_stack_string<CharT, Capacity, Traits>> {
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
        basic_stack_string<CharT, Capacity, Traits>>;

  public:
    constexpr basic_stack_string() = default;

    template<typename CharU, auto Cap, typename TraitsT>
        requires(std::same_as<CharU, CharT> && Cap == Capacity && std::same_as<Traits, TraitsT>)
    // NOLINTNEXTLINE(*-explicit-conversions)
    constexpr basic_stack_string(detail::substr_proxy<CharU, Cap, TraitsT> proxy) {
        auto view = proxy.view();
        assert(Capacity > view.length());

        std::copy(view.begin(), view.end(), m_chars.data());
        m_len = view.length() + 1;
        at(m_chars, m_len) = base::NUL;
    }

    // NOLINTNEXTLINE(*-explicit-conversions)
    constexpr basic_stack_string(const CharT (&str)[Capacity]) : m_len {Capacity} {
        std::copy(std::begin(str), std::end(str), m_chars.data());
    }

    constexpr auto max_size() const noexcept -> size_type {
        return Capacity - 1;
    }

    // NOLINTNEXTLINE(*-explicit-conversions)
    constexpr operator fixed_string<value_type, size_type, Traits>() const noexcept {
        return fixed_string {this->data(), this->capacity(), this->size()};
    }

    // --------------------- Assign ---------------------

    using base::assign;

    template<std::random_access_iterator RandAccIt>
    constexpr auto assign(RandAccIt first, RandAccIt last) noexcept
        -> expected<typename base::non_null_ptr, Error> {
        return base::assign(first, last);
    }

    template<std::input_iterator InputIter>
        requires std::convertible_to<std::iter_value_t<InputIter>, CharT>
        && (!std::random_access_iterator<InputIter>)
    constexpr auto assign(InputIter first, InputIter last) noexcept
        -> expected<typename base::non_null_ptr, std::pair<InputIter, Error>> {
        if (this->empty()) {
            TryV(this->assign_iter(first, last));
        } else {
            basic_stack_string tmp;

            TryV(tmp.assign_iter(first, last));
            *this = tmp;
        }

        return typename base::non_null_ptr {*this};
    }

    // --------------------- Insert ---------------------

    using base::insert;

    template<std::random_access_iterator RandAccIt>
        requires std::convertible_to<std::iter_value_t<RandAccIt>, CharT>
    constexpr auto insert(typename base::const_iterator pos, RandAccIt first, RandAccIt last)
        -> expected<typename base::non_null_ptr, Error> {
        return base::insert(pos, first, last);
    }

    template<std::input_iterator InputIter>
        requires std::convertible_to<std::iter_value_t<InputIter>, CharT>
    constexpr auto insert(typename base::const_iterator pos, InputIter first, InputIter last)
        -> expected<typename base::non_null_ptr, std::pair<InputIter, Error>> {
        if (pos == base::end()) {
            return base::insert_at_end(first, last);
        }

        basic_stack_string tmp;
        auto tmp_res = tmp.assign(first, last);
        auto res = base::insert(pos, tmp.begin(), tmp.end());

        // All rows inseted into `tmp` vector? If so, return the `res`.
        if (tmp_res || !res) {
            Throw(std::make_pair(last, std::move(res).error()));
        }
        // If, all rows were not inserted into the `tmp` vector, error must be returned.
        return tmp_res;
    }

    // --------------------- Replace ---------------------

    using base::replace;

    template<std::random_access_iterator RandAccIt>
        requires std::convertible_to<std::iter_value_t<RandAccIt>, CharT>
    constexpr auto replace(
        typename base::const_iterator first,
        typename base::const_iterator last,
        RandAccIt first2,
        RandAccIt last2) noexcept -> expected<typename base::non_null_ptr, Error> {
        return base::replace(first, last, first2, last2);
    }

    template<std::input_iterator InputIt>
        requires std::convertible_to<std::iter_value_t<InputIt>, CharT>
    constexpr auto replace(
        typename base::const_iterator first,
        typename base::const_iterator last,
        InputIt first2,
        InputIt last2) noexcept
        -> expected<typename base::non_null_ptr, std::pair<InputIt, Error>> {
        basic_stack_string tmp;

        if (auto res = tmp.append(this->cbegin(), first); !res) {
            Throw(std::make_pair(first2, std::move(res).error()));
        }
        TryV(tmp.append(first2, last2));
        if (auto res = tmp.append(last, this->cend()); !res) {
            Throw(std::make_pair(last2, std::move(res).error()));
        }

        *this = tmp;

        return typename base::non_null_ptr {*this};
    }

    // --------------------- Substr ---------------------

    constexpr auto substr(size_type pos = 0, size_type count = base::npos) const& noexcept
        -> expected<detail::substr_proxy<CharT, Capacity, Traits>, Error> {
        Try(ss, basic_string_view {*this}.substr(pos, count));
        return ss;
    }

    constexpr auto substr(size_type pos = 0, size_type count = base::npos) && noexcept
        -> expected<typename base::non_null_ptr, Error> {
        auto [beg, end, _] = get_storage();
        auto size = end - beg - 1;

        if (pos > size) [[unlikely]] {
            Throw(Error::IndexOutOfBounds);
        }

        count = std::min<size_type>(count, size - pos);

        if (pos > 0) {
            end = std::move(beg + pos, beg + pos + count, beg);
        }
        beg[count] = base::NUL;
        m_len = count + 1;

        return typename base::non_null_ptr {*this};
    }

  private:
    // Allow access to internal members. Classic CRTP.
    friend class detail::string_ops<
        CharT,
        Traits,
        detail::size_t<Capacity>,
        basic_stack_string<CharT, Capacity, Traits>>;

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

    constexpr auto grow(usize req_len) noexcept -> expected<void, Error> {
        if (req_len > Capacity) [[unlikely]] {
            Throw(Error::BufferFull);
        }
        return {};
    }
    constexpr auto grow_uninit(usize req_len) noexcept -> expected<void, Error> {
        return grow(req_len);
    }

    constexpr auto set_len(size_type new_len) noexcept {
        assert(new_len <= Capacity && "length cannot exceed capacity");
        m_len = new_len;
    }

    std::array<CharT, Capacity> m_chars = {base::NUL};
    size_type m_len = 1;
};

template<typename CharT, auto Capacity, typename Traits = std::char_traits<CharT>>
basic_stack_string(detail::substr_proxy<CharT, Capacity, Traits>)
    -> basic_stack_string<CharT, Capacity, Traits>;

template<auto Capacity>
using stack_string = basic_stack_string<char, Capacity>;
template<auto Capacity>
using stack_u8string = basic_stack_string<char8_t, Capacity>;
template<auto Capacity>
using stack_u16string = basic_stack_string<char16_t, Capacity>;
template<auto Capacity>
using stack_u32string = basic_stack_string<char32_t, Capacity>;
template<auto Capacity>
using stack_wstring = basic_stack_string<wchar_t, Capacity>;

template<typename CharT, auto N>
constexpr auto make_stack_string(const CharT (&chars)[N]) noexcept -> basic_stack_string<CharT, N> {
    using str_t = basic_stack_string<CharT, N>;
    str_t str;

    str.assign(chars, N - 1);
    return str;
}

template<auto VCapacity, typename CharT, auto N>
constexpr auto make_stack_string(const CharT (&chars)[N]) noexcept
    -> basic_stack_string<CharT, VCapacity> {
    static_assert(VCapacity >= N);

    using str_t = basic_stack_string<CharT, VCapacity>;
    str_t str;

    str.assign(chars, N - 1);

    return str;
}
}  // namespace ktl