#pragma once

#include <compare>
#include <limits>
#include <ranges>
#include <type_traits>

#include "assert.hpp"
#include "contiguous_iterator.hpp"
#include "error.hpp"
#include "expected.hpp"
#include "hash.hpp"
#include "int.hpp"

namespace ktl {
template<typename CharT, typename Traits = std::char_traits<CharT>>
class basic_string_view {
  public:
    // types
    using traits_type = Traits;
    using value_type = CharT;
    using pointer = CharT*;
    using const_pointer = const CharT*;
    using reference = CharT&;
    using const_reference = const CharT&;
    using const_iterator = contiguous_iterator<const CharT>;  // See [string.view.iterators]
    using iterator = const_iterator;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;
    using reverse_iterator = const_reverse_iterator;
    using size_type = usize;
    using difference_type = isize;

    // NOLINTNEXTLINE(*-dynamic-static-initializers)
    static constexpr const size_type npos = -1;  // size_type(-1);

    static_assert(
        (!std::is_array_v<value_type>),
        "Character type of basic_string_view must not be an array");
    static_assert(
        (std::is_standard_layout_v<value_type>),
        "Character type of basic_string_view must be standard-layout");
    static_assert(
        (std::is_trivial_v<value_type>),
        "Character type of basic_string_view must be trivial");
    static_assert(
        (std::is_same_v<CharT, typename traits_type::char_type>),
        "traits_type::char_type must be the same type as CharT");

    // [string.view.cons], construct/copy
    constexpr inline basic_string_view() noexcept : m_data {nullptr}, m_size {0} {}

    constexpr basic_string_view(const basic_string_view&) noexcept = default;
    constexpr auto operator=(const basic_string_view&) noexcept -> basic_string_view& = default;
    constexpr basic_string_view(basic_string_view&&) noexcept = default;
    constexpr auto operator=(basic_string_view&&) noexcept -> basic_string_view& = default;
    constexpr ~basic_string_view() = default;

    constexpr inline basic_string_view(const CharT* s, size_type len) noexcept :
        m_data {s},
        m_size {len} {
        check_(
            len == 0 || s != nullptr,
            "string_view::string_view(CharT *, usize): received nullptr");
    }

    template<std::contiguous_iterator It, std::sized_sentinel_for<It> End>
        requires(std::same_as<std::iter_value_t<It>, CharT> && !std::convertible_to<End, size_type>)
    constexpr basic_string_view(It begin, End end) noexcept :
        m_data {std::to_address(begin)},
        m_size {end - begin} {
        check_(
            (end - begin) >= 0,
            "std::string_view::string_view(iterator, sentinel) received invalid range");
    }

    template<class Range>
        requires(!std::same_as<std::remove_cvref_t<Range>, basic_string_view>
                 && std::ranges::contiguous_range<Range> && std::ranges::sized_range<Range>
                 && std::same_as<std::ranges::range_value_t<Range>, CharT>
                 && !std::convertible_to<Range, const CharT*>
                 && (!requires(std::remove_cvref_t<Range> & d) {
                          d.operator std::basic_string_view<CharT, Traits>();
                      }))
    // NOLINTNEXTLINE(*-forwarding-reference-overload)
    constexpr explicit basic_string_view(Range&& r) noexcept :
        m_data {std::ranges::data(r)},
        m_size {std::ranges::size(r)} {}

    // NOLINTNEXTLINE(*-explicit-conversions)
    constexpr inline basic_string_view(const CharT* s) noexcept :
        m_data {s},
        m_size {length_checked(s)} {}

    basic_string_view(std::nullptr_t) = delete;

    // [string.view.iterators], iterators
    [[nodiscard]] constexpr inline auto begin() const noexcept -> const_iterator {
        return cbegin();
    }

    [[nodiscard]] constexpr inline auto end() const noexcept -> const_iterator {
        return cend();
    }

    [[nodiscard]] constexpr inline auto cbegin() const noexcept -> const_iterator {
        return make_contiguous_iterator(m_data, m_data, m_data + m_size);
    }

    [[nodiscard]] constexpr inline auto cend() const noexcept -> const_iterator {
        return make_contiguous_iterator(m_data + m_size, m_data, m_data + m_size);
    }

    [[nodiscard]] constexpr inline auto rbegin() const noexcept -> const_reverse_iterator {
        return const_reverse_iterator(cend());
    }

    [[nodiscard]] constexpr inline auto rend() const noexcept -> const_reverse_iterator {
        return const_reverse_iterator(cbegin());
    }

    [[nodiscard]] constexpr inline auto crbegin() const noexcept -> const_reverse_iterator {
        return const_reverse_iterator(cend());
    }

    [[nodiscard]] constexpr inline auto crend() const noexcept -> const_reverse_iterator {
        return const_reverse_iterator(cbegin());
    }

    // [string.view.capacity], capacity
    [[nodiscard]] constexpr inline auto size() const noexcept -> size_type {
        return m_size;
    }

    [[nodiscard]] constexpr inline auto length() const noexcept -> size_type {
        return m_size;
    }

    [[nodiscard]] constexpr inline auto max_size() const noexcept -> size_type {
        return std::numeric_limits<size_type>::max() / sizeof(value_type);
    }

    [[nodiscard]] inline constexpr auto empty() const noexcept -> bool {
        return m_size == 0;
    }

    // [string.view.access], element access
    constexpr inline auto operator[](size_type pos) const noexcept -> const_reference {
        check_(pos < size(), "string_view[] index out of bounds");
        return m_data[pos];
    }

    [[nodiscard]] constexpr inline auto at(size_type pos) const noexcept
        -> expected<std::reference_wrapper<const value_type>, Error> {
        if (pos >= size()) {
            return make_unexpected(Error::IndexOutOfBounds);
        }
        return m_data[pos];
    }

    [[nodiscard]] constexpr inline auto front() const noexcept -> const_reference {
        return check_(!empty(), "string_view::front(): string is empty"), m_data[0];
    }

    [[nodiscard]] constexpr inline auto back() const noexcept -> const_reference {
        return check_(!empty(), "string_view::back(): string is empty"), m_data[m_size - 1];
    }

    [[nodiscard]] constexpr inline auto data() const noexcept -> const_pointer {
        return m_data;
    }

    // [string.view.modifiers], modifiers:
    constexpr inline void remove_prefix(size_type n) noexcept {
        check_(n <= size(), "remove_prefix() can't remove more than size()");
        m_data += n;
        m_size -= n;
    }

    constexpr inline void remove_suffix(size_type n) noexcept {
        check_(n <= size(), "remove_suffix() can't remove more than size()");
        m_size -= n;
    }

    constexpr inline void swap(basic_string_view& other) noexcept {
        const value_type* p = m_data;
        m_data = other.m_data;
        other.m_data = p;

        size_type sz = m_size;
        m_size = other.m_size;
        other.m_size = sz;
    }

    inline constexpr auto copy(CharT* s, size_type n, size_type pos = 0) const noexcept
        -> expected<size_type, Error> {
        if (pos > size()) {
            return make_unexpected(Error::IndexOutOfBounds);
        }
        size_type rlen = std::min(n, size() - pos);
        Traits::copy(s, data() + pos, rlen);
        return rlen;
    }

    [[nodiscard]] constexpr inline auto substr(size_type pos = 0, size_type n = npos) const noexcept
        -> expected<basic_string_view, Error> {
        if (pos > size()) {
            return make_unexpected(Error::IndexOutOfBounds);
        }

        return basic_string_view {data() + pos, std::min(n, size() - pos)};
    }

    [[nodiscard]] constexpr auto compare(basic_string_view sv) const noexcept -> int {
        size_type rlen = std::min(size(), sv.size());
        int retval = Traits::compare(data(), sv.data(), rlen);
        if (retval == 0)  // first rlen chars matched
            retval = size() == sv.size() ? 0 : (size() < sv.size() ? -1 : 1);
        return retval;
    }

    [[nodiscard]] constexpr inline auto
    compare(size_type pos1, size_type n1, basic_string_view sv) const noexcept
        -> expected<int, Error> {
        return Try(substr(pos1, n1)).compare(sv);
    }

    [[nodiscard]] constexpr inline auto
    compare(size_type pos1, size_type n1, basic_string_view sv, size_type pos2, size_type n2)
        const noexcept -> expected<int, Error> {
        return Try(substr(pos1, n1)).compare(Try(sv.substr(pos2, n2)));
    }

    constexpr inline auto compare(const CharT* s) const noexcept -> int {
        return compare(s);
    }

    constexpr inline auto compare(size_type pos1, size_type n1, const CharT* s) const noexcept
        -> expected<int, Error> {
        return Try(substr(pos1, n1)).compare(s);
    }

    constexpr inline auto
    compare(size_type pos1, size_type n1, const CharT* s, size_type n2) const noexcept
        -> expected<int, Error> {
        return Try(substr(pos1, n1)).compare(s, n2);
    }

    // find
    [[nodiscard]] constexpr inline auto find(basic_string_view s, size_type pos = 0) const noexcept
        -> size_type {
        check_(s.size() == 0 || s.data() != nullptr, "string_view::find(): received nullptr");

        if (size() < s.size()) {
            return npos;
        }

        auto ipos = std::search(begin() + pos, end(), s.begin(), s.end());
        return ipos == end() ? npos : std::distance(begin(), ipos);
    }

    [[nodiscard]] constexpr inline auto find(CharT c, size_type pos = 0) const noexcept
        -> size_type {
        return find({std::addressof(c), 1}, pos);
    }

    constexpr inline auto find(const CharT* s, size_type pos, size_type n) const noexcept
        -> size_type {
        check_(n == 0 || s != nullptr, "string_view::find(): received nullptr");
        return find(basic_string_view {s, n}, pos);
    }

    constexpr inline auto find(const CharT* s, size_type pos = 0) const noexcept -> size_type {
        check_(s != nullptr, "string_view::find(): received nullptr");
        return find(basic_string_view {s}, pos);
    }

    // rfind
    [[nodiscard]] constexpr inline auto
    rfind(basic_string_view s, size_type pos = npos) const noexcept -> size_type {
        check_(s.size() == 0 || s.data() != nullptr, "string_view::find(): received nullptr");

        auto my_len = length();
        auto s_len = s.length();

        pos = std::min(pos, my_len);
        pos = s_len < my_len - pos ? pos + s_len : my_len;

        auto start = begin() + pos;
        auto it = std::find_end(begin(), start, s.begin(), s.end(), Traits::eq);
        if (s_len > 0 && it == start)
            return npos;
        return std::distance(begin(), it);
    }

    [[nodiscard]] constexpr inline auto rfind(CharT c, size_type pos = npos) const noexcept
        -> size_type {
        return rfind(basic_string_view {std::addressof(c), 1}, pos);
    }

    constexpr inline auto rfind(const CharT* s, size_type pos, size_type n) const noexcept
        -> size_type {
        check_(n == 0 || s != nullptr, "string_view::rfind(): received nullptr");
        return rfind(basic_string_view {s, n}, pos);
    }

    constexpr inline auto rfind(const CharT* s, size_type pos = npos) const noexcept -> size_type {
        check_(s != nullptr, "string_view::rfind(): received nullptr");
        return rfind(basic_string_view {s}, pos);
    }

    // find_first_of
    [[nodiscard]] constexpr inline auto
    find_first_of(basic_string_view s, size_type pos = 0) const noexcept -> size_type {
        check_(
            s.size() == 0 || s.data() != nullptr,
            "string_view::find_first_of(): received nullptr");

        if (pos >= size() || s.size() == 0)
            return npos;

        auto it = std::find_first_of(begin() + pos, end(), s.begin(), s.end(), Traits::eq);
        if (it == end())
            return npos;
        return std::distance(begin(), it);
    }

    [[nodiscard]] constexpr inline auto find_first_of(CharT c, size_type pos = 0) const noexcept
        -> size_type {
        return find_first_of(basic_string_view {std::addressof(c), 1}, pos);
    }

    constexpr inline auto find_first_of(const CharT* s, size_type pos, size_type n) const noexcept
        -> size_type {
        check_(n == 0 || s != nullptr, "string_view::find_first_of(): received nullptr");
        return find_first_of(basic_string_view {s, n}, pos);
    }

    constexpr inline auto find_first_of(const CharT* s, size_type pos = 0) const noexcept
        -> size_type {
        check_(s != nullptr, "string_view::find_first_of(): received nullptr");
        return find_first_of(basic_string_view {s}, pos);
    }

    // find_last_of
    [[nodiscard]] constexpr inline auto
    find_last_of(basic_string_view s, size_type pos = npos) const noexcept -> size_type {
        check_(
            s.size() == 0 || s.data() != nullptr,
            "string_view::find_last_of(): received nullptr");

        if (s.empty()) {
            return npos;
        }

        pos = pos < size() ? pos + 1 : size();

        auto my_beg = begin();
        auto s_beg = s.data();
        auto s_len = s.length();
        for (auto it = my_beg + pos; it != my_beg;) {
            if (Traits::find(s_beg, s_len, *--it) != nullptr) {
                return std::distance(my_beg, it);
            }
        }
        return npos;
    }

    [[nodiscard]] constexpr inline auto find_last_of(CharT c, size_type pos = npos) const noexcept
        -> size_type {
        return find_last_of(basic_string_view {std::addressof(c), 1}, pos);
    }

    constexpr inline auto find_last_of(const CharT* s, size_type pos, size_type n) const noexcept
        -> size_type {
        check_(n == 0 || s != nullptr, "string_view::find_last_of(): received nullptr");
        return find_last_of(basic_string_view {s, n}, pos);
    }

    constexpr inline auto find_last_of(const CharT* s, size_type pos = npos) const noexcept
        -> size_type {
        check_(s != nullptr, "string_view::find_last_of(): received nullptr");
        return find_last_of(basic_string_view {s}, pos);
    }

    // find_first_not_of
    [[nodiscard]] constexpr inline auto
    find_first_not_of(basic_string_view s, size_type pos = 0) const noexcept -> size_type {
        check_(
            s.size() == 0 || s.data() != nullptr,
            "string_view::find_first_not_of(): received nullptr");

        if (pos < size()) {
            auto my_beg = begin();
            auto my_end = end();
            auto s_beg = s.data();
            auto s_len = s.length();
            for (auto it = my_beg + pos; it != my_end; ++it) {
                if (Traits::find(s_beg, s_len, *it) == nullptr) {
                    return std::distance(my_beg, it);
                }
            }
        }
        return npos;
    }

    [[nodiscard]] constexpr inline auto find_first_not_of(CharT c, size_type pos = 0) const noexcept
        -> size_type {
        return find_first_not_of(basic_string_view {std::addressof(c), 1}, pos);
    }

    constexpr inline auto
    find_first_not_of(const CharT* s, size_type pos, size_type n) const noexcept -> size_type {
        check_(n == 0 || s != nullptr, "string_view::find_first_not_of(): received nullptr");
        return find_first_not_of(basic_string_view {s, n}, pos);
    }

    constexpr inline auto find_first_not_of(const CharT* s, size_type pos = 0) const noexcept
        -> size_type {
        check_(s != nullptr, "string_view::find_first_not_of(): received nullptr");
        return find_first_not_of(basic_string_view {s}, pos);
    }

    // find_last_not_of
    [[nodiscard]] constexpr inline auto
    find_last_not_of(basic_string_view s, size_type pos = npos) const noexcept -> size_type {
        check_(
            s.size() == 0 || s.data() != nullptr,
            "string_view::find_last_not_of(): received nullptr");

        pos = pos < size() ? pos + 1 : size();

        auto my_beg = begin();
        auto s_beg = s.data();
        auto s_len = s.length();
        for (auto it = my_beg + pos; it != my_beg;) {
            if (Traits::find(s_beg, s_len, *--it) == nullptr) {
                return std::distance(my_beg, it);
            }
        }
        return npos;
    }

    [[nodiscard]] constexpr inline auto
    find_last_not_of(CharT c, size_type pos = npos) const noexcept -> size_type {
        return find_last_not_of(basic_string_view {std::addressof(c), 1}, pos);
    }

    constexpr inline auto
    find_last_not_of(const CharT* s, size_type pos, size_type n) const noexcept -> size_type {
        check_(n == 0 || s != nullptr, "string_view::find_last_not_of(): received nullptr");
        return find_last_not_of(basic_string_view {s, n}, pos);
    }

    constexpr inline auto find_last_not_of(const CharT* s, size_type pos = npos) const noexcept
        -> size_type {
        check_(s != nullptr, "string_view::find_last_not_of(): received nullptr");
        return find_last_not_of(basic_string_view {s}, pos);
    }

    [[nodiscard]] constexpr inline auto starts_with(basic_string_view s) const noexcept -> bool {
        return size() >= s.size() && compare(0, s.size(), s) == 0;
    }

    [[nodiscard]] constexpr inline auto starts_with(value_type c) const noexcept -> bool {
        return !empty() && Traits::eq(front(), c);
    }

    constexpr inline auto starts_with(const value_type* s) const noexcept -> bool {
        return starts_with(basic_string_view {s});
    }

    [[nodiscard]] constexpr inline auto ends_with(basic_string_view s) const noexcept -> bool {
        return size() >= s.size() && compare(size() - s.size(), npos, s) == 0;
    }

    [[nodiscard]] constexpr inline auto ends_with(value_type c) const noexcept -> bool {
        return !empty() && Traits::eq(back(), c);
    }

    constexpr inline auto ends_with(const value_type* s) const noexcept -> bool {
        return ends_with(basic_string_view {s});
    }

    [[nodiscard]] constexpr inline auto contains(basic_string_view sv) const noexcept -> bool {
        return find(sv) != npos;
    }

    [[nodiscard]] constexpr inline auto contains(value_type c) const noexcept -> bool {
        return find(c) != npos;
    }

    constexpr inline auto contains(const value_type* s) const noexcept -> bool {
        return find(s) != npos;
    }

  private:
    static constexpr inline auto length_checked(const value_type* s) noexcept -> usize {
        check_(
            s != nullptr,
            "null pointer passed to non-null argument of char_traits<...>::length");
        return Traits::length(s);
    }

    const value_type* m_data;
    size_type m_size;
};
}  // namespace ktl

namespace std {
template<typename CharT, typename Traits>
inline constexpr bool std::ranges::enable_view<ktl::basic_string_view<CharT, Traits>> = true;

template<typename CharT, typename Traits>
inline constexpr bool std::ranges::enable_borrowed_range<ktl::basic_string_view<CharT, Traits>> =
    true;
}  // namespace std

namespace ktl {
// [string.view.deduct]

template<std::contiguous_iterator It, std::sized_sentinel_for<It> End>
basic_string_view(It, End) -> basic_string_view<std::iter_value_t<It>>;

template<std::ranges::contiguous_range Range>
basic_string_view(Range) -> basic_string_view<std::ranges::range_value_t<Range>>;

// [string.view.comparison]
// operator ==
template<typename CharT, typename Traits>
constexpr inline auto
operator==(basic_string_view<CharT, Traits> lhs, basic_string_view<CharT, Traits> rhs) noexcept
    -> bool {
    if (lhs.size() != rhs.size())
        return false;
    return lhs.compare(rhs) == 0;
}

// The dummy default template parameters are used to work around a MSVC issue with mangling, see
// VSO-409326 for details. This applies to the other sufficient overloads below for the other
// comparison operators.
template<typename CharT, typename Traits>
constexpr inline auto operator==(
    basic_string_view<CharT, Traits> lhs,
    std::type_identity_t<basic_string_view<CharT, Traits>> rhs) noexcept -> bool {
    if (lhs.size() != rhs.size())
        return false;
    return lhs.compare(rhs) == 0;
}

template<typename CharT, typename Traits>
constexpr auto
operator<=>(basic_string_view<CharT, Traits> lhs, basic_string_view<CharT, Traits> rhs) noexcept {
    if constexpr (requires { typename Traits::comparison_category; }) {
        // [string.view]/4
        static_assert(
            !std::is_void_v<
                std::common_comparison_category_t<typename Traits::comparison_category>>,
            "return type is not a comparison category type");
        return static_cast<typename Traits::comparison_category>(lhs.compare(rhs) <=> 0);
    } else {
        return static_cast<std::weak_ordering>(lhs.compare(rhs) <=> 0);
    }
}

template<typename CharT, typename Traits, int = 1>
constexpr auto operator<=>(
    basic_string_view<CharT, Traits> lhs,
    std::type_identity_t<basic_string_view<CharT, Traits>> rhs) noexcept {
    if constexpr (requires { typename Traits::comparison_category; }) {
        // [string.view]/4
        static_assert(
            !std::is_void_v<
                std::common_comparison_category_t<typename Traits::comparison_category>>,
            "return type is not a comparison category type");
        return static_cast<typename Traits::comparison_category>(lhs.compare(rhs) <=> 0);
    } else {
        return static_cast<std::weak_ordering>(lhs.compare(rhs) <=> 0);
    }
}

// operator !=
template<typename CharT, typename Traits>
constexpr inline auto
operator!=(basic_string_view<CharT, Traits> lhs, basic_string_view<CharT, Traits> rhs) noexcept
    -> bool {
    if (lhs.size() != rhs.size())
        return true;
    return lhs.compare(rhs) != 0;
}

template<typename CharT, typename Traits, int = 1>
constexpr inline auto operator!=(
    basic_string_view<CharT, Traits> lhs,
    std::type_identity_t<basic_string_view<CharT, Traits>> rhs) noexcept -> bool {
    if (lhs.size() != rhs.size())
        return true;
    return lhs.compare(rhs) != 0;
}

template<typename CharT, typename Traits, int = 2>
constexpr inline auto operator!=(
    std::type_identity_t<basic_string_view<CharT, Traits>> lhs,
    basic_string_view<CharT, Traits> rhs) noexcept -> bool {
    if (lhs.size() != rhs.size())
        return true;
    return lhs.compare(rhs) != 0;
}

// operator <
template<typename CharT, typename Traits>
constexpr inline auto
operator<(basic_string_view<CharT, Traits> lhs, basic_string_view<CharT, Traits> rhs) noexcept
    -> bool {
    return lhs.compare(rhs) < 0;
}

template<typename CharT, typename Traits, int = 1>
constexpr inline auto operator<(
    basic_string_view<CharT, Traits> lhs,
    std::type_identity_t<basic_string_view<CharT, Traits>> rhs) noexcept -> bool {
    return lhs.compare(rhs) < 0;
}

template<typename CharT, typename Traits, int = 2>
constexpr inline auto operator<(
    std::type_identity_t<basic_string_view<CharT, Traits>> lhs,
    basic_string_view<CharT, Traits> rhs) noexcept -> bool {
    return lhs.compare(rhs) < 0;
}

// operator >
template<typename CharT, typename Traits>
constexpr inline auto
operator>(basic_string_view<CharT, Traits> lhs, basic_string_view<CharT, Traits> rhs) noexcept
    -> bool {
    return lhs.compare(rhs) > 0;
}

template<typename CharT, typename Traits, int = 1>
constexpr inline auto operator>(
    basic_string_view<CharT, Traits> lhs,
    std::type_identity_t<basic_string_view<CharT, Traits>> rhs) noexcept -> bool {
    return lhs.compare(rhs) > 0;
}

template<typename CharT, typename Traits, int = 2>
constexpr inline auto operator>(
    std::type_identity_t<basic_string_view<CharT, Traits>> lhs,
    basic_string_view<CharT, Traits> rhs) noexcept -> bool {
    return lhs.compare(rhs) > 0;
}

// operator <=
template<typename CharT, typename Traits>
constexpr inline auto
operator<=(basic_string_view<CharT, Traits> lhs, basic_string_view<CharT, Traits> rhs) noexcept
    -> bool {
    return lhs.compare(rhs) <= 0;
}

template<typename CharT, typename Traits, int = 1>
constexpr inline auto operator<=(
    basic_string_view<CharT, Traits> lhs,
    std::type_identity_t<basic_string_view<CharT, Traits>> rhs) noexcept -> bool {
    return lhs.compare(rhs) <= 0;
}

template<typename CharT, typename Traits, int = 2>
constexpr inline auto operator<=(
    std::type_identity_t<basic_string_view<CharT, Traits>> lhs,
    basic_string_view<CharT, Traits> rhs) noexcept -> bool {
    return lhs.compare(rhs) <= 0;
}

// operator >=
template<typename CharT, typename Traits>
constexpr inline auto
operator>=(basic_string_view<CharT, Traits> lhs, basic_string_view<CharT, Traits> rhs) noexcept
    -> bool {
    return lhs.compare(rhs) >= 0;
}

template<typename CharT, typename Traits, int = 1>
constexpr inline auto operator>=(
    basic_string_view<CharT, Traits> lhs,
    std::type_identity_t<basic_string_view<CharT, Traits>> rhs) noexcept -> bool {
    return lhs.compare(rhs) >= 0;
}

template<typename CharT, typename Traits, int = 2>
constexpr inline auto operator>=(
    std::type_identity_t<basic_string_view<CharT, Traits>> lhs,
    basic_string_view<CharT, Traits> rhs) noexcept -> bool {
    return lhs.compare(rhs) >= 0;
}
}  // namespace ktl

namespace std {
template<>
struct hash<ktl::basic_string_view<char, std::char_traits<char>>>:
    wy::internal::hash_string_base<ktl::basic_string_view<char, std::char_traits<char>>> {};

template<>
struct hash<ktl::basic_string_view<char8_t, std::char_traits<char8_t>>>:
    wy::internal::hash_string_base<ktl::basic_string_view<char8_t, std::char_traits<char8_t>>> {};

template<>
struct hash<ktl::basic_string_view<char16_t, std::char_traits<char16_t>>>:
    wy::internal::hash_string_base<ktl::basic_string_view<char16_t, std::char_traits<char16_t>>> {};

template<>
struct hash<ktl::basic_string_view<char32_t, std::char_traits<char32_t>>>:
    wy::internal::hash_string_base<ktl::basic_string_view<char32_t, std::char_traits<char32_t>>> {};

template<>
struct hash<ktl::basic_string_view<wchar_t, std::char_traits<wchar_t>>>:
    wy::internal::hash_string_base<ktl::basic_string_view<wchar_t, std::char_traits<wchar_t>>> {};
}  // namespace std

namespace ktl {
inline namespace literals {
    inline constexpr auto operator"" _sv(const char* str, usize len) noexcept
        -> basic_string_view<char> {
        return {str, len};
    }

    inline constexpr auto operator"" _sv(const wchar_t* str, usize len) noexcept
        -> basic_string_view<wchar_t> {
        return {str, len};
    }

    inline constexpr auto operator"" _sv(const char8_t* str, usize len) noexcept
        -> basic_string_view<char8_t> {
        return {str, len};
    }

    inline constexpr auto operator"" _sv(const char16_t* str, usize len) noexcept
        -> basic_string_view<char16_t> {
        return {str, len};
    }

    inline constexpr auto operator"" _sv(const char32_t* str, usize len) noexcept
        -> basic_string_view<char32_t> {
        return {str, len};
    }
}  // namespace literals

using string_view = basic_string_view<char>;
using wstring_view = basic_string_view<wchar_t>;
using u8string_view = basic_string_view<char8_t>;
using u16string_view = basic_string_view<char16_t>;
using u32string_view = basic_string_view<char32_t>;
}  // namespace ktl
