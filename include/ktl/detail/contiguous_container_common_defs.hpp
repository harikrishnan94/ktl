#pragma once

#include <algorithm>
#include <concepts>

namespace ktl {
namespace detail {
    template<typename T>
    struct valid_type: std::true_type {};

    template<typename Container>
    concept erasable = requires(Container c) {
        { valid_type<typename Container::value_type>::value };
        { std::integral<typename Container::size_type> };
        { std::begin(c) } -> std::contiguous_iterator;
        { std::end(c) } -> std::contiguous_iterator;
        {
            std::same_as<std::iter_value_t<decltype(std::begin(c))>, typename Container::value_type>
        };
        { c.erase(std::begin(c), std::end(c)) };
    };
}  // namespace detail

template<detail::erasable Container, typename T>
    requires std::equality_comparable_with<T, typename Container::value_type>
constexpr auto erase(Container& c, const T& value) -> typename Container::size_type {
    auto end = std::end(c);
    auto it = std::remove(std::begin(c), end, value);
    auto r = std::distance(it, end);
    c.erase(it, end);
    return r;
}

template<detail::erasable Container, typename Pred>
    requires std::convertible_to<std::invoke_result_t<Pred, typename Container::value_type>, bool>
constexpr auto erase_if(Container& c, Pred pred) -> typename Container::size_type {
    auto end = std::end(c);
    auto it = std::remove_if(std::begin(c), end, pred);
    auto r = std::distance(it, end);
    c.erase(it, end);
    return r;
}

namespace detail {
    // Very specific implementation, shared by both static_string and static_vector
    template<std::movable T, std::integral SizeT>
    constexpr void swap_range_with_len(T* r1, SizeT& len1, T* r2, SizeT& len2) noexcept {
        auto a_len = len1;
        auto b_len = len2;
        auto a_begin = r1;
        auto b_begin = r2;

        if (a_len > b_len) {
            std::swap_ranges(a_begin, a_begin + b_len, b_begin);
            uninitialized_move_n(a_begin + b_len, a_len - b_len, b_begin);
        } else {
            std::swap_ranges(a_begin, a_begin + a_len, b_begin);
            uninitialized_move_n(b_begin + a_len, b_len - a_len, a_begin);
        }

        using std::swap;
        swap(len1, len2);
    }
}  // namespace detail
}  // namespace ktl