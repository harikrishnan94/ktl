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

    // Very specific implementation, shared by both static_string and static_vector
    template<typename Container>
    constexpr void swap_contiguous_static_containers(Container& a, Container& b) noexcept {
        auto a_len = a.m_len;
        auto b_len = b.m_len;
        auto a_begin = a.get_storage().begin;
        auto b_begin = b.get_storage().begin;

        if (a_len > b_len) {
            std::swap_ranges(a_begin, a_begin + b_len, b_begin);
            uninitialized_move_n(a_begin + b_len, a_len - b_len, b_begin);
        } else {
            std::swap_ranges(a_begin, a_begin + a_len, b_begin);
            uninitialized_move_n(b_begin + a_len, b_len - a_len, a_begin);
        }

        using std::swap;
        swap(a.m_len, b.m_len);
    }
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
}  // namespace ktl