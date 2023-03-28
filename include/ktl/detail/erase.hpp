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
                               std::same_as<
                                   std::iter_value_t<decltype(std::begin(c))>,
                                   typename Container::value_type>
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
}  // namespace ktl