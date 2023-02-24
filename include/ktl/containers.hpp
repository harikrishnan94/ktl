#pragma once

#include <etl/platform.h>

#include "assert.hpp"
#include "contiguous_iterator.hpp"

namespace ktl {
namespace detail {
    template<class T>
    struct has_subscript {
        static auto test(...) -> std::false_type;

        template<class U>
        static auto test(const U& cont) -> decltype(cont[0], std::true_type {});

        static constexpr bool value = decltype(test(std::declval<T>()))::value;
    };

    template<typename C>
    concept sequence_container = requires(const C& cont) {
                                     { cont.begin() } -> std::contiguous_iterator;
                                     { cont.end() } -> std::contiguous_iterator;
                                 };
    template<typename C>
    concept random_access_container = has_subscript<C>::value && requires(const C& cont) {
                                                                     {
                                                                         cont.size()
                                                                         } -> std::integral;
                                                                 };
}  // namespace detail

constexpr auto begin(detail::sequence_container auto&& container) noexcept {
    using value_type = typename std::decay_t<decltype(container)>::value_type;
    contiguous_iterator<value_type, KTL_ENABLE_CHECKED_ITERATORS> iter {
        container.begin(),
        container.end(),
        container.begin()};

    return iter;
}

constexpr auto end(detail::sequence_container auto&& container) noexcept {
    using value_type = typename std::decay_t<decltype(container)>::value_type;
    contiguous_iterator<value_type, KTL_ENABLE_CHECKED_ITERATORS> iter {
        container.begin(),
        container.end(),
        container.end()};

    return iter;
}

constexpr auto rbegin(detail::sequence_container auto&& container) noexcept {
    auto iter = begin(std::forward<std::decay_t<decltype(container)>>(container));
    return std::reverse_iterator {iter};
}

constexpr auto rend(detail::sequence_container auto&& container) noexcept {
    auto iter = end(std::forward<std::decay_t<decltype(container)>>(container));
    return std::reverse_iterator {iter};
}
}  // namespace ktl