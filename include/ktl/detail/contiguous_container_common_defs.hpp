#pragma once

#include <algorithm>
#include <concepts>

#include <ktl/memory.hpp>

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

    template<typename Container>
    concept has_swap_mem_fn = requires(Container c) {
        { c.swap(c) } -> std::same_as<void>;
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

template<detail::has_swap_mem_fn Container>
constexpr auto swap(Container& c1, Container& c2) noexcept {
    c1.swap(c2);
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
            ktl::uninitialized_move_n(a_begin + b_len, a_len - b_len, b_begin + b_len);
        } else {
            std::swap_ranges(a_begin, a_begin + a_len, b_begin);
            ktl::uninitialized_move_n(b_begin + a_len, b_len - a_len, a_begin + a_len);
        }

        using std::swap;
        swap(len1, len2);
    }

    template<typename T>
    constexpr void sanitizer_annotate_contiguous_container(
        const T* beg,
        const T* end,
        const T* old_mid,
        const T* new_mid) noexcept {
        if constexpr (ASAN_ENABLED) {
            if (!std::is_constant_evaluated()) {
                __sanitizer_annotate_contiguous_container(beg, end, old_mid, new_mid);
            }
        }
    }

#define ASAN_ANNOTATION_HELPERS \
    constexpr void start_lifetime() const noexcept { \
        if constexpr (ASAN_ENABLED) { \
            if (!std::is_constant_evaluated()) { \
                auto [begin, end, end_cap] = get_storage_impl(); \
                sanitizer_annotate_contiguous_container(begin, end_cap, end_cap, end); \
            } \
        } \
    } \
    constexpr void end_lifetime() const noexcept { \
        if constexpr (ASAN_ENABLED) { \
            if (!std::is_constant_evaluated()) { \
                auto [begin, end, end_cap] = get_storage_impl(); \
                if (begin != nullptr) { \
                    sanitizer_annotate_contiguous_container(begin, end_cap, end, end_cap); \
                } \
            } \
        } \
    } \
    constexpr void adjust_lifetime(usize new_len) const noexcept { \
        if constexpr (ASAN_ENABLED) { \
            if (!std::is_constant_evaluated()) { \
                if constexpr (requires(const container& cont) { cont.adjust_lifetime_impl(0); }) { \
                    static_cast<const container&>(*this).adjust_lifetime_impl(new_len); \
                } else { \
                    auto [begin, end, end_cap] = get_storage_impl(); \
                    check_( \
                        static_cast<usize>(end_cap - begin) >= new_len, \
                        "asan annotation bug: adjust_lifetime"); \
                    sanitizer_annotate_contiguous_container(begin, end_cap, end, begin + new_len); \
                } \
            } \
        } \
    }
}  // namespace detail

// ------------------ Container clone ----------------------

namespace detail {
    template<typename C>
    concept has_clone_mem_fn = requires(const C& o) {
        { o.clone() } -> std::same_as<expected<C, Error>>;
    };

    template<typename C>
    concept is_clone_implementable = requires(C& c) {
        { allocator_like<typename C::allocator_type> };
        { !std::is_void_v<typename C::value_type> };
        {
            std::is_copy_constructible_v<typename C::value_type>
                || requires { clone(std::declval<typename C::value_type>()); }
        };
        { c.get_allocator_for_clone() } -> std::same_as<typename C::allocator_type>;
        { C {c.get_allocator_for_clone()} };
        { std::cbegin(c) } -> std::same_as<typename C::const_iterator>;
        { std::cend(c) } -> std::same_as<typename C::const_iterator>;
        {
            detail::is_expected<
                decltype(std::declval<C>().assign(std::cbegin(c), std::cend(c)))>::value
        };
        {
            detail::is_expected<decltype(c.push_back(
                std::declval<typename C::value_type>()))>::value
        };
    };
}  // namespace detail

template<typename C>
concept clonable = detail::has_clone_mem_fn<C> || requires(const C& o) {
    { clone(o) } -> std::same_as<expected<C, Error>>;
} || detail::is_clone_implementable<C>;

template<typename C>
    requires detail::has_clone_mem_fn<C> || detail::is_clone_implementable<C>
constexpr auto clone(const C& cont) noexcept -> expected<C, Error> {
    if constexpr (detail::has_clone_mem_fn<C>) {
        return cont.clone();
    } else {
        C copy {cont.get_allocator_for_clone()};

        if constexpr (std::is_copy_constructible_v<typename C::value_type>) {
            TryV(copy.assign(std::cbegin(cont), std::cend(cont)));
        } else if (requires { clone(cont[0]); }) {
            if constexpr (requires { copy.reserve(cont.size()); }) {
                TryV(copy.reserve(cont.size()));
            }

            for (auto& v : cont) {
                Try(c, clone(v));
                TryV(copy.push_back(std::move(c)));
            }
        }

        return copy;
    }
}

}  // namespace ktl