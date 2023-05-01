#pragma once

#include <algorithm>
#include <limits>
#include <memory>

#include <ktl/error.hpp>
#include <ktl/expected.hpp>
#include <ktl/int.hpp>
#include <ktl/not_null.hpp>

namespace ktl {

template<typename A>
concept allocator_like = std::copy_constructible<A> && requires(A a, A b, usize n) {
    requires(!std::is_void_v<typename A::value_type>);
    { a.allocate(n) } -> std::same_as<expected<not_null<typename A::value_type*>, Error>>;

    requires requires { A::is_noop_dealloc::value == true; }
        || requires { a.deallocate(std::declval<not_null<typename A::value_type*>>(), n); };

    requires requires { A::is_always_equal::value == true; } || requires { std::is_empty_v<A>; }
        || requires { a == b; };
};

template<typename A, typename T>
concept allocator_for = allocator_like<A> && std::same_as<typename A::value_type, T>;

template<allocator_like Alloc>
class allocator_traits {
  public:
    using allocator_type = Alloc;
    using value_type = typename allocator_type::value_type;
    using pointer = value_type*;
    using const_pointer = typename std::pointer_traits<pointer>::template rebind<const value_type>;
    using void_pointer = typename std::pointer_traits<pointer>::template rebind<void>;
    using const_void_pointer = typename std::pointer_traits<pointer>::template rebind<const void>;
    using difference_type = typename std::pointer_traits<pointer>::difference_type;
    using size_type = std::make_unsigned_t<difference_type>;

    using is_always_equal = std::bool_constant<requires {
        Alloc::is_always_equal::value == true;
    } || requires { std::is_empty_v<Alloc>; }>;

    using is_noop_dealloc = std::bool_constant<requires { Alloc::is_noop_dealloc::value == true; }>;

    using has_select_on_container_copy_construction = std::bool_constant<requires(const Alloc& a) {
        { a.select_on_container_copy_construction() } -> std::convertible_to<Alloc>;
    }>;

    using propagate_on_container_copy_assignment = std::bool_constant<requires {
        Alloc::propagate_on_container_copy_assignment::value == true;
    }>;

    using propagate_on_container_move_assignment = std::bool_constant<requires {
        Alloc::propagate_on_container_move_assignment::value == true;
    }>;

    using propagate_on_container_swap =
        std::bool_constant<requires { Alloc::propagate_on_container_swap::value == true; }>;

    // Assert either all or true or none are true.
    static_assert(
        static_cast<int>(propagate_on_container_swap::value)
                + static_cast<int>(propagate_on_container_move_assignment::value)
                + static_cast<int>(propagate_on_container_swap::value)
            == 3
        || static_cast<int>(propagate_on_container_swap::value)
                + static_cast<int>(propagate_on_container_move_assignment::value)
                + static_cast<int>(propagate_on_container_swap::value)
            == 0);

  private:
    using has_max_size = std::bool_constant<requires(Alloc a) {
        { a.max_size() } -> std::integral;
    }>;

    template<typename Template>
    struct rebinder;

    template<
        template<typename, typename...>
        typename Template,
        typename From,
        typename... OtherArgs>
    struct rebinder<Template<From, OtherArgs...>> {
        template<typename To>
        struct rebind {
            using other = Template<To, OtherArgs...>;
        };
    };

  public:
    template<typename OtherType>
    static constexpr auto has_rebind = requires {
        requires allocator_for<typename Alloc::template rebind<OtherType>::other, OtherType>;
    };

    template<typename U>
    using rebind_alloc =
        typename std::conditional_t<has_rebind<U>, Alloc, rebinder<Alloc>>::template rebind<
            U>::other;

    template<typename U>
    using rebind_traits = allocator_traits<rebind_alloc<U>>;

    static constexpr auto allocate(Alloc& alloc, usize n) noexcept
        -> expected<not_null<value_type*>, Error> {
        return alloc.allocate(n);
    }

    static constexpr void deallocate(Alloc& alloc, value_type* ptr, usize n) noexcept {
        if constexpr (!is_noop_dealloc::value) {
            if (ptr) {
                alloc.deallocate(ptr, n);
            }
        }
    }

    static constexpr auto max_size(const Alloc& a) noexcept -> usize {
        if constexpr (has_max_size::value) {
            return a.max_size();
        } else {
            return std::numeric_limits<usize>::max();
        }
    }

    static constexpr auto equals(const Alloc& a, const Alloc& b) noexcept -> bool {
        if constexpr (is_always_equal::value) {
            return true;
        } else {
            return a == b;
        }
    }

    template<typename OtherType>
    static constexpr auto rebind(const Alloc& a) noexcept -> rebind_alloc<OtherType> {
        return rebind_alloc<OtherType> {a};
    }

    static constexpr auto select_on_container_copy_construction(const Alloc& a) noexcept -> Alloc {
        if constexpr (has_select_on_container_copy_construction::value) {
            return a.select_on_container_copy_construction();
        } else {
            return a;
        }
    }
};

// ------------------ Uninitialized Memory algorithms --------------------

template<std::forward_iterator ForwardIterator, std::integral Size, std::copyable T>
constexpr auto uninitialized_fill_n(ForwardIterator first, Size n, const T& x) -> ForwardIterator {
    if (std::is_constant_evaluated()) {
        for (Size i = 0; i < n; i++, first++) {
            std::construct_at(std::addressof(*first), x);
        }
        return first;
    } else {  // NOLINT
        return std::uninitialized_fill_n(first, n, x);
    }
}

template<std::integral Size, std::forward_iterator ForwardIterator>
constexpr auto uninitialized_copy_n(std::input_iterator auto first, Size n, ForwardIterator result)
    -> ForwardIterator {
    if (std::is_constant_evaluated()) {
        for (Size i = 0; i < n; i++, result++, first++) {
            std::construct_at(std::addressof(*result), *first);
        }
        return result;
    } else {  // NOLINT
        return std::uninitialized_copy_n(first, n, result);
    }
}

template<std::integral Size, std::forward_iterator ForwardIterator>
constexpr auto uninitialized_move_n(std::input_iterator auto first, Size n, ForwardIterator result)
    -> ForwardIterator {
    if (std::is_constant_evaluated()) {
        for (Size i = 0; i < n; i++, result++, first++) {
            std::construct_at(std::addressof(*result), std::move(*first));
        }
        return result;
    } else {  // NOLINT
        return std::uninitialized_move_n(first, n, result).second;
    }
}

template<std::bidirectional_iterator BidirIt1, std::bidirectional_iterator BidirIt2>
constexpr auto uninitialized_move_backward(BidirIt1 first, BidirIt1 last, BidirIt2 d_last)
    -> BidirIt2 {
    while (first != last) {
        std::construct_at(std::addressof(*(--d_last)), std::move(*(--last)));
    }
    return d_last;
}

// ------------------ Container Growth policy ----------------------
template<typename GP>
concept growth_policy = std::is_trivial_v<GP> && requires(GP p, usize old_cap, usize req_cap) {
    { p.new_capacity(old_cap, req_cap) } -> std::same_as<usize>;
};

template<typename GP, typename Alloc>
concept growth_policy_for = allocator_like<Alloc> && std::is_trivial_v<GP>
    && requires(GP p, Alloc&& a, usize old_cap, usize req_cap) {
           { p.new_capacity(std::forward<Alloc>(a), old_cap, req_cap) } -> std::same_as<usize>;
       };

template<typename GP, typename Alloc>
    requires(allocator_like<std::decay_t<Alloc>> && growth_policy<GP>)
    || growth_policy_for<GP, Alloc>
constexpr auto grow(Alloc&& a, usize old_cap, usize req_cap) noexcept -> usize {
    GP gp;

    if constexpr (growth_policy_for<GP, Alloc>) {
        return gp.new_capacity(std::forward<Alloc>(a), old_cap, req_cap);
    } else {
        return gp.new_capacity(old_cap, req_cap);
    }
}

struct default_growth_policy {
    [[nodiscard]] static constexpr auto new_capacity(usize old_cap, usize req_cap) noexcept
        -> usize {
        return std::max(old_cap * 2, req_cap);
    }
};
}  // namespace ktl

#ifdef __has_feature
    #if __has_feature(address_sanitizer)
        #define __SANITIZE_ADDRESS__ 1
    #endif
#endif

#if __SANITIZE_ADDRESS__ == 1 || defined(__CLANGD__)
namespace ktl {
static constexpr auto ASAN_ENABLED = true;
}  // namespace ktl
    #include <sanitizer/common_interface_defs.h>
#else
namespace ktl {
static constexpr auto ASAN_ENABLED = false;
}
#endif

namespace ktl {
template<typename T>
constexpr usize ASAN_ALIGN = ASAN_ENABLED ? std::max<usize>(alignof(T), 8) : alignof(T);
}