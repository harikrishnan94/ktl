#pragma once

#include <algorithm>
#include <limits>
#include <memory>

#include <ktl/error.hpp>
#include <ktl/expected.hpp>
#include <ktl/int.hpp>

namespace ktl {
template<typename Ptr>
    requires std::is_pointer_v<Ptr>
class not_null {
  public:
    not_null(std::nullptr_t) = delete;

    // NOLINTNEXTLINE(*-explicit-conversions)
    [[gnu::nonnull(2)]] constexpr not_null(Ptr p) : m_ptr(p) {
        check_(m_ptr != nullptr, "nullptr is not expected");
    }

    // NOLINTNEXTLINE(*-explicit-conversions)
    constexpr operator Ptr() const noexcept {
        return m_ptr;
    }

    constexpr auto operator*() const noexcept -> auto& {
        return *m_ptr;
    }
    constexpr auto operator->() const noexcept -> Ptr {
        return m_ptr;
    }

  private:
    Ptr m_ptr;
};

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

    template<typename OtherType>
    static constexpr auto has_rebind = requires {
        requires allocator_for<typename Alloc::template rebind<OtherType>::other, OtherType>;
    };

  public:
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

// ------------------ Container clone ----------------------

namespace detail {
    template<typename C>
    concept has_clone_mem_fn = requires(const C& o) {
        { o.clone() } -> std::same_as<expected<C, Error>>;
    };

    template<typename C>
    concept is_clone_implementable = requires(const C& c) {
        { allocator_like<typename C::allocator_type> };
        { c.get_allocator_for_clone() } -> std::same_as<typename C::allocator_type>;
        { C {c.get_allocator_for_clone()} };
        { std::cbegin(c) } -> std::same_as<typename C::const_iterator>;
        { std::cend(c) } -> std::same_as<typename C::const_iterator>;
        {
            detail::is_expected<
                decltype(std::declval<C>().assign(std::cbegin(c), std::cend(c)))>::value
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

        TryV(copy.assign(std::cbegin(cont), std::cend(cont)));

        return copy;
    }
}
}  // namespace ktl