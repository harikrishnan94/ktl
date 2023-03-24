///
// expected - An implementation of std::expected with extensions
// obtained from https://github.com/TartanLlama/expected/blob/master/include/tl/expected.hpp
///

#pragma once

#include <functional>  // mem_fn
#include <type_traits>
#include <utility>

#include "assert.hpp"
#include "detail/preproc.hpp"

// NOLINTBEGIN

namespace ktl {
template<typename T, typename E>
class expected;

struct monostate {};

struct in_place_t {
    explicit in_place_t() = default;
} static constexpr in_place;

template<typename E>
class unexpected {
  public:
    static_assert(!std::is_same<E, void>::value, "E must not be void");

    unexpected() = delete;
    constexpr explicit unexpected(const E& e) noexcept : m_val(e) {}

    constexpr explicit unexpected(E&& e) noexcept : m_val(std::move(e)) {}

    template<
        typename... Args,
        typename std::enable_if<std::is_constructible<E, Args&&...>::value>::type* = nullptr>
    constexpr explicit unexpected(Args&&... args) noexcept : m_val(std::forward<Args>(args)...) {}
    template<
        typename U,
        typename... Args,
        typename std::enable_if<
            std::is_constructible<E, std::initializer_list<U>&, Args&&...>::value>::type* = nullptr>
    constexpr unexpected(std::initializer_list<U> l, Args&&... args) noexcept :
        m_val(l, std::forward<Args>(args)...) {}

    [[nodiscard]] constexpr const E& value() const& noexcept {
        return m_val;
    }
    constexpr E& value() & noexcept {
        return m_val;
    }
    constexpr E&& value() && noexcept {
        return std::move(m_val);
    }
    [[nodiscard]] constexpr const E&& value() const&& noexcept {
        return std::move(m_val);
    }

  private:
    E m_val;
};

template<typename E>
unexpected(E) -> unexpected<E>;

template<typename E>
constexpr bool operator==(const unexpected<E>& lhs, const unexpected<E>& rhs) noexcept {
    return lhs.value() == rhs.value();
}
template<typename E>
constexpr bool operator!=(const unexpected<E>& lhs, const unexpected<E>& rhs) noexcept {
    return lhs.value() != rhs.value();
}
template<typename E>
constexpr bool operator<(const unexpected<E>& lhs, const unexpected<E>& rhs) noexcept {
    return lhs.value() < rhs.value();
}
template<typename E>
constexpr bool operator<=(const unexpected<E>& lhs, const unexpected<E>& rhs) noexcept {
    return lhs.value() <= rhs.value();
}
template<typename E>
constexpr bool operator>(const unexpected<E>& lhs, const unexpected<E>& rhs) noexcept {
    return lhs.value() > rhs.value();
}
template<typename E>
constexpr bool operator>=(const unexpected<E>& lhs, const unexpected<E>& rhs) noexcept {
    return lhs.value() >= rhs.value();
}

template<typename E>
constexpr unexpected<typename std::decay<E>::type> make_unexpected(E&& e) noexcept {
    return unexpected<typename std::decay<E>::type>(std::forward<E>(e));
}

struct unexpect_t {
    unexpect_t() = default;
};
static constexpr unexpect_t unexpect {};

namespace detail {
    template<typename E>
    [[noreturn]] constexpr void throw_exception(E&& /* e */) noexcept {
        abort_("expected doesn't have value");
    }

    // Trait for checking if a type is a tl::expected
    template<typename T>
    struct is_expected_impl: std::false_type {};
    template<typename T, typename E>
    struct is_expected_impl<expected<T, E>>: std::true_type {};
    template<typename T>
    using is_expected = is_expected_impl<std::decay_t<T>>;

    template<typename T, typename E, typename U>
    using expected_enable_forward_value = std::enable_if_t<
        std::is_constructible<T, U&&>::value && !std::is_same<std::decay_t<U>, in_place_t>::value
        && !std::is_same<expected<T, E>, std::decay_t<U>>::value
        && !std::is_same<unexpected<E>, std::decay_t<U>>::value>;

    template<typename T, typename E, typename U, typename G, typename UR, typename GR>
    using expected_enable_from_other = std::enable_if_t<
        std::is_constructible<T, UR>::value && std::is_constructible<E, GR>::value
        && !std::is_constructible<T, expected<U, G>&>::value
        && !std::is_constructible<T, expected<U, G>&&>::value
        && !std::is_constructible<T, const expected<U, G>&>::value
        && !std::is_constructible<T, const expected<U, G>&&>::value
        && !std::is_convertible<expected<U, G>&, T>::value
        && !std::is_convertible<expected<U, G>&&, T>::value
        && !std::is_convertible<const expected<U, G>&, T>::value
        && !std::is_convertible<const expected<U, G>&&, T>::value>;

    template<typename T, typename U>
    using is_void_or = std::conditional_t<std::is_void<T>::value, std::true_type, U>;

    template<typename T>
    using is_copy_constructible_or_void = is_void_or<T, std::is_copy_constructible<T>>;

    template<typename T>
    using is_move_constructible_or_void = is_void_or<T, std::is_move_constructible<T>>;

    template<typename T>
    using is_copy_assignable_or_void = is_void_or<T, std::is_copy_assignable<T>>;

    template<typename T>
    using is_move_assignable_or_void = is_void_or<T, std::is_move_assignable<T>>;

    struct no_init_t {};
    static constexpr no_init_t no_init {};

    // Implements the storage of the values, and ensures that the destructor is
    // trivial if it can be.
    //
    // This specialization is for where neither `T` or `E` is trivially
    // destructible, so the destructors must be called on destruction of the
    // `expected`
    template<
        typename T,
        typename E,
        bool = std::is_trivially_destructible<T>::value,
        bool = std::is_trivially_destructible<E>::value>
    struct expected_storage_base {
        constexpr expected_storage_base() noexcept : m_val(T {}), m_has_val(true) {}
        constexpr expected_storage_base(no_init_t) noexcept : m_no_init(), m_has_val(false) {}

        template<
            typename... Args,
            std::enable_if_t<std::is_constructible<T, Args&&...>::value>* = nullptr>
        constexpr expected_storage_base(in_place_t, Args&&... args) noexcept :
            m_val(std::forward<Args>(args)...),
            m_has_val(true) {}

        template<
            typename U,
            typename... Args,
            std::enable_if_t<
                std::is_constructible<T, std::initializer_list<U>&, Args&&...>::value>* = nullptr>
        constexpr expected_storage_base(
            in_place_t,
            std::initializer_list<U> il,
            Args&&... args) noexcept :
            m_val(il, std::forward<Args>(args)...),
            m_has_val(true) {}
        template<
            typename... Args,
            std::enable_if_t<std::is_constructible<E, Args&&...>::value>* = nullptr>
        constexpr explicit expected_storage_base(unexpect_t, Args&&... args) noexcept :
            m_unexpect(std::forward<Args>(args)...),
            m_has_val(false) {}

        template<
            typename U,
            typename... Args,
            std::enable_if_t<
                std::is_constructible<E, std::initializer_list<U>&, Args&&...>::value>* = nullptr>
        constexpr explicit expected_storage_base(
            unexpect_t,
            std::initializer_list<U> il,
            Args&&... args) noexcept :
            m_unexpect(il, std::forward<Args>(args)...),
            m_has_val(false) {}

        ~expected_storage_base() noexcept {
            if (m_has_val) {
                m_val.~T();
            } else {
                m_unexpect.~unexpected<E>();
            }
        }
        union {
            T m_val;
            unexpected<E> m_unexpect;
            char m_no_init;
        };
        bool m_has_val;
    };

    // This specialization is for when both `T` and `E` are trivially-destructible,
    // so the destructor of the `expected` can be trivial.
    template<typename T, typename E>
    struct expected_storage_base<T, E, true, true> {
        constexpr expected_storage_base() noexcept : m_val(T {}), m_has_val(true) {}
        constexpr expected_storage_base(no_init_t) noexcept : m_no_init(), m_has_val(false) {}

        template<
            typename... Args,
            std::enable_if_t<std::is_constructible<T, Args&&...>::value>* = nullptr>
        constexpr expected_storage_base(in_place_t, Args&&... args) noexcept :
            m_val(std::forward<Args>(args)...),
            m_has_val(true) {}

        template<
            typename U,
            typename... Args,
            std::enable_if_t<
                std::is_constructible<T, std::initializer_list<U>&, Args&&...>::value>* = nullptr>
        constexpr expected_storage_base(
            in_place_t,
            std::initializer_list<U> il,
            Args&&... args) noexcept :
            m_val(il, std::forward<Args>(args)...),
            m_has_val(true) {}
        template<
            typename... Args,
            std::enable_if_t<std::is_constructible<E, Args&&...>::value>* = nullptr>
        constexpr explicit expected_storage_base(unexpect_t, Args&&... args) noexcept :
            m_unexpect(std::forward<Args>(args)...),
            m_has_val(false) {}

        template<
            typename U,
            typename... Args,
            std::enable_if_t<
                std::is_constructible<E, std::initializer_list<U>&, Args&&...>::value>* = nullptr>
        constexpr explicit expected_storage_base(
            unexpect_t,
            std::initializer_list<U> il,
            Args&&... args) noexcept :
            m_unexpect(il, std::forward<Args>(args)...),
            m_has_val(false) {}

        ~expected_storage_base() noexcept = default;
        union {
            T m_val;
            unexpected<E> m_unexpect;
            char m_no_init;
        };
        bool m_has_val;
    };

    // T is trivial, E is not.
    template<typename T, typename E>
    struct expected_storage_base<T, E, true, false> {
        constexpr expected_storage_base() noexcept : m_val(T {}), m_has_val(true) {}
        constexpr expected_storage_base(no_init_t) noexcept : m_no_init(), m_has_val(false) {}

        template<
            typename... Args,
            std::enable_if_t<std::is_constructible<T, Args&&...>::value>* = nullptr>
        constexpr expected_storage_base(in_place_t, Args&&... args) noexcept :
            m_val(std::forward<Args>(args)...),
            m_has_val(true) {}

        template<
            typename U,
            typename... Args,
            std::enable_if_t<
                std::is_constructible<T, std::initializer_list<U>&, Args&&...>::value>* = nullptr>
        constexpr expected_storage_base(
            in_place_t,
            std::initializer_list<U> il,
            Args&&... args) noexcept :
            m_val(il, std::forward<Args>(args)...),
            m_has_val(true) {}
        template<
            typename... Args,
            std::enable_if_t<std::is_constructible<E, Args&&...>::value>* = nullptr>
        constexpr explicit expected_storage_base(unexpect_t, Args&&... args) noexcept :
            m_unexpect(std::forward<Args>(args)...),
            m_has_val(false) {}

        template<
            typename U,
            typename... Args,
            std::enable_if_t<
                std::is_constructible<E, std::initializer_list<U>&, Args&&...>::value>* = nullptr>
        constexpr explicit expected_storage_base(
            unexpect_t,
            std::initializer_list<U> il,
            Args&&... args) noexcept :
            m_unexpect(il, std::forward<Args>(args)...),
            m_has_val(false) {}

        ~expected_storage_base() noexcept {
            if (!m_has_val) {
                m_unexpect.~unexpected<E>();
            }
        }

        union {
            T m_val;
            unexpected<E> m_unexpect;
            char m_no_init;
        };
        bool m_has_val;
    };

    // E is trivial, T is not.
    template<typename T, typename E>
    struct expected_storage_base<T, E, false, true> {
        constexpr expected_storage_base() noexcept : m_val(T {}), m_has_val(true) {}
        constexpr expected_storage_base(no_init_t) noexcept : m_no_init(), m_has_val(false) {}

        template<
            typename... Args,
            std::enable_if_t<std::is_constructible<T, Args&&...>::value>* = nullptr>
        constexpr expected_storage_base(in_place_t, Args&&... args) noexcept :
            m_val(std::forward<Args>(args)...),
            m_has_val(true) {}

        template<
            typename U,
            typename... Args,
            std::enable_if_t<
                std::is_constructible<T, std::initializer_list<U>&, Args&&...>::value>* = nullptr>
        constexpr expected_storage_base(
            in_place_t,
            std::initializer_list<U> il,
            Args&&... args) noexcept :
            m_val(il, std::forward<Args>(args)...),
            m_has_val(true) {}
        template<
            typename... Args,
            std::enable_if_t<std::is_constructible<E, Args&&...>::value>* = nullptr>
        constexpr explicit expected_storage_base(unexpect_t, Args&&... args) noexcept :
            m_unexpect(std::forward<Args>(args)...),
            m_has_val(false) {}

        template<
            typename U,
            typename... Args,
            std::enable_if_t<
                std::is_constructible<E, std::initializer_list<U>&, Args&&...>::value>* = nullptr>
        constexpr explicit expected_storage_base(
            unexpect_t,
            std::initializer_list<U> il,
            Args&&... args) noexcept :
            m_unexpect(il, std::forward<Args>(args)...),
            m_has_val(false) {}

        ~expected_storage_base() noexcept {
            if (m_has_val) {
                m_val.~T();
            }
        }
        union {
            T m_val;
            unexpected<E> m_unexpect;
            char m_no_init;
        };
        bool m_has_val;
    };

    // `T` is `void`, `E` is trivially-destructible
    template<typename E>
    struct expected_storage_base<void, E, false, true> {
        constexpr expected_storage_base() noexcept : m_has_val(true) {}

        constexpr expected_storage_base(no_init_t) noexcept : m_val(), m_has_val(false) {}

        constexpr expected_storage_base(in_place_t) noexcept : m_has_val(true) {}

        template<
            typename... Args,
            std::enable_if_t<std::is_constructible<E, Args&&...>::value>* = nullptr>
        constexpr explicit expected_storage_base(unexpect_t, Args&&... args) noexcept :
            m_unexpect(std::forward<Args>(args)...),
            m_has_val(false) {}

        template<
            typename U,
            typename... Args,
            std::enable_if_t<
                std::is_constructible<E, std::initializer_list<U>&, Args&&...>::value>* = nullptr>
        constexpr explicit expected_storage_base(
            unexpect_t,
            std::initializer_list<U> il,
            Args&&... args) noexcept :
            m_unexpect(il, std::forward<Args>(args)...),
            m_has_val(false) {}

        ~expected_storage_base() noexcept = default;
        struct dummy {};
        union {
            unexpected<E> m_unexpect;
            dummy m_val;
        };
        bool m_has_val;
    };

    // `T` is `void`, `E` is not trivially-destructible
    template<typename E>
    struct expected_storage_base<void, E, false, false> {
        constexpr expected_storage_base() noexcept : m_dummy(), m_has_val(true) {}
        constexpr expected_storage_base(no_init_t) noexcept : m_dummy(), m_has_val(false) {}

        constexpr expected_storage_base(in_place_t) noexcept : m_dummy(), m_has_val(true) {}

        template<
            typename... Args,
            std::enable_if_t<std::is_constructible<E, Args&&...>::value>* = nullptr>
        constexpr explicit expected_storage_base(unexpect_t, Args&&... args) noexcept :
            m_unexpect(std::forward<Args>(args)...),
            m_has_val(false) {}

        template<
            typename U,
            typename... Args,
            std::enable_if_t<
                std::is_constructible<E, std::initializer_list<U>&, Args&&...>::value>* = nullptr>
        constexpr explicit expected_storage_base(
            unexpect_t,
            std::initializer_list<U> il,
            Args&&... args) noexcept :
            m_unexpect(il, std::forward<Args>(args)...),
            m_has_val(false) {}

        ~expected_storage_base() noexcept {
            if (!m_has_val) {
                m_unexpect.~unexpected<E>();
            }
        }

        union {
            unexpected<E> m_unexpect;
            char m_dummy;
        };
        bool m_has_val;
    };

    // This base class provides some handy member functions which can be used in
    // further derived classes
    template<typename T, typename E>
    struct expected_operations_base: expected_storage_base<T, E> {
        using expected_storage_base<T, E>::expected_storage_base;

        template<typename... Args>
        void construct(Args&&... args) noexcept {
            new (std::addressof(this->m_val)) T(std::forward<Args>(args)...);
            this->m_has_val = true;
        }

        template<typename Rhs>
        void construct_with(Rhs&& rhs) noexcept {
            new (std::addressof(this->m_val)) T(std::forward<Rhs>(rhs).get());
            this->m_has_val = true;
        }

        template<typename... Args>
        void construct_error(Args&&... args) noexcept {
            new (std::addressof(this->m_unexpect)) unexpected<E>(std::forward<Args>(args)...);
            this->m_has_val = false;
        }

        // If exceptions are disabled then we can just copy-construct
        void assign(const expected_operations_base& rhs) noexcept {
            if (!this->m_has_val && rhs.m_has_val) {
                geterr().~unexpected<E>();
                construct(rhs.get());
            } else {
                assign_common(rhs);
            }
        }

        void assign(expected_operations_base&& rhs) noexcept {
            if (!this->m_has_val && rhs.m_has_val) {
                geterr().~unexpected<E>();
                construct(std::move(rhs).get());
            } else {
                assign_common(std::move(rhs));
            }
        }

        // The common part of move/copy assigning
        template<typename Rhs>
        void assign_common(Rhs&& rhs) noexcept {
            if (this->m_has_val) {
                if (rhs.m_has_val) {
                    get() = std::forward<Rhs>(rhs).get();
                } else {
                    destroy_val();
                    construct_error(std::forward<Rhs>(rhs).geterr());
                }
            } else {
                if (!rhs.m_has_val) {
                    geterr() = std::forward<Rhs>(rhs).geterr();
                }
            }
        }

        [[nodiscard]] bool has_value() const noexcept {
            return this->m_has_val;
        }

        constexpr T& get() & noexcept {
            return this->m_val;
        }
        [[nodiscard]] constexpr const T& get() const& noexcept {
            return this->m_val;
        }
        constexpr T&& get() && noexcept {
            return std::move(this->m_val);
        }

        [[nodiscard]] constexpr const T&& get() const&& noexcept {
            return std::move(this->m_val);
        }

        constexpr unexpected<E>& geterr() & noexcept {
            return this->m_unexpect;
        }
        [[nodiscard]] constexpr const unexpected<E>& geterr() const& noexcept {
            return this->m_unexpect;
        }
        constexpr unexpected<E>&& geterr() && noexcept {
            return std::move(this->m_unexpect);
        }

        [[nodiscard]] constexpr const unexpected<E>&& geterr() const&& noexcept {
            return std::move(this->m_unexpect);
        }

        constexpr void destroy_val() noexcept {
            get().~T();
        }
    };

    // This base class provides some handy member functions which can be used in
    // further derived classes
    template<typename E>
    struct expected_operations_base<void, E>: expected_storage_base<void, E> {
        using expected_storage_base<void, E>::expected_storage_base;

        template<typename... Args>
        void construct() noexcept {
            this->m_has_val = true;
        }

        // This function doesn't use its argument, but needs it so that code in
        // levels above this can work independently of whether T is void
        template<typename Rhs>
        void construct_with(Rhs&&) noexcept {
            this->m_has_val = true;
        }

        template<typename... Args>
        void construct_error(Args&&... args) noexcept {
            new (std::addressof(this->m_unexpect)) unexpected<E>(std::forward<Args>(args)...);
            this->m_has_val = false;
        }

        template<typename Rhs>
        void assign(Rhs&& rhs) noexcept {
            if (!this->m_has_val) {
                if (rhs.m_has_val) {
                    geterr().~unexpected<E>();
                    construct();
                } else {
                    geterr() = std::forward<Rhs>(rhs).geterr();
                }
            } else {
                if (!rhs.m_has_val) {
                    construct_error(std::forward<Rhs>(rhs).geterr());
                }
            }
        }

        [[nodiscard]] bool has_value() const noexcept {
            return this->m_has_val;
        }

        constexpr unexpected<E>& geterr() & noexcept {
            return this->m_unexpect;
        }
        constexpr const unexpected<E>& geterr() const& noexcept {
            return this->m_unexpect;
        }
        constexpr unexpected<E>&& geterr() && noexcept {
            return std::move(this->m_unexpect);
        }
        constexpr const unexpected<E>&& geterr() const&& noexcept {
            return std::move(this->m_unexpect);
        }

        constexpr void destroy_val() noexcept {
            // no-op
        }
    };

    // This class manages conditionally having a trivial copy constructor
    // This specialization is for when T and E are trivially copy constructible
    template<
        typename T,
        typename E,
        bool = is_void_or<T, std::is_trivially_copy_constructible<T>>::value&&
            std::is_trivially_copy_constructible<E>::value>
    struct expected_copy_base: expected_operations_base<T, E> {
        using expected_operations_base<T, E>::expected_operations_base;
    };

    // This specialization is for when T or E are not trivially copy constructible
    template<typename T, typename E>
    struct expected_copy_base<T, E, false>: expected_operations_base<T, E> {
        using expected_operations_base<T, E>::expected_operations_base;

        expected_copy_base() noexcept = default;
        expected_copy_base(const expected_copy_base& rhs) noexcept :
            expected_operations_base<T, E>(no_init) {
            if (rhs.has_value()) {
                this->construct_with(rhs);
            } else {
                this->construct_error(rhs.geterr());
            }
        }

        expected_copy_base(expected_copy_base&& rhs) noexcept = default;
        expected_copy_base& operator=(const expected_copy_base& rhs) noexcept = default;
        expected_copy_base& operator=(expected_copy_base&& rhs) noexcept = default;
    };

    // This class manages conditionally having a trivial move constructor
    // Unfortunately there's no way to achieve this in GCC < 5 AFAIK, since it
    // doesn't implement an analogue to std::is_trivially_move_constructible. We
    // have to make do with a non-trivial move constructor even if T is trivially
    // move constructible
    template<
        typename T,
        typename E,
        bool = is_void_or<T, std::is_trivially_move_constructible<T>>::value&&
            std::is_trivially_move_constructible<E>::value>
    struct expected_move_base: expected_copy_base<T, E> {
        using expected_copy_base<T, E>::expected_copy_base;
    };

    template<typename T, typename E>
    struct expected_move_base<T, E, false>: expected_copy_base<T, E> {
        using expected_copy_base<T, E>::expected_copy_base;

        expected_move_base() noexcept = default;
        expected_move_base(const expected_move_base& rhs) noexcept = default;

        expected_move_base(expected_move_base&& rhs) noexcept : expected_copy_base<T, E>(no_init) {
            if (rhs.has_value()) {
                this->construct_with(std::move(rhs));
            } else {
                this->construct_error(std::move(rhs.geterr()));
            }
        }
        expected_move_base& operator=(const expected_move_base& rhs) noexcept = default;
        expected_move_base& operator=(expected_move_base&& rhs) noexcept = default;
    };

    // This class manages conditionally having a trivial copy assignment operator
    template<
        typename T,
        typename E,
        bool = is_void_or<
            T,
            std::conjunction<
                std::is_trivially_copy_assignable<T>,
                std::is_trivially_copy_constructible<T>,
                std::is_trivially_destructible<T>>>::value&&
            std::is_trivially_copy_assignable<E>::value&& std::is_trivially_copy_constructible<
                E>::value&& std::is_trivially_destructible<E>::value>
    struct expected_copy_assign_base: expected_move_base<T, E> {
        using expected_move_base<T, E>::expected_move_base;
    };

    template<typename T, typename E>
    struct expected_copy_assign_base<T, E, false>: expected_move_base<T, E> {
        using expected_move_base<T, E>::expected_move_base;

        expected_copy_assign_base() noexcept = default;
        expected_copy_assign_base(const expected_copy_assign_base& rhs) noexcept = default;

        expected_copy_assign_base(expected_copy_assign_base&& rhs) noexcept = default;
        expected_copy_assign_base& operator=(const expected_copy_assign_base& rhs) noexcept {
            this->assign(rhs);
            return *this;
        }
        expected_copy_assign_base& operator=(expected_copy_assign_base&& rhs) noexcept = default;
    };

    // This class manages conditionally having a trivial move assignment operator
    // Unfortunately there's no way to achieve this in GCC < 5 AFAIK, since it
    // doesn't implement an analogue to std::is_trivially_move_assignable. We have
    // to make do with a non-trivial move assignment operator even if T is trivially
    // move assignable
    template<
        typename T,
        typename E,
        bool = is_void_or<
            T,
            std::conjunction<
                std::is_trivially_destructible<T>,
                std::is_trivially_move_constructible<T>,
                std::is_trivially_move_assignable<T>>>::value&&
            std::is_trivially_destructible<E>::value&& std::is_trivially_move_constructible<
                E>::value&& std::is_trivially_move_assignable<E>::value>
    struct expected_move_assign_base: expected_copy_assign_base<T, E> {
        using expected_copy_assign_base<T, E>::expected_copy_assign_base;
    };

    template<typename T, typename E>
    struct expected_move_assign_base<T, E, false>: expected_copy_assign_base<T, E> {
        using expected_copy_assign_base<T, E>::expected_copy_assign_base;

        expected_move_assign_base() noexcept = default;
        expected_move_assign_base(const expected_move_assign_base& rhs) noexcept = default;

        expected_move_assign_base(expected_move_assign_base&& rhs) noexcept = default;

        expected_move_assign_base& operator=(const expected_move_assign_base& rhs) = default;

        expected_move_assign_base& operator=(expected_move_assign_base&& rhs) noexcept {
            this->assign(std::move(rhs));
            return *this;
        }
    };

    // expected_delete_ctor_base will conditionally delete copy and move
    // constructors depending on whether T is copy/move constructible
    template<
        typename T,
        typename E,
        bool EnableCopy =
            (is_copy_constructible_or_void<T>::value && std::is_copy_constructible<E>::value),
        bool EnableMove =
            (is_move_constructible_or_void<T>::value && std::is_move_constructible<E>::value)>
    struct expected_delete_ctor_base {
        expected_delete_ctor_base() noexcept = default;
        expected_delete_ctor_base(const expected_delete_ctor_base&) noexcept = default;
        expected_delete_ctor_base(expected_delete_ctor_base&&) noexcept = default;
        expected_delete_ctor_base& operator=(const expected_delete_ctor_base&) noexcept = default;
        expected_delete_ctor_base& operator=(expected_delete_ctor_base&&) noexcept = default;
    };

    template<typename T, typename E>
    struct expected_delete_ctor_base<T, E, true, false> {
        expected_delete_ctor_base() noexcept = default;
        expected_delete_ctor_base(const expected_delete_ctor_base&) noexcept = default;
        expected_delete_ctor_base(expected_delete_ctor_base&&) = delete;
        expected_delete_ctor_base& operator=(const expected_delete_ctor_base&) noexcept = default;
        expected_delete_ctor_base& operator=(expected_delete_ctor_base&&) noexcept = default;
    };

    template<typename T, typename E>
    struct expected_delete_ctor_base<T, E, false, true> {
        expected_delete_ctor_base() noexcept = default;
        expected_delete_ctor_base(const expected_delete_ctor_base&) = delete;
        expected_delete_ctor_base(expected_delete_ctor_base&&) noexcept = default;
        expected_delete_ctor_base& operator=(const expected_delete_ctor_base&) noexcept = default;
        expected_delete_ctor_base& operator=(expected_delete_ctor_base&&) noexcept = default;
    };

    template<typename T, typename E>
    struct expected_delete_ctor_base<T, E, false, false> {
        expected_delete_ctor_base() noexcept = default;
        expected_delete_ctor_base(const expected_delete_ctor_base&) = delete;
        expected_delete_ctor_base(expected_delete_ctor_base&&) = delete;
        expected_delete_ctor_base& operator=(const expected_delete_ctor_base&) = default;
        expected_delete_ctor_base& operator=(expected_delete_ctor_base&&) noexcept = default;
    };

    // expected_delete_assign_base will conditionally delete copy and move
    // constructors depending on whether T and E are copy/move constructible +
    // assignable
    template<
        typename T,
        typename E,
        bool EnableCopy =
            (is_copy_constructible_or_void<T>::value && std::is_copy_constructible<E>::value
             && is_copy_assignable_or_void<T>::value && std::is_copy_assignable<E>::value),
        bool EnableMove =
            (is_move_constructible_or_void<T>::value && std::is_move_constructible<E>::value
             && is_move_assignable_or_void<T>::value && std::is_move_assignable<E>::value)>
    struct expected_delete_assign_base {
        expected_delete_assign_base() noexcept = default;
        expected_delete_assign_base(const expected_delete_assign_base&) noexcept = default;
        expected_delete_assign_base(expected_delete_assign_base&&) noexcept = default;
        expected_delete_assign_base&
        operator=(const expected_delete_assign_base&) noexcept = default;
        expected_delete_assign_base& operator=(expected_delete_assign_base&&) noexcept = default;
    };

    template<typename T, typename E>
    struct expected_delete_assign_base<T, E, true, false> {
        expected_delete_assign_base() noexcept = default;
        expected_delete_assign_base(const expected_delete_assign_base&) noexcept = default;
        expected_delete_assign_base(expected_delete_assign_base&&) noexcept = default;
        expected_delete_assign_base&
        operator=(const expected_delete_assign_base&) noexcept = default;
        expected_delete_assign_base& operator=(expected_delete_assign_base&&) = delete;
    };

    template<typename T, typename E>
    struct expected_delete_assign_base<T, E, false, true> {
        expected_delete_assign_base() noexcept = default;
        expected_delete_assign_base(const expected_delete_assign_base&) noexcept = default;
        expected_delete_assign_base(expected_delete_assign_base&&) noexcept = default;
        expected_delete_assign_base& operator=(const expected_delete_assign_base&) = delete;
        expected_delete_assign_base& operator=(expected_delete_assign_base&&) noexcept = default;
    };

    template<typename T, typename E>
    struct expected_delete_assign_base<T, E, false, false> {
        expected_delete_assign_base() noexcept = default;
        expected_delete_assign_base(const expected_delete_assign_base&) = default;
        expected_delete_assign_base(expected_delete_assign_base&&) noexcept = default;
        expected_delete_assign_base& operator=(const expected_delete_assign_base&) = delete;
        expected_delete_assign_base& operator=(expected_delete_assign_base&&) = delete;
    };

    // This is needed to be able to construct the expected_default_ctor_base which
    // follows, while still conditionally deleting the default constructor.
    struct default_constructor_tag {
        explicit constexpr default_constructor_tag() noexcept = default;
    };

    // expected_default_ctor_base will ensure that expected has a deleted default
    // consturctor if T is not default constructible.
    // This specialization is for when T is default constructible
    template<
        typename T,
        typename E,
        bool Enable = std::is_default_constructible<T>::value || std::is_void<T>::value>
    struct expected_default_ctor_base {
        constexpr expected_default_ctor_base() noexcept = default;
        constexpr expected_default_ctor_base(expected_default_ctor_base const&) noexcept = default;
        constexpr expected_default_ctor_base(expected_default_ctor_base&&) noexcept = default;
        expected_default_ctor_base& operator=(expected_default_ctor_base const&) noexcept = default;
        expected_default_ctor_base& operator=(expected_default_ctor_base&&) noexcept = default;

        constexpr explicit expected_default_ctor_base(default_constructor_tag) {}
    };

    // This specialization is for when T is not default constructible
    template<typename T, typename E>
    struct expected_default_ctor_base<T, E, false> {
        constexpr expected_default_ctor_base() = delete;
        constexpr expected_default_ctor_base(expected_default_ctor_base const&) noexcept = default;
        constexpr expected_default_ctor_base(expected_default_ctor_base&&) noexcept = default;
        expected_default_ctor_base& operator=(expected_default_ctor_base const&) noexcept = default;
        expected_default_ctor_base& operator=(expected_default_ctor_base&&) noexcept = default;

        constexpr explicit expected_default_ctor_base(default_constructor_tag) {}
    };
}  // namespace detail

/// An `expected<T, E>` object is an object that contains the storage for
/// another object and manages the lifetime of this contained object `T`.
/// Alternatively it could contain the storage for another unexpected object
/// `E`. The contained object may not be initialized after the expected object
/// has been initialized, and may not be destroyed before the expected object
/// has been destroyed. The initialization state of the contained object is
/// tracked by the expected object.
template<typename T, typename E>
class expected:
    private detail::expected_move_assign_base<T, E>,
    private detail::expected_delete_ctor_base<T, E>,
    private detail::expected_delete_assign_base<T, E>,
    private detail::expected_default_ctor_base<T, E> {
    static_assert(!std::is_reference<T>::value, "T must not be a reference");
    static_assert(
        !std::is_same<T, std::remove_cv<in_place_t>::type>::value,
        "T must not be in_place_t");
    static_assert(
        !std::is_same<T, std::remove_cv<unexpect_t>::type>::value,
        "T must not be unexpect_t");
    static_assert(
        !std::is_same<T, typename std::remove_cv<unexpected<E>>::type>::value,
        "T must not be unexpected<E>");
    static_assert(!std::is_reference<E>::value, "E must not be a reference");

    constexpr T* valptr() noexcept {
        return std::addressof(this->m_val);
    }
    [[nodiscard]] constexpr const T* valptr() const noexcept {
        return std::addressof(this->m_val);
    }
    constexpr unexpected<E>* errptr() noexcept {
        return std::addressof(this->m_unexpect);
    }
    [[nodiscard]] constexpr const unexpected<E>* errptr() const noexcept {
        return std::addressof(this->m_unexpect);
    }

    template<typename U = T, std::enable_if_t<!std::is_void<U>::value>* = nullptr>
    constexpr U& val() noexcept {
        return this->m_val;
    }
    constexpr unexpected<E>& err() noexcept {
        return this->m_unexpect;
    }

    template<typename U = T, std::enable_if_t<!std::is_void<U>::value>* = nullptr>
    [[nodiscard]] constexpr const U& val() const noexcept {
        return this->m_val;
    }
    [[nodiscard]] constexpr const unexpected<E>& err() const noexcept {
        return this->m_unexpect;
    }

    using impl_base = detail::expected_move_assign_base<T, E>;
    using ctor_base = detail::expected_default_ctor_base<T, E>;

  public:
    using value_type = T;
    using error_type = E;
    using unexpected_type = unexpected<E>;

    template<typename F>
    constexpr auto and_then(F&& f) & noexcept {
        return and_then_impl(*this, std::forward<F>(f));
    }
    template<typename F>
    constexpr auto and_then(F&& f) && noexcept {
        return and_then_impl(std::move(*this), std::forward<F>(f));
    }
    template<typename F>
    constexpr auto and_then(F&& f) const& noexcept {
        return and_then_impl(*this, std::forward<F>(f));
    }

    template<typename F>
    constexpr auto and_then(F&& f) const&& noexcept {
        return and_then_impl(std::move(*this), std::forward<F>(f));
    }

    template<typename F>
    constexpr auto map(F&& f) & noexcept {
        return expected_map_impl(*this, std::forward<F>(f));
    }
    template<typename F>
    constexpr auto map(F&& f) && noexcept {
        return expected_map_impl(std::move(*this), std::forward<F>(f));
    }
    template<typename F>
    constexpr auto map(F&& f) const& noexcept {
        return expected_map_impl(*this, std::forward<F>(f));
    }
    template<typename F>
    constexpr auto map(F&& f) const&& noexcept {
        return expected_map_impl(std::move(*this), std::forward<F>(f));
    }

    template<typename F>
    constexpr auto transform(F&& f) & noexcept {
        return expected_map_impl(*this, std::forward<F>(f));
    }
    template<typename F>
    constexpr auto transform(F&& f) && noexcept {
        return expected_map_impl(std::move(*this), std::forward<F>(f));
    }
    template<typename F>
    constexpr auto transform(F&& f) const& noexcept {
        return expected_map_impl(*this, std::forward<F>(f));
    }
    template<typename F>
    constexpr auto transform(F&& f) const&& noexcept {
        return expected_map_impl(std::move(*this), std::forward<F>(f));
    }

    template<typename F>
    constexpr auto map_error(F&& f) & noexcept {
        return map_error_impl(*this, std::forward<F>(f));
    }
    template<typename F>
    constexpr auto map_error(F&& f) && noexcept {
        return map_error_impl(std::move(*this), std::forward<F>(f));
    }
    template<typename F>
    constexpr auto map_error(F&& f) const& noexcept {
        return map_error_impl(*this, std::forward<F>(f));
    }
    template<typename F>
    constexpr auto map_error(F&& f) const&& noexcept {
        return map_error_impl(std::move(*this), std::forward<F>(f));
    }

    template<typename F>
    constexpr auto transform_error(F&& f) & noexcept {
        return map_error_impl(*this, std::forward<F>(f));
    }
    template<typename F>
    constexpr auto transform_error(F&& f) && noexcept {
        return map_error_impl(std::move(*this), std::forward<F>(f));
    }
    template<typename F>
    constexpr auto transform_error(F&& f) const& noexcept {
        return map_error_impl(*this, std::forward<F>(f));
    }
    template<typename F>
    constexpr auto transform_error(F&& f) const&& noexcept {
        return map_error_impl(std::move(*this), std::forward<F>(f));
    }

    template<typename F>
    expected constexpr or_else(F&& f) & noexcept {
        return or_else_impl(*this, std::forward<F>(f));
    }

    template<typename F>
    expected constexpr or_else(F&& f) && noexcept {
        return or_else_impl(std::move(*this), std::forward<F>(f));
    }

    template<typename F>
    expected constexpr or_else(F&& f) const& noexcept {
        return or_else_impl(*this, std::forward<F>(f));
    }

    template<typename F>
    expected constexpr or_else(F&& f) const&& noexcept {
        return or_else_impl(std::move(*this), std::forward<F>(f));
    }

    constexpr expected() noexcept = default;
    constexpr expected(const expected& rhs) noexcept = default;
    constexpr expected(expected&& rhs) noexcept = default;
    expected& operator=(const expected& rhs) noexcept = default;
    expected& operator=(expected&& rhs) noexcept = default;

    template<
        typename... Args,
        std::enable_if_t<std::is_constructible<T, Args&&...>::value>* = nullptr>
    constexpr expected(in_place_t, Args&&... args) noexcept :
        impl_base(in_place, std::forward<Args>(args)...),
        ctor_base(detail::default_constructor_tag {}) {}

    template<
        typename U,
        typename... Args,
        std::enable_if_t<std::is_constructible<T, std::initializer_list<U>&, Args&&...>::value>* =
            nullptr>
    constexpr expected(in_place_t, std::initializer_list<U> il, Args&&... args) noexcept :
        impl_base(in_place, il, std::forward<Args>(args)...),
        ctor_base(detail::default_constructor_tag {}) {}

    template<
        typename G = E,
        std::enable_if_t<std::is_constructible<E, const G&>::value>* = nullptr,
        std::enable_if_t<!std::is_convertible<const G&, E>::value>* = nullptr>
    explicit constexpr expected(const unexpected<G>& e) noexcept :
        impl_base(unexpect, e.value()),
        ctor_base(detail::default_constructor_tag {}) {}

    template<
        typename G = E,
        std::enable_if_t<std::is_constructible<E, const G&>::value>* = nullptr,
        std::enable_if_t<std::is_convertible<const G&, E>::value>* = nullptr>
    constexpr expected(unexpected<G> const& e) noexcept :
        impl_base(unexpect, e.value()),
        ctor_base(detail::default_constructor_tag {}) {}

    template<
        typename G = E,
        std::enable_if_t<std::is_constructible<E, G&&>::value>* = nullptr,
        std::enable_if_t<!std::is_convertible<G&&, E>::value>* = nullptr>
    explicit constexpr expected(unexpected<G>&& e) noexcept :
        impl_base(unexpect, std::move(e.value())),
        ctor_base(detail::default_constructor_tag {}) {}

    template<
        typename G = E,
        std::enable_if_t<std::is_constructible<E, G&&>::value>* = nullptr,
        std::enable_if_t<std::is_convertible<G&&, E>::value>* = nullptr>
    constexpr expected(unexpected<G>&& e) noexcept :
        impl_base(unexpect, std::move(e.value())),
        ctor_base(detail::default_constructor_tag {}) {}

    template<
        typename... Args,
        std::enable_if_t<std::is_constructible<E, Args&&...>::value>* = nullptr>
    constexpr explicit expected(unexpect_t, Args&&... args) noexcept :
        impl_base(unexpect, std::forward<Args>(args)...),
        ctor_base(detail::default_constructor_tag {}) {}

    template<
        typename U,
        typename... Args,
        std::enable_if_t<std::is_constructible<E, std::initializer_list<U>&, Args&&...>::value>* =
            nullptr>
    constexpr explicit expected(unexpect_t, std::initializer_list<U> il, Args&&... args) noexcept :
        impl_base(unexpect, il, std::forward<Args>(args)...),
        ctor_base(detail::default_constructor_tag {}) {}

    template<
        typename U,
        typename G,
        std::enable_if_t<!(
            std::is_convertible<U const&, T>::value && std::is_convertible<G const&, E>::value)>* =
            nullptr,
        detail::expected_enable_from_other<T, E, U, G, const U&, const G&>* = nullptr>
    explicit constexpr expected(const expected<U, G>& rhs) noexcept :
        ctor_base(detail::default_constructor_tag {}) {
        if (rhs.has_value()) {
            this->construct(*rhs);
        } else {
            this->construct_error(rhs.error());
        }
    }

    template<
        typename U,
        typename G,
        std::enable_if_t<
            (std::is_convertible<U const&, T>::value && std::is_convertible<G const&, E>::value)>* =
            nullptr,
        detail::expected_enable_from_other<T, E, U, G, const U&, const G&>* = nullptr>
    constexpr expected(const expected<U, G>& rhs) noexcept :
        ctor_base(detail::default_constructor_tag {}) {
        if (rhs.has_value()) {
            this->construct(*rhs);
        } else {
            this->construct_error(rhs.error());
        }
    }

    template<
        typename U,
        typename G,
        std::enable_if_t<
            !(std::is_convertible<U&&, T>::value && std::is_convertible<G&&, E>::value)>* = nullptr,
        detail::expected_enable_from_other<T, E, U, G, U&&, G&&>* = nullptr>
    explicit constexpr expected(expected<U, G>&& rhs) noexcept :
        ctor_base(detail::default_constructor_tag {}) {
        if (rhs.has_value()) {
            this->construct(std::move(*rhs));
        } else {
            this->construct_error(std::move(rhs.error()));
        }
    }

    template<
        typename U,
        typename G,
        std::enable_if_t<
            (std::is_convertible<U&&, T>::value && std::is_convertible<G&&, E>::value)>* = nullptr,
        detail::expected_enable_from_other<T, E, U, G, U&&, G&&>* = nullptr>
    constexpr expected(expected<U, G>&& rhs) noexcept :
        ctor_base(detail::default_constructor_tag {}) {
        if (rhs.has_value()) {
            this->construct(std::move(*rhs));
        } else {
            this->construct_error(std::move(rhs.error()));
        }
    }

    template<
        typename U = T,
        std::enable_if_t<!std::is_convertible<U&&, T>::value>* = nullptr,
        detail::expected_enable_forward_value<T, E, U>* = nullptr>
    explicit constexpr expected(U&& v) noexcept : expected(in_place, std::forward<U>(v)) {}

    template<
        typename U = T,
        std::enable_if_t<std::is_convertible<U&&, T>::value>* = nullptr,
        detail::expected_enable_forward_value<T, E, U>* = nullptr>
    constexpr expected(U&& v) noexcept : expected(in_place, std::forward<U>(v)) {}

    template<
        typename U = T,
        typename G = T,
        std::enable_if_t<std::is_nothrow_constructible<T, U&&>::value>* = nullptr,
        std::enable_if_t<!std::is_void<G>::value>* = nullptr,
        std::enable_if_t<
            (!std::is_same<expected<T, E>, std::decay_t<U>>::value
             && !std::conjunction<std::is_scalar<T>, std::is_same<T, std::decay_t<U>>>::value
             && std::is_constructible<T, U>::value && std::is_assignable<G&, U>::value
             && std::is_nothrow_move_constructible<E>::value)>* = nullptr>
    expected& operator=(U&& v) noexcept {
        if (has_value()) {
            val() = std::forward<U>(v);
        } else {
            err().~unexpected<E>();
            ::new (valptr()) T(std::forward<U>(v));
            this->m_has_val = true;
        }

        return *this;
    }

    template<
        typename U = T,
        typename G = T,
        std::enable_if_t<!std::is_nothrow_constructible<T, U&&>::value>* = nullptr,
        std::enable_if_t<!std::is_void<U>::value>* = nullptr,
        std::enable_if_t<
            (!std::is_same<expected<T, E>, std::decay_t<U>>::value
             && !std::conjunction<std::is_scalar<T>, std::is_same<T, std::decay_t<U>>>::value
             && std::is_constructible<T, U>::value && std::is_assignable<G&, U>::value
             && std::is_nothrow_move_constructible<E>::value)>* = nullptr>
    expected& operator=(U&& v) noexcept {
        if (has_value()) {
            val() = std::forward<U>(v);
        } else {
            ::new (valptr()) T(std::forward<U>(v));
            this->m_has_val = true;
        }

        return *this;
    }

    template<
        typename G = E,
        std::enable_if_t<
            std::is_nothrow_copy_constructible<G>::value && std::is_assignable<G&, G>::value>* =
            nullptr>
    expected& operator=(const unexpected<G>& rhs) noexcept {
        if (!has_value()) {
            err() = rhs;
        } else {
            this->destroy_val();
            ::new (errptr()) unexpected<E>(rhs);
            this->m_has_val = false;
        }

        return *this;
    }

    template<
        typename G = E,
        std::enable_if_t<
            std::is_nothrow_move_constructible<G>::value && std::is_move_assignable<G>::value>* =
            nullptr>
    expected& operator=(unexpected<G>&& rhs) noexcept {
        if (!has_value()) {
            err() = std::move(rhs);
        } else {
            this->destroy_val();
            ::new (errptr()) unexpected<E>(std::move(rhs));
            this->m_has_val = false;
        }

        return *this;
    }

    template<
        typename... Args,
        std::enable_if_t<std::is_nothrow_constructible<T, Args&&...>::value>* = nullptr>
    void emplace(Args&&... args) noexcept {
        if (has_value()) {
            val().~T();
        } else {
            err().~unexpected<E>();
            this->m_has_val = true;
        }
        ::new (valptr()) T(std::forward<Args>(args)...);
    }

    template<
        typename... Args,
        std::enable_if_t<!std::is_nothrow_constructible<T, Args&&...>::value>* = nullptr>
    void emplace(Args&&... args) noexcept {
        if (has_value()) {
            val().~T();
            ::new (valptr()) T(std::forward<Args>(args)...);
        } else {
            ::new (valptr()) T(std::forward<Args>(args)...);
            this->m_has_val = true;
        }
    }

    template<
        typename U,
        typename... Args,
        std::enable_if_t<
            std::is_nothrow_constructible<T, std::initializer_list<U>&, Args&&...>::value>* =
            nullptr>
    void emplace(std::initializer_list<U> il, Args&&... args) noexcept {
        if (has_value()) {
            T t(il, std::forward<Args>(args)...);
            val() = std::move(t);
        } else {
            err().~unexpected<E>();
            ::new (valptr()) T(il, std::forward<Args>(args)...);
            this->m_has_val = true;
        }
    }

    template<
        typename U,
        typename... Args,
        std::enable_if_t<
            !std::is_nothrow_constructible<T, std::initializer_list<U>&, Args&&...>::value>* =
            nullptr>
    void emplace(std::initializer_list<U> il, Args&&... args) noexcept {
        if (has_value()) {
            T t(il, std::forward<Args>(args)...);
            val() = std::move(t);
        } else {
            ::new (valptr()) T(il, std::forward<Args>(args)...);
            this->m_has_val = true;
        }
    }

  private:
    using t_is_void = std::true_type;
    using t_is_not_void = std::false_type;
    using t_is_nothrow_move_constructible = std::true_type;
    using move_constructing_t_can_throw = std::false_type;
    using e_is_nothrow_move_constructible = std::true_type;
    using move_constructing_e_can_throw = std::false_type;

    void swap_where_both_have_value(expected& /*rhs*/, t_is_void) noexcept {
        // swapping void is a no-op
    }

    void swap_where_both_have_value(expected& rhs, t_is_not_void) noexcept {
        using std::swap;
        swap(val(), rhs.val());
    }

    void swap_where_only_one_has_value(expected& rhs, t_is_void) noexcept {
        ::new (errptr()) unexpected_type(std::move(rhs.err()));
        rhs.err().~unexpected_type();
        std::swap(this->m_has_val, rhs.m_has_val);
    }

    void swap_where_only_one_has_value(expected& rhs, t_is_not_void) noexcept {
        swap_where_only_one_has_value_and_t_is_not_void(
            rhs,
            typename std::is_nothrow_move_constructible<T>::type {},
            typename std::is_nothrow_move_constructible<E>::type {});
    }

    void swap_where_only_one_has_value_and_t_is_not_void(
        expected& rhs,
        t_is_nothrow_move_constructible,
        e_is_nothrow_move_constructible) noexcept {
        auto temp = std::move(val());
        val().~T();
        ::new (errptr()) unexpected_type(std::move(rhs.err()));
        rhs.err().~unexpected_type();
        ::new (rhs.valptr()) T(std::move(temp));
        std::swap(this->m_has_val, rhs.m_has_val);
    }

    void swap_where_only_one_has_value_and_t_is_not_void(
        expected& rhs,
        t_is_nothrow_move_constructible,
        move_constructing_e_can_throw) noexcept {
        auto temp = std::move(val());
        val().~T();

        ::new (errptr()) unexpected_type(std::move(rhs.err()));
        rhs.err().~unexpected_type();
        ::new (rhs.valptr()) T(std::move(temp));
        std::swap(this->m_has_val, rhs.m_has_val);
    }

    void swap_where_only_one_has_value_and_t_is_not_void(
        expected& rhs,
        move_constructing_t_can_throw,
        e_is_nothrow_move_constructible) noexcept {
        auto temp = std::move(rhs.err());
        rhs.err().~unexpected_type();

        ::new (rhs.valptr()) T(std::move(val()));
        val().~T();
        ::new (errptr()) unexpected_type(std::move(temp));
        std::swap(this->m_has_val, rhs.m_has_val);
    }

  public:
    template<typename OT = T, typename OE = E>
    std::enable_if_t<
        std::is_swappable<OT>::value && std::is_swappable<OE>::value
        && (std::is_nothrow_move_constructible<OT>::value
            || std::is_nothrow_move_constructible<OE>::value)>
    swap(expected& rhs) noexcept {
        if (has_value() && rhs.has_value()) {
            swap_where_both_have_value(rhs, typename std::is_void<T>::type {});
        } else if (!has_value() && rhs.has_value()) {
            rhs.swap(*this);
        } else if (has_value()) {
            swap_where_only_one_has_value(rhs, typename std::is_void<T>::type {});
        } else {
            using std::swap;
            swap(err(), rhs.err());
        }
    }

    constexpr const T* operator->() const noexcept {
        check_(has_value(), "dereferencing unexpected");
        return valptr();
    }
    constexpr T* operator->() noexcept {
        check_(has_value(), "dereferencing unexpected");
        return valptr();
    }

    template<typename U = T, std::enable_if_t<!std::is_void<U>::value>* = nullptr>
    constexpr const U& operator*() const& noexcept {
        check_(has_value(), "dereferencing unexpected");
        return val();
    }
    template<typename U = T, std::enable_if_t<!std::is_void<U>::value>* = nullptr>
    constexpr U& operator*() & noexcept {
        check_(has_value(), "dereferencing unexpected");
        return val();
    }
    template<typename U = T, std::enable_if_t<!std::is_void<U>::value>* = nullptr>
    constexpr const U&& operator*() const&& noexcept {
        check_(has_value(), "dereferencing unexpected");
        return std::move(val());
    }
    template<typename U = T, std::enable_if_t<!std::is_void<U>::value>* = nullptr>
    constexpr U&& operator*() && noexcept {
        check_(has_value(), "dereferencing unexpected");
        return std::move(val());
    }

    [[nodiscard]] constexpr bool has_value() const noexcept {
        return this->m_has_val;
    }
    constexpr explicit operator bool() const noexcept {
        return this->m_has_val;
    }

    template<typename U = T, std::enable_if_t<!std::is_void<U>::value>* = nullptr>
    constexpr const U& value() const& noexcept {
        if (!has_value())
            detail::throw_exception(bad_expected_access<E>(err().value()));
        return val();
    }
    template<typename U = T, std::enable_if_t<!std::is_void<U>::value>* = nullptr>
    constexpr U& value() & noexcept {
        if (!has_value())
            detail::throw_exception(bad_expected_access<E>(err().value()));
        return val();
    }
    template<typename U = T, std::enable_if_t<!std::is_void<U>::value>* = nullptr>
    constexpr const U&& value() const&& noexcept {
        if (!has_value())
            detail::throw_exception(bad_expected_access<E>(std::move(err()).value()));
        return std::move(val());
    }
    template<typename U = T, std::enable_if_t<!std::is_void<U>::value>* = nullptr>
    constexpr U&& value() && noexcept {
        if (!has_value())
            detail::throw_exception(bad_expected_access<E>(std::move(err()).value()));
        return std::move(val());
    }

    [[nodiscard]] constexpr const E& error() const& noexcept {
        check_(!has_value(), "found `expected`, when `unexpected` is expected");
        return err().value();
    }
    constexpr E& error() & noexcept {
        check_(!has_value(), "found `expected`, when `unexpected` is expected");
        return err().value();
    }
    [[nodiscard]] constexpr const E&& error() const&& noexcept {
        check_(!has_value(), "found `expected`, when `unexpected` is expected");
        return std::move(err().value());
    }
    constexpr E&& error() && noexcept {
        check_(!has_value(), "found `expected`, when `unexpected` is expected");
        return std::move(err().value());
    }

    template<typename U>
    constexpr T value_or(U&& v) const& noexcept {
        static_assert(
            std::is_copy_constructible<T>::value && std::is_convertible<U&&, T>::value,
            "T must be copy-constructible and convertible to from U&&");
        return bool(*this) ? **this : static_cast<T>(std::forward<U>(v));
    }
    template<typename U>
    constexpr T value_or(U&& v) && noexcept {
        static_assert(
            std::is_move_constructible<T>::value && std::is_convertible<U&&, T>::value,
            "T must be move-constructible and convertible to from U&&");
        return bool(*this) ? std::move(**this) : static_cast<T>(std::forward<U>(v));
    }
};

namespace detail {
    template<typename Exp>
    using exp_t = typename std::decay_t<Exp>::value_type;
    template<typename Exp>
    using err_t = typename std::decay_t<Exp>::error_type;
    template<typename Exp, typename Ret>
    using ret_t = expected<Ret, err_t<Exp>>;

    template<
        typename Exp,
        typename F,
        std::enable_if_t<!std::is_void<exp_t<Exp>>::value>* = nullptr,
        typename Ret = decltype(std::invoke(std::declval<F>(), *std::declval<Exp>()))>
    constexpr auto and_then_impl(Exp&& exp, F&& f) noexcept {
        static_assert(detail::is_expected<Ret>::value, "F must return an expected");

        return exp.has_value() ? std::invoke(std::forward<F>(f), *std::forward<Exp>(exp))
                               : Ret(unexpect, std::forward<Exp>(exp).error());
    }

    template<
        typename Exp,
        typename F,
        std::enable_if_t<std::is_void<exp_t<Exp>>::value>* = nullptr,
        typename Ret = decltype(std::invoke(std::declval<F>()))>
    constexpr auto and_then_impl(Exp&& exp, F&& f) noexcept {
        static_assert(detail::is_expected<Ret>::value, "F must return an expected");

        return exp.has_value() ? std::invoke(std::forward<F>(f))
                               : Ret(unexpect, std::forward<Exp>(exp).error());
    }

    template<
        typename Exp,
        typename F,
        std::enable_if_t<!std::is_void<exp_t<Exp>>::value>* = nullptr,
        typename Ret = decltype(std::invoke(std::declval<F>(), *std::declval<Exp>())),
        std::enable_if_t<!std::is_void<Ret>::value>* = nullptr>
    constexpr auto expected_map_impl(Exp&& exp, F&& f) noexcept {
        using result = ret_t<Exp, std::decay_t<Ret>>;
        return exp.has_value() ? result(std::invoke(std::forward<F>(f), *std::forward<Exp>(exp)))
                               : result(unexpect, std::forward<Exp>(exp).error());
    }

    template<
        typename Exp,
        typename F,
        std::enable_if_t<!std::is_void<exp_t<Exp>>::value>* = nullptr,
        typename Ret = decltype(std::invoke(std::declval<F>(), *std::declval<Exp>())),
        std::enable_if_t<std::is_void<Ret>::value>* = nullptr>
    auto expected_map_impl(Exp&& exp, F&& f) noexcept {
        using result = expected<void, err_t<Exp>>;
        if (exp.has_value()) {
            std::invoke(std::forward<F>(f), *std::forward<Exp>(exp));
            return result();
        }

        return result(unexpect, std::forward<Exp>(exp).error());
    }

    template<
        typename Exp,
        typename F,
        std::enable_if_t<std::is_void<exp_t<Exp>>::value>* = nullptr,
        typename Ret = decltype(std::invoke(std::declval<F>())),
        std::enable_if_t<!std::is_void<Ret>::value>* = nullptr>
    constexpr auto expected_map_impl(Exp&& exp, F&& f) noexcept {
        using result = ret_t<Exp, std::decay_t<Ret>>;
        return exp.has_value() ? result(std::invoke(std::forward<F>(f)))
                               : result(unexpect, std::forward<Exp>(exp).error());
    }

    template<
        typename Exp,
        typename F,
        std::enable_if_t<std::is_void<exp_t<Exp>>::value>* = nullptr,
        typename Ret = decltype(std::invoke(std::declval<F>())),
        std::enable_if_t<std::is_void<Ret>::value>* = nullptr>
    auto expected_map_impl(Exp&& exp, F&& f) noexcept {
        using result = expected<void, err_t<Exp>>;
        if (exp.has_value()) {
            std::invoke(std::forward<F>(f));
            return result();
        }

        return result(unexpect, std::forward<Exp>(exp).error());
    }

    template<
        typename Exp,
        typename F,
        std::enable_if_t<!std::is_void<exp_t<Exp>>::value>* = nullptr,
        typename Ret = decltype(std::invoke(std::declval<F>(), std::declval<Exp>().error())),
        std::enable_if_t<!std::is_void<Ret>::value>* = nullptr>
    constexpr auto map_error_impl(Exp&& exp, F&& f) noexcept {
        using result = expected<exp_t<Exp>, std::decay_t<Ret>>;
        return exp.has_value()
            ? result(*std::forward<Exp>(exp))
            : result(unexpect, std::invoke(std::forward<F>(f), std::forward<Exp>(exp).error()));
    }
    template<
        typename Exp,
        typename F,
        std::enable_if_t<!std::is_void<exp_t<Exp>>::value>* = nullptr,
        typename Ret = decltype(std::invoke(std::declval<F>(), std::declval<Exp>().error())),
        std::enable_if_t<std::is_void<Ret>::value>* = nullptr>
    auto map_error_impl(Exp&& exp, F&& f) noexcept {
        using result = expected<exp_t<Exp>, monostate>;
        if (exp.has_value()) {
            return result(*std::forward<Exp>(exp));
        }

        std::invoke(std::forward<F>(f), std::forward<Exp>(exp).error());
        return result(unexpect, monostate {});
    }
    template<
        typename Exp,
        typename F,
        std::enable_if_t<std::is_void<exp_t<Exp>>::value>* = nullptr,
        typename Ret = decltype(std::invoke(std::declval<F>(), std::declval<Exp>().error())),
        std::enable_if_t<!std::is_void<Ret>::value>* = nullptr>
    constexpr auto map_error_impl(Exp&& exp, F&& f) noexcept {
        using result = expected<exp_t<Exp>, std::decay_t<Ret>>;
        return exp.has_value()
            ? result()
            : result(unexpect, std::invoke(std::forward<F>(f), std::forward<Exp>(exp).error()));
    }
    template<
        typename Exp,
        typename F,
        std::enable_if_t<std::is_void<exp_t<Exp>>::value>* = nullptr,
        typename Ret = decltype(std::invoke(std::declval<F>(), std::declval<Exp>().error())),
        std::enable_if_t<std::is_void<Ret>::value>* = nullptr>
    auto map_error_impl(Exp&& exp, F&& f) noexcept {
        using result = expected<exp_t<Exp>, monostate>;
        if (exp.has_value()) {
            return result();
        }

        std::invoke(std::forward<F>(f), std::forward<Exp>(exp).error());
        return result(unexpect, monostate {});
    }

    template<
        typename Exp,
        typename F,
        typename Ret = decltype(std::invoke(std::declval<F>(), std::declval<Exp>().error())),
        std::enable_if_t<!std::is_void<Ret>::value>* = nullptr>
    constexpr auto or_else_impl(Exp&& exp, F&& f) noexcept {
        static_assert(detail::is_expected<Ret>::value, "F must return an expected");
        return exp.has_value() ? std::forward<Exp>(exp)
                               : std::invoke(std::forward<F>(f), std::forward<Exp>(exp).error());
    }

    template<
        typename Exp,
        typename F,
        typename Ret = decltype(std::invoke(std::declval<F>(), std::declval<Exp>().error())),
        std::enable_if_t<std::is_void<Ret>::value>* = nullptr>
    std::decay_t<Exp> or_else_impl(Exp&& exp, F&& f) noexcept {
        return exp.has_value() ? std::forward<Exp>(exp)
                               : (std::invoke(std::forward<F>(f), std::forward<Exp>(exp).error()),
                                  std::forward<Exp>(exp));
    }
}  // namespace detail

template<typename T, typename E, typename U, typename F>
constexpr bool operator==(const expected<T, E>& lhs, const expected<U, F>& rhs) noexcept {
    return (lhs.has_value() != rhs.has_value())
        ? false
        : (!lhs.has_value() ? lhs.error() == rhs.error() : *lhs == *rhs);
}
template<typename T, typename E, typename U, typename F>
constexpr bool operator!=(const expected<T, E>& lhs, const expected<U, F>& rhs) noexcept {
    return (lhs.has_value() != rhs.has_value())
        ? true
        : (!lhs.has_value() ? lhs.error() != rhs.error() : *lhs != *rhs);
}
template<typename E, typename F>
constexpr bool operator==(const expected<void, E>& lhs, const expected<void, F>& rhs) noexcept {
    return (lhs.has_value() != rhs.has_value())
        ? false
        : (!lhs.has_value() ? lhs.error() == rhs.error() : true);
}
template<typename E, typename F>
constexpr bool operator!=(const expected<void, E>& lhs, const expected<void, F>& rhs) noexcept {
    return (lhs.has_value() != rhs.has_value())
        ? true
        : (!lhs.has_value() ? lhs.error() == rhs.error() : false);
}

template<typename T, typename E, typename U>
constexpr bool operator==(const expected<T, E>& x, const U& v) noexcept {
    return x.has_value() ? *x == v : false;
}
template<typename T, typename E, typename U>
constexpr bool operator==(const U& v, const expected<T, E>& x) noexcept {
    return x.has_value() ? *x == v : false;
}
template<typename T, typename E, typename U>
constexpr bool operator!=(const expected<T, E>& x, const U& v) noexcept {
    return x.has_value() ? *x != v : true;
}
template<typename T, typename E, typename U>
constexpr bool operator!=(const U& v, const expected<T, E>& x) noexcept {
    return x.has_value() ? *x != v : true;
}

template<typename T, typename E>
constexpr bool operator==(const expected<T, E>& x, const unexpected<E>& e) noexcept {
    return x.has_value() ? false : x.error() == e.value();
}
template<typename T, typename E>
constexpr bool operator==(const unexpected<E>& e, const expected<T, E>& x) noexcept {
    return x.has_value() ? false : x.error() == e.value();
}
template<typename T, typename E>
constexpr bool operator!=(const expected<T, E>& x, const unexpected<E>& e) noexcept {
    return x.has_value() ? true : x.error() != e.value();
}
template<typename T, typename E>
constexpr bool operator!=(const unexpected<E>& e, const expected<T, E>& x) noexcept {
    return x.has_value() ? true : x.error() != e.value();
}

template<
    typename T,
    typename E,
    std::enable_if_t<
        (std::is_void<T>::value || std::is_move_constructible<T>::value)
        && std::is_swappable<T>::value && std::is_move_constructible<E>::value
        && std::is_swappable<E>::value>* = nullptr>
void swap(expected<T, E>& lhs, expected<T, E>& rhs) noexcept {
    lhs.swap(rhs);
}
}  // namespace ktl

#define TryAutoV2(tvar, expr) \
    static_assert(::ktl::detail::is_expected<std::decay_t<decltype((expr))>>::value); \
    auto tvar = (expr); \
    if (!tvar) [[unlikely]] { \
        return ::ktl::make_unexpected(::std::move(tvar).error()); \
    }
#define TryAuto2(var, tvar, expr) \
    TryAutoV2(tvar, (expr)); \
    auto&& var = *std::move(tvar)

#define TryV(expr) \
    do { \
        TryAutoV2(CONCAT(tmp_, __LINE__), (expr)); \
    } while (false)
#define Try(expr) \
    ({ \
        static_assert(::ktl::detail::is_expected<std::decay_t<decltype((expr))>>::value); \
        auto res = (expr); \
        if (!res) [[unlikely]] { \
            return ::ktl::make_unexpected(::std::move(res).error()); \
        }; \
        *std::move(res); \
    })

#define TryAuto(var, expr) TryAuto2((var), CONCAT(tmp_, __LINE__), (expr))
#define TryAssign(lvalue, expr) \
    TryAutoV2(CONCAT(tmp_, __LINE__), (expr)); \
    (lvalue) = *::std::move(CONCAT(tmp_, __LINE__))

#define TryReturnV(expr) \
    TryAutoV2(CONCAT(tmp_, __LINE__), (expr)); \
    return {}
#define TryReturn(expr) \
    TryAuto2(CONCAT(ret_, __LINE__), CONCAT(tmp_, __LINE__), (expr)); \
    return *::std::move(CONCAT(ret_, __LINE__))

// NOLINTEND