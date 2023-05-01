#pragma once

#include <coroutine>
#include <variant>

#include "detail/generator_common.hpp"

namespace ktl::coro {
namespace detail {
    // Generic recursive_promise type for recursive_generator implementation
    // Promises are stacked, and always the latest (top) promise is resumed.
    template<typename Generator>
    class recursive_promise {
      private:
        using generator_type = Generator;
        using allocator_type = generator_type::allocator_type;
        using error_type = generator_type::error_type;
        using value_type = generator_type::value_type;
        using handle_type = std::coroutine_handle<recursive_promise>;

      public:
        recursive_promise() = default;
        ~recursive_promise() = default;

        // Recursive Promise is a `Pinned` type. (I.e) it's address doesn't change throught it's
        // lifetime.
        recursive_promise(recursive_promise&&) noexcept = delete;
        auto operator=(recursive_promise&&) noexcept -> recursive_promise& = delete;
        auto operator=(const recursive_promise&) -> recursive_promise& = delete;
        recursive_promise(const recursive_promise&) = delete;

        [[nodiscard]] constexpr auto has_error() const noexcept -> bool {
            return std::holds_alternative<not_null<error_type*>>(m_state);
        }
        constexpr auto error() noexcept -> not_null<error_type*>* {
            check_(has_error(), "");
            return std::get_if<not_null<error_type*>>(&m_state);
        }

        constexpr auto value() noexcept -> not_null<value_type*>* {
            check_(std::holds_alternative<not_null<value_type*>>(m_state), "");
            return std::get_if<not_null<value_type*>>(&m_state);
        }

        // Return the top promise from the stack.
        // Since, we don't have the pointer to the `top` ready, we need to work it out by following
        // `child` pointers
        constexpr auto pull() noexcept -> recursive_promise& {
            auto* p = this;
            while (p->m_child) {
                p = p->m_child;
            }
            return *p;
        }

        // Pop the coroutine from the top of the stack (must be top)
        // and return it's parent (new top).
        constexpr auto pop() const noexcept -> recursive_promise* {
            check_(is_top(), "");

            // Obtain the parent (new top)
            if (m_parent) {
                check_(m_parent->m_child == this, "");  // Assert stack is well formed.
                m_parent->m_child = nullptr;  // Make `parent` the new top.
            }
            // After this point, current promise is no longer in the stack.
            // And is reachable only from it's generator (if stored).
            // But the coroutine should've been done at this point, so only `done()` and `destroy()`
            // can be used.
            return m_parent;
        }

        // Check if current coroutine is still at the top of the stack.
        // If a `child` coroutine is produced by yielding a generator, the new (child) promise
        // becomes the new top. See `yield_gen`.
        [[nodiscard]] constexpr auto is_top() const noexcept -> bool {
            // Top coro must not have any child. This is by definition.
            return m_child == nullptr;
        }

        auto get_return_object() noexcept {
            return handle<recursive_promise>::from_promise(*this);
        }
        static auto get_return_object_on_allocation_failure() noexcept {
            Throw(CoroError<error_type>::AllocFailure());
        }

        auto initial_suspend() noexcept -> std::suspend_always {
            return {};
        }
        auto final_suspend() noexcept -> std::suspend_always {
            return {};
        }

        // Value return'ed by co_yield or co_return is allocated in coroutine frame.
        // So, it's safe to just store pointer to the return value.
        auto yield_value(std::remove_reference_t<value_type>& value) noexcept -> std::suspend_always
            requires(!std::is_rvalue_reference_v<value_type>)
        {
            m_state = std::addressof(value);
            return {};
        }
        auto yield_value(std::remove_reference_t<value_type>&& value) noexcept
            -> std::suspend_always {
            m_state = std::addressof(value);
            return {};
        }

        // yield a generator (recursive coroutine)
        auto yield_value(expected<generator_type, error_type>&& gen) noexcept {
            return yield_gen(gen);
        }
        auto yield_value(expected<generator_type, error_type>& gen) noexcept {
            return yield_gen(gen);
        }

        void return_void() noexcept {}

        void unhandled_exception() noexcept {
            abort_("unexpected exception");
        }
        void rethrow_if_exception() noexcept {}

        // Don't allow any use of 'co_await' inside the generator coroutine.
        template<typename U>
        auto await_transform(U&& value) -> std::suspend_never = delete;

        PROMISE_TYPE_ALLOCATOR_DEFS

      private:
        // Stack the new coroutine, if it's not an error.
        auto yield_gen(expected<generator_type, error_type>& gen) noexcept {
            struct awaitable {
                constexpr explicit awaitable(std::nullptr_t) noexcept : m_child {nullptr} {}
                explicit constexpr awaitable(recursive_promise& p) noexcept : m_child {&p} {}

                // If child is null, execute the current coroutine immediately.
                constexpr auto await_ready() const noexcept {
                    return m_child == nullptr;
                }

                constexpr void await_suspend(handle_type /*h*/) noexcept {}
                constexpr void await_resume() noexcept {}

                recursive_promise* m_child;  // NOLINT(*-non-private-member-variables-in-classes)
            };

            if (gen) {
                recursive_promise& child = gen->m_coro.promise();

                // If child promise is done, execute the current coroutine immediately.
                if (handle_type::from_promise(child).done()) {
                    return awaitable {nullptr};
                }

                // Push child onto the stack (making it the new top).
                child.m_parent = this;
                this->m_child = &child;

                return awaitable {child};
            }
            // If child has an error, stop store the error (this signals the generator to stop
            // coroutine execution and entire stack is aborted)
            m_state = &gen.error();
            return awaitable {nullptr};
        }

        // Pointer to either value_type or error.
        std::variant<std::monostate, not_null<value_type*>, not_null<error_type*>> m_state;
        // Child promise, yielded by this coroutine.
        recursive_promise* m_child = nullptr;
        // Next (parent) promise to execute after `this` is done.
        // This is present only as optimization to fetch the `previous top` promise, once current
        // one is done.
        recursive_promise* m_parent = nullptr;
    };

    // Recursive Generator supports yielding a coroutine directly from another coroutine.
    template<typename T, error_like Error, allocator_like Allocator>
    class recursive_generator {
      public:
        using value_type = T;
        using error_type = Error;
        using allocator_type = Allocator;
        // Don't define promise_type, instead use someother name.
        // This prevents generator from being directly used as coroutine return type.
        using promise_t = recursive_promise<recursive_generator>;
        using handle_type = handle<promise_t>;

        // NOLINTNEXTLINE(*-explicit-conversions)
        recursive_generator(handle_type coro) : m_coro {std::move(coro)} {}

        constexpr auto begin() noexcept -> iterator<recursive_generator> {
            return iterator {*this};
        }
        constexpr auto end() noexcept -> std::default_sentinel_t {
            return std::default_sentinel;
        }

        // Is Coroutine Done or contains it an error?
        // Once, this function returns true, no calls to `resume()` must be made.
        [[nodiscard]] auto done() const noexcept -> bool {
            check_(m_coro, "");
            return m_coro.done() || has_error();
        }

        // Returns the value yielded by the top most coroutine in the stack
        // pre: `!done()`
        [[nodiscard]] constexpr auto current_value() const noexcept -> value_type& {
            check_(!done(), "");
            return **m_top_promise->value();
        }

        // `not has_error()`
        constexpr explicit operator bool() const noexcept {
            return !has_error();
        }
        // Did the top_promise encounter an error?
        [[nodiscard]] constexpr auto has_error() const noexcept -> bool {
            check_(m_coro, "");
            return m_top_promise->has_error();
        }
        // pre: `has_error()`
        [[nodiscard]] constexpr auto error() const noexcept -> error_type& {
            check_(has_error(), "");
            return **m_top_promise->error();
        }

        // Resume the coroutine execution.
        // This always resumes the top most coroutine and also updates the cached top.
        // pre: `!done()`
        void resume() noexcept {
            check_(!done(), "");

            while (true) {
                check_(m_top_promise->is_top(), "");
                auto top = handle_type::pointer_type::from_promise(*m_top_promise);

                // resume top most coroutine.
                top.resume();

                // Top coro is done?
                if (top.done()) [[unlikely]] {
                    // Obtain the new top.
                    if (auto* new_top = m_top_promise->pop()) [[likely]] {
                        m_top_promise = new_top;
                        continue;
                    }

                    // This mean's `m_coro` is already the top coroutine and it's done.
                    // Entire stack is completed, no more `resume()` calls are allowed
                    break;
                }

                // If `m_top_promise` is still the top, this means we've got a Value or Error (and
                // not a child coroutine). Good, return.
                if (m_top_promise->is_top()) [[likely]] {
                    break;
                }

                // Pull the new top and resume from there.
                m_top_promise = &m_top_promise->pull();
            }
        }

      private:
        friend promise_t;
        handle_type m_coro;
        // Cached value of the top most coroutine's promise.
        not_null<promise_t*> m_top_promise = &m_coro.promise();
    };
}  // namespace detail

// Basic Recursive Generator: Can accept any `error_like` type as Error.
template<typename T, error_like Error, allocator_like Allocator>
using basic_recursive_generator = expected<detail::recursive_generator<T, Error, Allocator>, Error>;

// specialized recursive generator: `Error` defined to ktl::Error.
template<typename T, allocator_like Allocator>
using recursive_generator = basic_recursive_generator<T, Error, Allocator>;
}  // namespace ktl::coro

namespace std {
// Partial specialization to allow any `expected<basic_recursive_generator, error_like>` to be used
// as return type of coroutine function
template<
    typename T,
    ktl::coro::error_like Error,
    ktl::coro::allocator_like Allocator,
    typename... Args>
struct coroutine_traits<ktl::coro::basic_recursive_generator<T, Error, Allocator>, Args...> {
    using promise_type = ktl::coro::detail::recursive_promise<
        ktl::coro::detail::recursive_generator<T, Error, Allocator>>;
};
}  // namespace std