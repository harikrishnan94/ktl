#pragma once

#include <utility>

#include <ktl/error.hpp>
#include <ktl/memory.hpp>

#include "detail/generator_common.hpp"

namespace ktl::coro {
namespace detail {
    // Generic promise type for generator implementation
    // This simply stores the yield value of the coroutine and makes it available to the generator.
    template<typename Generator>
    class promise {
      private:
        using generator_type = Generator;
        using allocator_type = generator_type::allocator_type;
        using error_type = generator_type::error_type;
        using value_type = generator_type::value_type;

      public:
        promise() = default;
        ~promise() = default;

        // Promise is a `Pinned` type. (I.e) it's address doesn't change throught it's lifetime.
        promise(promise&&) noexcept = delete;
        auto operator=(promise&&) noexcept -> promise& = delete;
        auto operator=(const promise&) -> promise& = delete;
        promise(const promise&) = delete;

        // Used by generator to access the yield value
        auto value() noexcept -> value_type* {
            return m_yield_value;
        }

        auto get_return_object() noexcept {
            return handle<promise>::from_promise(*this);
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
            m_yield_value = std::addressof(value);
            return {};
        }
        auto yield_value(std::remove_reference_t<value_type>&& value) noexcept
            -> std::suspend_always {
            m_yield_value = std::addressof(value);
            return {};
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

        // Value return'ed by co_yield or co_return is allocated in coroutine frame.
        // So, it's safe to just store pointer to the return value.
        value_type* m_yield_value = nullptr;
    };

    template<typename T, error_like Error, allocator_like Allocator>
    class generator {
      public:
        using value_type = T;
        using error_type = Error;
        using allocator_type = Allocator;
        // Don't define promise_type, instead use someother name.
        // This prevents generator from being directly used as coroutine return type.
        using promise_t = promise<generator>;
        using handle_type = handle<promise_t>;

        // NOLINTNEXTLINE(*-explicit-conversions)
        generator(handle_type coro) : m_coro {std::move(coro)} {}

        constexpr auto begin() noexcept -> iterator<generator> {
            return iterator {*this};
        }
        constexpr auto end() noexcept -> std::default_sentinel_t {
            return std::default_sentinel;
        }

        // Is Coroutine Done?
        // Once, this function returns true, no calls to `resume()` must be made.
        [[nodiscard]] auto done() const noexcept -> bool {
            return m_coro.done();
        }

        // Return the value returned by latest call to resume.
        // pre: `!done()`
        [[nodiscard]] auto current_value() const noexcept -> value_type& {
            check_(!done(), "");
            return *m_coro.promise().value();
        }

        // Resume the coroutine.
        // pre: `!done()`
        void resume() noexcept {
            m_coro.resume();
        }

      private:
        handle_type m_coro;
    };
}  // namespace detail

// Basic Generator: Can accept any `error_like` type as Error.
template<typename T, error_like Error, allocator_like Allocator>
using basic_generator = expected<detail::generator<T, Error, Allocator>, Error>;

// specialized generator: `Error` defined to ktl::Error.
template<typename T, allocator_like Allocator>
using generator = basic_generator<T, Error, Allocator>;
}  // namespace ktl::coro

namespace std {
// Partial specialization to allow any `expected<basic_generator, error_like>` to be used as
// return type of coroutine function
template<
    typename T,
    ktl::coro::error_like Error,
    ktl::coro::allocator_like Allocator,
    typename... Args>
struct coroutine_traits<ktl::coro::basic_generator<T, Error, Allocator>, Args...> {
    using promise_type =
        ktl::coro::detail::promise<ktl::coro::detail::generator<T, Error, Allocator>>;
};
}  // namespace std
