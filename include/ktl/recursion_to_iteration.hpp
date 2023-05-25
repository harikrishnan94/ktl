#pragma once

#include <optional>
#include <variant>

#include "detail/generator_common.hpp"

// This header provides tools for writing iterative algorithms easily, by automatically converting
// recursion into iteration. This is made possible by utilizing C++20 coroutines.
// C++20 coroutines enables returning (yielding) from a function multiple times and allows
// maintaining state across resumptions.
//
// Implementation notes:
// --------------------
// C++ coroutine requires a promise type through which the coroutine needs to be interacted.
// `enable_iteration_promise` type provides the promise type for the coroutine.
// To reduce the coroutine frame size, `enable_iteration_promise` is designed to contain only bare
// minimal contents and all other common (across all coroutine frame of a particular coroutine)
// arguments like Allocator, Return Value storage, etc are stored outside the promise in a separate
// `Context` called (`enable_iteration`).
// On the flip side, this separation mandates implementations to inherit `enable_iteration` to
// obtain the iteration capability. This means, the implementation has to wrapped inside a
// struct/class which inherits an appropriate instantiation of `enable_iteration` template. On the
// plus side, common arguments to the coroutine can be stored in the `Context` object itself,
// reducing the coroutine frame size.

namespace ktl::coro {
namespace detail {
    // Opaque type to disallow using the return type of the coroutine.
    template<typename T, typename Context>
    struct opaque_coro_return;

    // Promise type for coroutines to `enable_iteration`.
    // Return type of `enable_iteration` coroutines is the pointer to the promise itself (wrapped as
    // `opaque_coro_return`).
    // Coroutine return type is cannot be utilized by the users in a meaningful manner as entire
    // state (or context) is stored outside and is provided by a separate `Context` object.
    // Promise only stores the link to the next coroutine to be executed once, current coroutine is
    // done.
    template<typename T, typename Context>
    class enable_iteration_promise {
      public:
        using return_type = opaque_coro_return<T, Context>*;

        // `Type erase` the promise type by returning pointer to an opaque type.
        auto get_return_object() noexcept -> return_type {
            return std::bit_cast<return_type>(this);
        }
        static constexpr auto get_return_object_on_allocation_failure() noexcept -> return_type {
            return nullptr;
        }

        // Store the value yield'ed by the coroutine into the `context`.
        // Instance to the current `Context` is also provided by the coroutine.
        constexpr auto yield_value(std::pair<Context*, T>&& val) noexcept -> std::suspend_always {
            auto&& [context, v] = val;
            context->set_current_value(std::move(v));
            return {};
        }

        // Current coroutine `recurses` i.e calls another coroutine and yields a promise to it.
        auto yield_value(std::pair<Context*, return_type> promise) noexcept {
            struct awaitable {
                constexpr explicit awaitable(std::nullptr_t) noexcept : m_child {nullptr} {}
                explicit constexpr awaitable(enable_iteration_promise& p) noexcept : m_child {&p} {}

                // If child is null, execute the current coroutine immediately.
                [[nodiscard]] constexpr auto await_ready() const noexcept {
                    return m_child == nullptr;
                }

                constexpr void
                await_suspend(std::coroutine_handle<enable_iteration_promise> /*h*/) noexcept {}
                constexpr void await_resume() noexcept {}

                // NOLINTNEXTLINE(*-non-private-member-variables-in-classes)
                enable_iteration_promise* m_child;
            };

            auto&& [context, child_p] = promise;
            auto* child = std::bit_cast<enable_iteration_promise*>(child_p);

            // If the child promise is empty (allocation failure) or done (reaches the end), then
            // resume the current coroutine instantly by returning true from `await_ready` of the
            // awaitable.
            if (child == nullptr) [[unlikely]] {
                context->set_error(error::OutOfMemory);
                return awaitable {nullptr};
            }

            auto handle = std::coroutine_handle<enable_iteration_promise>::from_promise(*child);
            if (handle.done()) [[unlikely]] {
                return awaitable {nullptr};
            }

            // Child coroutine is valid and will yield atleast one `value`. So, suspend the current
            // coroutine, push child onto stack, to resume it as early as possible.
            child->m_next = this;
            context->push(child);

            return awaitable {*child};
        }

        [[nodiscard]] constexpr auto next() const noexcept {
            return m_next;
        }

        constexpr void return_void() noexcept {}
        constexpr auto initial_suspend() noexcept -> std::suspend_always {
            return {};
        }
        constexpr auto final_suspend() noexcept -> std::suspend_always {
            return {};
        }

        void unhandled_exception() noexcept {
            abort_("unexpected exception");
        }
        constexpr void rethrow_if_exception() noexcept {}

        // Don't allow any use of 'co_await' inside the generator coroutine.
        template<typename U>
        auto await_transform(U&& value) -> std::suspend_never = delete;

        // Allocate memory for the coroutine frame from the `allocator` provided by the context.
        template<typename... Args>
        static auto operator new(usize n, Context& context, const Args&... /*args*/) noexcept
            -> void* {
            return context.allocate_bytes(n);
        }

        // We don't have the allocator to delete the memory here.
        // So, just store the size of the memory region to be deleted in the start of the memory
        // itself and leave it to the `Context` to free the memory.
        static void operator delete(void* ptr, usize n) noexcept {
            check_(n >= sizeof(usize), "");
            // Store the size of the region. Will be deleted by `Context`
            // Use `construct_at` to begin lifetime for `size` object.
            std::construct_at(static_cast<usize*>(ptr), n);
        }

      private:
        enable_iteration_promise* m_next = nullptr;  // Next promise in the stack.
    };

    // Used by `invoke` family of functions to push the initiating coroutine's promise into the
    // stack.
    struct push_promise {
        template<typename EnableIteration, typename EnableIterationPromise>
        constexpr push_promise(EnableIteration& ctx, EnableIterationPromise* promise) noexcept {
            ctx.push(promise);
        }
    };
}  // namespace detail

// Base class for the implementation `Context` s.
// This class provides mechanism to store and resume stacked coroutines at appropriate times to
// model recursion.
// Coroutines present in the implementations must use `enable_iteration::yield` member function to
// yield a value to caller or to recurse.
// Also, such coroutines must be declared to return `enable_iteration::return_type`.
// To maximize the performance and to reduce coroutine frame size, arguments constant (same) across
// all recursive invocations of the coroutine must be stored in the Context object itself.
template<std::movable T, coro::error_like E, coro::allocator_like CoroAllocator>
class enable_iteration {
  public:
    using value_type = T;
    using error_type = E;
    using iterator = detail::iterator<enable_iteration>;
    using promise_type = detail::enable_iteration_promise<value_type, enable_iteration>;
    using return_type = promise_type::return_type;
    using handle_type = std::coroutine_handle<promise_type>;
    using is_invocable = std::true_type;

  private:
    using unexpected_t = unexpected<error_type>;

  public:
    // Models a range (input_iterator) of values yielded by the coroutine.
    class range {
      public:
        using value_type = T;
        using iterator = enable_iteration::iterator;

        auto begin() const noexcept -> iterator {
            check_(!m_ctx->done(), "");
            return iterator {*m_ctx};
        }

        [[nodiscard]] constexpr auto end() const noexcept -> std::default_sentinel_t {
            return std::default_sentinel;
        }

      private:
        friend enable_iteration;

        explicit constexpr range(enable_iteration& ctx) noexcept : m_ctx {&ctx} {}

        not_null<enable_iteration*> m_ctx;
    };

    explicit constexpr enable_iteration(CoroAllocator coro_alloc) noexcept :
        m_coro_alloc {std::move(coro_alloc)} {}

    explicit constexpr enable_iteration(unexpected_t error) noexcept :
        m_cur_yield {std::move(error)} {}

    ~enable_iteration() noexcept {
        // Destory coroutines in the reverse order of their construction
        while (m_top) {
            destroy(std::exchange(m_top, m_top->next()));
        }
    }

    enable_iteration(const enable_iteration&) = delete;
    auto operator=(const enable_iteration&) -> enable_iteration& = delete;

    constexpr enable_iteration(enable_iteration&& o) noexcept :
        m_coro_alloc {std::move(o.m_coro_alloc)},
        m_cur_yield {std::move(o.m_cur_yield)},
        m_top {std::exchange(o.m_top, nullptr)} {}

    constexpr auto operator=(enable_iteration&& o) noexcept -> enable_iteration& {
        using std::swap;
        swap(m_coro_alloc, o.m_coro_alloc);
        swap(m_cur_yield, o.m_cur_yield);
        swap(m_top, o.m_top);

        return *this;
    }

    // Obtain a range(input iterator) to iterate over the values yield by the corotine stack
    constexpr auto iter() noexcept -> expected<range, Error> {
        if (has_error()) {
            Throw(error());
        }
        return range {*this};
    }

    // Did the coroutine invocation or resume encounter an error?
    [[nodiscard]] constexpr auto has_error() const noexcept -> bool {
        return std::holds_alternative<unexpected_t>(m_cur_yield);
    }

    // `not has_error()`
    constexpr explicit operator bool() const noexcept {
        return !has_error();
    }

    // pre: `has_error()`
    [[nodiscard]] constexpr auto error() const noexcept -> Error {
        check_(has_error(), "");
        return std::get<unexpected_t>(m_cur_yield).value();
    }

    // Is Coroutine Done or contains it an error?
    // Once, this function returns true, no calls to `resume()` must be made.
    [[nodiscard]] constexpr auto done() const noexcept -> bool {
        return m_top == nullptr || has_error();
    }

    // Returns the value yielded by the top most coroutine in the stack
    // pre: `!done()`
    [[nodiscard]] constexpr auto current_value() noexcept -> value_type& {
        check_(!done() && std::holds_alternative<value_type>(m_cur_yield), "");
        return std::get<value_type>(m_cur_yield);
    }

    // Resume the coroutine execution.
    // This always resumes the top most coroutine.
    // pre: `!done()`
    void resume() noexcept {
        check_(!done(), "");

        while (m_top != nullptr) {
            auto top = m_top;
            auto coro = handle_type::from_promise(*top);

            coro.resume();
            if (coro.done()) {
                m_top = top->next();
                destroy(top);
                continue;
            }

            if (m_top == top) {
                break;
            }
        }
    }

  protected:
    // Yield a promise: Store the Recursive promise
    constexpr auto yield(return_type promise) noexcept
        -> std::pair<enable_iteration*, return_type> {
        return {this, promise};
    }

    // Yield a value convertible to `T`
    template<std::convertible_to<value_type> U>
    constexpr auto yield(U&& v) noexcept -> std::pair<enable_iteration*, value_type> {
        return {this, std::forward<U>(v)};
    }

  private:
    friend promise_type;
    friend struct detail::push_promise;

    // Allocate memory for coroutine frame
    auto allocate_bytes(usize n) noexcept -> void* {
        check_(m_coro_alloc, "");
        auto ptr = allocator_traits<CoroAllocator>::allocate(*m_coro_alloc, n);
        if (ptr) {
            return static_cast<void*>(&**ptr);
        }
        set_error(std::move(ptr).error());
        return nullptr;
    }

    // pre: !done()
    constexpr void set_current_value(value_type v) noexcept {
        check_(!done(), "");
        m_cur_yield = std::move(v);
    }

    constexpr void set_error(error_type e) noexcept {
        if (!std::holds_alternative<unexpected_t>(m_cur_yield)) [[likely]] {
            m_cur_yield = make_unexpected(std::move(e));
        }
    }

    // pre: !has_error()
    constexpr void push(promise_type* top) noexcept {
        check_(!has_error(), "");
        m_top = top;
    }

    void destroy(promise_type* promise) {
        check_(m_coro_alloc, "");
        auto handle = handle_type::from_promise(*promise);
        auto addr = handle.address();

        // Following will only call `destructors` for the coroutine frame and promise object.
        // Promise object doesn't have information needed (allocator) to delete the memory.
        // Instead it stores the size of the memory region to be deleted and returns.
        handle.destroy();

        // Obtain the size of the memory region (stored by promise's delete operator) and free the
        // memory.
        auto* size = static_cast<usize*>(addr);
        check_(*size != 0, "");
        allocator_traits<CoroAllocator>::deallocate(*m_coro_alloc, static_cast<u8*>(addr), *size);
    }

    struct empty_t {};

    std::optional<CoroAllocator> m_coro_alloc = {};
    std::variant<empty_t, value_type, unexpected_t> m_cur_yield = {};
    promise_type* m_top = nullptr;
};

// `make_iterator` helper constructs the `Context` type with arguments provided (`cons_args`) and
// invokes the coroutine (member function of `Context`) with the arguments provided (`coro_args`)
// and pushes the initiating coroutine into the `Context`s stack.
template<typename Context, typename... CoroArgs, typename... ConstructorArgs>
    requires Context::is_invocable::value
    && std::constructible_from<Context, ConstructorArgs...> && (sizeof...(CoroArgs) > 0)
auto make_iterator(
    auto (Context::*coro)(CoroArgs...),
    std::tuple<ConstructorArgs...> cons_args,
    std::tuple<CoroArgs...> coro_args) noexcept {
    // Construct an instance of the `Context` using the constructor arguments (`cons_args`)
    auto ctx = std::apply(
        [&](ConstructorArgs... cons_args) {
            Context ctx {std::forward<ConstructorArgs>(cons_args)...};
            return ctx;
        },
        std::move(cons_args));

    // Invoke the coroutine to obtain an initial promise to it..
    auto promise = std::apply(
        [&](CoroArgs... coro_args) {
            return std::invoke(coro, ctx, std::forward<CoroArgs>(coro_args)...);
        },
        std::move(coro_args));

    // Then, stack the promise if it's not null
    if (promise != nullptr) {
        detail::push_promise(
            ctx,
            std::bit_cast<std::add_pointer_t<typename Context::promise_type>>(promise));
    }

    return ctx;
}

// `make_iterator` helper constructs the `Context` type with arguments provided (`cons_args`) and
// invokes the coroutine (member function of `Context`) and pushes the initiating coroutine into the
// `Context`s stack.
template<typename Context, typename... ConstructorArgs>
    requires Context::is_invocable::value && std::constructible_from<Context, ConstructorArgs...>
auto make_iterator(auto (Context::*coro)(), std::tuple<ConstructorArgs...> cons_args) noexcept {
    // Construct an instance of the `Context` using the constructor arguments (`cons_args`)
    auto ctx = std::apply(
        [&](ConstructorArgs... cons_args) {
            Context ctx {std::forward<ConstructorArgs>(cons_args)...};
            return ctx;
        },
        std::move(cons_args));

    // Invoke the coroutine to obtain an initial promise to it, and stack the promise if it's not
    // null
    if (auto promise = std::invoke(coro, ctx)) {
        detail::push_promise(
            ctx,
            std::bit_cast<std::add_pointer_t<typename Context::promise_type>>(promise));
    }

    return ctx;
}
}  // namespace ktl::coro

namespace std {
template<
    typename T,
    typename E,
    ::ktl::coro::allocator_like Allocator,
    typename Derived,
    typename... Args>
    requires is_base_of_v<::ktl::coro::enable_iteration<T, E, Allocator>, Derived>
struct coroutine_traits<
    ::ktl::coro::detail::opaque_coro_return<T, ::ktl::coro::enable_iteration<T, E, Allocator>>*,
    Derived&,
    Args...> {
    using promise_type = ::ktl::coro::detail::
        enable_iteration_promise<T, ::ktl::coro::enable_iteration<T, E, Allocator>>;
};
}  // namespace std