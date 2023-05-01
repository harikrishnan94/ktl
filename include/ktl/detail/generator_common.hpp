#pragma once

#include <coroutine>
#include <iterator>
#include <utility>

#include <ktl/align.hpp>
#include <ktl/error.hpp>
#include <ktl/int.hpp>
#include <ktl/memory.hpp>
#include <ktl/not_null.hpp>

namespace ktl {
template<typename ErrorT>
struct CoroError;

template<>
struct CoroError<Error> {
    // Coroutine Frame Allocation Failure
    static auto AllocFailure() noexcept -> Error {
        DEFINE_ERROR_LOCAL_EXT(AllocFailure, "CoroAllocFailure");
        return AllocFailure;
    }
};

namespace coro {
    // Simple RAII Wrapper over std::coroutine_handle<Promise>
    template<typename Promise>
    class handle {
      public:
        using promise_type = Promise;
        using pointer_type = std::coroutine_handle<promise_type>;

        handle() = default;
        static auto from_promise(promise_type& p) -> handle {
            handle h;
            h.m_handle = pointer_type::from_promise(p);
            return h;
        }

        handle(const handle&) = delete;
        auto operator=(const handle&) -> handle& = delete;

        handle(handle&& o) noexcept : m_handle {std::exchange(o.m_handle, {})} {}
        auto operator=(handle&& o) noexcept -> handle& {
            using std::swap;

            swap(m_handle, o.m_handle);
            return *this;
        }

        ~handle() noexcept {
            if (m_handle) {
                m_handle.destroy();
            }
        }

        constexpr explicit operator bool() const noexcept {
            return static_cast<bool>(m_handle);
        }

        [[nodiscard]] auto done() const noexcept -> bool {
            check_(m_handle, "");
            return m_handle.done();
        }

        void resume() const {
            check_(m_handle && !m_handle.done(), "");
            m_handle.resume();
        }

        auto promise() const -> promise_type& {
            check_(m_handle, "");
            return m_handle.promise();
        }

        auto raw() const -> pointer_type {
            check_(m_handle, "");
            return m_handle;
        }

      private:
        pointer_type m_handle = {};
    };

    // Coro Allocator concept
    template<typename A>
    concept allocator_like = allocator_for<A, u8>;

    template<typename E>
    concept error_like = requires {
        { CoroError<E>::AllocFailure() } -> std::same_as<E>;
    };

    namespace detail {
        // An Input iterator that can work with any generator like class
        template<typename Generator>
        class iterator {
          public:
            using value_type = Generator::value_type;
            using difference_type = std::ptrdiff_t;
            using reference = value_type&;
            using pointer = not_null<value_type*>;

            friend auto
            operator==(const iterator& it, std::default_sentinel_t /* sentinel */) noexcept
                -> bool {
                return it.m_gen->done();
            }

            auto operator*() const noexcept -> reference {
                return m_gen->current_value();
            }
            auto operator->() const noexcept -> pointer {
                return &m_gen->current_value();
            }

            auto operator++() noexcept -> iterator& {
                m_gen->resume();
                return *this;
            }
            constexpr auto operator++(int) noexcept -> iterator {
                auto copy = *this;
                ++*this;
                return copy;
            }

          private:
            explicit constexpr iterator(Generator& gen) : m_gen {&gen} {
                m_gen->resume();
            }

            friend Generator;

            not_null<Generator*> m_gen;
        };

#define PROMISE_TYPE_ALLOCATOR_DEFS \
    /*  custom non-throwing overload of new */ \
    template<typename... Args> \
    static auto operator new(usize n, allocator_type alloc, const Args&... /*args*/) noexcept \
        -> void* { \
        return allocate(n, alloc); \
    } \
\
    template<typename FirstArg, typename... Args> \
        requires( \
            !std::same_as<FirstArg, allocator_type> \
            && std::is_default_constructible_v<allocator_type>) \
    static auto operator new( \
        usize n, \
        const FirstArg& /* arg1 */, \
        const Args&... /*args*/) noexcept -> void* { \
        return allocate(n, allocator_type {}); \
    } \
\
    /*  Enforces HALO optimization. If HALO optimization is not done, error is thrown in */ \
    /*  ** RUNTIME **. */ \
    template<typename FirstArg, typename... Args> \
        requires( \
            !std::same_as<FirstArg, allocator_type> \
            && !std::is_default_constructible_v<allocator_type>) \
    static auto operator new( \
        usize /* n */, \
        const FirstArg& /* arg1 */, \
        const Args&... /*args*/) noexcept -> void* { \
        return nullptr; \
    } \
\
    static void operator delete(void* ptr, usize sz) noexcept { \
        if (ptr != nullptr) { \
            deallocate(ptr, sz); \
        } \
    } \
\
  private: \
    static auto allocate(usize n, allocator_type alloc) noexcept -> void* { \
        auto allocator_offset = AlignUp(n, alignof(allocator_type)); \
        if (auto res = allocator_traits<allocator_type>::allocate( \
                alloc, \
                allocator_offset + sizeof(allocator_type))) { \
            auto* ptr = static_cast<u8*>(*res); \
            /*  Copy allocator into coroutine frame for, accesing during delete. */ \
            std::construct_at( \
                std::bit_cast<allocator_type*>(ptr + allocator_offset), \
                std::move(alloc)); \
            return ptr; \
        } \
        return nullptr; \
    } \
\
    static void deallocate(void* ptr, usize sz) noexcept { \
        auto allocator_offset = AlignUp(sz, alignof(allocator_type)); \
        auto* alloc_ptr = \
            std::bit_cast<allocator_type*>(static_cast<u8*>(ptr) + allocator_offset); \
        auto alloc = std::move(*alloc_ptr); \
\
        std::destroy_at(alloc_ptr); \
        allocator_traits<allocator_type>::deallocate( \
            alloc, \
            static_cast<u8*>(ptr), \
            sz + sizeof(allocator_type)); \
    }
    }  // namespace detail
}  // namespace coro
}  // namespace ktl
