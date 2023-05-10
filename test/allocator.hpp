#pragma once

#include <ktl/assert.hpp>
#include <ktl/int.hpp>
#include <ktl/memory.hpp>

namespace ktl {
template<typename T>
struct Allocator {
  public:
    using value_type = T;

    Allocator() = default;

    template<typename U>
    Allocator(Allocator<U> /* alloc */) {}

    constexpr auto allocate(usize n) noexcept -> expected<not_null<T*>, Error> {
        return std::allocator<T> {}.allocate(n);
    }

    constexpr void deallocate(T* ptr, usize n) noexcept {
        std::allocator<T> {}.deallocate(ptr, n);
    }
};
}  // namespace ktl