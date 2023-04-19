#pragma once

#include <ktl/assert.hpp>
#include <ktl/int.hpp>
#include <ktl/memory.hpp>

namespace ktl {
template<typename T>
struct Allocator {
  public:
    using value_type = T;

    constexpr auto allocate(usize n) noexcept -> expected<not_null<T*>, Error> {
        if (std::is_constant_evaluated()) {
            return std::allocator<T> {}.allocate(n);
        }
        return std::allocator<T> {}.allocate(n);
    }

    constexpr void deallocate(T* ptr, usize n) noexcept {
        if (std::is_constant_evaluated()) {
            std::allocator<T> {}.deallocate(ptr, n);
        } else {
            std::allocator<T> {}.deallocate(ptr, n);
        }
    }
};
}  // namespace ktl