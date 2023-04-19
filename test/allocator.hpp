#pragma once

#include <ktl/assert.hpp>
#include <ktl/int.hpp>
#include <ktl/memory.hpp>

namespace ktl {
template<typename T>
struct ConstAllocator {
  public:
    using value_type = T;

    constexpr auto allocate(usize n) noexcept -> expected<not_null<T*>, Error> {
        if (std::is_constant_evaluated()) {
            return std::allocator<T> {}.allocate(n);
        }
#if ASAN_ENABLED == 1
        return std::allocator<T> {}.allocate(n);
#else
        check_(false, "must be used only in constexpr expressions");
#endif
    }

    constexpr void deallocate(T* ptr, usize n) noexcept {
        if (std::is_constant_evaluated()) {
            std::allocator<T> {}.deallocate(ptr, n);
        } else {
#if ASAN_ENABLED == 1
            std::allocator<T> {}.deallocate(ptr, n);
#else
            check_(false, "must be used only in constexpr expressions");
#endif
        }
    }
};

#if ASAN_ENABLED == 1
template<typename T, auto Capacity = 0>
using BumpAllocator = ConstAllocator<T>;
#else
template<typename T, auto Capacity = 256>
class BumpAllocator {
  public:
    using value_type = T;
    using is_noop_dealloc = std::true_type;

    auto allocate(usize n) noexcept -> expected<not_null<T*>, Error> {
        void* current = arr.data() + (arr.size() - remaining);
        auto* ptr = std::align(ASAN_ALIGN<T>, n, current, remaining);
        if (ptr == nullptr) {
            Throw(Error::OutOfMemory);
        }
        return static_cast<T*>(ptr);
    }

  private:
    static inline std::array<char, Capacity * sizeof(T)> arr = {};
    static inline usize remaining = arr.size();
};
#endif
}  // namespace ktl