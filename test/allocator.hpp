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
        check_(false, "must be used only in constexpr expressions");
    }

    constexpr void deallocate(T* ptr, usize n) noexcept {
        if (std::is_constant_evaluated()) {
            std::allocator<T> {}.deallocate(ptr, n);
        } else {
            check_(false, "must be used only in constexpr expressions");
        }
    }
};

template<typename T, auto Capacity = 256>
class BumpAllocator {
  public:
    using value_type = T;
    using is_noop_dealloc = std::true_type;

    auto allocate(usize n) noexcept -> expected<not_null<T*>, Error> {
        auto idx = allocated;
        if (allocated + n > Capacity) {
            Throw(Error::BufferFull);
        }
        allocated += n;

        return std::bit_cast<T*>(&at(arr, idx * sizeof(T)));
    }

  private:
    alignas(T) static inline std::array<char, Capacity * sizeof(T)> arr = {};
    static inline usize allocated = 0;
};
}  // namespace ktl