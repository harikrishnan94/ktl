#pragma once

#include <memory>

#include <ktl/int.hpp>

namespace ktl::detail {
enum class VectorError { OOM };

enum class buffer_type { stack, non_owning, dynamic };

template<typename T, typename Allocator, buffer_type BufferType>
class contiguous_buffer {
  public:
    static constexpr auto growth_factor = 2;
    static constexpr auto is_dynamic = BufferType == buffer_type::dynamic;

    using size_type = std::conditional_t<is_dynamic, usize, typename Allocator::size_type>;

    [[nodiscard]] constexpr auto capacity() const noexcept -> size_type {
        if constexpr (is_dynamic) {
            return m_capacity;
        } else {
            return m_allocator.capacity();
        }
    }

    constexpr auto data() const noexcept -> const T* {
        if constexpr (is_dynamic) {
            return m_base;
        } else {
            return m_allocator.base();
        }
    }

    constexpr auto data() noexcept -> T* {
        if constexpr (is_dynamic) {
            return m_base;
        } else {
            return m_allocator.base();
        }
    }

    constexpr auto grow(usize reqlen) noexcept -> bool {
        if constexpr (is_dynamic) {
            if (reqlen > capacity()) {
                return false;
            }
        } else {
            if (reqlen <= capacity()) {
                return true;
            }
            auto new_cap = std::max(reqlen, m_capacity * growth_factor);
            m_allocator->resize();
        }
    }

  private:
    struct empty_t {};
    using pointer_t = std::conditional_t<is_dynamic, T*, empty_t>;
    using capacity_t = std::conditional_t<is_dynamic, typename Allocator::size_type, empty_t>;

    [[no_unique_address]] Allocator m_allocator;
    [[no_unique_address]] pointer_t m_base;
    [[no_unique_address]] capacity_t m_capacity;
};
}  // namespace ktl::detail