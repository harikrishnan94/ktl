#pragma once

#include <bit>
#include <limits>
#include <memory>

#include "detail/vector_ops.hpp"

namespace ktl {
template<typename T, std::integral Size>
    requires(!std::is_const_v<T>)
class fixed_vector: public detail::vector_ops<T, Size, fixed_vector<T, Size>> {
  public:
    using value_type = T;
    using size_type = Size;
    using difference_type = isize;
    using reference = T&;
    using const_reference = const T&;
    using pointer = T*;
    using const_pointer = const T*;

  private:
    using base = detail::vector_ops<T, size_type, fixed_vector<T, size_type>>;

  public:
    static_assert(
        std::is_nothrow_move_constructible_v<T> && std::is_nothrow_move_assignable_v<T>
        && std::is_nothrow_destructible_v<T>);

    // Special member function definitions
    constexpr fixed_vector() = default;
    constexpr ~fixed_vector() = default;
    constexpr fixed_vector(const fixed_vector&) = default;
    constexpr fixed_vector(fixed_vector&&) noexcept = default;
    constexpr auto operator=(const fixed_vector&) -> fixed_vector& = default;
    constexpr auto operator=(fixed_vector&& o) noexcept -> fixed_vector& = default;

    // Initialize as fixed_vector vec {ptr, {.capacity = CAP, .length = LEN}};
    constexpr explicit fixed_vector(pointer base, size_type capacity, size_type len = 0) noexcept :
        m_data {base},
        m_capacity {capacity},
        m_len {len} {}

    template<auto Capacity>
        requires std::integral<std::decay_t<decltype(Capacity)>>
    constexpr explicit fixed_vector(std::array<T, Capacity>& arr, Size len = 0) noexcept :
        m_data {arr.data()},
        m_capacity {Capacity},
        m_len {static_cast<size_type>(len)} {}

    constexpr auto max_size() const noexcept -> size_type {
        return m_capacity;
    }

    constexpr auto deep_swap(fixed_vector& o) noexcept -> std::optional<Error> {
        if (m_len > o.m_capacity || o.m_len > m_capacity)
            return Error::BufferFull;

        if (m_len > o.m_len) {
            std::swap_ranges(m_data, m_data + o.m_len, o.m_data);
            detail::uninitialized_move_n(m_data + o.m_len, m_len - o.m_len, o.m_data);
        } else {
            std::swap_ranges(m_data, m_data + m_len, o.m_data);
            detail::uninitialized_move_n(o.m_data + m_len, o.m_len - m_len, m_data);
        }

        using std::swap;
        swap(m_len, o.m_len);
    }

    constexpr void destroy() noexcept {
        if constexpr (!std::is_trivially_destructible_v<T>) {
            if (m_len) {
                std::destroy_n(m_data, m_len);
            }
        }
    }

    template<std::input_iterator InputIter>
    constexpr auto clear_and_assign(InputIter first, InputIter last) noexcept
        -> expected<void, std::pair<InputIter, Error>> {
        this->clear();
        return this->assign_iter(first, last);
    }

  private:
    // Allow access to internal members. Classic CRTP.
    friend class detail::vector_ops<T, size_type, fixed_vector<T, size_type>>;

    [[nodiscard]] constexpr auto get_storage() const noexcept -> detail::vector_storage<const T> {
        return {.begin = m_data, .end = m_data + m_len, .end_cap = m_data + m_capacity};
    }
    constexpr auto get_storage() noexcept -> detail::vector_storage<T> {
        return {.begin = m_data, .end = m_data + m_len, .end_cap = m_data + m_capacity};
    }

    constexpr auto grow(usize req_len) noexcept -> expected<void, Error> {
        if (req_len > m_capacity) [[unlikely]] {
            Throw(Error::BufferFull);
        }
        return {};
    }

    constexpr auto set_len(size_type new_len) noexcept {
        assert(new_len <= m_capacity && "length cannot exceed capacity");
        m_len = new_len;
    }

    pointer m_data = nullptr;
    size_type m_capacity = 0;
    size_type m_len = 0;
};

template<typename T, auto Capacity>
fixed_vector(std::array<T, Capacity>&, usize) -> fixed_vector<T, detail::size_t<Capacity>>;

template<typename T, typename SizeT>
fixed_vector(T* ptr, SizeT) -> fixed_vector<T, SizeT>;

template<typename T, typename SizeT>
fixed_vector(T* ptr, SizeT, SizeT) -> fixed_vector<T, SizeT>;
}  // namespace ktl