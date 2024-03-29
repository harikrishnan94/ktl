#pragma once

#include <bit>
#include <limits>
#include <memory>

#include "detail/vector_ops.hpp"

namespace ktl {
template<typename T, auto Capacity>
    requires std::integral<std::decay_t<decltype(Capacity)>>
class static_vector;

template<typename T, std::integral Size>
    requires(!std::is_const_v<T>)
class fixed_vector:
    public detail::vec::vector_ops<T, std::make_unsigned_t<Size>, fixed_vector<T, Size>> {
  public:
    using value_type = T;
    using size_type = std::make_unsigned_t<Size>;
    using difference_type = isize;
    using reference = T&;
    using const_reference = const T&;
    using pointer = T*;
    using const_pointer = const T*;

  private:
    using base = detail::vec::vector_ops<T, size_type, fixed_vector<T, size_type>>;

  public:
    static_assert(
        std::is_nothrow_move_constructible_v<T> && std::is_nothrow_move_assignable_v<T>
        && std::is_nothrow_destructible_v<T>);

    // Special member function definitions
    fixed_vector() = delete;
    fixed_vector(const fixed_vector&) = delete;
    auto operator=(const fixed_vector&) -> fixed_vector& = delete;

    ~fixed_vector() = default;
    fixed_vector(fixed_vector&&) noexcept = default;
    auto operator=(fixed_vector&&) noexcept -> fixed_vector& = default;

    // NOLINTNEXTLINE(*-easily-swappable-parameters)
    constexpr fixed_vector(not_null<pointer> base, size_type& len, size_type capacity) noexcept :
        m_data {base},
        m_len {&len},
        m_capacity {capacity} {
        this->start_lifetime();
    }

    template<auto Capacity>
        requires std::integral<std::decay_t<decltype(Capacity)>>
        && (std::numeric_limits<size_type>::max() >= Capacity)
    // NOLINTNEXTLINE(*-explicit-conversions)
    constexpr fixed_vector(std::array<T, Capacity>& arr, size_type& len) noexcept :
        fixed_vector {arr.data(), len, arr.size()} {}

    template<auto Capacity>
        requires std::integral<std::decay_t<decltype(Capacity)>>
        && (std::numeric_limits<size_type>::max() >= Capacity)
    // NOLINTNEXTLINE(*-explicit-conversions)
    constexpr fixed_vector(static_vector<T, Capacity>& vec) noexcept :
        fixed_vector {vec.as_fixed_vector()} {}

    constexpr auto max_size() const noexcept -> size_type {
        return m_capacity;
    }

  private:
    // Allow access to internal members. Classic CRTP.
    friend class detail::vec::vector_ops<T, size_type, fixed_vector<T, size_type>>;

    [[nodiscard]] constexpr auto get_storage() const noexcept
        -> detail::vec::vector_storage<const T> {
        return {.begin = m_data, .end = m_data + *m_len, .end_cap = m_data + m_capacity};
    }
    constexpr auto get_storage() noexcept -> detail::vec::vector_storage<T> {
        return {.begin = m_data, .end = m_data + *m_len, .end_cap = m_data + m_capacity};
    }

    constexpr auto grow(usize req_len) noexcept -> expected<void, Error> {
        if (req_len > m_capacity) [[unlikely]] {
            Throw(error::BufferFull);
        }
        this->adjust_lifetime(req_len);
        return {};
    }
    constexpr auto grow_uninit(usize req_len) noexcept -> expected<void, Error> {
        auto res = grow(req_len);
        if (res) {
            std::destroy_n(m_data, *m_len);
        }
        return res;
    }

    constexpr auto set_len(size_type new_len) noexcept {
        assert(new_len <= m_capacity && "length cannot exceed capacity");
        *m_len = new_len;
    }

    pointer m_data;
    size_type* __restrict__ m_len;
    size_type m_capacity;
};

template<typename T, typename SizeT>
fixed_vector(not_null<T*> ptr, SizeT& len, SizeT capacity) -> fixed_vector<T, SizeT>;

template<typename T, typename SizeT>
fixed_vector(T* ptr, SizeT& len, SizeT capacity) -> fixed_vector<T, SizeT>;

template<typename T, auto Capacity>
fixed_vector(std::array<T, Capacity>&, typename std::array<T, Capacity>::size_type&)
    -> fixed_vector<T, typename std::array<T, Capacity>::size_type>;

template<typename T, auto Capacity>
fixed_vector(static_vector<T, Capacity>&)
    -> fixed_vector<T, typename static_vector<T, Capacity>::size_type>;
}  // namespace ktl

namespace std {
template<typename T, std::integral Capacity>
inline constexpr bool ranges::enable_borrowed_range<ktl::fixed_vector<T, Capacity>> = true;

template<typename ElementType, std::integral Capacity>
inline constexpr bool ranges::enable_view<ktl::fixed_vector<ElementType, Capacity>> = true;
}  // namespace std
