#pragma once

#include <bit>
#include <memory>

#include "detail/vector_ops.hpp"

namespace ktl {
template<typename T, std::integral Size>
    requires(!std::is_const_v<T>)
class fixed_vector;

template<typename T, auto Capacity>
    requires std::integral<std::decay_t<decltype(Capacity)>>
class static_vector:
    public detail::vector_ops<T, detail::size_t<Capacity>, static_vector<T, Capacity>> {
  public:
    using value_type = T;
    using size_type = detail::size_t<Capacity>;
    using difference_type = isize;
    using reference = T&;
    using const_reference = const T&;
    using pointer = T*;
    using const_pointer = const T*;

  private:
    using base = detail::vector_ops<T, size_type, static_vector<T, Capacity>>;

  public:
    static_assert(
        std::is_nothrow_move_constructible_v<T> && std::is_nothrow_move_assignable_v<T>
        && std::is_nothrow_destructible_v<T>);

    // Special member function definitions
    constexpr static_vector() = default;

    constexpr ~static_vector()
        requires std::is_trivially_destructible_v<T>
    = default;
    constexpr ~static_vector()
        requires(!std::is_trivially_destructible_v<T>)
    {
        if (m_len) {
            std::destroy_n(get_storage().begin, m_len);
        }
    }

    constexpr static_vector(const static_vector&)
        requires std::is_trivially_copy_constructible_v<T>
    = default;
    constexpr static_vector(const static_vector& o) noexcept
        requires(!std::is_trivially_copy_constructible_v<T>)
        : m_len {o.m_len} {
        detail::uninitialized_copy_n(o.begin(), m_len, get_storage().begin);
    }

    constexpr static_vector(static_vector&&) noexcept
        requires std::is_trivially_move_constructible_v<T>
    = default;
    constexpr static_vector(static_vector&& o) noexcept
        requires(!std::is_trivially_move_constructible_v<T>)
    {
        swap(o);
    }

    constexpr auto operator=(const static_vector&) -> static_vector&
        requires std::is_trivially_copy_assignable_v<T>
    = default;
    constexpr auto operator=(static_vector&&) noexcept -> static_vector&
        requires std::is_trivially_move_assignable_v<T>
    = default;
    constexpr auto operator=(const static_vector& o) noexcept -> static_vector&
        requires(!std::is_trivially_copy_assignable_v<T>)
    {
        static_vector {o}.swap(*this);
        return *this;
    }
    constexpr auto operator=(static_vector&& o) noexcept -> static_vector&
        requires(!std::is_trivially_move_assignable_v<T>)
    {
        swap(o);
        return *this;
    }

    friend constexpr void swap(static_vector& a, static_vector& b) noexcept {
        a.swap(b);
    }

    constexpr auto max_size() const noexcept -> size_type {
        return Capacity;
    }

    constexpr void swap(static_vector& o) noexcept {
        auto len = m_len;
        auto o_len = o.m_len;
        auto begin = get_storage().begin;
        auto o_begin = o.get_storage().begin;

        if (len > o_len) {
            std::swap_ranges(begin, begin + o_len, o_begin);
            detail::uninitialized_move_n(begin + o_len, len - o_len, o_begin);
        } else {
            std::swap_ranges(begin, begin + len, o_begin);
            detail::uninitialized_move_n(o_begin + len, o_len - len, begin);
        }

        using std::swap;
        swap(m_len, o.m_len);
    }

    // NOLINTNEXTLINE(*-explicit-conversions)
    constexpr operator fixed_vector<value_type, size_type>() const noexcept {
        return fixed_vector {this->data(), this->capacity(), this->size()};
    }

    template<typename U, typename... OT>
    friend constexpr auto make_static_vector(U&& first_val, OT&&... other_vals) noexcept
        -> static_vector<std::common_type_t<U, OT...>, sizeof...(OT) + 1>;

    template<auto VCapacity, typename U, typename... OT>
    friend constexpr auto make_static_vector(U&& first_val, OT&&... other_vals) noexcept
        -> static_vector<std::common_type_t<U, OT...>, VCapacity>;

  private:
    static constexpr auto is_trivial = std::is_trivial_v<T>;

    // Allow access to internal members. Classic CRTP.
    friend class detail::vector_ops<T, size_type, static_vector<T, Capacity>>;

    [[nodiscard]] constexpr auto get_storage() const noexcept -> detail::vector_storage<const T> {
        auto data = [&] {
            if constexpr (is_trivial) {
                return m_storage.elems.data();
            } else {
                return std::bit_cast<const T*>(m_storage.elems.data());
            }
        }();
        return {.begin = data, .end = data + m_len, .end_cap = data + Capacity};
    }
    constexpr auto get_storage() noexcept -> detail::vector_storage<T> {
        auto data = [&] {
            if constexpr (is_trivial) {
                return m_storage.elems.data();
            } else {
                return std::bit_cast<T*>(m_storage.elems.data());
            }
        }();
        return {.begin = data, .end = data + m_len, .end_cap = data + Capacity};
    }

    constexpr auto grow(usize req_len) noexcept -> expected<void, Error> {
        if (req_len > Capacity) [[unlikely]] {
            Throw(Error::BufferFull);
        }
        return {};
    }
    constexpr auto grow_uninit(usize req_len) noexcept -> expected<void, Error> {
        return grow(req_len);
    }

    constexpr auto set_len(size_type new_len) noexcept {
        assert(new_len <= Capacity && "length cannot exceed capacity");
        m_len = new_len;
    }

    struct trivial_storage_t {
        [[no_unique_address]] std::array<T, Capacity> elems = [] {
            if (std::is_constant_evaluated()) {
                std::array<T, Capacity> elems = {};
                return elems;
            } else {  // NOLINT NOLINTNEXTLINE
                std::array<T, Capacity> elems;
                return elems;
            }
        }();
    };

    struct generic_storage_t {
        [[no_unique_address]] alignas(T) std::array<std::byte, sizeof(T) * Capacity> elems;
    };

    [[no_unique_address]] std::conditional_t<is_trivial, trivial_storage_t, generic_storage_t>
        m_storage;
    size_type m_len = 0;
};

template<typename T, typename... OT>
constexpr auto make_static_vector(T&& first_val, OT&&... other_vals) noexcept
    -> static_vector<std::common_type_t<T, OT...>, sizeof...(OT) + 1> {
    static_vector<std::common_type_t<T, OT...>, sizeof...(OT) + 1> vec;
    auto data = vec.get_storage().begin;

    std::construct_at(data, std::forward<T>(first_val));
    (std::construct_at(++data, std::forward<OT>(other_vals)), ...);
    vec.set_len(sizeof...(OT) + 1);

    return vec;
}

template<auto VCapacity, typename T, typename... OT>
constexpr auto make_static_vector(T&& first_val, OT&&... other_vals) noexcept
    -> static_vector<std::common_type_t<T, OT...>, VCapacity> {
    static_assert(VCapacity >= sizeof...(OT) + 1);

    using ValueT = std::common_type_t<T, OT...>;
    static_vector<ValueT, VCapacity> vec;
    auto data = vec.get_storage().begin;

    std::construct_at(data, std::forward<T>(first_val));
    (std::construct_at(++data, std::forward<OT>(other_vals)), ...);
    vec.set_len(sizeof...(OT) + 1);

    if (std::is_constant_evaluated()) {
        data = vec.get_storage().begin;
        for (auto i = vec.size(); i < vec.capacity(); i++) {
            std::construct_at(data + i, ValueT {});
        }
    }

    return vec;
}
}  // namespace ktl