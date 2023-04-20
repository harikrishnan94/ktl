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
    public detail::vec::vector_ops<T, detail::vec::size_t<Capacity>, static_vector<T, Capacity>> {
  public:
    using value_type = T;
    using size_type = detail::vec::size_t<Capacity>;
    using difference_type = isize;
    using reference = T&;
    using const_reference = const T&;
    using pointer = T*;
    using const_pointer = const T*;

  private:
    using base = detail::vec::vector_ops<T, size_type, static_vector<T, Capacity>>;

  public:
    static_assert(
        std::is_nothrow_move_constructible_v<T> && std::is_nothrow_move_assignable_v<T>
        && std::is_nothrow_destructible_v<T>);

    // Special member function definitions
    static_vector()
        requires(!ASAN_ENABLED)
    = default;

    constexpr static_vector()
        requires(ASAN_ENABLED)
    {
        this->start_lifetime();
    }

    constexpr ~static_vector()
        requires std::is_trivially_destructible_v<T> && (!ASAN_ENABLED)
    = default;
    constexpr ~static_vector()
        requires(!std::is_trivially_destructible_v<T> || ASAN_ENABLED)
    {
        this->clear();
    }

    constexpr static_vector(const static_vector& o) noexcept : m_len {o.m_len} {
        ktl::uninitialized_copy_n(o.begin(), m_len, get_storage().begin);
        this->start_lifetime();
    }

    constexpr static_vector(static_vector&& o) noexcept {
        if constexpr (std::is_trivially_copyable_v<T>) {
            m_len = o.m_len;
            ktl::uninitialized_copy_n(o.begin(), m_len, get_storage().begin);
            this->start_lifetime();
        } else {
            swap(o);
        }
    }

    constexpr auto operator=(const static_vector& o) noexcept -> static_vector& {
        if constexpr (std::is_trivially_copyable_v<T>) {
            if (this == &o) {
                return *this;
            }

            this->adjust_lifetime(o.m_len);
            m_len = o.m_len;
            ktl::uninitialized_copy_n(o.begin(), m_len, get_storage().begin);
        } else {
            static_vector {o}.swap(*this);
        }
        return *this;
    }

    constexpr auto operator=(static_vector&& o) noexcept -> static_vector& {
        if constexpr (std::is_trivially_copyable_v<T>) {
            if (this == &o) {
                return *this;
            }
            this->adjust_lifetime(o.m_len);
            m_len = o.m_len;
            ktl::uninitialized_copy_n(o.begin(), m_len, get_storage().begin);
        } else {
            swap(o);
        }
        return *this;
    }

    friend constexpr void swap(static_vector& a, static_vector& b) noexcept {
        a.swap(b);
    }

    constexpr void swap(static_vector& o) noexcept {
        if (this == &o)
            return;

        this->adjust_lifetime(std::max(m_len, o.m_len));
        o.adjust_lifetime(std::max(m_len, o.m_len));

        detail::swap_range_with_len(this->get_data(), m_len, o.get_data(), o.m_len);

        this->adjust_lifetime(m_len);
        o.adjust_lifetime(o.m_len);
    }

    constexpr auto max_size() const noexcept -> size_type {
        return Capacity;
    }

    // NOLINTNEXTLINE(*-explicit-conversions)
    constexpr auto as_fixed_vector() noexcept -> fixed_vector<value_type, size_type> {
        return {this->data(), m_len, this->capacity()};
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
    friend class detail::vec::vector_ops<T, size_type, static_vector<T, Capacity>>;

    template<typename U, typename... OU>
        requires(sizeof...(OU) + 1 <= Capacity)
    constexpr void load(U&& first_val, OU&&... other_vals) noexcept {
        this->end_lifetime();

        auto data = get_data();
        std::construct_at(data, std::forward<U>(first_val));
        (std::construct_at(++data, std::forward<OU>(other_vals)), ...);
        set_len(sizeof...(OU) + 1);

        this->start_lifetime();
    }

    [[nodiscard]] constexpr auto get_data() const noexcept -> const T* {
        if constexpr (is_trivial) {
            return m_storage.elems.data();
        } else {
            return std::bit_cast<const T*>(m_storage.elems.data());
        }
    }
    [[nodiscard]] constexpr auto get_data() noexcept -> T* {
        if constexpr (is_trivial) {
            return m_storage.elems.data();
        } else {
            return std::bit_cast<T*>(m_storage.elems.data());
        }
    }

    [[nodiscard]] constexpr auto get_storage() const noexcept
        -> detail::vec::vector_storage<const T> {
        auto data = get_data();
        return {.begin = data, .end = data + m_len, .end_cap = data + Capacity};
    }
    constexpr auto get_storage() noexcept -> detail::vec::vector_storage<T> {
        auto data = get_data();
        return {.begin = data, .end = data + m_len, .end_cap = data + Capacity};
    }

    constexpr auto grow(usize req_len) noexcept -> expected<void, Error> {
        if (req_len > Capacity) [[unlikely]] {
            Throw(Error::BufferFull);
        }
        this->adjust_lifetime(req_len);
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

    size_type m_len = 0;
    alignas(ASAN_ALIGN<T>) [[no_unique_address]] std::
        conditional_t<is_trivial, trivial_storage_t, generic_storage_t> m_storage;
};

template<typename T, typename... OT>
constexpr auto make_static_vector(T&& first_val, OT&&... other_vals) noexcept
    -> static_vector<std::common_type_t<T, OT...>, sizeof...(OT) + 1> {
    static_vector<std::common_type_t<T, OT...>, sizeof...(OT) + 1> vec;

    vec.load(std::forward<T>(first_val), std::forward<OT>(other_vals)...);

    return vec;
}

template<auto VCapacity, typename T, typename... OT>
constexpr auto make_static_vector(T&& first_val, OT&&... other_vals) noexcept
    -> static_vector<std::common_type_t<T, OT...>, VCapacity> {
    static_assert(VCapacity >= sizeof...(OT) + 1);

    using ValueT = std::common_type_t<T, OT...>;
    static_vector<ValueT, VCapacity> vec;

    vec.load(std::forward<T>(first_val), std::forward<OT>(other_vals)...);

    return vec;
}
}  // namespace ktl