#pragma once

#include <bit>
#include <memory>

#include "detail/vector_ops.hpp"

namespace ktl {
template<typename T, auto Capacity>
    requires std::integral<std::decay_t<decltype(Capacity)>>
class stack_vector:
    public detail::vector_ops<T, detail::size_t<Capacity>, stack_vector<T, Capacity>> {
  public:
    using value_type = T;
    using size_type = detail::size_t<Capacity>;
    using difference_type = isize;
    using reference = T&;
    using const_reference = const T&;
    using pointer = T*;
    using const_pointer = const T*;

  private:
    using base = detail::vector_ops<T, size_type, stack_vector<T, Capacity>>;

  public:
    static_assert(
        std::is_nothrow_move_constructible_v<T> && std::is_nothrow_move_assignable_v<T>
        && std::is_nothrow_destructible_v<T>);

    // Special member function definitions
    constexpr stack_vector() = default;

    constexpr ~stack_vector()
        requires std::is_trivially_destructible_v<T>
    = default;
    constexpr ~stack_vector()
        requires(!std::is_trivially_destructible_v<T>)
    {
        if (m_len) {
            std::destroy_n(get_storage().begin, m_len);
        }
    }

    constexpr stack_vector(const stack_vector&)
        requires std::is_trivially_copy_constructible_v<T>
    = default;
    constexpr stack_vector(const stack_vector& o) noexcept
        requires(!std::is_trivially_copy_constructible_v<T>)
        : m_len {o.m_len} {
        detail::uninitialized_copy_n(o.begin(), m_len, get_storage().begin);
    }

    constexpr stack_vector(stack_vector&&) noexcept
        requires std::is_trivially_move_constructible_v<T>
    = default;
    constexpr stack_vector(stack_vector&& o) noexcept
        requires(!std::is_trivially_move_constructible_v<T>)
    {
        swap(o);
    }

    constexpr auto operator=(const stack_vector&) -> stack_vector&
        requires std::is_trivially_copy_assignable_v<T>
    = default;
    constexpr auto operator=(stack_vector&&) noexcept -> stack_vector&
        requires std::is_trivially_move_assignable_v<T>
    = default;
    constexpr auto operator=(const stack_vector& o) noexcept -> stack_vector&
        requires(!std::is_trivially_copy_assignable_v<T>)
    {
        stack_vector {o}.swap(*this);
        return *this;
    }
    constexpr auto operator=(stack_vector&& o) noexcept -> stack_vector&
        requires(!std::is_trivially_move_assignable_v<T>)
    {
        swap(o);
        return *this;
    }

    friend constexpr void swap(stack_vector& a, stack_vector& b) noexcept {
        a.swap(b);
    }

    constexpr explicit stack_vector(size_type count, const T& value) noexcept : m_len {count} {
        detail::uninitialized_fill_n(get_storage().begin, count, value);
    }

    constexpr explicit stack_vector(size_type count) noexcept : m_len {count} {
        std::uninitialized_default_construct_n(get_storage().begin, count);
    }

    constexpr auto max_size() const noexcept -> size_type {
        return Capacity;
    }

    constexpr void swap(stack_vector& o) noexcept {
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

    template<typename U, typename... OT>
    friend constexpr auto make_stack_vector(U&& first_val, OT&&... other_vals) noexcept
        -> stack_vector<std::common_type_t<U, OT...>, sizeof...(OT) + 1>;

    template<auto VCapacity, typename U, typename... OT>
    friend constexpr auto make_stack_vector(U&& first_val, OT&&... other_vals) noexcept
        -> stack_vector<std::common_type_t<U, OT...>, VCapacity>;

    using base::assign;

    template<std::random_access_iterator RandAccIt>
    constexpr auto assign(RandAccIt first, RandAccIt last) noexcept -> expected<void, Error> {
        return base::assign(first, last);
    }

    template<std::input_iterator InputIter>
    constexpr auto assign(InputIter first, InputIter last) noexcept
        -> expected<void, std::pair<InputIter, Error>> {
        if (this->empty()) {
            TryV(this->assign_iter(first, last));
        } else {
            stack_vector tmp;

            TryV(tmp.assign_iter(first, last));
            *this = std::move(tmp);
        }

        return {};
    }

    using base::insert;

    template<std::random_access_iterator RandAccIt>
    constexpr auto insert(typename base::const_iterator pos, RandAccIt first, RandAccIt last)
        -> expected<typename base::iterator, Error> {
        return base::insert(pos, first, last);
    }

    template<std::input_iterator InputIt>
    constexpr auto insert(typename base::const_iterator pos, InputIt first, InputIt last)
        -> expected<typename base::iterator, Error> {
        if (pos == base::end()) {
            return base::insert_at_end(first, last);
        }

        stack_vector tmp;
        auto tmp_res = tmp.assign(first, last);
        auto res = base::insert(pos, tmp.begin(), tmp.end());

        // All rows inseted into `tmp` vector? If so, return the `res`.
        if (tmp_res || !res) {
            return res;
        }
        // If, all rows were not inserted into the `tmp` vector, error must be returned.
        Throw(std::move(tmp_res).error().second);
    }

  private:
    static constexpr auto is_trivial = std::is_trivial_v<T>;

    // Allow access to internal members. Classic CRTP.
    friend class detail::vector_ops<T, size_type, stack_vector<T, Capacity>>;

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
constexpr auto make_stack_vector(T&& first_val, OT&&... other_vals) noexcept
    -> stack_vector<std::common_type_t<T, OT...>, sizeof...(OT) + 1> {
    stack_vector<std::common_type_t<T, OT...>, sizeof...(OT) + 1> vec;
    auto data = vec.get_storage().begin;

    std::construct_at(data, std::forward<T>(first_val));
    (std::construct_at(++data, std::forward<OT>(other_vals)), ...);
    vec.set_len(sizeof...(OT) + 1);

    return vec;
}

template<auto VCapacity, typename T, typename... OT>
constexpr auto make_stack_vector(T&& first_val, OT&&... other_vals) noexcept
    -> stack_vector<std::common_type_t<T, OT...>, VCapacity> {
    static_assert(VCapacity >= sizeof...(OT) + 1);

    using ValueT = std::common_type_t<T, OT...>;
    stack_vector<ValueT, VCapacity> vec;
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