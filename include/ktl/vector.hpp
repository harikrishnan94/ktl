#pragma once

#include <bit>

#include <ktl/memory.hpp>

#include "detail/vector_ops.hpp"

namespace ktl {
template<typename T, std::integral Size>
    requires(!std::is_const_v<T>)
class fixed_vector;

template<typename T, allocator_for<T> Allocator>
class vector: public detail::vector_ops<T, usize, vector<T, Allocator>> {
  public:
    using value_type = T;
    using size_type = usize;
    using difference_type = isize;
    using reference = T&;
    using const_reference = const T&;
    using pointer = T*;
    using const_pointer = const T*;
    using allocator_type = Allocator;

  private:
    using base = detail::vector_ops<T, size_type, vector<T, Allocator>>;
    using alloc_traits = allocator_traits<Allocator>;

  public:
    static_assert(
        std::is_nothrow_move_constructible_v<T> && std::is_nothrow_move_assignable_v<T>
        && std::is_nothrow_destructible_v<T>);

    // ------------------------ Special member functions --------------------------
    constexpr vector() = default;

    constexpr explicit vector(const Allocator& a) : m_alloc {a} {}
    constexpr explicit vector(Allocator&& a) : m_alloc {std::move(a)} {}

    constexpr ~vector() {
        this->clear();
        alloc_traits::deallocate(m_alloc, m_data, m_capacity);
    }

    constexpr vector(const vector&) = delete;
    constexpr auto operator=(const vector&) -> vector& = delete;

    constexpr vector(vector&& o) noexcept :
        m_data {std::exchange(o.m_data, nullptr)},
        m_capacity {std::exchange(o.m_capacity, 0)},
        m_len {std::exchange(o.m_len, 0)},
        m_alloc {std::move(o.m_alloc)} {}

    // Based on Howard Hinnat's answer: https://stackoverflow.com/a/27472502
    constexpr auto operator=(vector&& o) noexcept -> vector& {
        using std::swap;

        if constexpr (alloc_traits::propagate_on_container_move_assignment::value) {
            swap(m_data, o.m_data);
            swap(m_capacity, o.m_capacity);
            swap(m_len, o.m_len);
            swap(m_alloc, o.m_alloc);
        } else {
            if (alloc_traits::equals(m_alloc, o.m_alloc)) {
                swap(m_data, o.m_data);
                swap(m_capacity, o.m_capacity);
                swap(m_len, o.m_len);
            } else {
                static_assert(
                    std::is_move_constructible_v<value_type>
                    && std::is_move_assignable_v<value_type>);
                [[maybe_unused]] auto res = this->assign(
                    std::make_move_iterator(o.begin()),
                    std::make_move_iterator(o.end()));
                check_(res, "move assignment must not fail");
            }
        }

        return *this;
    }

    // Follows move assignment.
    constexpr void swap(vector& o) noexcept {
        using std::swap;

        if constexpr (alloc_traits::propagate_on_container_swap::value) {
            swap(m_data, o.m_data);
            swap(m_capacity, o.m_capacity);
            swap(m_len, o.m_len);
            swap(m_alloc, o.m_alloc);
        } else {
            if (alloc_traits::equals(m_alloc, o.m_alloc)) {
                swap(m_data, o.m_data);
                swap(m_capacity, o.m_capacity);
                swap(m_len, o.m_len);
            } else {
                static_assert(
                    std::is_move_constructible_v<value_type>
                    && std::is_move_assignable_v<value_type>);

                vector tmp {m_alloc};

                {
                    [[maybe_unused]] auto res = tmp.assign(
                        std::make_move_iterator(o.begin()),
                        std::make_move_iterator(o.end()));
                    check_(res, "move assignment must not fail");
                }
                {
                    [[maybe_unused]] auto res = o.assign(
                        std::make_move_iterator(this->begin()),
                        std::make_move_iterator(this->end()));
                    check_(res, "move assignment must not fail");
                }

                *this = std::move(tmp);
            }
        }
    }

    // Explicit Copy Construction
    constexpr auto clone() noexcept -> expected<vector, Error> {
        vector copy(alloc_traits::select_on_container_copy_construction(m_alloc));

        TryV(copy.assign(this->begin(), this->end()));

        return copy;
    }

    // NOLINTNEXTLINE(*-explicit-conversions)
    constexpr operator fixed_vector<value_type, size_type>() const noexcept {
        return fixed_vector {this->data(), this->capacity(), this->size()};
    }

    // ------------------------ Capacity & Size access & manipulation --------------------------

    [[nodiscard]] constexpr auto max_size() const noexcept -> size_type {
        return m_capacity;
    }

    constexpr auto reserve(size_type new_cap) noexcept -> expected<void, Error> {
        return grow(new_cap);
    }

    constexpr auto shrink_to_fit() noexcept -> expected<void, Error> {
        Try(new_data, alloc_traits::allocate(m_alloc, m_len));

        std::move(m_data, m_data + m_len, new_data);
        m_data = new_data;
        m_capacity = m_len;
    }

    template<typename Alloc, typename U, typename... OU>
    friend constexpr auto make_vector(const Alloc& a, U&& first_val, OU&&... other_vals) noexcept
        -> expected<vector<std::common_type_t<U, OU...>, Alloc>, Error>;

  private:
    // Allow access to internal members. Classic CRTP.
    friend class detail::vector_ops<T, size_type, vector<T, Allocator>>;

    [[nodiscard]] constexpr auto get_storage() const noexcept -> detail::vector_storage<const T> {
        assert(m_data == nullptr ? m_len == 0 && m_capacity == 0 : true);
        return {.begin = m_data, .end = m_data + m_len, .end_cap = m_data + m_capacity};
    }
    constexpr auto get_storage() noexcept -> detail::vector_storage<T> {
        assert(m_data == nullptr ? m_len == 0 && m_capacity == 0 : true);
        return {.begin = m_data, .end = m_data + m_len, .end_cap = m_data + m_capacity};
    }

    constexpr auto grow(usize req_cap) noexcept -> expected<void, Error> {
        assert(m_data == nullptr ? m_len == 0 && m_capacity == 0 : true);
        if (req_cap > m_capacity) [[unlikely]] {
            Try(new_data, alloc_traits::allocate(m_alloc, req_cap));

            std::move(m_data, m_data + m_len, static_cast<value_type*>(new_data));
            m_data = new_data;
            m_capacity = req_cap;
        }
        return {};
    }
    constexpr auto grow_uninit(usize req_len) noexcept -> expected<void, Error> {
        assert(m_data == nullptr ? m_len == 0 && m_capacity == 0 : true);
        if (req_len > m_capacity) [[unlikely]] {
            Try(new_data, alloc_traits::allocate(m_alloc, req_len));

            std::destroy_n(m_data, m_len);
            m_data = new_data;
            m_len = 0;
            m_capacity = req_len;
        }
        return {};
    }

    constexpr auto set_len(size_type new_len) noexcept {
        assert(new_len <= m_capacity && "length cannot exceed capacity");
        assert(m_data != nullptr && "length cannot exceed capacity");
        m_len = new_len;
    }

    pointer m_data = nullptr;
    size_type m_len = 0;
    size_type m_capacity = 0;
    [[no_unique_address]] Allocator m_alloc = {};
};

template<typename Alloc, typename T, typename... OT>
constexpr auto make_vector(const Alloc& a, T&& first_val, OT&&... other_vals) noexcept
    -> expected<vector<std::common_type_t<T, OT...>, Alloc>, Error> {
    using vec_t = vector<std::common_type_t<T, OT...>, Alloc>;

    vec_t vec {a};
    typename vec_t::size_type len = sizeof...(OT) + 1;
    typename vec_t::size_type i = 1;

    vec.reserve(len);
    vec.set_len(len);

    vec[0] = std::forward<T>(first_val);
    ((vec[i++] = std::forward<OT>(other_vals)), ...);

    return vec;
}

template<typename Alloc, typename T, typename... OT>
constexpr auto make_vector(T&& first_val, OT&&... other_vals) noexcept
    -> expected<vector<std::common_type_t<T, OT...>, Alloc>, Error> {
    return make_vector({}, std::forward<T>(first_val), std::forward<OT>(other_vals)...);
}

template<std::copy_constructible T, typename Alloc>
constexpr auto make_vector(usize count, const T& val, const Alloc& a = Alloc {}) noexcept
    -> expected<vector<T, Alloc>, Error> {
    using vec_t = vector<T, Alloc>;

    vec_t vec {a};

    vec.assign(count, val);

    return vec;
}

template<typename T, typename Alloc>
    requires std::copy_constructible<T>
    && std::is_default_constructible_v<T>
constexpr auto make_vector(usize count, const Alloc& a = Alloc {}) noexcept
    -> expected<vector<T, Alloc>, Error> {
    using vec_t = vector<T, Alloc>;

    vec_t vec {a};

    vec.assign(count, T {});

    return vec;
}

template<std::copy_constructible T, typename Alloc>
constexpr auto copy_vector(const vector<T, Alloc>& o, const Alloc& a = Alloc {}) noexcept
    -> expected<vector<T, Alloc>, Error> {
    using vec_t = vector<T, Alloc>;

    vec_t vec {a};

    vec.assign(a.begin(), a.end());

    return vec;
}

template<std::copy_constructible T, typename Alloc>
constexpr auto move_vector(vector<T, Alloc>&& o, const Alloc& a = Alloc {}) noexcept
    -> expected<vector<T, Alloc>, Error> {
    using vec_t = vector<T, Alloc>;

    vec_t vec {a};

    vec.resize_uninitialized(o.size());
    std::move(o.begin(), o.end(), vec.data());

    return vec;
}

template<std::copy_constructible T, typename Alloc>
constexpr auto move_vector(std::initializer_list<T> ilist, const Alloc& a = Alloc {}) noexcept
    -> expected<vector<T, Alloc>, Error> {
    using vec_t = vector<T, Alloc>;

    vec_t vec {a};

    vec.assign(ilist);

    return vec;
}
}  // namespace ktl