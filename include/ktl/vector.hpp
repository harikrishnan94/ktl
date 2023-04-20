#pragma once

#include "detail/vector_ops.hpp"

namespace ktl {
template<typename T, std::integral Size>
    requires(!std::is_const_v<T>)
class fixed_vector;

template<typename T, allocator_for<T> Allocator, typename GP = default_growth_policy>
    requires growth_policy<GP> || growth_policy_for<GP, Allocator>
class vector: public detail::vec::vector_ops<T, usize, vector<T, Allocator, GP>> {
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
    using base = detail::vec::vector_ops<T, size_type, vector<T, Allocator, GP>>;
    using alloc_traits = allocator_traits<Allocator>;

  public:
    static_assert(
        std::is_nothrow_move_constructible_v<T> && std::is_nothrow_move_assignable_v<T>
        && std::is_nothrow_destructible_v<T>);

    // ------------------------ Special member functions --------------------------
    vector() = default;

    constexpr explicit vector(const Allocator& a) : m_alloc {a} {}

    constexpr ~vector() {
        this->clear();
        alloc_traits::deallocate(m_alloc, m_data, m_capacity);
    }

    vector(const vector&) = delete;
    auto operator=(const vector&) -> vector& = delete;

    constexpr vector(vector&& o) noexcept :
        m_data {std::exchange(o.m_data, nullptr)},
        m_len {std::exchange(o.m_len, 0)},
        m_capacity {std::exchange(o.m_capacity, 0)},
        m_alloc {std::move(o.m_alloc)} {}

    // Based on Howard Hinnat's answer: https://stackoverflow.com/a/27472502
    constexpr auto operator=(vector&& o) noexcept -> vector& {
        using std::swap;

        if constexpr (alloc_traits::propagate_on_container_move_assignment::value) {
            swap_storage(o);
            swap(m_alloc, o.m_alloc);
        } else {
            if (alloc_traits::equals(m_alloc, o.m_alloc)) {
                swap_storage(o);
            } else {
                static_assert(
                    std::is_move_constructible_v<value_type>
                    && std::is_move_assignable_v<value_type>);
                check_(
                    this->assign(
                        std::make_move_iterator(o.begin()),
                        std::make_move_iterator(o.end())),
                    "move assignment must not fail");
            }
        }

        return *this;
    }

    // Follows move assignment.
    constexpr void swap(vector& o) noexcept {
        using std::swap;

        if constexpr (alloc_traits::propagate_on_container_swap::value) {
            swap_storage(o);
            swap(m_alloc, o.m_alloc);
        } else {
            if (alloc_traits::equals(m_alloc, o.m_alloc)) {
                swap_storage(o);
            } else {
                static_assert(
                    std::is_move_constructible_v<value_type>
                    && std::is_move_assignable_v<value_type>);

                vector tmp {m_alloc};

                check_(
                    tmp.assign(
                        std::make_move_iterator(o.begin()),
                        std::make_move_iterator(o.end())),
                    "move assignment must not fail");
                check_(
                    o.assign(
                        std::make_move_iterator(this->begin()),
                        std::make_move_iterator(this->end())),
                    "move assignment must not fail");

                swap_storage(tmp);
            }
        }
    }

    constexpr auto get_allocator() const noexcept -> Allocator {
        return m_alloc;
    }
    constexpr auto get_allocator_for_clone() const noexcept -> Allocator {
        return alloc_traits::select_on_container_copy_construction(m_alloc);
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
        if (new_cap <= m_capacity) {
            return {};
        }
        return grow(new_cap);
    }

    constexpr auto shrink_to_fit() noexcept -> expected<void, Error> {
        if (m_len != m_capacity) {
            if (m_len > 0) [[likely]] {
                Try(new_data, alloc_traits::allocate(m_alloc, m_len));

                ktl::uninitialized_move_n(m_data, m_len, static_cast<value_type*>(new_data));
                this->end_lifetime();
                alloc_traits::deallocate(m_alloc, m_data, m_capacity);
                m_data = new_data;
                m_capacity = m_len;
                this->start_lifetime();
            } else {
                alloc_traits::deallocate(m_alloc, m_data, m_capacity);
                m_data = nullptr;
                m_capacity = 0;
            }
        }

        return {};
    }

  private:
    // Allow access to internal members. Classic CRTP.
    friend class detail::vec::vector_ops<T, size_type, vector<T, Allocator, GP>>;

    constexpr auto swap_storage(vector& o) noexcept {
        using std::swap;
        swap(m_data, o.m_data);
        swap(m_capacity, o.m_capacity);
        swap(m_len, o.m_len);
    }

    [[nodiscard]] constexpr auto get_storage() const noexcept
        -> detail::vec::vector_storage<const T> {
        assert(m_data == nullptr ? m_len == 0 && m_capacity == 0 : true);
        return {.begin = m_data, .end = m_data + m_len, .end_cap = m_data + m_capacity};
    }
    constexpr auto get_storage() noexcept -> detail::vec::vector_storage<T> {
        assert(m_data == nullptr ? m_len == 0 && m_capacity == 0 : true);
        return {.begin = m_data, .end = m_data + m_len, .end_cap = m_data + m_capacity};
    }

    constexpr auto grow(usize req_cap) noexcept -> expected<void, Error> {
        return grow_impl(req_cap, [](auto* new_data, auto* data, auto len) {
            ktl::uninitialized_move_n(data, len, new_data);
        });
    }
    constexpr auto grow_uninit(usize req_cap) noexcept -> expected<void, Error> {
        return grow_impl(req_cap, [](auto, auto* data, auto& len) {
            std::destroy_n(data, len);
            len = 0;
        });
    }

    constexpr auto grow_impl(usize req_len, auto&& transfer) noexcept -> expected<void, Error> {
        assert(m_data == nullptr ? m_len == 0 && m_capacity == 0 : true);
        if (req_len > m_capacity) [[unlikely]] {
            auto new_cap = ktl::grow<GP>(m_alloc, m_capacity, req_len);
            Try(new_data, alloc_traits::allocate(m_alloc, new_cap));

            transfer(static_cast<T*>(new_data), m_data, m_len);
            this->end_lifetime();
            alloc_traits::deallocate(m_alloc, m_data, m_capacity);
            m_data = new_data;
            m_capacity = new_cap;
            this->start_lifetime();
        }
        this->adjust_lifetime(req_len);
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

template<std::copy_constructible T, allocator_for<T> Alloc>
constexpr auto make_vector(usize count, const T& val, const Alloc& a = Alloc {}) noexcept
    -> expected<vector<T, Alloc>, Error> {
    vector<T, Alloc> vec {a};

    TryV(vec.assign(count, val));

    return vec;
}

template<typename T, allocator_for<T> Alloc>
    requires std::copy_constructible<T> && std::is_default_constructible_v<T>
constexpr auto make_vector(usize count, const Alloc& a = Alloc {}) noexcept
    -> expected<vector<T, Alloc>, Error> {
    return make_vector<T, Alloc>(count, {}, a);
}

template<std::copy_constructible T, allocator_for<T> Alloc>
constexpr auto make_vector(std::initializer_list<T> ilist, const Alloc& a = Alloc {}) noexcept
    -> expected<vector<T, Alloc>, Error> {
    vector<T, Alloc> vec {a};

    TryV(vec.assign(ilist));

    return vec;
}

template<std::input_iterator InputIt, allocator_for<std::iter_value_t<InputIt>> Alloc>
constexpr auto make_vector(InputIt first, InputIt last, const Alloc& a = Alloc {}) noexcept
    -> expected<vector<std::iter_value_t<InputIt>, Alloc>, Error> {
    vector<std::iter_value_t<InputIt>, Alloc> vec {a};

    TryV(vec.assign(first, last));

    return vec;
}
}  // namespace ktl