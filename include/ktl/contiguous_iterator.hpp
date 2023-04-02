#pragma once

#include <concepts>
#include <iterator>

#include "assert.hpp"
#include "int.hpp"

namespace ktl {
namespace detail {
    // NOLINTBEGIN(misc-non-private-member-variables-in-classes)
    template<typename T>
    struct iterator_storage {
        iterator_storage() = default;

        // NOLINTNEXTLINE(*-easily-swappable-parameters)
        constexpr iterator_storage(T* current, T* start, T* end) :
            current {current},
            start {start},
            end {end} {}

        [[nodiscard]] constexpr auto is_dereferencable() const -> bool {
            return current >= start && current < end;
        }

        T* current;
        T* start;
        T* end;
    };

    /**
     * @brief Iterator with mandatory bounds checking.
     * @details satisfies ConstexprIterator and std::contigous_iterator
     * @tparam T value_type of the iterable.
     */
    template<typename T>
    class checked_iterator {
      public:
        using value_type = T;
        using element_type = value_type;
        using difference_type = isize;
        using reference = T&;
        using pointer = T*;
        using iterator_category = std::contiguous_iterator_tag;

        checked_iterator() = default;
        constexpr ~checked_iterator() = default;
        constexpr checked_iterator(const checked_iterator&) = default;
        constexpr checked_iterator(checked_iterator&&) noexcept = default;
        constexpr auto operator=(const checked_iterator&) -> checked_iterator& = default;
        constexpr auto operator=(checked_iterator&&) noexcept -> checked_iterator& = default;

        constexpr checked_iterator(T* current, T* start, T* end) : storage {current, start, end} {}

        // NOLINTNEXTLINE(*-explicit-conversions)
        constexpr checked_iterator(const checked_iterator<std::remove_const_t<T>>& o)
            requires(std::is_const_v<T>)
            : storage {o.storage.current, o.storage.start, o.storage.end} {}

        [[nodiscard]] constexpr auto is_dereferencable() const -> bool {
            return storage.is_dereferencable();
        }

        // Value Access operators
        constexpr auto operator*() const -> T& {
            if (!storage.is_dereferencable()) [[unlikely]] {
                abort_("iterator out of bounds");
            };
            return *storage.current;
        }

        constexpr auto operator->() const -> T* {
            return &**this;
        }

        // Increment and Decrement operators
        // NOLINTBEGIN(cppcoreguidelines-pro-bounds-pointer-arithmetic)
        constexpr auto operator++() -> checked_iterator& {
            ++storage.current;
            return *this;
        }

        constexpr auto operator++(int) -> checked_iterator {
            auto copy = *this;
            ++*this;
            return copy;
        }

        constexpr auto operator--() -> checked_iterator& {
            --storage.current;
            return *this;
        }

        constexpr auto operator--(int) -> checked_iterator {
            auto copy = *this;
            --*this;
            return copy;
        }

        // Random access
        constexpr auto operator+=(difference_type pos) -> checked_iterator& {
            storage.current += pos;
            return *this;
        }

        constexpr auto operator-=(difference_type pos) -> checked_iterator& {
            storage.current -= pos;
            return *this;
        }
        // NOLINTEND(cppcoreguidelines-pro-bounds-pointer-arithmetic)

        // Comparison operator
        constexpr auto operator<=>(const checked_iterator& rhs) const {
            check_(storage.start == rhs.storage.start, "iterators must belong to same container");
            return storage.current <=> rhs.storage.current;
        }

        constexpr auto operator==(const checked_iterator& rhs) const -> bool {
            check_(storage.start == rhs.storage.start, "iterators must belong to same container");
            return storage.current == rhs.storage.current;
        }

        constexpr auto operator+(difference_type pos) const -> checked_iterator {
            checked_iterator iter = *this;
            iter += pos;
            return iter;
        }

        constexpr auto operator-(difference_type pos) const -> checked_iterator {
            checked_iterator iter = *this;
            iter -= pos;
            return iter;
        }

        friend constexpr auto operator+(difference_type pos, checked_iterator self)
            -> checked_iterator {
            return self + pos;
        }

        // Index and difference operators
        constexpr auto operator-(const checked_iterator& rhs) const -> difference_type {
            check_(storage.start == rhs.storage.start, "iterators must belong to same container");
            return storage.current - rhs.storage.current;
        }

        constexpr auto operator[](difference_type idx) const -> reference {
            auto copy = *this + idx;
            return *copy;
        }

      private:
        friend class checked_iterator<const T>;
        iterator_storage<T> storage;
    };
    // NOLINTEND(misc-non-private-member-variables-in-classes)
}  // namespace detail

template<typename T, bool EnableChecks = KTL_CHECKS_ENABLED>
using contiguous_iterator = std::conditional_t<EnableChecks, detail::checked_iterator<T>, T*>;

template<
    bool EnableChecks = KTL_CHECKS_ENABLED,
    typename T,
    typename = std::enable_if_t<EnableChecks>>
constexpr auto make_contiguous_iterator(T* current, T* start, T* end)
    -> contiguous_iterator<T, true> {
    return {current, start, end};
}

template<
    bool EnableChecks = KTL_CHECKS_ENABLED,
    typename T,
    typename = std::enable_if_t<!EnableChecks>>
constexpr auto make_contiguous_iterator(T* current) -> contiguous_iterator<T, false> {
    return current;
}

template<
    bool EnableChecks = KTL_CHECKS_ENABLED,
    typename T,
    typename = std::enable_if_t<!EnableChecks>>
constexpr auto make_contiguous_iterator(T* current, T* /* start */, T* /* end */)
    -> contiguous_iterator<T, false> {
    return current;
}
}  // namespace ktl