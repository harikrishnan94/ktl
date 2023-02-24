#pragma once

#include <concepts>
#include <iterator>

#include "ktl/int.hpp"

namespace ktl {
namespace detail {
    // NOLINTBEGIN(misc-non-private-member-variables-in-classes)
    template<typename T, bool EnableChecks>
    struct iterator_store_if_checked {
        iterator_store_if_checked() = default;

        constexpr explicit iterator_store_if_checked(T* /* p */) {}
    };

    template<typename T>
    struct iterator_store_if_checked<T, true> {
        iterator_store_if_checked() = default;

        constexpr explicit iterator_store_if_checked(T* ptr) : ptr {ptr} {}

        T* ptr;
    };

    template<typename T, bool EnableChecks>
    struct iterator_storage {
        iterator_storage() = default;

        constexpr iterator_storage(T* start, T* end, T* current) :
            start {start},
            end {end},
            current {current} {}

        [[nodiscard]] constexpr auto is_dereferencable() const -> bool {
            if constexpr (EnableChecks) {
                return current >= start.ptr && current < end.ptr;
            } else {
                return true;
            }
        }

        [[no_unique_address]] iterator_store_if_checked<T, EnableChecks> start;
        [[no_unique_address]] iterator_store_if_checked<T, EnableChecks> end;
        T* current;
    };

    /**
     * @brief Contiguos Iterator with optional bounds checking.
     * @details satisfies ConstexprIterator and std::contiguous_iterator
     * @tparam T value_type of the iterable.
     */
    template<typename T, bool EnableChecks>
    class contiguous_iterator {
      public:
        using value_type = T;
        using element_type = value_type;
        using difference_type = isize;
        using reference = T&;
        using pointer = T*;
        using iterator_category = std::contiguous_iterator_tag;

        contiguous_iterator() = default;

        constexpr contiguous_iterator(T* start, T* end, T* current) :
            storage {start, end, current} {}

        template<bool EnableChecksV = EnableChecks, typename = std::enable_if_t<EnableChecksV>>
        [[nodiscard]] constexpr auto is_dereferencable() const -> bool {
            return storage.is_dereferencable();
        }

        // Value Access operators
        constexpr auto operator*() const -> T& {
            assert(storage.is_dereferencable());
            return *storage.current;
        }

        constexpr auto operator->() const -> T* {
            return &**this;
        }

        // Increment and Decrement operators
        // NOLINTBEGIN(cppcoreguidelines-pro-bounds-pointer-arithmetic)
        constexpr auto operator++() -> contiguous_iterator& {
            ++storage.current;
            return *this;
        }

        constexpr auto operator++(int) -> contiguous_iterator {
            auto copy = *this;
            ++*this;
            return copy;
        }

        constexpr auto operator--() -> contiguous_iterator& {
            --storage.current;
            return *this;
        }

        constexpr auto operator--(int) -> contiguous_iterator {
            auto copy = *this;
            --*this;
            return copy;
        }

        // Random access
        constexpr auto operator+=(difference_type pos) -> contiguous_iterator& {
            storage.current += pos;
            return *this;
        }

        constexpr auto operator-=(difference_type pos) -> contiguous_iterator& {
            storage.current -= pos;
            return *this;
        }
        // NOLINTEND(cppcoreguidelines-pro-bounds-pointer-arithmetic)

        // Comparison operator
        constexpr auto operator<=>(const contiguous_iterator& rhs) const {
            return storage.current <=> rhs.storage.current;
        }

        constexpr auto operator==(const contiguous_iterator& rhs) const -> bool {
            return storage.current == rhs.storage.current;
        }

        constexpr auto operator+(difference_type pos) const -> contiguous_iterator {
            contiguous_iterator iter = *this;
            iter += pos;
            return iter;
        }

        constexpr auto operator-(difference_type pos) const -> contiguous_iterator {
            contiguous_iterator iter = *this;
            iter -= pos;
            return iter;
        }

        friend constexpr auto operator+(difference_type pos, contiguous_iterator self)
            -> contiguous_iterator {
            return self + pos;
        }

        // Index and difference operators
        constexpr auto operator-(const contiguous_iterator& rhs) const -> difference_type {
            return storage.current - rhs.storage.current;
        }

        constexpr auto operator[](difference_type idx) const -> reference {
            auto copy = *this + idx;
            return *copy;
        }

      private:
        iterator_storage<T, EnableChecks> storage;
    };
    // NOLINTEND(misc-non-private-member-variables-in-classes)
}  // namespace detail

template<typename T, bool EnableChecks>
using contiguous_iterator = detail::contiguous_iterator<T, EnableChecks>;
}  // namespace ktl