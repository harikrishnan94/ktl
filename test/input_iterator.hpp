#pragma once

#include <iterator>

template<std::input_iterator It>
class InputIterator {
  public:
    using value_type = std::iter_value_t<It>;
    using difference_type = std::iter_difference_t<It>;
    using reference = std::iter_reference_t<It>;

    constexpr explicit InputIterator(It it) : m_it(it) {}

    constexpr auto operator*() const noexcept -> reference {
        return *m_it;
    }
    constexpr auto operator->() const noexcept -> value_type* {
        return &*m_it;
    }

    constexpr auto operator++() noexcept -> InputIterator& {
        ++m_it;
        return *this;
    }
    constexpr auto operator++(int) noexcept -> InputIterator {
        auto copy = *this;
        ++*this;
        return copy;
    }

    constexpr auto operator<=>(const InputIterator& o) const = default;

  private:
    It m_it;
};
