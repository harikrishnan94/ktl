#pragma once

#include <compare>
#include <memory>
#include <type_traits>

#include <ktl/assert.hpp>

namespace ktl {
template<typename Ptr>
    requires std::is_pointer_v<Ptr>
class not_null {
  public:
    using element_type = std::pointer_traits<Ptr>::element_type;

    not_null(std::nullptr_t) = delete;

    // NOLINTNEXTLINE(*-explicit-conversions)
    [[gnu::nonnull(2)]] constexpr not_null(element_type* p) : m_ptr {p} {
        if (!std::is_constant_evaluated()) {
            check_(m_ptr != nullptr, "nullptr is not expected");
        }
    }

    // NOLINTNEXTLINE(*-explicit-conversions)
    constexpr operator Ptr() const noexcept {
        return m_ptr;
    }

    constexpr auto operator*() const noexcept -> auto& {
        return *m_ptr;
    }
    constexpr auto operator->() const noexcept -> Ptr {
        return m_ptr;
    }

    friend auto operator==(const not_null&, const not_null&) -> bool = default;
    friend auto operator<=>(const not_null&, const not_null&) = default;

    friend auto operator==(const not_null& p1, element_type* p2) noexcept -> bool {
        return p1.m_ptr == p2;
    }
    friend auto operator<=>(const not_null& p1, element_type* p2) noexcept {
        return p1.m_ptr <=> p2;
    }

  private:
    Ptr m_ptr;
};

static_assert(std::same_as<std::pointer_traits<not_null<int*>>::element_type, int>);
}  // namespace ktl