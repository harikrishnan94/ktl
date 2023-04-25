#pragma once

#include <compare>
#include <type_traits>

#include <ktl/assert.hpp>

namespace ktl {
template<typename Ptr>
    requires std::is_pointer_v<Ptr>
class not_null {
  public:
    not_null(std::nullptr_t) = delete;

#ifndef __CLANGD__
    [[gnu::nonnull(2)]]
#endif
    // NOLINTNEXTLINE(*-explicit-conversions)
    constexpr not_null(Ptr p) :
        m_ptr(p) {
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

  private:
    Ptr m_ptr;
};

}  // namespace ktl