#pragma once

#include <bit>
#include <concepts>
#include <limits>

#include <ktl/assert.hpp>
#include <ktl/int.hpp>

// Full-featured constexpr bit manipulation module
namespace ktl {
template<std::unsigned_integral I>
constexpr auto CreateMask(uint start_pos, uint count) {
    check_(
        count > 0 && count <= std::numeric_limits<I>::digits,
        "mask bit count cannot exceed # bits");
    check_(start_pos < std::numeric_limits<I>::digits, "mask start bit must be less than # bits");
    return (std::numeric_limits<I>::max() >> (std::numeric_limits<I>::digits - count)) << start_pos;
}

template<std::unsigned_integral I, typename... Bits>
constexpr auto CreateMaskFor(Bits... bits) -> I {
    (check_(bits < std::numeric_limits<I>::digits, "mask bit must be less than # bits"), ...);
    return ((I {1} << bits) | ...);
}

template<std::unsigned_integral I>
constexpr auto GetMaskedBits(I val, I mask) {
    return val & mask;
}

template<std::unsigned_integral I>
constexpr auto ClearMaskedBits(I val, I mask) {
    return val & ~mask;
}

template<std::unsigned_integral I>
constexpr auto SetMaskedBits(I lhs, I rhs, I mask) {
    return (lhs & ~mask) | (rhs & mask);
}

template<std::unsigned_integral I>
constexpr auto HasAnyInMask(I val, I mask) -> bool {
    return GetMaskedBits(val, mask) != 0;
}

template<std::unsigned_integral I>
constexpr auto HasAllInMask(I val, I mask) -> bool {
    return GetMaskedBits(val, mask) == mask;
}
}  // namespace ktl
