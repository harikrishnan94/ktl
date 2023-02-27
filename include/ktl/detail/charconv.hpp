#pragma once

#include <array>
#include <charconv>
#include <concepts>
#include <limits>
#include <optional>

#include "ktl/assert.hpp"
#include "ktl/int.hpp"

namespace ktl::fmt::detail {
enum class int_base {
    bin = 2,
    oct = 8,
    dec = 10,
    hex = 16,
};

template<int_base Base, std::integral T>
static constexpr auto to_digit(char c) -> std::optional<T> {
    if constexpr (Base == int_base::bin) {
        if (c == '0' || c == '1') {
            return c - '0';
        }
    } else if constexpr (Base == int_base::oct) {
        if (c >= '0' && c <= '7') {
            return c - '0';
        }
    } else if constexpr (Base == int_base::dec) {
        if (c >= '0' && c <= '9') {
            return c - '0';
        }
    } else {
        if ((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f')) {
            constexpr auto DecEnd = 10;

            if (c >= '0' && c <= '9') {
                return c - '0';
            }
            if (c > 'A' && c <= 'F') {
                return DecEnd + c - 'A';
            }
            return DecEnd + c - 'a';
        }
    }

    return {};
}

template<std::integral T>
static constexpr auto checked_mul_and_add(T v, T m, T d) -> std::optional<T> {
    constexpr auto max = std::numeric_limits<T>::max();
    constexpr auto min = std::numeric_limits<T>::min();

    /*
     * Overflow can only happen if at least one value is outside the range
     * sqrt(min)..sqrt(max) so check that first as the division can be quite a
     * bit more expensive than the multiplication.
     *
     * Multiplying by 0 or 1 can't overflow of course and checking for 0
     * separately avoids any risk of dividing by 0.  Be careful about dividing
     * INT_MIN by -1 also, note reversing the a and b to ensure we're always
     * dividing it by a positive value.
     *
     */
    if constexpr (std::is_same_v<u64, T> || std::is_same_v<i64, T>) {
        constexpr auto max_32 =
            std::numeric_limits<std::conditional_t<std::is_signed_v<T>, i32, u32>>::max();
        constexpr auto min_32 =
            std::numeric_limits<std::conditional_t<std::is_signed_v<T>, i32, u32>>::min();

        if ((v > max_32 || v < min_32 || d > max_32 || d < min_32) && v != 0 && v != 1 && d != 0
            && d != 1
            && ((v > 0 && d > 0 && v > max / d) || (v > 0 && d < 0 && d < min / v)
                || (v < 0 && d > 0 && v < min / d) || (v < 0 && d < 0 && v < max / d))) {
            return {};
        }

        v *= m;

        if ((v > 0 && d > 0 && v > max - d) || (v < 0 && d < 0 && v < min - d)) {
            return {};
        }

        return v + d;
    } else {
        using wide = std::conditional_t<std::is_signed_v<T>, i64, u64>;

        auto res = static_cast<wide>(v) * m + d;

        if (res > max || res < min) {
            return {};
        }

        return res;
    }
}

template<int_base Base, std::integral T>
constexpr auto from_chars(const char* start, const char* end, T& out) -> std::from_chars_result {
    check_(start < end, "start pointer of the string must be less then end pointer");

    T v = 0;
    bool is_neg = false;

    auto result = [&](auto ptr, bool is_overflow) -> std::from_chars_result {
        if (is_overflow) {
            return {.ptr = start, .ec = std::errc::result_out_of_range};
        }
        if (ptr == start) {
            return {.ptr = start, .ec = std::errc::invalid_argument};
        }
        if (auto r = detail::checked_mul_and_add<T>(v, is_neg ? -1 : 1, 0)) {
            out = *r;
            return {.ptr = ptr, .ec = {}};
        }
        return {.ptr = start, .ec = std::errc::result_out_of_range};
    };

    for (const auto* ptr = start; ptr < end; ptr = std::next(ptr)) {
        auto c = *ptr;

        if (auto d = detail::to_digit<Base, T>(c)) {
            if (auto r = detail::checked_mul_and_add(v, static_cast<T>(Base), *d)) {
                v = *r;
            } else {
                return result(ptr, true);
            }
        } else {
            if (c == '-') {
                if (ptr == start) {
                    is_neg = true;
                    continue;
                }
            }
            return result(ptr, false);
        }
    }

    return result(end, false);
}
}  // namespace ktl::fmt::detail