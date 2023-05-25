#pragma once

#include <concepts>
#include <numeric>
#include <utility>

#include <ktl/int.hpp>

namespace ktl {
// `ratio` represents a runtime ratio and provides runtime rational arithmetic support.
// Denominator may not be zero and both Numerator and Denominator may not be equal to the most
// negative value.
template<std::integral Int = usize>
struct ratio {
    using int_type = Int;

    int_type num;
    int_type den;

    // Equality compares the two ratios.
    // Both the ratios must have been reduced to get correct result.
    friend constexpr auto operator==(const ratio&, const ratio&) noexcept -> bool = default;

    // Compares the two ratios.
    // Both the ratios must have been canonilized to get correct result.
    friend constexpr auto operator<=>(const ratio& r1, const ratio& r2) noexcept
        -> std::partial_ordering {
        if (r1.den != r2.den) {
            return std::partial_ordering::unordered;
        }
        return r1.num <=> r2.num;
    }
};

// Reduce the ratio by dividing both `num` and `den` by their `gcd`
template<std::integral Int>
constexpr auto reduce(ratio<Int> r) noexcept -> ratio<Int> {
    auto gcd = std::gcd(r.num, r.den);
    return {r.num / gcd, r.den / gcd};
}

// Makes a ratio out of `num` and `den`.
// Denominator may not be zero and both Numerator and Denominator may not be equal to the most
// negative value.
template<std::integral Int>
constexpr auto make_ratio(Int num, Int den) noexcept -> ratio<Int> {
    check_(den != 0, "");
    check_(num != std::numeric_limits<Int>::min(), "");
    return reduce<Int>({num, den});
}

// Canonicalize the ratios for comparison.
template<std::integral Int>
constexpr auto canonicalize(ratio<Int> r1, ratio<Int> r2) noexcept
    -> std::pair<ratio<Int>, ratio<Int>> {
    r1 = reduce(r1);
    r2 = reduce(r2);

    auto common_den = r1.den * r2.den;
    return {{r1.num * r2.den, common_den}, {r2.num * r1.den, common_den}};
}
}  // namespace ktl