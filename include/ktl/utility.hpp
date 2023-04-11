#pragma once

#include <ktl/int.hpp>
#include <ktl/string_view.hpp>

namespace ktl {
// NOLINTBEGIN(hicpp-avoid-c-arrays, hicpp-explicit-conversions,
// misc-non-private-member-variables-in-classes)
// Compile time string used to capture the `format string`
template<typename CharT, usize N>
struct const_string {
    using char_type = CharT;

    constexpr const_string(const CharT (&str)[N]) {
        std::copy_n(str, N, value);
    }

    constexpr auto begin() const noexcept -> const char_type* {
        return &value[0];
    }

    constexpr auto end() const noexcept -> const char_type* {
        return begin() + N - 1;  // Ignore trailing `NUL` char
    }

    constexpr auto view() const noexcept -> basic_string_view<char_type> {
        return {begin(), end()};
    }

    char_type value[N] = {};
};
// NOLINTEND(hicpp-avoid-c-arrays, hicpp-explicit-conversions,
// misc-non-private-member-variables-in-classes)
}  // namespace ktl