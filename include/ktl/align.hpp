#pragma once

#include <boost/align/align_down.hpp>
#include <boost/align/align_up.hpp>
#include <boost/align/is_aligned.hpp>
#include <concepts>

#include <ktl/int.hpp>

namespace ktl {
template<std::integral Int>
constexpr auto AlignUp(Int value, std::make_unsigned_t<Int> alignment) noexcept -> Int {
    using UInt = std::make_unsigned_t<Int>;
    return boost::alignment::align_up(static_cast<UInt>(value), alignment);
}

template<std::integral Int>
constexpr auto AlignUpOffset(Int value, std::make_unsigned_t<Int> alignment) noexcept -> Int {
    return AlignUp(value, alignment) - value;
}

template<std::integral Int>
constexpr auto AlignDown(Int value, std::make_unsigned_t<Int> alignment) noexcept -> Int {
    using UInt = std::make_unsigned_t<Int>;
    return boost::alignment::align_down(static_cast<UInt>(value), alignment);
}

template<std::integral Int>
constexpr auto AlignDownOffset(Int value, std::make_unsigned_t<Int> alignment) noexcept -> Int {
    return value - AlignDown(value, alignment);
}

template<std::integral Int>
constexpr inline auto IsAligned(Int value, std::make_unsigned_t<Int> alignment) noexcept -> bool {
    using UInt = std::make_unsigned_t<Int>;
    return boost::alignment::is_aligned(static_cast<UInt>(value), alignment);
}
}  // namespace ktl