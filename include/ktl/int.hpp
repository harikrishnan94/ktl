#pragma once

#include <cstddef>
#include <cstdint>
#include <type_traits>

namespace ktl {
using u8 = uint8_t;
using u16 = uint16_t;
using u32 = uint32_t;
using u64 = uint64_t;
using usize = size_t;
using uint = unsigned;

using i8 = std::make_signed_t<u8>;
using i16 = std::make_signed_t<u16>;
using i32 = std::make_signed_t<u32>;
using i64 = std::make_signed_t<u64>;
using isize = std::make_signed_t<usize>;
}  // namespace ktl