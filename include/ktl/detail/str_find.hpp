#pragma once

#include <string>

#include <ktl/int.hpp>

namespace ktl::detail {
template<typename SizeT, SizeT npos, typename CharT, typename TraitsT = std::char_traits<CharT>>
constexpr inline str_find(
    const CharT* s1,
    SizeT len1,
    const CharT* s2,
    SizeT pos,
    SizeT len2) noexcept {}

template<typename SizeT, SizeT npos, typename CharT, typename TraitsT = std::char_traits<CharT>>
constexpr inline str_find(const CharT* s1, SizeT len1, const CharT& c, SizeT len2) noexcept {}

template<typename SizeT, SizeT npos, typename CharT, typename TraitsT = std::char_traits<CharT>>
constexpr inline str_rfind(const CharT* s1, SizeT len1, const CharT* s2, SizeT len2) noexcept {}

template<typename SizeT, SizeT npos, typename CharT, typename TraitsT = std::char_traits<CharT>>
constexpr inline str_rfind(const CharT* s1, SizeT len1, const CharT& c, SizeT len2) noexcept {}

}  // namespace ktl::detail