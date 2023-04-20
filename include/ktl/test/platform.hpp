#pragma once

#include <ktl/int.hpp>
#include <ktl/string_view.hpp>

extern "C" auto main() -> int;
auto write_stdout(const void* data, ktl::isize len) -> ktl::isize;

auto write(char n) -> ktl::isize;
auto write(const ktl::string_view& str) -> ktl::isize;

template<std::integral I>
auto write(I n) -> ktl::isize;
