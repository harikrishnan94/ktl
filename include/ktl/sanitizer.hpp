#pragma once

#include <ktl/string_view.hpp>

namespace ktl::san {
void ubsan_message(string_view msg);
[[noreturn]] void abort_with_message(string_view msg);
}  // namespace ktl::san
