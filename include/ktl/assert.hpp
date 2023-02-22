#pragma once

#include <cassert>

#ifdef ENABLE_ASSERT
    #undef assert
    #define assert(expr) \
        (static_cast<bool>(expr) ? void(0) \
                                 : __assert_fail( \
                                     #expr, \
                                     __FILE__, \
                                     __LINE__, \
                                     static_cast<const char*>(__ASSERT_FUNCTION)))
    #define KTL_ENABLE_CHECKED_ITERATORS true
#else
#endif

namespace ktl {
[[noreturn]] void
Abort(const char* message, const char* file, unsigned int line, const char* function) noexcept;
}

// NOLINTNEXTLINE(*-macro-usage)
#define abort(message) \
    ktl::Abort(message, __FILE__, __LINE__, static_cast<const char*>(__ASSERT_FUNCTION))
