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
    #define KTL_CHECKS_ENABLED true
#else
    #ifndef KTL_CHECKS_ENABLED
        #define KTL_CHECKS_ENABLED false
    #endif

    #ifndef __ASSERT_FUNCTION
        #define __ASSERT_FUNCTION __PRETTY_FUNCTION__
    #endif
#endif

namespace ktl {
[[noreturn]] void
Abort(const char* message, const char* file, unsigned int line, const char* function) noexcept;
}

// NOLINTNEXTLINE(*-macro-usage)
#define abort_(message) \
    ktl::Abort(message, __FILE__, __LINE__, static_cast<const char*>(__ASSERT_FUNCTION))

// NOLINTNEXTLINE(*-macro-usage)
#define unreachable() __builtin_unreachable()

// Used for contract checking (validating inputs external to the API)
#ifndef DISABLE_CHECKS
    // NOLINTNEXTLINE(*-macro-usage)
    #define check_(expr, msg) \
        (static_cast<bool>(expr) ? void(0) : abort_("failed: " #expr "[" msg "]"))
#else
    #define check_(expr, msg) (static_cast<bool>(expr) ? void(0) : unreachable())
#endif
