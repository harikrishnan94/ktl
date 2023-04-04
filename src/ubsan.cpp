// llvm-project/compiler-rt/lib/ubsan_minimal/ubsan_minimal_handlers.cpp

#include <atomic>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <limits>
#include <unistd.h>

#include <ktl/fmt/format.hpp>
#include <ktl/sanitizer.hpp>
#include <ktl/spin_mutex.hpp>

namespace ktl::ubsan {
template<bool IsFatal>
void message(string_view msg) {
    if constexpr (IsFatal) {
        ktl::san::abort_with_message(msg);
    } else {
        ktl::san::ubsan_message(msg);
    }
}

// NOLINTBEGIN

[[gnu::noinline]] auto is_aready_reported(uintptr_t caller) -> bool {
    constexpr int MaxCallerPcs = 20;
    static std::atomic_uintptr_t caller_pcs[MaxCallerPcs];
    // Number of elements in caller_pcs. A special value of kMaxCallerPcs + 1 means
    // that "too many errors" has already been reported.
    static std::atomic_uint32_t caller_pcs_sz;

    if (caller == 0)
        return false;
    while (true) {
        auto sz = caller_pcs_sz.load(std::memory_order::relaxed);
        if (sz > MaxCallerPcs)
            return false;  // early exit
        // when sz==kMaxCallerPcs print "too many errors", but only when cmpxchg
        // succeeds in order to not print it multiple times.
        if (sz > 0 && sz < MaxCallerPcs) {
            uintptr_t p;
            for (u32 i = 0; i < sz; ++i) {
                p = caller_pcs[i].load(std::memory_order::relaxed);
                if (p == 0)
                    break;  // Concurrent update.
                if (p == caller)
                    return false;
            }
            if (p == 0)
                continue;  // FIXME: yield?
        }

        if (!caller_pcs_sz.compare_exchange_strong(sz, sz + 1, std::memory_order::seq_cst))
            continue;  // Concurrent update! Try again from the start.

        if (sz == MaxCallerPcs) {
            message<false>("ubsan: too many errors\n");
            return false;
        }
        caller_pcs[sz].store(caller, std::memory_order::seq_cst);
        return true;
    }
}

struct SourceLocation {
    const char* filename;
    u32 line;
    u32 column;
};

#define INTERFACE extern "C" __attribute__((visibility("default")))

#define GET_CALLER_PC() \
    std::bit_cast<uintptr_t>(__builtin_extract_return_addr(__builtin_return_address(0)))

template<bool IsFatal>
void handle_error(const char* msg, uintptr_t caller, not_null<const SourceLocation*> sloc) {
    static std::array<char, 1024> msgbuf;
    static spin_mutex mtx = {};

    std::lock_guard lock {mtx};
    fmt::fixed_buffer fb {msgbuf};
    if (auto res = fmt::format<"ubsan: {} by {:#x} @ {}:{}:{}\n">(
            fb,
            msg,
            caller,
            sloc->filename,
            sloc->line,
            sloc->column)) {
        msgbuf.data()[res->formatted_len()] = '\0';
        message<IsFatal>({msgbuf.data(), res->formatted_len()});
    } else {
        message<IsFatal>("ubsan: unknown error\n");
    }
}

#define HANDLER_RECOVER(name, msg) \
    INTERFACE void __ubsan_handle_##name(const ktl::ubsan::SourceLocation* sloc) { \
        auto caller = GET_CALLER_PC(); \
        if (!ktl::ubsan::is_aready_reported(caller)) \
            return; \
        ktl::ubsan::handle_error<false>(msg, caller, sloc); \
    }

#define HANDLER_NORECOVER(name, msg) \
    INTERFACE void __ubsan_handle_##name##_abort(const ktl::ubsan::SourceLocation* sloc) { \
        auto caller = GET_CALLER_PC(); \
        ktl::ubsan::handle_error<true>(msg, caller, sloc); \
    }
}  // namespace ktl::ubsan

#define HANDLER(name, msg) \
    HANDLER_RECOVER(name, msg) \
    HANDLER_NORECOVER(name, msg)

// NOLINTEND

extern "C" __attribute__((visibility("default"))) void
__ubsan_handle_type_mismatch(const ktl ::ubsan ::SourceLocation* sloc) {
    auto caller =
        std ::bit_cast<uintptr_t>(__builtin_extract_return_addr(__builtin_return_address(0)));
    if (!ktl ::ubsan ::is_aready_reported(caller))
        return;
    ktl ::ubsan ::handle_error<false>("type-mismatch", caller, sloc);
}
extern "C" __attribute__((visibility("default"))) void
__ubsan_handle_type_mismatch_abort(const ktl ::ubsan ::SourceLocation* sloc) {
    auto caller =
        std ::bit_cast<uintptr_t>(__builtin_extract_return_addr(__builtin_return_address(0)));
    ktl ::ubsan ::handle_error<true>("type-mismatch", caller, sloc);
}
HANDLER(type_mismatch_v1, "type-mismatch")
HANDLER(alignment_assumption, "alignment-assumption")
HANDLER(add_overflow, "add-overflow")
HANDLER(sub_overflow, "sub-overflow")
HANDLER(mul_overflow, "mul-overflow")
HANDLER(negate_overflow, "negate-overflow")
HANDLER(divrem_overflow, "divrem-overflow")
HANDLER(shift_out_of_bounds, "shift-out-of-bounds")
HANDLER(out_of_bounds, "out-of-bounds")
HANDLER_RECOVER(builtin_unreachable, "builtin-unreachable")
HANDLER_RECOVER(missing_return, "missing-return")
HANDLER(vla_bound_not_positive, "vla-bound-not-positive")
HANDLER(float_cast_overflow, "float-cast-overflow")
HANDLER(load_invalid_value, "load-invalid-value")
HANDLER(invalid_builtin, "invalid-builtin")
HANDLER(invalid_objc_cast, "invalid-objc-cast")
HANDLER(function_type_mismatch, "function-type-mismatch")
HANDLER(implicit_conversion, "implicit-conversion")
HANDLER(nonnull_arg, "nonnull-arg")
HANDLER(nonnull_return, "nonnull-return")
HANDLER(nullability_arg, "nullability-arg")
HANDLER(nullability_return, "nullability-return")
HANDLER(pointer_overflow, "pointer-overflow")
HANDLER(cfi_check_fail, "cfi-check-fail")
