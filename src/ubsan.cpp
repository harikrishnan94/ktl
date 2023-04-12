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
    static static_string<1024> msgbuf;
    static spin_mutex mtx = {};

    std::lock_guard lock {mtx};
    msgbuf.clear();
    if (auto res = fmt::format<"ubsan: {} by {:#x} @ {}:{}:{}\n">(
            msgbuf,
            msg,
            caller,
            sloc->filename,
            sloc->line,
            sloc->column)) {
        message<IsFatal>({msgbuf.data(), res->len()});
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

HANDLER_RECOVER(builtin_unreachable, "builtin-unreachable")
HANDLER_RECOVER(missing_return, "missing-return")
HANDLER(add_overflow, "add_overflow");
HANDLER(alignment_assumption, "alignment_assumption");
HANDLER(cfi_bad_type, "cfi_bad_type");
HANDLER(cfi_check_fail, "cfi_check_fail");
HANDLER(divrem_overflow, "divrem_overflow");
HANDLER(dynamic_type_cache_miss, "dynamic_type_cache_miss");
HANDLER(float_cast_overflow, "float_cast_overflow");
HANDLER(function_type_mismatch_v1, "function_type_mismatch_v1");
HANDLER(function_type_mismatch, "function-type-mismatch")
HANDLER(get_current_report_data, "get_current_report_data");
HANDLER(implicit_conversion, "implicit-conversion")
HANDLER(invalid_builtin, "invalid-builtin")
HANDLER(invalid_objc_cast, "invalid-objc-cast")
HANDLER(load_invalid_value, "load-invalid-value")
HANDLER(mul_overflow, "mul-overflow")
HANDLER(negate_overflow, "negate_overflow");
HANDLER(nonnull_arg, "nonnull_arg");
HANDLER(nonnull_return_v1, "nonnull_return_v1");
HANDLER(nonnull_return, "nonnull-return")
HANDLER(nullability_arg, "nullability_arg");
HANDLER(nullability_return_v1, "nullability_return_v1");
HANDLER(nullability_return, "nullability-return")
HANDLER(out_of_bounds, "out-of-bounds")
HANDLER(pointer_overflow, "pointer-overflow")
HANDLER(shift_out_of_bounds, "shift_out_of_bounds");
HANDLER(sub_overflow, "sub_overflow");
HANDLER(type_mismatch_v1, "type_mismatch_v1");
HANDLER(type_mismatch, "type-mismatch")
HANDLER(vla_bound_not_positive, "vla-bound-not-positive")
