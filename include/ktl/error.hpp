#pragma once

#include <bit>
#include <concepts>
#include <cstdint>

#include <ktl/detail/preproc.hpp>
#include <ktl/not_null.hpp>

namespace ktl {
namespace error::detail {
    // Derived classes override `what()` method to provide detailed information.
    struct ErrorBase {
        virtual ~ErrorBase() = default;

        [[nodiscard]] constexpr virtual auto what() const noexcept -> const char* = 0;
        [[nodiscard]] virtual auto code() const noexcept -> uintptr_t {
            return std::bit_cast<uintptr_t>(this);
        }

        ErrorBase() = default;
        ErrorBase(const ErrorBase&) = delete;
        ErrorBase(ErrorBase&&) = delete;
        auto operator=(const ErrorBase&) -> ErrorBase& = delete;
        auto operator=(ErrorBase&&) -> ErrorBase& = delete;
    };
}  // namespace error::detail

// NOLINTBEGIN
#define DEFINE_ERROR_LOCAL_EXT(name, what_str) \
    struct CONCAT(name, _t) : ::ktl::error::detail::ErrorBase { \
        /* Workaround for GCC-12 */ \
        constexpr ~CONCAT(name, _t)() override {} \
\
        [[nodiscard]] constexpr auto what() const noexcept -> const char* override { \
            return what_str; \
        } \
    } static constexpr CONCAT(name, _v); \
    static constexpr ::ktl::Error name { \
        &CONCAT(name, _v) \
    }

#define DEFINE_ERROR_EXT(name, what_str) \
    namespace detail::error_types { \
        struct CONCAT(name, _t) : ::ktl::error::detail::ErrorBase { \
            /* Workaround for GCC-12 */ \
            constexpr ~CONCAT(name, _t)() override {} \
\
            [[nodiscard]] constexpr auto what() const noexcept -> const char* override { \
                return what_str; \
            } \
        } inline constexpr name; \
    } \
    inline constexpr ::ktl::Error name { \
        &detail::error_types::name \
    }

#define DEFINE_ERROR_LOCAL(name) DEFINE_ERROR_LOCAL_EXT(name, #name)
#define DEFINE_ERROR(name) DEFINE_ERROR_EXT(name, #name)
// NOLINTEND

// Generic Error
using Error = not_null<const error::detail::ErrorBase*>;

// Used to signal errors happening in runtime.
namespace error {
    DEFINE_ERROR(OutOfMemory);  // Heap out of memory
    DEFINE_ERROR(IndexOutOfBounds);  // Container out of bounds index
    DEFINE_ERROR(BufferFull);  // Container/Local Buffer out of memory
    DEFINE_ERROR(ValueOutOfDomain);  // Width/Precision replacement is negative
    DEFINE_ERROR(ValueOutOfRange);  // value is not in the range of representable values of a type
}  // namespace error
}  // namespace ktl