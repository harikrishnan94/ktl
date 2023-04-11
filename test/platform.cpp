#include <bit>
#include <ktl/test/platform.h>
#include <sys/syscall.h>
#include <unistd.h>

#include <ktl/assert.hpp>
#include <ktl/fmt/format.hpp>
#include <ktl/int.hpp>
#include <ktl/sanitizer.hpp>

using namespace ktl;

// Will be provided by linker
extern void (*__init_array_start)();  // NOLINT
extern void (*__init_array_end)();  // NOLINT

template<long CALL_NO>
static auto
syscall(long arg1 = 0, long arg2 = 0, long arg3 = 0, long arg4 = 0, long arg5 = 0, long arg6 = 0)
    -> long {
    long ret = 0;

#if ARCH_IS_X86_64 == 1
    asm volatile(
        "mov %[call_no], %%rax\n"
        "mov %[arg1], %%rdi\n"
        "mov %[arg2], %%rsi\n"
        "mov %[arg3], %%rdx\n"
        "mov %[arg4], %%r10\n"
        "mov %[arg5], %%r8\n"
        "mov %[arg6], %%r9\n"
        "syscall\n"
        "mov %%rax, %[ret]\n"
        : [ret] "=r"(ret)
        : [call_no] "i"(CALL_NO),
          [arg1] "r"(arg1),
          [arg2] "r"(arg2),
          [arg3] "r"(arg3),
          [arg4] "r"(arg4),
          [arg5] "r"(arg5),
          [arg6] "r"(arg6)
        : "rax", "rdi", "rsi", "rdx", "r10", "r8", "r9");
#elif ARCH_IS_AARCH64 == 1
    asm volatile(  // NOLINT
        "mov x8, %[call_no]\n"
        "mov x0, %[arg1]\n"
        "mov x1, %[arg2]\n"
        "mov x2, %[arg3]\n"
        "mov x3, %[arg4]\n"
        "mov x4, %[arg5]\n"
        "mov x5, %[arg6]\n"
        "svc #0\n"
        : [ret] "=r"(ret)
        : [call_no] "i"(CALL_NO),
          [arg1] "r"(arg1),
          [arg2] "r"(arg2),
          [arg3] "r"(arg3),
          [arg4] "r"(arg4),
          [arg5] "r"(arg5),
          [arg6] "r"(arg6)
        : "x0", "x1", "x2", "x3", "x4", "x5", "x8");
#endif

    return ret;
}

void exit(int status) {
    syscall<SYS_exit>(status);
    __builtin_unreachable();
}

void* __dso_handle = nullptr;  // NOLINT
extern "C" void __cxa_atexit() {}  // NOLINT

static void call_static_constructors() {
    for (auto* cons = &__init_array_start; cons < &__init_array_end; cons++) {
        (*cons)();
    }
}

extern "C" [[noreturn]] void ENTRYPOINT() {
    call_static_constructors();
    exit(main());  // NOLINT
}

auto write_stdout(const void* data, isize len) -> isize {
    return syscall<SYS_write>(STDOUT_FILENO, std::bit_cast<long>(data), len);
}

auto write(const ktl::string_view& str) -> isize {
    return write_stdout(str.data(), static_cast<isize>(str.length()));
}

static constexpr auto MAX_INT_DIGITS = 20;

template<std::integral I>
auto write(I n) -> isize {
    std::array<char, MAX_INT_DIGITS> str {};
    fmt::fixed_buffer fb {str.data(), str.data() + str.size()};
    (void)fmt::format<"{}">(fb, n);
    return write(str.data());
}

auto write(char n) -> isize {
    std::array chs = {n, '\0'};
    return write(chs.data());
}

template auto write(short n) -> isize;
template auto write(int n) -> isize;
template auto write(long n) -> isize;
template auto write(long long n) -> isize;

template auto write(unsigned char n) -> isize;
template auto write(unsigned short n) -> isize;
template auto write(unsigned int n) -> isize;
template auto write(unsigned long n) -> isize;
template auto write(unsigned long long n) -> isize;

extern "C" void
__assert_fail(const char* assertion, const char* file, unsigned int line, const char* function) {
    write("assertion failed: [");
    ktl::Abort(assertion, file, line, function);
}

namespace ktl {
void Abort(
    const char* message,
    const char* file,
    unsigned int line,
    const char* function) noexcept {
    write(message);
    write("] @ ");
    write(function);
    write(" @ ");
    write(file);
    write(":");
    write(line);
    exit(-1);
}

namespace san {
    void ubsan_message(string_view msg) {
        write(msg);
    }

    [[noreturn]] void abort_with_message(string_view msg) {
        write(msg);
        exit(-1);
    }
}  // namespace san

}  // namespace ktl