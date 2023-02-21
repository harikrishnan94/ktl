#include <sys/syscall.h>

extern "C" auto main() -> int;

static auto syscall(
    [[maybe_unused]] long call_no,
    [[maybe_unused]] long arg1 = 0,
    [[maybe_unused]] long arg2 = 0,
    [[maybe_unused]] long arg3 = 0,
    [[maybe_unused]] long arg4 = 0,
    [[maybe_unused]] long arg5 = 0,
    [[maybe_unused]] long arg6 = 0,
    [[maybe_unused]] long arg7 = 0,
    [[maybe_unused]] long arg8 = 0) -> long {
#if ARCH_IS_X86_64 == 1
    asm volatile(
        "movq %rdi, %rax\n"
        "movq %rsi, %rdi\n"
        "movq %rdx, %rsi\n"
        "movq %rcx, %rdx\n"
        "movq %r8, %r10\n"
        "movq %r9, %r8\n"
        "movq 8(%rsp), %r9\n"
        "syscall\n"
        "ret\n");
#elif ARCH_IS_AARCH64 == 1
    asm volatile(
        "mov x8, x0\n"
        "mov x0, x1\n"
        "mov x1, x2\n"
        "mov x2, x3\n"
        "mov x3, x4\n"
        "mov x4, x5\n"
        "mov x5, x6\n"
        "mov x6, x7\n"
        "svc #0\n"
        "ret\n");
#endif
    __builtin_unreachable();
}

[[noreturn]] static void exit(int ret) {
    syscall(SYS_exit, ret);
    __builtin_unreachable();
}

static void call_static_constructors() {}
static void call_static_destructors() {}

extern "C" [[noreturn]] void ENTRYPOINT() {
    call_static_constructors();
    auto ret = main();
    call_static_destructors();
    exit(ret);
}