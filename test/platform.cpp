#include <sys/syscall.h>

extern "C" auto main() -> int;

// Will be provided by linker
extern void (*__init_array_start)();
extern void (*__init_array_end)();

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
    asm volatile(
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

[[noreturn]] static void exit(int ret) {
    syscall<SYS_exit>(ret);
    __builtin_unreachable();
}

void* __dso_handle = nullptr;
extern "C" void __cxa_atexit() {}

static void call_static_constructors() {
    for (auto* cons = &__init_array_start; cons < &__init_array_end; cons++) {
        (*cons)();
    }
}

extern "C" [[noreturn]] void ENTRYPOINT() {
    call_static_constructors();
    exit(main());
}