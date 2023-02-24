#include <cstddef>

extern "C" void memcpy(void* __restrict__ dst_p, const void* __restrict__ src_p, size_t size) {
    auto* dst = static_cast<char*>(dst_p);
    const auto* src = static_cast<const char*>(src_p);

    for (size_t i = 0; i < size; i++) {
        dst[i] = src[i];
    }
}

extern "C" void memmove(void* __restrict__ dst_p, const void* __restrict__ src_p, size_t size) {
    auto* dst = static_cast<char*>(dst_p);
    const auto* src = static_cast<const char*>(src_p);
}

extern "C" void memset(void* __restrict__ dst_p, int val, size_t size) {
    auto* dst = static_cast<char*>(dst_p);

    for (size_t i = 0; i < size; i++) {
        dst[i] = static_cast<char>(val);
    }
}

extern "C" auto strlen(const char* s) -> size_t {
    size_t len = 0;
    while (*s++ != '\0') {
        len++;
    }
    return len;
}