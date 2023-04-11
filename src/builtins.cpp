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

    if (size == 0 || dst == src)
        return;

    if (dst < src) {
        /* Copy forwards */
        for (size_t i = 0; i < size; i++) {
            dst[i] = src[i];
        }
    } else {
        /* Copy backwards */
        for (size_t i = size - 1; i != 0; i--) {
            dst[i] = src[i];
        }
        *dst = *src;
    }
}

extern "C" void memset(void* __restrict__ dst_p, int val, size_t size) {
    auto* dst = static_cast<char*>(dst_p);

    for (size_t i = 0; i < size; i++) {
        dst[i] = static_cast<char>(val);
    }
}

extern "C" auto memcmp(const void* __restrict__ a_p, const void* __restrict__ b_p, size_t count)
    -> int {
    const auto* a = static_cast<const std::byte*>(a_p);
    const auto* b = static_cast<const std::byte*>(b_p);

    for (size_t i = 0; i < count; i++) {
        if (a[i] == b[i]) {
            continue;
        }

        return *a < *b ? -1 : 1;
    }

    return 0;
}

extern "C" auto strlen(const char* s) -> size_t {
    size_t len = 0;
    while (*s++ != '\0') {
        len++;
    }
    return len;
}

extern "C" auto memchr(const void* ptr_v, int ch, size_t count) -> const void* {
    const auto* ptr = static_cast<const unsigned char*>(ptr_v);
    for (size_t i = 0; i < count; i++) {
        if (ptr[i] == ch) {
            return ptr + i;
        }
    }
    return nullptr;
}