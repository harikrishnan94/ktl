#pragma once

namespace ktl {
// Used to signal errors happening in runtime.
// All errors are positive integers.
// 0 means success, though not defined explicitly
enum class Error : int {
    OutOfMemory = 1,  // Heap out of memory
    IndexOutOfBounds,  // Container out of bounds index

    BufferFull,  // Container/Local Buffer out of memory

    ValueOutOfDomain,  // Width/Precision replacement is negative
    ValueOutOfRange,  // value is not in the range of representable values of a type
};
}  // namespace ktl