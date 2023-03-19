#include <ktl/bitops.hpp>

using namespace ktl;

auto main() -> int {
    // NOLINTBEGIN(*-magic-numbers)
    static_assert(CreateMask<u64>(0, 2) == 0b11, "start pos can be zero");
    static_assert(CreateMask<u64>(1, 2) == 0b110, "start pos can be value < # bits");
    static_assert(CreateMask<u64>(0, 64) == 0xFFFFFFFFFFFFFFFF, "count can == # bits");
    static_assert(CreateMask<u64>(0, 63) == 0x7FFFFFFFFFFFFFFF, "set all bits except MSB");
    static_assert(CreateMask<u64>(1, 63) == 0xFFFFFFFFFFFFFFFE, "set all bits except LSB");
    static_assert(
        CreateMask<u64>(32, 63) == 0xFFFFFFFF00000000,
        "must support count + start_pos > # bits");

    static_assert(
        CreateMaskFor<u64>(1, 2, 3, 63) == 0x800000000000000E,
        "CreateMaskFor sanity test");

    static_assert(GetMaskedBits(0b1101U, CreateMask<u32>(2, 2)) == 0b1100);
    static_assert(GetMaskedBits(0b1101U, CreateMask<u32>(0, 1)) == 0b1);
    static_assert(GetMaskedBits(0xDEADBEEFC5C5C5C5, CreateMask<u64>(32, 32)) == 0xDEADBEEF00000000);

    static_assert(ClearMaskedBits(0b1111U, CreateMaskFor<u32>(0, 2)) == 0b1010);
    static_assert(SetMaskedBits(0b1011U, 0b0100U, CreateMaskFor<u32>(2, 3)) == 0b0111);

    static_assert(HasAnyInMask(0b0011U, CreateMaskFor<u32>(0, 1)));
    static_assert(HasAnyInMask(0b1010U, CreateMaskFor<u32>(0, 1)));
    static_assert(HasAllInMask(0b0011U, CreateMaskFor<u32>(0, 1)));
    static_assert(!HasAllInMask(0b1010U, CreateMaskFor<u32>(0, 1)));
    static_assert(!HasAllInMask(0b1000U, CreateMaskFor<u32>(0, 1)));
    static_assert(std::popcount(0b1010U) == 2);
    // NOLINTEND(*-magic-numbers)

    return 0;
}