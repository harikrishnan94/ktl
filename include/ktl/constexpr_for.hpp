#pragma once

#include <concepts>
#include <functional>
#include <type_traits>

namespace ktl {
namespace detail {
    template<typename T>
    struct is_integer_sequence;

    template<std::integral I, I... ints>
    struct is_integer_sequence<std::integer_sequence<I, ints...>> {
        static constexpr bool value = true;
    };

    template<typename IntegerSequence>
    concept integer_sequence_like = is_integer_sequence<IntegerSequence>::value;

    template<typename LoopBody, typename T, T... ints>
    constexpr void
    constexpr_for(std::integer_sequence<T, ints...> /* int_seq */, LoopBody&& body) noexcept {
        (std::invoke(std::forward<LoopBody>(body), std::integral_constant<T, ints> {}), ...);
    }

}  // namespace detail

// Break `constexpr_for` iteration
struct break_t {};

// `constexpr_for` iterates over the `LoopBody` from `start` index to `End` index.
// `LoopBody` must accept std::integral_constant<int, Ind> as argument to obtain the index value
// To break the iteratio early, an instance of `break_t`{} can be returned.
template<int Start, int End, typename LoopBody>
constexpr void constexpr_for(LoopBody&& body) noexcept {
    if constexpr (Start < End) {
        using Index = std::integral_constant<int, Start>;
        constexpr auto Break = std::same_as<std::invoke_result_t<LoopBody, Index>, break_t>;

        std::invoke(std::forward<LoopBody>(body), Index {});
        if constexpr (!Break) {
            constexpr_for<Start + 1, End>(std::forward<LoopBody>(body));
        }
    }
}

// `constexpr_for` over an integer sequence
template<typename LoopBody>
constexpr void constexpr_for(detail::integer_sequence_like auto int_seq, LoopBody&& body) noexcept {
    detail::constexpr_for(int_seq, std::forward<LoopBody>(body));
}

// NOLINTNEXTLINE(*-macro-usage)
#define CONSTEXPR_FOR_SEQ(body, ...) \
    ::ktl::constexpr_for(std::integer_sequence<std::ptrdiff_t, __VA_ARGS__> {}, (body))

// NOLINTBEGIN(*-magic-numbers)
static_assert([] {
    int sum = 0;
    constexpr_for<1, 11>([&sum](auto i) { sum += i.value; });
    return sum;
}() == 55);

static_assert([] {
    int sum = 0;
    constexpr_for<1, 11>([&sum](auto i) {
        sum += i.value;
        if constexpr (i.value == 5) {
            return break_t {};
        }
    });
    return sum;
}() == 15);

static_assert([] {
    int sum = 0;
    CONSTEXPR_FOR_SEQ(
        [&sum](auto i) {
            sum += i.value;
            if constexpr (i.value == 5) {
                return break_t {};
            }
        },
        1,
        3,
        5);
    return sum;
}() == 9);

// NOLINTEND(*-magic-numbers)
}  // namespace ktl
