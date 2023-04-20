#pragma once

#include "assert.hpp"
#include "containers.hpp"
#include "int.hpp"

namespace ktl {
constexpr auto at(detail::random_access_container auto&& container, usize index) noexcept -> auto& {
    check_(index < container.size(), "out of bounds indexing");
    return container[index];
}

template<typename T, usize N>
constexpr auto at(T (&arr)[N], usize index) noexcept -> auto& {
    check_(index < N, "out of bounds indexing");
    return arr[index];
}

template<typename Optional>
constexpr auto unwrap(Optional&& optional) -> auto& {
    check_(std::forward<Optional>(optional).has_value(), "empty optional unwrap");
    return *std::forward<Optional>(optional);
}
}  // namespace ktl