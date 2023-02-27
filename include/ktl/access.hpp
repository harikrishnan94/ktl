#pragma once

#include "assert.hpp"
#include "containers.hpp"
#include "int.hpp"

namespace ktl {
constexpr auto
at(detail::random_access_container auto&& container, std::integral auto index) noexcept -> auto& {
    check_(index < container.size(), "out of bounds indexing");
    return container[index];
}

template<typename Optional>
constexpr auto unwrap(Optional&& optional) -> auto& {
    check_(std::forward<Optional>(optional).has_value(), "empty optional unwrap");
    return *std::forward<Optional>(optional);
}
}  // namespace ktl