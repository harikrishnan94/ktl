#pragma once

#include "containers.hpp"
#include "ktl/int.hpp"

namespace ktl {
constexpr auto
at(detail::random_access_container auto&& container, std::integral auto index) noexcept -> auto& {
    assert(index < container.size());
    return container[index];
}

template<typename Optional>
constexpr auto unwrap(Optional&& optional) -> auto& {
    assert(std::forward<Optional>(optional).has_value());
    return *std::forward<Optional>(optional);
}
}  // namespace ktl