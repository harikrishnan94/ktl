#pragma once

namespace ktl {
template<class... Ts>
struct overloaded: Ts... {
    using Ts::operator()...;
};
}  // namespace ktl