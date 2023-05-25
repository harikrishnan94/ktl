#pragma once

#include "ktl/not_null.hpp"
namespace ktl {
namespace detail {
    struct type_id_t {};

    // An unique object for each type.
    template<typename T>
    inline constexpr type_id_t type_id = {};
}  // namespace detail

// Represents an ID for a type.
// Only operation on type_id can be the equality comparison.
// `type_id` is a trivially copyable type.
class type_id {
  public:
    type_id(std::nullptr_t) = delete;

    friend auto operator==(const type_id&, const type_id&) -> bool = default;

  private:
    template<typename T>
    friend auto type_id_of() -> type_id;

    explicit constexpr type_id(const detail::type_id_t& id) : m_id {&id} {}

    not_null<const detail::type_id_t*> m_id;
};

// Returns the runtime unique type_id of the given type `T`.
// It is guanteed that for every type in the program `type_id_of` returns an unique value same
// for every invocation.
template<typename T>
auto type_id_of() -> type_id {
    return type_id {detail::type_id<T>};
}
}  // namespace ktl