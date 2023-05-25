#pragma once

#include <ktl/error.hpp>
#include <ktl/expected.hpp>
#include <ktl/int.hpp>
#include <ktl/typeid.hpp>

namespace ktl::pmr {
// The class ktl::pmr::memory_resource is an abstract interface to an unbounded set of classes
// encapsulating memory resources.
class memory_resource {
  public:
    // Constructs `memory_resource` base class for a derived type having id `typid`.
    explicit constexpr memory_resource(type_id typid) : m_typeid {typid} {}

    memory_resource(const memory_resource&) = default;
    memory_resource(memory_resource&&) = default;
    auto operator=(const memory_resource&) -> memory_resource& = default;
    auto operator=(memory_resource&&) -> memory_resource& = default;

    virtual ~memory_resource() = default;

    // Allocates storage with a size of at least bytes bytes, aligned to the specified alignment.
    // see `do_allocate` for more details
    [[nodiscard]] auto allocate(usize bytes, usize alignment = alignof(std::max_align_t)) noexcept
        -> expected<not_null<void*>, Error> {
        return do_allocate(bytes, alignment);
    }

    // Deallocates the storage pointed to by p. p shall have been returned by a prior call to
    // allocate(bytes, alignment) on a memory_resource that compares equal to *this, and the storage
    // it points to shall not yet have been deallocated.
    // see `do_deallocate` for more details
    void deallocate(
        void* p,
        std::size_t bytes,
        std::size_t alignment = alignof(std::max_align_t)) noexcept {
        do_deallocate(p, bytes, alignment);
    }

    // Compares `*this` for equality with other. Two memory_resources compare equal if and only if
    // memory allocated from one memory_resource can be deallocated from the other and vice versa.
    // see `do_is_equal` for more details
    [[nodiscard]] auto is_equal(const memory_resource& other) const noexcept -> bool {
        return do_is_equal(other);
    }

    // Compares the memory_resources a and b for equality. Two memory_resources compare equal if and
    // only if memory allocated from one memory_resource can be deallocated from the other and vice
    // versa.
    friend constexpr auto operator==(const memory_resource& a, const memory_resource& b) noexcept
        -> bool {
        return &a == &b || a.is_equal(b);
    }

    // Returns the `type_id` of the derived type.
    [[nodiscard]] constexpr auto get_type_id() const noexcept -> type_id {
        return m_typeid;
    }

  private:
    // Allocates storage with a size of at least bytes bytes, aligned to the specified alignment.
    // alignment shall be a power of two.
    // Returns an error if storage of the requested size and alignment cannot be obtained.
    virtual auto do_allocate(usize bytes, usize alignment) noexcept
        -> expected<not_null<void*>, Error> = 0;

    // Deallocates the storage pointed to by p.
    // p must have been returned by a prior call to allocate(bytes, alignment) on a memory_resource
    // that compares equal to `*this`, and the storage it points to must not yet have been
    // deallocated, otherwise the behavior is undefined.
    virtual void do_deallocate(not_null<void*> p, usize bytes, usize alignment) noexcept = 0;

    // Compares `*this` for equality with other.
    // Two memory_resources compare equal if and only if memory allocated from one memory_resource
    // can be deallocated from the other and vice versa.
    // NOTE: The most-derived type of other may not match the most derived type of *this. A derived
    // class implementation therefore must typically check whether the most derived types of `*this`
    // and other match using `type_id` comparison, and immediately return false if the cast fails.
    [[nodiscard]] virtual auto do_is_equal(const memory_resource& other) const noexcept -> bool = 0;

    type_id m_typeid;
};

// Returns a pointer p to a static storage duration object of a type derived from
// `ktl::pmr::memory_resource`, with the following properties:
// its `allocate()` function always throws `OutOfMemory`;
// its `deallocate()` function has no effect;
// for any memory_resource r, p->is_equal(r) returns &r == p.
// The same value is returned every time this function is called.
inline auto null_memory_resource() noexcept -> memory_resource& {
    class null_memory_resource_t final: public memory_resource {
      public:
        null_memory_resource_t() noexcept : memory_resource(type_id_of<null_memory_resource_t>()) {}

      private:
        auto do_allocate(usize /* bytes */, usize /* alignment */) noexcept
            -> expected<not_null<void*>, Error> override {
            Throw(error::OutOfMemory);
        }

        void do_deallocate(
            not_null<void*> /* p */,
            usize /* bytes */,
            usize /* alignment */) noexcept override {}

        [[nodiscard]] auto do_is_equal(const memory_resource& other) const noexcept
            -> bool override {
            return other.get_type_id() == get_type_id();
        }
    } static null_memory_resource;  // NOLINT(*-dynamic-static-initializers)

    return null_memory_resource;
}

template<typename T>
class polymorphic_allocator;
}  // namespace ktl::pmr