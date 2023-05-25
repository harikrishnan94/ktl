#pragma once

#include <ktl/memory_resource.hpp>
#include <ktl/ratio.hpp>

namespace ktl::pmr {
// The class `ktl::pmr::monotonic_buffer_resource` is a special-purpose memory resource class that
// releases the allocated memory only when the resource is destroyed. It is intended for very fast
// memory allocations in situations where memory is used to build up a few objects and then is
// released all at once.
// `monotonic_buffer_resource` can be constructed with an initial buffer. If there is no initial
// buffer, or if the buffer is exhausted, additional buffers are obtained from an upstream memory
// resource supplied at construction. The size of buffers obtained follows a geometric progression.
// `monotonic_buffer_resource` is `not thread-safe`.
class monotonic_buffer_resource final: public memory_resource {
  public:
    using growth_ratio_type = ratio<u8>;

    monotonic_buffer_resource(
        not_null<void*> initial_buf,
        usize initial_size,
        growth_ratio_type growth_ratio = growth_ratio_type {2, 1},
        not_null<memory_resource*> upstream = &null_memory_resource()) noexcept :
        memory_resource {type_id_of<monotonic_buffer_resource>()},
        m_upstream {upstream},
        m_growth_ratio {growth_ratio},
        m_initial_buffer {initial_buf},
        m_initial_size {initial_size} {}

    explicit monotonic_buffer_resource(
        usize initial_size,
        growth_ratio_type growth_ratio = growth_ratio_type {2, 1},
        not_null<memory_resource*> upstream = &null_memory_resource()) noexcept :
        memory_resource {type_id_of<monotonic_buffer_resource>()},
        m_upstream {upstream},
        m_growth_ratio {growth_ratio},
        m_initial_size {initial_size} {}

    monotonic_buffer_resource(
        growth_ratio_type growth_ratio,
        not_null<memory_resource*> upstream) noexcept :
        memory_resource {type_id_of<monotonic_buffer_resource>()},
        m_upstream {upstream},
        m_growth_ratio {growth_ratio} {}

    monotonic_buffer_resource(const monotonic_buffer_resource&) = delete;
    auto operator=(const monotonic_buffer_resource&) -> monotonic_buffer_resource& = delete;

    ~monotonic_buffer_resource() noexcept override {
        if (m_chunks_head != nullptr) {
            release();
        }
    }

    monotonic_buffer_resource(monotonic_buffer_resource&& o) noexcept :
        memory_resource {std::move(o)},
        m_upstream {o.m_upstream},
        m_growth_ratio {o.m_growth_ratio},
        m_initial_buffer {std::exchange(o.m_initial_buffer, nullptr)},
        m_initial_size {std::exchange(o.m_initial_size, 0)},
        m_next_size {std::exchange(o.m_next_size, 0)},
        m_avail {std::exchange(o.m_avail, 0)},
        m_cur_buffer {std::exchange(o.m_cur_buffer, nullptr)},
        m_chunks_head {std::exchange(o.m_chunks_head, nullptr)} {}

    auto operator=(monotonic_buffer_resource&& o) noexcept -> monotonic_buffer_resource& {
        using std::swap;

        memory_resource::operator=(std::move(o));
        swap(m_upstream, o.m_upstream);
        swap(m_growth_ratio, o.m_growth_ratio);
        swap(m_initial_buffer, o.m_initial_buffer);
        swap(m_initial_size, o.m_initial_size);
        swap(m_next_size, o.m_next_size);
        swap(m_avail, o.m_avail);
        swap(m_cur_buffer, o.m_cur_buffer);
        swap(m_chunks_head, o.m_chunks_head);

        return *this;
    }

    // Returns a pointer to the upstream memory resource. This is the same value as the upstream
    // argument passed to the constructor of this object.
    [[nodiscard]] constexpr auto upstream_resource() const noexcept -> not_null<memory_resource*> {
        return m_upstream;
    }

    // Releases all allocated memory by calling the deallocate function on the upstream memory
    // resource as necessary. Resets current buffer and next buffer size to their initial values at
    // construction.
    // Memory is released back to the upstream resource even if deallocate has not been called for
    // some of the allocated blocks.
    void release() noexcept {
        // Reset the state to right after construction.
        m_cur_buffer = m_initial_buffer;
        m_next_size = m_initial_size;
        m_avail = m_initial_size;

        if (m_chunks_head != nullptr) {
            release_buffers();
        }
    }

  private:
    // Chunk holds multiple blocks of memory (atleast 1).
    struct Chunk {
        usize req_size;  // Requested size of the chunk.
        usize alignment;
        Chunk* next;
        u8 mem[];  // Storage follows. // NOLINT(*-avoid-c-arrays)
    };
    // Allocates storage.
    // If the current buffer has sufficient unused space to fit a block with the specified size and
    // alignment, allocates the return block from the current buffer. Otherwise, this function
    // allocates a new buffer by calling upstream_resource()->allocate(n, m), where n is not less
    // than the greater of bytes and the next buffer size and m is not less than alignment. It sets
    // the new buffer as the current buffer, increases the next buffer size by an
    // implementation-defined growth factor (which is not necessarily integral), and then allocates
    // the return block from the newly allocated buffer.
    //
    // Return value:
    // ============
    // A pointer to allocated storage of at least bytes bytes in size, aligned to the specified
    // alignment if such alignment is supported, and to alignof(std::max_align_t) otherwise.
    //
    // Unexpected Return:
    // =================
    // Nothing unless calling allocate() on the upstream memory resource returns an error.
    auto do_allocate(usize bytes, usize alignment) noexcept
        -> expected<not_null<void*>, Error> override;

    // Compare *this with other for identity - memory allocated using a monotonic_buffer_resource
    // can only be deallocated using that same resource.
    //
    // `Return value`
    // ===========
    // this == &other
    [[nodiscard]] auto do_is_equal(const memory_resource& other) const noexcept -> bool override {
        return this == &other;
    }

    // This function has no effect. Memory used by a monotonic_buffer_resource, as its name
    // indicates, increases monotonically until the resource is destroyed.
    void do_deallocate(not_null<void*> /* p */, usize /* bytes */, usize /* alignment */) noexcept
        override {}

    void release_buffers() noexcept;

    // Allocates a new chunk of size alteast `size` and of alignment not less than `align` from the
    // `res`. Return error if chunk cannot be allocated.
    [[nodiscard]] static auto allocate_chunk(memory_resource& res, usize size, usize align) noexcept
        -> expected<std::pair<not_null<Chunk*>, not_null<void*>>, Error>;

    static constexpr usize InitialBufferSize = 1024;

    // Initial values obtained from constructor
    not_null<memory_resource*> m_upstream;
    growth_ratio_type m_growth_ratio;
    void* m_initial_buffer = nullptr;
    usize m_initial_size = 0;

    // Points to either `m_initial_buffer` or `chunk head`
    usize m_next_size = InitialBufferSize;
    usize m_avail = m_initial_size;
    void* m_cur_buffer = m_initial_buffer;

    Chunk* m_chunks_head = nullptr;
};
}  // namespace ktl::pmr