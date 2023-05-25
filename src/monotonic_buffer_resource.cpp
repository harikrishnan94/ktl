#include <ktl/align.hpp>
#include <ktl/monotonic_buffer_resource.hpp>

namespace ktl::pmr {
auto monotonic_buffer_resource::do_allocate(usize bytes, usize alignment) noexcept
    -> expected<not_null<void*>, Error> {
    auto* ret = std::align(alignment, bytes, m_cur_buffer, m_avail);

    // If current buffer cannot hold the block, allocate new chunk for the block.
    if (ret == nullptr) [[unlikely]] {
        auto chunk_size = std::max(bytes + alignment, m_next_size);
        auto [chunk, mem] = try$(allocate_chunk(*m_upstream, chunk_size, alignment));

        m_avail = m_next_size;
        m_cur_buffer = mem;
        // If requested size is greater than next chunk size, allocate separate chunk for that block
        // alone.
        m_next_size = chunk_size < m_next_size
            ? (m_next_size * m_growth_ratio.num) / m_growth_ratio.den
            : m_next_size;
        chunk->next = m_chunks_head;
        m_chunks_head = chunk;

        return do_allocate(bytes, alignment);
    }

    return ret;
}

auto monotonic_buffer_resource::allocate_chunk(
    memory_resource& res,
    usize size,
    usize align) noexcept -> expected<std::pair<not_null<Chunk*>, not_null<void*>>, Error> {
    size = AlignUp(size, alignof(Chunk));

    auto mem = try$(res.allocate(size + sizeof(Chunk), align));
    auto* cnk = std::bit_cast<Chunk*>(static_cast<char*>(mem.get()) + size);
    std::construct_at(cnk, Chunk {.req_size = size, .alignment = align, .next = nullptr});
    return std::make_pair(not_null {cnk}, mem);
}

void monotonic_buffer_resource::release_buffers() noexcept {
    while (m_chunks_head != nullptr) {
        void* chunk = std::bit_cast<char*>(m_chunks_head) - m_chunks_head->req_size;
        auto* next = m_chunks_head->next;
        m_upstream->deallocate(
            chunk,
            m_chunks_head->req_size + sizeof(Chunk),
            m_chunks_head->alignment);
        m_chunks_head = next;
    }
}
}  // namespace ktl::pmr