#pragma once

#include <algorithm>
#include <bit>
#include <concepts>
#include <iterator>
#include <limits>

#include <ktl/assert.hpp>
#include <ktl/int.hpp>

// A constexpr full-featured bitset
namespace ktl {
template<std::unsigned_integral WordType = usize>
class bitset {
  public:
    using word_type = WordType;

    class reference {
      public:
        constexpr ~reference() noexcept = default;
        constexpr reference(const reference&) noexcept = default;
        constexpr reference(reference&&) noexcept = default;
        constexpr auto operator=(const reference& x) noexcept -> reference& = default;
        constexpr auto operator=(reference&& x) noexcept -> reference& = default;

        constexpr auto operator=(bool v) noexcept -> reference& {
            auto mask = create_mask_for(m_bit);
            auto newval = create_mask_using(v, m_bit);

            m_word = assign_masked_range(mask, m_word, newval);
            return *this;
        }

        constexpr operator bool() const noexcept {
            return any_in_mask(m_word, create_mask_for(m_bit));
        }

        constexpr auto operator~() const noexcept -> bool {
            return !static_cast<bool>(*this);
        }

        constexpr auto flip() noexcept -> reference& {
            auto mask = create_mask_for(m_bit);
            auto inverted_val = (~m_word) & mask;

            m_word = assign_masked_range(mask, m_word, inverted_val);
            return *this;
        }

      private:
        friend class bitset;

        constexpr reference(word_type* word, usize bit) : m_word(word), m_bit(bit) {}

        word_type* m_word;
        word_type m_bit;
    };

    constexpr bitset() = default;
    constexpr bitset(word_type* bits, usize num_bits) : m_words(bits), m_nbits(num_bits) {}

    template<std::contiguous_iterator Iter>
    constexpr bitset(Iter first, Iter last) :
        m_words(std::addressof(*first)),
        m_nbits(std::distance(first, last) * NUM_BITS_PER_WORD) {}

    constexpr auto operator==(const bitset& rhs) const noexcept -> bool {
        if (m_nbits != rhs.m_nbits) {
            return false;
        }
        return std::equal(m_words, m_words + num_words(), rhs.m_words);
    }

  private:
    static constexpr uint NUM_BITS_PER_WORD = std::numeric_limits<WordType>::digits;
    static constexpr auto M_ONE = std::numeric_limits<WordType>::max();
    static constexpr WordType ONE = 1;
    static constexpr WordType ZERO = 0;

    static constexpr auto create_mask(uint start_pos, uint count) -> WordType {
        check_(
            start_pos < NUM_BITS_PER_WORD,
            "start bit position of mask cannot equal or exceed bit count");
        check_(count <= NUM_BITS_PER_WORD, "total bit count must not exceed number of bits");
        check_(count != 0, "bit count of mask cannot be zero");
        return (M_ONE >> (NUM_BITS_PER_WORD - count)) << start_pos;
    }

    template<std::unsigned_integral... Bits>
    static constexpr auto create_mask_for(Bits... bits) -> WordType {
        (check_(bits < NUM_BITS_PER_WORD, "mask bit position cannot equal or exceed bit count"),
         ...);
        return ((ONE << bits) | ...);
    }

    template<std::unsigned_integral... Bits>
    static constexpr auto create_mask_using(bool v, Bits... bits) -> WordType {
        (check_(bits < NUM_BITS_PER_WORD, "mask bit position cannot equal or exceed bit count"),
         ...);
        return ((static_cast<WordType>(v) << bits) | ...);
    }

    static constexpr auto assign_masked_range(WordType mask, WordType lhs, WordType rhs) {
        return (lhs & ~mask) | (rhs & mask);
    }

    static constexpr auto any_in_mask(WordType word, WordType mask) -> bool {
        return (word & mask) != 0;
    }

    static constexpr auto all_in_mask(WordType word, WordType mask) -> bool {
        return (word & mask) == mask;
    }

    [[nodiscard]] constexpr auto num_words() const noexcept -> usize {
        return ((m_nbits % NUM_BITS_PER_WORD) != 0 ? 1 : 0) + m_nbits / NUM_BITS_PER_WORD;
    }

    WordType* m_words = nullptr;
    usize m_nbits = 0;
};

template<std::contiguous_iterator Iter>
bitset(Iter first, Iter last) -> bitset<std::iter_value_t<Iter>>;

}  // namespace ktl
