#pragma once

#include <algorithm>
#include <concepts>

#include <ktl/memory.hpp>

namespace ktl {
namespace detail {
    template<typename T>
    struct valid_type: std::true_type {};

    template<typename Container>
    concept erasable = requires(Container c) {
        { valid_type<typename Container::value_type>::value };
        { std::integral<typename Container::size_type> };
        { std::begin(c) } -> std::contiguous_iterator;
        { std::end(c) } -> std::contiguous_iterator;
        {
            std::same_as<std::iter_value_t<decltype(std::begin(c))>, typename Container::value_type>
        };
        { c.erase(std::begin(c), std::end(c)) };
    };
}  // namespace detail

template<detail::erasable Container, typename T>
    requires std::equality_comparable_with<T, typename Container::value_type>
constexpr auto erase(Container& c, const T& value) -> typename Container::size_type {
    auto end = std::end(c);
    auto it = std::remove(std::begin(c), end, value);
    auto r = std::distance(it, end);
    c.erase(it, end);
    return r;
}

template<detail::erasable Container, typename Pred>
    requires std::convertible_to<std::invoke_result_t<Pred, typename Container::value_type>, bool>
constexpr auto erase_if(Container& c, Pred pred) -> typename Container::size_type {
    auto end = std::end(c);
    auto it = std::remove_if(std::begin(c), end, pred);
    auto r = std::distance(it, end);
    c.erase(it, end);
    return r;
}

namespace detail {
    template<typename T>
    constexpr void sanitizer_annotate_contiguous_container(
        const T* beg,
        const T* end,
        const T* old_mid,
        const T* new_mid) noexcept {
        if constexpr (ASAN_ENABLED) {
            if (!std::is_constant_evaluated()) {
                __sanitizer_annotate_contiguous_container(beg, end, old_mid, new_mid);
            }
        }
    }

    template<typename ContiguousContainer>
    class RealAsanAnnotator {
      public:
        RealAsanAnnotator(const RealAsanAnnotator&) = delete;
        RealAsanAnnotator(RealAsanAnnotator&&) = delete;
        auto operator=(const RealAsanAnnotator&) -> RealAsanAnnotator& = delete;
        auto operator=(RealAsanAnnotator&&) -> RealAsanAnnotator& = delete;

        constexpr explicit RealAsanAnnotator(const ContiguousContainer& cont) noexcept :
            m_cont {&cont} {
            if (!std::is_constant_evaluated()) {
                auto [begin, end, end_cap] = cont.get_storage_for_asan_annotator();
                std::tie(m_beg, m_mid, m_end) = std::tie(begin, end, end_cap);
            }
        }

        constexpr ~RealAsanAnnotator() noexcept {
            if (!std::is_constant_evaluated()) {
                if (m_cont) {
                    auto [beg, new_mid, end] = m_cont->get_storage_for_asan_annotator();
                    check_(
                        beg == m_beg && end == m_end,
                        "reallocate happened without notification");
                    if (m_mid != new_mid) {
                        __sanitizer_annotate_contiguous_container(m_beg, m_end, m_mid, new_mid);
                    }
                }
            }
        }

        constexpr static void start_lifetime(ContiguousContainer& cont) noexcept {
            if (!std::is_constant_evaluated()) {
                auto [begin, end, end_cap] = cont.get_storage_for_asan_annotator();
                if (begin) {
                    __sanitizer_annotate_contiguous_container(begin, end_cap, begin, end);
                }
            }
        }

        constexpr void allow_full_access() noexcept {
            if (!std::is_constant_evaluated()) {
                if (m_beg) {
                    __sanitizer_annotate_contiguous_container(m_beg, m_end, m_mid, m_end);
                }
                m_mid = m_end;
            }
        }

        constexpr void deallocate() noexcept {
            if (!std::is_constant_evaluated()) {
                if (m_beg) {
                    __sanitizer_annotate_contiguous_container(m_beg, m_end, m_mid, m_beg);
                }
                m_beg = m_end = m_mid = nullptr;
            }
        }

        template<typename T = ContiguousContainer::value_type>
        constexpr void update() noexcept {
            if (!std::is_constant_evaluated()) {
                auto [begin, end, end_cap] = m_cont->get_storage_for_asan_annotator();
                std::tie(m_beg, m_mid, m_end) = std::tie(begin, end, end_cap);
                if (m_beg) {
                    __sanitizer_annotate_contiguous_container(m_beg, m_end, m_beg, m_mid);
                }
            }
        }

      private:
        const ContiguousContainer* m_cont;
        typename ContiguousContainer::const_pointer m_beg;
        typename ContiguousContainer::const_pointer m_mid;
        typename ContiguousContainer::const_pointer m_end;
    };

    template<typename ContiguousContainer>
    struct [[maybe_unused]] DummyAsanAnnotator {
        constexpr explicit DummyAsanAnnotator(
            [[maybe_unused]] const ContiguousContainer& /*cont*/) {}

        constexpr static void start_lifetime() noexcept {}
        constexpr void allow_full_access() noexcept {}
        constexpr void deallocate() noexcept {}
        constexpr void update() noexcept {}
    };
}  // namespace detail

template<typename ContiguousContainer>
using AsanAnnotator = std::conditional_t<
    ASAN_ENABLED,
    detail::RealAsanAnnotator<ContiguousContainer>,
    detail::DummyAsanAnnotator<ContiguousContainer>>;

namespace detail {
    // Very specific implementation, shared by both static_string and static_vector
    template<typename Container>
    constexpr void swap_contiguous_static_containers(Container& a, Container& b) noexcept {
        auto a_len = a.m_len;
        auto b_len = b.m_len;
        auto a_begin = a.get_storage().begin;
        auto b_begin = b.get_storage().begin;
        AsanAnnotator<Container> asan_annotator_a {a};
        AsanAnnotator<Container> asan_annotator_b {b};

        asan_annotator_a.allow_full_access();
        asan_annotator_b.allow_full_access();

        if (a_len > b_len) {
            std::swap_ranges(a_begin, a_begin + b_len, b_begin);
            uninitialized_move_n(a_begin + b_len, a_len - b_len, b_begin);
        } else {
            std::swap_ranges(a_begin, a_begin + a_len, b_begin);
            uninitialized_move_n(b_begin + a_len, b_len - a_len, a_begin);
        }

        using std::swap;
        swap(a.m_len, b.m_len);
    }
}  // namespace detail
}  // namespace ktl