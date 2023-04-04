#pragma once

#include <atomic>

namespace ktl {
template<typename BP>
concept backoff_policy = std::is_default_constructible_v<BP> && requires(BP bp) {
    { bp.yield() };
};

struct default_backoff_policy {
    constexpr void yield() const noexcept {}
};

template<backoff_policy BackoffPolicy = default_backoff_policy>
class spin_mutex {
  public:
    spin_mutex() = default;

    [[nodiscard]] auto is_locked() const noexcept {
        return m_state.load(std::memory_order::acquire) == Unlocked;
    }

    [[nodiscard]] auto try_lock() noexcept -> bool {
        auto state = m_state.load(std::memory_order::acquire);
        return state == Unlocked
            && m_state.compare_exchange_weak(state, Locked, std::memory_order::acq_rel);
    }

    void lock() noexcept {
        BackoffPolicy backoff;
        while (true) {
            auto state = m_state.load(std::memory_order::acquire);
            if (state == Unlocked
                && m_state.compare_exchange_weak(state, Locked, std::memory_order::acq_rel)) {
                return;
            }

            backoff.yield();
        }
    }

    void unlock() noexcept {
        m_state.store(Unlocked, std::memory_order::release);
    }

  private:
    enum state_t { Locked, Unlocked };
    std::atomic<state_t> m_state = Unlocked;
};
}  // namespace ktl