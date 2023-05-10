#include <ranges>

#include <ktl/access.hpp>
#include <ktl/generator.hpp>
#include <ktl/recursion_to_iteration.hpp>
#include <ktl/recursive_generator.hpp>
#include <ktl/span.hpp>

#include "allocator.hpp"

using namespace ktl;
using namespace ktl::coro;
using namespace std::views;

template<coro::allocator_like CoroAllocator>
auto fibonacci(CoroAllocator /* coro_alloc */) -> generator<int, CoroAllocator> {
    int a = 0;
    int b = 1;
    while (true) {
        auto cur = a;
        co_yield cur;
        a = b;
        b += cur;
    }
}

template<coro::allocator_like CoroAllocator>
auto preorder(
    CoroAllocator coro_alloc,
    ktl::span<int> binary_tree,
    int mul,
    usize node = 0) noexcept -> recursive_generator<int, CoroAllocator> {
    if (node < binary_tree.size()) {
        co_yield binary_tree[node];
        co_yield binary_tree[node] * mul;
        co_yield preorder(coro_alloc, binary_tree, mul, 2 * node + 1);
        co_yield preorder(coro_alloc, binary_tree, mul, 2 * node + 2);
    }
}

template<coro::allocator_like CoroAllocator>
class Traverser: public enable_iteration<int, Error, CoroAllocator> {
  public:
    using base = enable_iteration<int, Error, CoroAllocator>;

    Traverser(ktl::span<int> binary_tree, int mul, CoroAllocator coro_alloc) :
        base {std::move(coro_alloc)},
        m_binary_tree {binary_tree},
        m_mul {mul} {}

    auto preorder() noexcept -> base::return_type {
        return preorder(0);
    }

  private:
    auto preorder(usize node) noexcept -> base::return_type {
        if (node < m_binary_tree.size()) {
            co_yield base::yield(m_binary_tree[node]);
            co_yield base::yield(m_binary_tree[node] * m_mul);
            co_yield base::yield(preorder(2 * node + 1));
            co_yield base::yield(preorder(2 * node + 2));
        }
    }

    ktl::span<int> m_binary_tree;
    int m_mul;
};

namespace {
auto generator_test() -> int {
    auto fib = fibonacci(Allocator<u8> {});

    check_(fib, "");
    check_(
        std::ranges::equal(take(unwrap(fib), 10), std::array {0, 1, 1, 2, 3, 5, 8, 13, 21, 34}),
        "");

    return 0;
}

auto recursive_generator_test() -> int {
    std::array binary_tree = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
    std::make_heap(binary_tree.begin(), binary_tree.end());

    constexpr int mul = 2;
    auto traversal = preorder(Allocator<u8> {}, binary_tree, mul);
    check_(traversal, "");
    check_(
        std::ranges::equal(unwrap(traversal), std::array {15, 15 * mul, 11, 11 * mul, 9,  9 * mul,
                                                          8,  8 * mul,  4,  4 * mul,  10, 10 * mul,
                                                          2,  2 * mul,  5,  5 * mul,  14, 14 * mul,
                                                          13, 13 * mul, 12, 12 * mul, 6,  6 * mul,
                                                          7,  7 * mul,  3,  3 * mul,  1,  1 * mul}),
        "");

    return 0;
}

auto convert_recursion_to_iteration() -> int {
    std::array binary_tree = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
    std::make_heap(binary_tree.begin(), binary_tree.end());

    constexpr int mul = 2;
    auto traversal = make_iterator(
        &Traverser<Allocator<u8>>::preorder,
        std::forward_as_tuple(binary_tree, mul, Allocator<u8> {}));
    check_(traversal, "");
    check_(
        std::ranges::equal(
            unwrap(traversal.iter()),
            std::array {15, 15 * mul, 11, 11 * mul, 9, 9 * mul, 8,  8 * mul,  4,  4 * mul,
                        10, 10 * mul, 2,  2 * mul,  5, 5 * mul, 14, 14 * mul, 13, 13 * mul,
                        12, 12 * mul, 6,  6 * mul,  7, 7 * mul, 3,  3 * mul,  1,  1 * mul}),
        "");

    return 0;
}
}  // namespace

auto main() -> int {
    if (auto ret = generator_test()) {
        return ret;
    }

    if (auto ret = recursive_generator_test()) {
        return ret;
    }

    if (auto ret = convert_recursion_to_iteration()) {
        return ret;
    }

    return 0;
}