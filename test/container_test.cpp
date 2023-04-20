#include <algorithm>
#include <random>

#include <ktl/string.hpp>
#include <ktl/test/platform.hpp>
#include <ktl/vector.hpp>

#include "allocator.hpp"

using ktl::usize;

template<typename T>
using vector = ktl::vector<T, ktl::Allocator<T>>;

using string = ktl::string<ktl::Allocator<char>>;

namespace {

template<usize MaxStrLen>
auto generate_random_string() -> string {
    ktl::string_view ALNUM = "abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ 0123456789";
    static std::mt19937_64 gen {std::random_device {}()};
    static std::uniform_int_distribution<usize> len_dist {1, MaxStrLen};
    static std::uniform_int_distribution<usize> ind_dist {0, ALNUM.length() - 1};

    const auto len = len_dist(gen);
    string str;

    check_(str.resize_uninitialized(len), "");
    for (usize i = 0; i < len; i++) {
        str[i] = ALNUM[ind_dist(gen)];
    }

    return str;
}

template<usize MaxVecLen, usize MaxStrLen>
auto generate_random_vector() -> vector<string> {
    static std::mt19937_64 gen {std::random_device {}()};
    static std::uniform_int_distribution<usize> len_dist {1, MaxVecLen};

    const auto len = len_dist(gen);
    vector<string> strs;

    check_(strs.reserve(len), "");
    for (usize i = 0; i < len; i++) {
        check_(strs.push_back(generate_random_string<MaxStrLen>()), "");
    }

    return strs;
}
}  // namespace

auto main() -> int {
    constexpr usize MaxStrLen = 64;
    constexpr usize MaxVecLen = 8;
    constexpr usize NumVecVecs = 256;

    vector<vector<string>> str_vecs;
    check_(str_vecs.reserve(NumVecVecs), "");

    for (usize i = 0; i < NumVecVecs; i++) {
        check_(str_vecs.push_back(generate_random_vector<MaxVecLen, MaxStrLen>()), "");
    }

    std::ranges::sort(str_vecs);

    return 0;
}