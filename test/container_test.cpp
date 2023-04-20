#include <algorithm>
#include <random>

#include <ktl/static_string.hpp>
#include <ktl/static_vector.hpp>
#include <ktl/string.hpp>
#include <ktl/test/platform.hpp>
#include <ktl/vector.hpp>

#include "allocator.hpp"

using ktl::usize;

template<typename T>
using vector = ktl::vector<T, ktl::Allocator<T>>;

template<typename T, auto>
using vector_dyn = vector<T>;

using string = ktl::string<ktl::Allocator<char>>;

namespace {
template<typename StringT, usize MaxStrLen>
auto generate_random_string() -> StringT {
    static std::mt19937_64 gen {std::random_device {}()};
    auto rand_str = [] {
        ktl::string_view ALNUM = "abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ 0123456789";
        static std::uniform_int_distribution<usize> len_dist {1, MaxStrLen / 2};
        static std::uniform_int_distribution<usize> ind_dist {0, ALNUM.length() - 1};

        const auto len = len_dist(gen);
        StringT str;

        check_(str.resize_uninitialized(len), "");
        for (usize i = 0; i < len; i++) {
            str[i] = ALNUM[ind_dist(gen)];
        }
        return str;
    };

    auto str = rand_str();

    // Insert random string into original string.
    {
        std::uniform_int_distribution<usize> pos_dist {0, static_cast<usize>(str.length() - 1)};
        auto pos = pos_dist(gen);
        std::uniform_int_distribution<usize> count_dist {1, str.length() - pos};
        auto count = count_dist(gen);
        auto str2 = rand_str();

        check_(str.replace(pos, count, str2), "");
    }

    return str;
}

template<template<typename, auto> typename Vec, typename StringT, usize MaxVecLen, usize MaxStrLen>
auto generate_random_vector() -> Vec<StringT, MaxVecLen> {
    static std::mt19937_64 gen {std::random_device {}()};
    static std::uniform_int_distribution<usize> len_dist {1, MaxVecLen};

    const auto len = len_dist(gen);
    Vec<StringT, MaxVecLen> strs;

    if constexpr (requires { strs.reserve(len); }) {
        check_(strs.reserve(len), "");
    }

    for (usize i = 0; i < len; i++) {
        check_(strs.push_back(generate_random_string<StringT, MaxStrLen>()), "");
    }

    std::ranges::sort(strs);
    check_(std::ranges::is_sorted(strs), "must be sorted");

    return strs;
}

template<
    template<typename, auto>
    typename Vec,
    typename StringT,
    usize MaxStrLen,
    usize MaxVecLen,
    usize NumVecVecs>
void test() {
    vector<Vec<StringT, MaxVecLen>> str_vecs;
    check_(str_vecs.reserve(NumVecVecs), "");

    for (usize i = 0; i < NumVecVecs; i++) {
        check_(
            str_vecs.insert(
                str_vecs.begin(),
                generate_random_vector<Vec, StringT, MaxVecLen, MaxStrLen>()),
            "");
    }

    std::ranges::sort(str_vecs);
    check_(std::ranges::is_sorted(str_vecs), "must be sorted");

    check_(clone(str_vecs), "");

    while (!str_vecs.empty()) {
        str_vecs.erase(str_vecs.begin());
    }
};
}  // namespace

auto main() -> int {
    constexpr usize MaxStrLen = 64;
    constexpr usize MaxVecLen = 32;
    constexpr usize NumVecVecs = 256;

    test<vector_dyn, string, MaxStrLen, MaxVecLen, NumVecVecs>();
    test<vector_dyn, ktl::static_string<MaxStrLen + 1>, MaxStrLen, MaxVecLen, NumVecVecs>();
    test<ktl::static_vector, ktl::static_string<MaxStrLen + 1>, MaxStrLen, MaxVecLen, NumVecVecs>();
    test<ktl::static_vector, string, MaxStrLen, MaxVecLen, NumVecVecs>();

    return 0;
}