#include <vector>
#include <cstdint>
#include <iostream>
#include "hashidsxx/hashids.h"

using namespace std;
using vec64 = vector<uint64_t>;
using hashidsxx::Hashids;

struct U64StartEnd {
    uint64_t *start;
    uint64_t *end;
};

/* note: this assumes that T is contiguous. */
template <typename T>
inline U64StartEnd bounds_of(T &t) {
    return {t.data(), t.data() + t.size()};
}

const vec64 nums = {3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9};

extern "C" uint64_t round_trip_bench(uint64_t rounds) {
    Hashids hashids("a salt.", 25, DEFAULT_ALPHABET);
    for (unsigned _ = 0; _ < rounds; _++) {
        hashids.decode(hashids.encode(nums.begin(), nums.end()));
    }
    return rounds;
}

extern "C" uint64_t control(uint64_t) {
    return 4;  /* chosen by coin flip */
}
