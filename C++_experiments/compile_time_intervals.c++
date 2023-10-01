#include <cmath>

template<typename Lhs, typename Rhs>
concept Overlapping = requires {
    requires (Rhs::low <= Lhs::low && Lhs::low <= Rhs::high) || (Rhs::low <= Lhs::high && Lhs::high <= Rhs::high);
};

template<typename T, T tLow, T tHigh>
struct Pair {
    using Type = T;
    static constexpr T low = tLow;
    static constexpr T high = tHigh;
    constexpr static bool contains(T const& t) { return low <= t && t < high; }
};

template<auto... tRest>
struct Union;

template<auto tHead, auto... tRest>
struct Union<tHead, tRest...> {
    static constexpr auto head = tHead;
    static constexpr auto next = Union<tRest...>{};
    constexpr static bool contains(auto const& t) { return head.contains(t) || next.contains(t); }
};

template<auto tHead>
struct Union<tHead> {
    static constexpr auto head = tHead;
    constexpr static bool contains(auto const& t) { return head.contains(t); }
};

template<typename LhsPair, typename RhsPair>
constexpr auto operator&(LhsPair const&, RhsPair const&)
    -> Pair<typename LhsPair::Type, std::min(LhsPair::low, RhsPair::low), std::max(LhsPair::high, RhsPair::high)>
    requires Overlapping<LhsPair, RhsPair>
{ return {}; }

template<typename LhsPair, typename RhsPair>
constexpr auto operator&(LhsPair const&, RhsPair const&)
    -> Union<LhsPair{}, RhsPair{}>
{ return {}; }

template<int l, int h>
constexpr auto ival = Pair<int, l, h>{};

template<auto T>
struct Debug;

static_assert((ival<1, 2> & ival<2, 3>).low == 1);
static_assert(decltype(ival<1, 2> & ival<2, 3>)::low == 1);
static_assert((ival<1, 2> & ival<3, 4>).head.low == 1);
static_assert((ival<1, 2> & ival<3, 4>).next.head.high);
static_assert((ival<1, 2> & ival<3, 4>).contains(1));
static_assert((ival<1, 2> & ival<3, 4>).contains(3));
static_assert(not (ival<1, 2> & ival<3, 4>).contains(4));
static_assert(not (ival<1, 2> & ival<3, 4>).contains(2));

int main() {}