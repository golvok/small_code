#include <cmath>
#include <cstdio>
#include <vector>
#include <iostream>
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <bitset>

using namespace std;

template<typename T, typename STREAM>
T get(STREAM& is) {
	T t;
	is >> t;
	return t;
}

struct print_printable { };

template<typename T, typename STREAM>
auto operator<<(STREAM& os, const T& t) -> decltype(static_cast<const print_printable*>(&t),os) {
	t.print(os);
	return os;
}

template<typename INDEX, typename INITIAL_RESULT, typename FUNC, typename COMB_FUNC, typename NEXT_FUNC>
auto combine(INDEX first, INDEX last, INITIAL_RESULT ir, FUNC f, COMB_FUNC combiner, NEXT_FUNC next) {
	decltype(f(first)) result = ir;
	for (auto i = first;; i = next(i)) {
		result = combiner(result,f(i));
		if (i == last) {
			break;
		}
	}
	return result;
}

template<typename INDEX, typename FUNC>
auto sum(INDEX first, INDEX last, FUNC f) {
	return combine(
		first,
		last,
		0,
		f,
		[&](auto lhs, auto rhs) {
			return lhs + rhs;
		},
		[&](auto i) {
			return i + 1;
		}
	);
}

template<typename INDEX, typename FUNC>
auto reverse_sum(INDEX first, INDEX last, FUNC f) {
	return combine(
		last,
		first,
		0,
		f,
		[&](auto lhs, auto rhs) {
			return lhs + rhs;
		},
		[&](auto i) {
			return i - 1;
		}
	);
}

template<typename INDEX, typename FUNC>
auto multiply(INDEX first, INDEX last, FUNC f) {
	return combine(
		first,
		last,
		1,
		f,
		[&](auto lhs, auto rhs) {
			return lhs * rhs;
		},
		[&](auto i) {
			return i + 1;
		}
	);
}

template<typename STREAM, typename T>
auto print_and_return(STREAM& os, const std::string& s, const T& expr) {
	os << s << ' ' << expr << '\n';
	return expr;
}

int main() {

	while (true) {
		auto num_squares = get<uint>(std::cin);
		if (num_squares == 0) {
			break;
		}

		vector<double> probs { 1 };
		for (uint i = 1; i < num_squares; ++i) {
			probs.push_back(get<double>(std::cin));
		}

		/*
			            N
			           ---    i
			           \   /-----        \
			            |  | | | Probs[j]|
			           /   \ | |         /
			           ---   j=0
			           i=0
			--------------------------------------------
			       N-1
			 /|    --- /                 i-1          \
			  |    \   |/            \ /-----        \|
			  | --  |  ||1 - Probs[i]| | | | Probs[j]||
			  |    /   |\            / \ | |         /|
			 ---   --- \                 j=0          /
			       i=1
		*/

		std::cout << std::llround(
			(
				reverse_sum(
					(uint)0,
					num_squares-1,
					[&](uint i) {
						return multiply(
							(uint)0,
							i,
							[&](uint j) {
								return probs[j];
							}
						);
					}
				)
			)/(
				1 - reverse_sum(
					(uint)1,
					num_squares-1,
					[&](uint i) {
						return (1 - probs[i]) * multiply(
							(uint)0,
							i-1,
							[&](uint j) {
								return probs[j];
							}
						);
					}
				)
			)
		) << '\n';

	}

	return 0;
}
