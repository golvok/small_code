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

int main() {

	while (true) {
		// std::cout << "ready\n";
		auto num_squares = get<uint>(std::cin);
		if (num_squares == 0) {
			// std::cout << "no squares\n";
			break;
		}

		// std::cout << "num_squares = " << num_squares << '\n';

		vector<double> probs { 1 };
		for (uint i = 1; i < num_squares; ++i) {
			probs.push_back(get<double>(std::cin));
		}

		// std::cout << "probs:";
		// std::for_each(probs.begin() + 1, probs.end(), [&](double prob){
			// std::cout << prob << ' ';
		// });
		// std::cout << '\n';


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
			       i=0
		*/

		double numerator = 1;
		// std::cout << "(1";
		for (uint i = 1; i < num_squares; ++i) {
			// std::cout << " + " << probs[i];
			double product_term = probs[i];
			for (uint j = i-1; j != 0; --j) {
				// std::cout << '*' << probs[j];
				product_term *= probs[j];
			}
			numerator += product_term;
		}
		// std::cout << ")/(1 -";

		double denominator = 1;
		for (uint i = 1; i < num_squares; ++i) {
			// std::cout << " ( (1-" << probs[i] << ")";
			double sum_term = 1-probs[i];
			for (uint j = 1; j < i; ++j) {
				// std::cout << "*" << probs[j];
				sum_term *= probs[j];
			}
			if (i != num_squares-1) {
				// std::cout << " ) -";
			}
			denominator -= sum_term;
		}
		// std::cout << " ) ) + 1 \n";

		double expected = numerator/denominator;
		std::cout << std::llround(expected) << '\n';
		// std::cout << expected << '\n';

	}

	return 0;
}
