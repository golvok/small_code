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

		vector<float> probs { 1 };
		for (uint i = 1; i < num_squares; ++i) {
			probs.push_back(get<float>(std::cin));
		}

		// std::cout << "probs:";
		// std::for_each(probs.begin() + 1, probs.end(), [&](float prob){
			// std::cout << prob << ' ';
		// });
		// std::cout << '\n';

		float numerator = 1;
		// std::cout << "(1";
		for (uint i = 1; i < num_squares; ++i) {
			// std::cout << " + " << probs[i];
			float product_term = probs[i];
			for (uint j = i-1; j != 0; --j) {
				// std::cout << '*' << probs[j];
				product_term *= probs[j];
			}
			numerator += product_term;
		}
		// std::cout << ")/(1 -";

		float denominator = 1;
		for (uint i = 1; i < num_squares; ++i) {
			// std::cout << " ( (1-" << probs[i] << ")";
			float sum_term = 1-probs[i];
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

		float expected = numerator/denominator;
		std::cout << std::llround(expected) << '\n';
		// std::cout << expected << '\n';

	}

	return 0;
}
