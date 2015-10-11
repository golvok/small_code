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


		struct seq_prob : print_printable {
			float prob;
			uint square;
			seq_prob(float prob, uint square) : prob(prob), square(square) { }
			void print(std::ostream& os) const {
				os << "{p=" << prob << ",s=" << square << '}';
			}
		};

		const float p_of_1_to_N = [&]() -> float {
			float p = 1;
			std::for_each(probs.begin() + 1, probs.end(), [&](float prob){
				p *= prob;
			});
			return p;
		}();

		// std::cout << "p_of_1_to_N = " << p_of_1_to_N << '\n';

		double expected = ( p_of_1_to_N ) * ( num_squares );
		// std::cout << "expcted is now = " << expected << std::endl;

		uint num_moves = 0;
		while (true) {
			double next_expected = 0;
			std::cout << "\nlevel = " << num_moves << '\n';

			unsigned long long int bin_sequence = 0;

			while (true) {
				double elem_prob = 1;
				uint seqence_num = 0;
				// std::cout << "binary: " << std::bitset<sizeof(bin_sequence)*8>(bin_sequence) << '\n';
				for (size_t i = 0; i <= num_moves; ++i) {
					if ((bin_sequence & (1ULL << (num_moves - i)))) {
						seqence_num += 1;
						if (seqence_num == num_squares) {
							// std::cout << "X ";
							elem_prob = 0;
							bin_sequence = bin_sequence | ((1ULL << (num_moves - i))-1);
							// std::cout << "binary skips to: " << std::bitset<sizeof(bin_sequence)*8>(bin_sequence) << '\n';
							break;
						} else {
							elem_prob *= probs[seqence_num-1];
							// std::cout << '(' << probs[seqence_num-1] << ')';
						}
					} else {
						if (i != 0) {
							elem_prob *= (1-probs[seqence_num]);
							// std::cout << '(' << (1-probs[seqence_num]) << ')';
						} else {
							// std::cout << '(' << elem_prob << ')';
						}
						seqence_num = 1; // go back to one
					}
					// std::cout << seqence_num << ' ';
				}
				if (elem_prob != 0) {
					// std::cout << '(' << (1-probs[seqence_num]) << ") 1 ";
					elem_prob *= 1-probs[seqence_num];

					// std::cout << ": " << elem_prob;
					elem_prob *= p_of_1_to_N;
					double elem_expected = ( elem_prob ) * (num_moves + num_squares);
					// std::cout << " : " << elem_prob << " : " << elem_expected << '\n';

					next_expected += elem_expected;
				} else {
					// std::cout << '\n';
				}

				bin_sequence += 1;
				// done if incremented first one isn't going to be a 1
				if (bin_sequence & (1ULL << (num_moves))) {
					break;
				}
			}

			std::cout << "next_expected = " << next_expected << '\n';

			expected += next_expected;
			num_moves += 1;

			std::cout << "expcted is now = " << expected << std::endl; // '\n';

			if (next_expected < 0.01) {
				break;
			}
		}

		// std::cout << std::lround(expected) << '\n';
		std::cout << expected << '\n';

	}

	return 0;
}
