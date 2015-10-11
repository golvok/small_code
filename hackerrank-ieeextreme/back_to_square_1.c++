#include <cmath>
#include <cstdio>
#include <vector>
#include <iostream>
#include <algorithm>
#include <cmath>

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
		

		double expected = 1; // takes one move to "get to" start

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

		uint num_moves = 0;

		std::vector<seq_prob> seq_probs { seq_prob( 1, 0 ) };

		while (true) {
			double next_expected = 0;
			std::vector<seq_prob> next_seq_probs;
			next_seq_probs.reserve(seq_probs.size()*2);
			// std::cout << "\nlevel = " << num_moves << '\n';

			for (auto elem : seq_probs) {

				next_seq_probs.push_back( seq_prob( elem.prob * (1-probs[elem.square]), 1 ) );
				// std::cout << "added " << next_seq_probs.back() << '\n';

				float elem_expected = ( elem.prob * p_of_1_to_N ) * (num_moves + num_squares);
				// std::cout << "elem_expected = " << elem_expected << '\n';

				next_expected += elem_expected;
				if ( elem.square + 1 == num_squares) {
					// drop this branch 
				} else if (num_moves != 0) {
					elem.prob *= probs[elem.square];
					elem.square += 1;
					next_seq_probs.push_back( elem );
					// std::cout << "added " << next_seq_probs.back() << '\n';
				}
			}

			// std::cout << "next_expected = " << next_expected << '\n';

			expected += next_expected;
			seq_probs = std::move(next_seq_probs);
			num_moves += 1;

			// std::cout << "expcted is now = " << expected << '\n';

			if (next_expected < 0.01) {
				break;
			}
		}

		// std::cout << std::lround(expected) << '\n';
		std::cout << expected << '\n';
		
	}
	
	return 0;
}
