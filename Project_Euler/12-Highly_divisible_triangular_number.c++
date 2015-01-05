#include "common.h"

const ullong DIVISOR_THRESHOLD = 500;

int main() {
	bool done = false;
	ullong current_num = 1;
	ullong best_factor_count_so_far = 0;
	ullong numbers_since_best = 1;
	ullong numbers_so_far = 2;
	for (ullong next_offset = 2; !done; ++next_offset) {
		current_num += next_offset;
		ullong num_factors_pairs = 1;
		// std::cout << current_num << "'s lower factor pair members: 1";
		ullong sqrt_of_current_num = std::sqrt(current_num);
		for (ullong test_divisor = 2; test_divisor < sqrt_of_current_num; ++test_divisor) {
			if (current_num % test_divisor == 0) {
				// std::cout << ", " << test_divisor;
				++num_factors_pairs;
			}
		}
		// std::cout << ". (total = " << num_factors_pairs << "*2 = " << num_factors_pairs*2 << ")" << std::endl;
		if (num_factors_pairs > best_factor_count_so_far) {
			best_factor_count_so_far = num_factors_pairs;
			std::cout << "new best: " << current_num << " with " << num_factors_pairs*2 << " divisors, at " << "index = " << numbers_so_far << " with offset = " << numbers_since_best << std::endl;
			numbers_since_best = 0;
		}
		if (num_factors_pairs >= DIVISOR_THRESHOLD/2) {
			std::cout << "first one = " << current_num << std::endl;
			return 0;
		}
		++numbers_since_best;
		++numbers_so_far;
	}
	return 0;
}
