#include <iostream>

const unsigned int DIVISOR_THRESHOLD = 50;

int main() {
	bool done = false;
	unsigned int current_num = 0;
	for (unsigned int next_offset = 1; !done; ++next_offset) {
		current_num += next_offset;
		unsigned int num_factors = 2;
		std::cout << current_num << " divisible by: 1, " << current_num << ", ";
		for (unsigned int test_divisor = 2; test_divisor < current_num; ++test_divisor) {
			if (current_num % test_divisor == 0) {
				std::cout << test_divisor << ", ";
				++num_factors;
				if (num_factors >= DIVISOR_THRESHOLD) {
					std::cout << "first one = " << current_num << std::endl;
					return 0;
				}
			}
		}
		std::cout << std::endl;
	}
	return 0;
}
