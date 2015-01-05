#include "common.h"

int main () {
	size_t prev_num = 0;
	size_t num = 1;
	size_t sum = 0;

	while (num < 4000000) {
		// std::cout << num << std::endl;
		if (num % 2 == 0) {
			sum += num;
		}
		size_t new_num = num + prev_num;
		prev_num = num;
		num = new_num;
	}
	std::cout << "sum : " << sum << std::endl;
}