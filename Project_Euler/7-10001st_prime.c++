#include "common.h"

int main() {
	int prime_count = 1;
	for (long long i = 2; prime_count <= 10001; ++i) {
		int sqrt_of_i = std::sqrt(i);
		bool is_prime = true;
		for (long long j = 2; j <= sqrt_of_i;j++) {
			if (i % j == 0) {
				is_prime = false;
				break;
			}
		}
		if (is_prime) {
			std::cout << '#' << prime_count << " is " << i << std::endl;
			++prime_count;
		}
	}
}
