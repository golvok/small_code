#include <iostream>
#include <math.h>

const size_t NUMBER = 600851475143;

int main () {

	size_t current_number = NUMBER;
	size_t max_pf = 1;

	std::cout << "candidate prime factors: ";
	for (size_t i = 2; i <= current_number;) {
		// std::cout << "trying " << i << std::endl;
		if (current_number % i == 0) {
			max_pf = i;
			std::cout << i << ", " << std::endl;
			current_number = current_number/i;
		} else {
			i += (i==2) ? 1 : 2;
		}
	}
	std::cout << "\nmax_pf = " << max_pf << std::endl;
}