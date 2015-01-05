#include "common.h"

const size_t NUM_DIGITS = 6;
const size_t START_VAL = pow(10,NUM_DIGITS) - 1;
const size_t END_VAL = pow(10,NUM_DIGITS - 1) - 1;
const size_t OVERFLOW_TEST = START_VAL*START_VAL;

/**
 * 5:     99681 *     99979 =           9 966 006 699 in   
 * 6:    999001 *    999999 =         999 000 000 999 in   0.2s
 * 7:   9997647 *   9998017 =      99 956 644 665 999 in   0.4s
 * 8:  99990001 *  99999999 =   9 999 000 000 009 999 in   3.7s
 * 9: 999920317 * 999980347 = 999 900 665 566 009 999 in 293.8s
 */

int main() {

	if (sqrt(OVERFLOW_TEST) != START_VAL) {
		std::cout << "too many digits for type\n";
		return 0;
	}

	size_t digits[NUM_DIGITS*2];

	size_t largest_so_far = 0;
	std::pair<size_t,size_t> factors {0,0};

	for (size_t i = START_VAL; i > END_VAL; --i) {
		// std::cout << i << ": "; // DEBUG1
		bool found_something_larger = false;
		for (size_t j = START_VAL; j >= i; --j) {
			size_t num = i*j;

			if (num < largest_so_far) {
				break;
			}

			// std::cout << j << " "; // DEBUG1

			size_t k = 0;
			// std::cout << i << '*' << j << '=' << num << "'s digits are: "; // DEBUG2

			{
				size_t tmp_num = num;
				while (true) {
					digits[k] = tmp_num % 10;
					tmp_num /= 10;
					// std::cout << digits[k]; // DEBUG2
					if (tmp_num > 0) {
						++k;
					} else {
						break;
					}
				}
			}

			// std::cout << "\n"; // DEBUG1

			bool is_palindrome = true;
			for(size_t l = 0; l <= k/2; ++l) {
				if (digits[k-l] != digits[l]) {
					is_palindrome = false;
					break;
				}
			}

			if (num > largest_so_far) {
				found_something_larger = true;
				if (is_palindrome) {
					// std::cout << '^'; // DEBUG1
					largest_so_far = num;
					factors = std::make_pair(i,j);
				}
			}
		}
		// std::cout << '\n'; // DEBUG1
		if (found_something_larger == false) {
			// std::cout << "didn't find anything larger\n"; // DEBUG1
			break;
		}
	}
	std::cout << "largest palindrome: " << factors.first << '*' << factors.second << '=' << largest_so_far << std::endl;
}