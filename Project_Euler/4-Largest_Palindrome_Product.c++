#include <iostream>
#include <math.h>

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

	for (size_t i = START_VAL; i > END_VAL; --i) {
		for (size_t j = START_VAL; j >= i; --j) {
			size_t num = i*j;
			size_t k = 0;
			// std::cout << i << '*' << j << '=' << num << "'s digits are: ";
			while (true) {
				digits[k] = num % 10;
				num /= 10;
				// std::cout << digits[k];
				if (num > 0) {
					++k;
				} else {
					break;
				}
			}
			// std::cout << std::endl;

			bool is_palindrome = true;
			for(size_t l = 0; l <= k/2; ++l) {
				if (digits[k-l] != digits[l]) {
					is_palindrome = false;
					break;
				}
			}

			if (is_palindrome) {
				std::cout << "largest palindrome: " << i << '*' << j << '=' << i*j << std::endl;
				return 0;
			}
		}
	}
}