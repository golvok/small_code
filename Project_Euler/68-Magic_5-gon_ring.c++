#include <iostream>
#include <vector>
#include <array>
#include <algorithm>

typedef unsigned long long int ullong;
typedef unsigned char uchar;

template<size_t SIZE>
class Gon {
private:
	std::array<std::array<uchar,3>,SIZE> numbers;
public:
	const static size_t NUM_DIGITS = SIZE*2;
	Gon()
		: numbers()
	{}

	bool trySet_i(ullong composite_digits) {
		std::array<uchar,NUM_DIGITS> digits;

		for (size_t i = 0; i < NUM_DIGITS; ++i) {
			digits[i] = composite_digits % 10;
			composite_digits /= 10;
		}
		return trySet(digits);
	}

	template<typename ArrayType>
	bool trySet(ArrayType digits) {
		for (size_t i = 0; i < numbers.size(); i += 1) {
			numbers[i][0] = digits[i*2];
			numbers[i][2] = digits[i*2+1];
			numbers[(i+1) % numbers.size()][1] = numbers[i][2];
		}

		ullong previous_sum = 0;
		for (size_t i = 0; i < numbers.size(); ++i) {
			ullong sum = 0;
			for (size_t j = 0; j < numbers[i].size(); ++j) {
				sum += numbers[i][j];
			}
			if (i == 0) {
				previous_sum = sum;
			} else {
				if (sum != previous_sum) {
					return false;
				}
			}
		}
		return true;
	}

	ullong getNumber() const {
		size_t index_of_smallest = 0;
		{
			uchar smallest = NUM_DIGITS+1;
			for (size_t i = 0; i < numbers.size(); ++i) {
				if (numbers[i][0] < smallest) {
					index_of_smallest = i;
					smallest = numbers[i][0];
				}
			}
		}
		ullong the_number = 0;
		for (size_t i = 0; i < numbers.size(); ++i) {
			size_t real_index = (i+index_of_smallest) % numbers.size();

			for (size_t j = 0; j < numbers[real_index].size(); ++j) {
				ullong n = numbers[real_index][j];
				ullong power_of_10 = 1;
				do {
					the_number *= 10;
					power_of_10 *= 10;
				} while (power_of_10 <= n);
				the_number += n;
			}
		}

		return the_number;
	}

};

template<size_t SIZE>
std::ostream& operator<<(std::ostream& os, const Gon<SIZE> gon) {
	ullong the_number = gon.getNumber();
	while (the_number != 0) {
		os << the_number % 10;
		the_number /= 10;
		os << ((the_number % 3) ? "; " : ", ");
	}
	return os;
}

int main() {
	const ullong N = 5;
	const ullong NUM_DIGITS = N*2;

	Gon<N> gon;
	ullong max = 0;
	ullong max_16 = 0;

	std::array<uchar,NUM_DIGITS> digits;
	for (size_t i = 0; i < digits.size(); ++i) {
		digits[i] = i+1;
	}

	while (std::next_permutation(digits.begin(),digits.end())) {
		if (gon.trySet(digits)) {
			for (auto d : digits) {
				std::cout << (int)d << " ";
			}
			std::cout << "-> ";
			ullong the_number = gon.getNumber();
			std::cout << the_number << std::endl;
			if (the_number > max) {
				max = the_number;
			}
			if (the_number < 10000000000000000 && the_number > max_16) {
				max_16 = the_number;
			}
		}
	}

	std::cout << "max = " << max << std::endl;
	std::cout << "16-digit max = " << max_16 << std::endl;
	return 0;
}
