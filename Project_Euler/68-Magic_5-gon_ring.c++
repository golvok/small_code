#include <iostream>
#include <vector>
#include <array>

typedef unsigned long long int ullong;
typedef unsigned char uchar;

const ullong N = 3;
const ullong NUM_DIGITS = N*2;
const ullong MAXIMAL_SUM = NUM_DIGITS*3 - 3;

template<size_t SIZE>
class Gon {
private:
	std::array<std::array<uchar,3>,SIZE> numbers;
public:
	const static size_t NUM_DIGITS = SIZE*2;
	Gon()
		: numbers()
	{}

	bool trySet(ullong composite_digits) {
		std::array<int,NUM_DIGITS> digits;

		// std::cout << "extracting: ";
		for (size_t i = 0; i < NUM_DIGITS; ++i) {
			digits[i] = composite_digits % 10;
			// std::cout << composite_digits % 10;
			composite_digits /= 10;
		}
		// std::cout << std::endl;

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
					// std::cout << "sum of " << i << " is " << sum << " and not " << previous_sum << std::endl;
					return false;
				}
			}
		}
		return true;
	}

	ullong getNumber() const {
		ullong the_number = 0;
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
		for (size_t i = numbers.size(); i != 0; --i) {
			size_t real_index = (i+index_of_smallest) % numbers.size();

			for (size_t j = 0; j < numbers[real_index].size(); ++j) {
				the_number *= 10;
				the_number += numbers[real_index][j];
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
	ullong start = 0;
	ullong end = 0;

	for (size_t i = 1; i <= NUM_DIGITS; ++i) {
		start *= 10;
		end *= 10;
		start += (NUM_DIGITS-i) + 1;
		end += i;
	}

	Gon<N> gon;
	ullong max = 0;
	
	for (ullong i = start; i >= end; --i) {
		{
			ullong digits = 0;
			ullong current_num = i;
			while (current_num != 0) {
				digits |= 0x1 << current_num % 10;
				current_num /= 10;
			}
			if (digits != ((((ullong)-1) >> (sizeof(ullong)*8-(NUM_DIGITS+1)) & ((ullong)-1) << 1))) {
				continue;
			}
		}

		// std::cout << "trying " << i << std::endl;
		
		if (gon.trySet(i)) {
			ullong the_number = gon.getNumber();
			// std::cout << the_number << std::endl;
			if (the_number > max) {
				max = the_number;
			}
		}
	}

	std::cout << max << std::endl;
	return 0;
}
