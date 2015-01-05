#include "common.h"

template<size_t SIZE>
class Gon {
private:
	std::array<std::array<uchar,3>,SIZE> numbers;
	size_t num_added_digit_pairs;
	ullong previous_sum;
public:
	const static size_t NUM_DIGITS = SIZE*2;
	Gon()
		: numbers()
		, num_added_digit_pairs(0)
		, previous_sum(0)
	{}

	bool addDigitPair(uchar digit1, uchar digit2) {
		numbers[num_added_digit_pairs][0] = digit1;
		numbers[num_added_digit_pairs][1] = digit2;
		numbers[(num_added_digit_pairs-1+numbers.size()) % numbers.size()][2] = digit2;

		num_added_digit_pairs += 1;

		if (num_added_digit_pairs >= 2) {
			if (checkSumOf(num_added_digit_pairs-2) == false) {
				return false;
			}
			if (num_added_digit_pairs == numbers.size()) {
				if (checkSumOf(num_added_digit_pairs-1) == false) {
					return false;
				}
			}
		}
		return true;
	}

	bool checkSumOf(size_t set_to_check) {
		ullong sum = 0;
		for (size_t j = 0; j < numbers[set_to_check].size(); ++j) {
			sum += numbers[set_to_check][j];
		}

		if (set_to_check == 0) {
			previous_sum = sum;
		} else {
			if (sum != previous_sum) {
				// std::cout << "sum of " << set_to_check << " is " << sum << " and not " << previous_sum << std::endl;
				return false;
			}
		}
		return true;
	}

	void removeDigiPair() {
		assert(num_added_digit_pairs > 0);
		num_added_digit_pairs -= 1;
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

template<size_t N>
void checkGon(Gon<N>& gon, std::vector<uchar> digits, ullong& max, ullong& max_16, size_t digits_left, const size_t NUM_DIGITS) {
	digits_left -= 2;
	for (uchar i = 1; i <= NUM_DIGITS; ++i) {
		if (std::find(digits.begin(),digits.end(),i) != digits.end()) {
			// already used? then skip it.
			continue;
		}
		digits.push_back(i);

		for (uchar i = 1; i <= NUM_DIGITS; ++i) {
			if (std::find(digits.begin(),digits.end(),i) != digits.end()) {
				// already used? then skip it.
				continue;
			}
			digits.push_back(i);

			if (gon.addDigitPair(*(++digits.rbegin()),*(digits.rbegin())) == true) {
				if (digits_left == 0) {
					// success! see if this solution is better
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
				} else {
					checkGon(gon,digits,max,max_16,digits_left,NUM_DIGITS);
				}
			}

			digits.pop_back();
			gon.removeDigiPair();
		}
		digits.pop_back();
	}
}

int main() {
	const ullong N = 5;
	const ullong NUM_DIGITS = N*2;

	Gon<N> gon;
	ullong max = 0;
	ullong max_16 = 0;

	std::vector<uchar> digits;
	digits.reserve(NUM_DIGITS);

	checkGon(gon,digits,max,max_16,NUM_DIGITS,NUM_DIGITS);

	std::cout << "max = " << max << std::endl;
	std::cout << "16-digit max = " << max_16 << std::endl;
	return 0;
}
