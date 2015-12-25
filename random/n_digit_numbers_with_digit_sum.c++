
#include "../C++_experiments/generator.h++"

#include <algorithm>
#include <iomanip>
#include <iostream>
#include <cstdint>

#include <boost/iterator/filter_iterator.hpp>
#include <boost/range/algorithm.hpp>
#include <boost/range/iterator_range.hpp>

template<typename INTEGRAL>
class int_iter : public std::iterator<std::forward_iterator_tag, const INTEGRAL, std::make_signed<INTEGRAL>, const INTEGRAL*, const INTEGRAL&> {
public:
	int_iter(const INTEGRAL& i) :i(i) { }

	int_iter(const int_iter&) = default;
	int_iter& operator=(const int_iter&) = default;

	int_iter& operator++() { ++i; return *this; }
	int_iter operator++(int) { auto copy = *this; ++i; return copy; }

	const INTEGRAL& operator*() const { return i; }
	bool operator!=(const int_iter& rhs) const { return this->i != rhs.i; }
	bool operator==(const int_iter& rhs) const { return this->i == rhs.i; }
private:
	INTEGRAL i;
};

template<typename INTEGRAL>
auto ndndws_range(const size_t num_digits, const size_t max_digit_sum) {
	const size_t past_max_num = [&]() {
		size_t result = 1;
		for (size_t i = 0; i < num_digits; ++i) {
			result *= 10;
		}
		return result;
	}();

	auto predicate = [=](const INTEGRAL& i) {
		auto num_copy = i;
		size_t digit_sum = 0;

		while (num_copy != 0) {
			digit_sum += num_copy % 10;
			num_copy /= 10;
		}

		return digit_sum <= max_digit_sum;
	};

	return boost::make_iterator_range(
		boost::make_filter_iterator( predicate, int_iter<INTEGRAL>(0            ), int_iter<INTEGRAL>(past_max_num) ),
		boost::make_filter_iterator( predicate, int_iter<INTEGRAL>(past_max_num ), int_iter<INTEGRAL>(past_max_num) )
	);
}

namespace me {
	struct power {
		template<typename T1, typename T2>
		auto operator()(const T1& base, const T2& exponent) {
			return std::pow(base, exponent);
		}
	};
}

int main(int argc, char** argv) {
	(void)argc;
	(void)argv;
	if (argc != 3) { std::cout << "call with 2 arguments\n"; return 1; }

	const size_t num_digits = std::strtoull(argv[1], nullptr, 10);
	const size_t max_digit_sum = std::strtoull(argv[2], nullptr, 10);

	std::vector<uint64_t> baseline_results;

	for (auto num : ndndws_range<uint64_t>(num_digits, max_digit_sum)) {
		baseline_results.push_back(num);
		// std::cout << std::setfill('0') << std::setw(num_digits) << num << '\n';
	}

	// std::cout << "---------------------\n";

	std::vector<uint64_t> new_results;

	std::vector<uint64_t> current_num;
	current_num.resize(num_digits, 0); // start at 0s
	size_t current_digit_sum = 0;

	while (true) {
		{
			// add the number to the result vector
			uint64_t num = 0;
			for (auto it = current_num.rbegin(); it != current_num.rend(); ++it) {
				num *= 10;
				num += *it;
			}
			new_results.push_back(num);
			std::cout << std::setfill('0') << std::setw(num_digits) << num << '\n';
		}

		// the last number will be of the form 0000...000[max_digit_sum], and the only one
		// to end with max_digit_sum
		if (current_num.back() == max_digit_sum) {
			break;
		}

		// calculate the next number
		for (auto& digit : current_num) {
			if (digit < max_digit_sum && current_digit_sum < max_digit_sum) {
				current_digit_sum += 1;
				digit += 1;
				break;
			} else {
				current_digit_sum -= digit;
				digit = 0;
			}
		}
	}

	auto mismatch_results = boost::mismatch(baseline_results, new_results);
	if (mismatch_results.first != baseline_results.end() || mismatch_results.second != new_results.end()) {
		std::cout << "PROBLEM!!!\n";

		for (
			auto base_r_iter = baseline_results.begin(), new_r_iter = new_results.begin();
			base_r_iter != baseline_results.end() && new_r_iter != new_results.end();
			++base_r_iter, ++new_r_iter
		) {
			std::cout << std::setfill('0') << std::setw(num_digits) << *base_r_iter << ' ' << std::setfill('0') << std::setw(num_digits) << *new_r_iter << '\n';
		}
	}
}
