#include <cmath>
#include <cstdio>
#include <vector>
#include <list>
#include <iostream>
#include <algorithm>
#include <iterator>
#include <set>
#include <numeric>
using namespace std;

namespace detail {
	struct printer {
	template<typename STREAM, typename T>
		void operator()(STREAM& os, const T& t) const {
			os << t;
		}
	};
}

template<typename CONTAINER, typename OSTREAM, typename FUNC = ::detail::printer>
void print_container(
	const CONTAINER& c,
	OSTREAM&& os,
	const std::string& sep = ", ",
	const std::string& prefix_str = "{ ",
	const std::string& suffix_str = " }",
	FUNC func = FUNC{}
) {
	auto beg = begin(c);
	auto en = end(c);

	os << prefix_str;
	if (beg != en) {
		func(os,*beg);
		std::for_each(std::next(beg), en, [&](const auto& v){
			os << sep;
			func(os,v);
		});
	}
	os << suffix_str;
}

template<typename T>
T get() {
	T t;
	std::cin >> t;
	return t;
}

template<typename T>
auto readMultiset(const size_t size) {
	std::multiset<T> ms;
	for (size_t i = 0; i < size; ++i) {
		ms.insert(get<T>());
	}
	return ms;
}

template<typename T>
auto readVector(const size_t size) {
	std::vector<T> ms;
	for (size_t i = 0; i < size; ++i) {
		ms.push_back(get<T>());
	}
	return ms;
}

template<typename CONTAINER, typename TEST_FUNC, typename INIT_T>
auto accumulate_while(const CONTAINER& c, TEST_FUNC tf, INIT_T init) {
	using std::begin; using std::end;

	auto sum = init;
	for (auto it = begin(c); it != end(c) && tf(*it); ++it) {
		sum += *it;
	}
	
	return sum;
}

long long int doProblem() {
	const auto set_size = get<size_t>();

	auto setX = readVector<int32_t>(set_size);
	auto setY = readVector<int32_t>(set_size);

	auto sumX = std::accumulate(begin(setX), end(setX), 0);
	auto sumY = std::accumulate(begin(setY), end(setY), 0);

	if (sumX != sumY) { 
		return -1;
	}

	if (set_size == 1) {
		return 0; // we know they're equal
	}

	double mean = sumX/(double)set_size;

	std::sort(begin(setX), end(setX));
	std::sort(begin(setY), end(setY));

	print_container(setX, std::cout);
	std::cout << '\n';
	print_container(setY, std::cout);
	std::cout << '\n';

	if (false) {

		const bool x_starts_smaller = *begin(setX) < *begin(setY);
		long long unsigned int difference_in_first_region = 0;

		{ auto x_iter = begin(setX); auto y_iter = begin(setY);
		for (; x_iter != end(setX); ++x_iter, ++y_iter) {
			if ((*x_iter < *y_iter) != x_starts_smaller) {
				break;
			} else {
				difference_in_first_region += std::abs(*x_iter - *y_iter);
			}
		}}

		return difference_in_first_region;

	} else {
		auto less_than_mean = [&](auto&& num) {
			return num < mean;
		};

		auto before_mean_value_sumX = accumulate_while(setX, less_than_mean, 0LL);
		// auto before_mean_value_sumY = accumulate_while(setY, less_than_mean, 0LL);

		return (std::abs(before_mean_value_sumX)+1)/2;
	}
}

int main() {
	std::cout << doProblem() << '\n';
}
