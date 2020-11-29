#include <catch.hpp>

#include "../mergesort.h++"

#include <vector>

namespace {
	template<typename C, typename Compare = std::less<>>
	auto sorted(const C& c, Compare comp = {}) {
		auto result = c;
		std::sort(result.begin(), result.end(), comp);
		return result;
	}

	void check_will_sort(std::vector<int> v) {
		auto sorted_v = sorted(v);
		mergesort(v.begin(), v.end());
		REQUIRE(v == sorted_v);
	}

	template<typename Compare>
	void check_will_sort(std::vector<int> v, Compare comp) {
		auto sorted_v = sorted(v, comp);
		mergesort(v.begin(), v.end(), comp);
		REQUIRE(v == sorted_v);
	}
}

SCENARIO("Sort") {
	check_will_sort({4});
	check_will_sort({4,3});
	check_will_sort({3,4});
	check_will_sort({3,3});
	check_will_sort({2,3,4});
	check_will_sort({2,4,3});
	check_will_sort({3,2,4});
	check_will_sort({3,4,2});
	check_will_sort({4,3,2});
	check_will_sort({4,2,3});
	check_will_sort({4,2,3,1});
	check_will_sort({4,546,2,1,9,5,4,2,1,6,4654,654,321,564,61,321});
	
	check_will_sort({4,2,3,1}, std::greater<>{});
}
