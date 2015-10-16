#include <algorithm>
#include <array>
#include <bitset>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <functional>
#include <iostream>
#include <limits>
#include <list>
#include <sstream>
#include <stack>
#include <string>
#include <deque>
#include <stdexcept>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

template<typename T, typename STREAM>
T get(STREAM& is) {
	T t;
	is >> t;
	return t;
}

struct print_printable { };

template<typename T, typename STREAM>
auto operator<<(STREAM& os, const T& t) -> decltype(static_cast<const print_printable*>(&t),os) {
	t.print(os);
	return os;
}

using ball_type = unsigned long;
using query_type = unsigned long;

int main() {
	auto num_balls = get<size_t>(std::cin);
	
	std::vector<ball_type> balls;
	for (size_t i = 0; i < num_balls; ++i) {
		balls.push_back(get<ball_type>(std::cin));
	}

	auto num_queries = get<size_t>(std::cin);

	std::vector<query_type> queries;
	for (size_t i = 0; i < num_queries) {
		queries.push_back(get<query_type>(std::cin));
	}

	// count number of each ball & store in map
	// calculate all factors for each unique number
	// for each GCD, find all numbers with that factor
	//    - need to make sure that at least on has it, but not it's square.
}
