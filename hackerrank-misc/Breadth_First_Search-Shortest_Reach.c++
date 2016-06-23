#include <algorithm>
#include <cmath>
#include <deque>
#include <limits>
#include <iostream>
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

void do_test_case();

int main() {

	auto num_test_cases = get<uint>(std::cin);

	for (uint itest = 0; itest < num_test_cases; ++itest) {
		do_test_case();
	}

	return 0;
}

void do_test_case() {

	const auto num_nodes = get<size_t>(std::cin);
	const auto num_edges = get<size_t>(std::cin);

	std::vector<std::vector<size_t>> out_nodes_of(num_nodes);
	for (size_t iedge = 0; iedge < num_edges; ++iedge) {
		const auto v1 = get<size_t>(std::cin) - 1;
		const auto v2 = get<size_t>(std::cin) - 1;

		out_nodes_of[v1].emplace_back(v2);
		out_nodes_of[v2].emplace_back(v1);
	}

	const auto source_id = get<size_t>(std::cin) - 1;

	using Distance = long long unsigned int;
	std::vector<Distance> shortest_known_distance_to(num_nodes, std::numeric_limits<Distance>::max());
	std::vector<bool> has_been_in_queue(num_nodes, false);
	std::vector<bool> has_been_explored(num_nodes, false);
	std::deque<size_t> deque;
	
	// setup source
	deque.emplace_front(source_id);
	shortest_known_distance_to[source_id] = 0;

	while (deque.empty() == false) {
		const auto v1 = deque.front();
		has_been_explored[v1] = true;
		deque.pop_front();

		const auto dist_to_v1 = shortest_known_distance_to[v1];

		for (const auto& v2 : out_nodes_of[v1]) {
			if (has_been_explored[v2]) {
				continue;
			}

			const auto test_dist = dist_to_v1 + 6;
			if (shortest_known_distance_to[v2] > test_dist) {
				shortest_known_distance_to[v2] = test_dist;
				for (auto it = deque.begin(); ; ++it) {
					if (it == deque.end() || shortest_known_distance_to[*it] > test_dist) {
						deque.insert(it, v2);
						break;
					}
				}
			}
		}
	}

	for (auto& dist : shortest_known_distance_to) {
		if (dist == 0) { // is source
			// do nothing
		} else if (dist == std::numeric_limits<Distance>::max()) {
			std::cout << "-1 ";
		} else {
			std::cout << dist << ' ';
		}
	}

	std::cout << '\n';
}
