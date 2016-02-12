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


template<typename VALUE_TYPE, typename NEXT, typename DONE>
class generator_iterator {
private:
	VALUE_TYPE current;
	NEXT next;
	DONE done;
	bool is_end_iterator;

public:
	generator_iterator(VALUE_TYPE current, NEXT next, DONE done, bool is_end_iterator)
		: current(current)
		, next(next)
		, done(done)
		, is_end_iterator(is_end_iterator)
	{ }

	generator_iterator& operator++() {
		current = next(current);
		return *this;
	}

	bool operator==(const generator_iterator& rhs) const {
		if (rhs.is_end_iterator && is_end_iterator) {
			return true;
		} else if (rhs.is_end_iterator) {
			return rhs.done(current);
		} else if (is_end_iterator) {
			return done(rhs.current);
		} else {
			return current == rhs.current;
		}
	}

	bool operator!=(const generator_iterator& rhs) const { return !(*this == rhs); }

	VALUE_TYPE operator*() const {
		return current;
	}
};

template<typename VALUE_TYPE, typename NEXT, typename DONE>
class generator {
public:
	using iter_type = generator_iterator<VALUE_TYPE,NEXT,DONE>;

private:
	VALUE_TYPE current;
	DONE done;
	NEXT next;

public:
	generator(VALUE_TYPE initial, DONE done, NEXT next)
		: current(initial)
		, done(done)
		, next(next)
	{ }

	iter_type begin() {
		return iter_type(current,next,done,false);
	}

	iter_type end() {
		return iter_type(current,next,done,true);
	}
};

template<typename VALUE_TYPE, typename NEXT, typename DONE>
auto make_generator(VALUE_TYPE initial, DONE done, NEXT next) {
	return generator<VALUE_TYPE,NEXT,DONE>(
		initial,
		done,
		next
	);
}

template<typename VALUE_TYPE, typename NEXT>
auto make_generator(VALUE_TYPE initial, VALUE_TYPE past_end, NEXT next) {
	return make_generator(
		initial,
		[=](const VALUE_TYPE& current ) { return current == past_end; },
		next
	);
}

template<typename GEN>
auto make_generator(GEN&& gen) {
	using value_type = typename GEN::value_type;
	return make_generator(
		gen.initial(),
		[&](const value_type& current) { return gen.done(current); },
		[&](const value_type& current) { return gen.next(current); }
	);
}

template<typename CONTAINER>
auto index_iterator(const CONTAINER& c) {
	return make_generator(size_t(0),c.size(),[](size_t i) -> size_t { return i + 1; });
}

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

template<typename Integral>
Integral pwr(Integral base, Integral exponent, Integral mod) {
    base %= mod;

    Integral i;
    Integral rtn = base;

    if (exponent == 0) {
        return 1;
    }

    if (exponent == 1) {
        return rtn;
    }

    for (i=2; i<=exponent; ++i) {
        rtn = (rtn * base) % mod;
        if (rtn == 1 || rtn == base) {
            Integral r = exponent % i;
            if (rtn == base) {
                r += 1;
            }
            return pwr(base, r, mod);
        }
    }
    return rtn;
}

using ball_type = unsigned long long;
using query_type = unsigned long long;

query_type MOD = 1000000007;

int main() {
	auto num_balls = get<size_t>(std::cin);
	
	std::vector<ball_type> balls;
	for (size_t i = 0; i < num_balls; ++i) {
		balls.push_back(get<ball_type>(std::cin));
	}

	auto num_queries = get<size_t>(std::cin);

	std::vector<query_type> queries;
	for (size_t i = 0; i < num_queries; ++i) {
		queries.push_back(get<query_type>(std::cin));
	}

	// count number of each ball & store in map
	// calculate all factors for each unique number
	// for each GCD, find all numbers with that factor
	//    - need to make sure that at least one has *it*, but *not* it's square.

	struct ball_factor_info {
		query_type count;
		std::vector<ball_type> factors;

		ball_factor_info() : count(0), factors() { }
		ball_factor_info(const ball_factor_info&) = default;
		ball_factor_info(ball_factor_info&&) = default;
		ball_factor_info(query_type count, std::vector<ball_type>&& factors)
			: count(count), factors(std::move(factors))
		{ }
	};

	std::unordered_map<ball_type,ball_factor_info> ball_factors;

	for (const auto& ball : balls) {
		const auto& eplace_results = ball_factors.emplace(ball,ball_factor_info());
		auto& this_bfi = eplace_results.first->second;
		std::cout << ball << ": ";
		this_bfi.count += 1;

		if (eplace_results.second == false) {
			std::cout << "seen earlier\n";
			continue; // already did this one
		}

		this_bfi.factors.push_back(1);

		// holds factors greater than sqrt
		std::vector<ball_type> big_factors;

		ball_type sqrt_of_ball = std::lround((std::sqrt(ball)));
		for (ball_type i = 2; i <= sqrt_of_ball; ++i) {
			if (ball % i == 0) {
				// directly check for square here as well, and store that too?
				this_bfi.factors.push_back(i);
				big_factors.push_back(ball/i);
			}
		}

		std::copy(big_factors.rbegin(),big_factors.rend(),std::back_inserter(this_bfi.factors));
		this_bfi.factors.push_back(ball);

		for (const auto& factor : this_bfi.factors) {
			std::cout << factor << ' ';
		}
		std::cout << '\n';
	}

	for (const auto& query : queries) {
		size_t num_with_just_query = 0;
		size_t num_with_query_square = 0;
		for (const auto& ball_factor_item : ball_factors) {
			const auto& this_ball_count = ball_factor_item.second.count;
			const auto& this_ball_factors = ball_factor_item.second.factors;

			if (std::binary_search(this_ball_factors.begin(), this_ball_factors.end(), query)) {
				if (std::binary_search(this_ball_factors.begin(), this_ball_factors.end(), query*query)) {
					num_with_query_square += this_ball_count;
				} else {
					num_with_just_query += this_ball_count;
				}
			}
		}
		std::cout << query << " : num_with_just_query=" << num_with_just_query << ", num_with_query_square=" << num_with_query_square << " : ";

		for (auto& num_ptr : {&num_with_just_query, &num_with_query_square}) {
			auto& num = *num_ptr;
			if (num != 0) {
				num -= 1;
			}
		}

		std::cout << (
			(
				(
					(
						pwr((query_type)2,(query_type)num_with_just_query,MOD) * pwr((query_type)2,(query_type)num_with_query_square,MOD)
					) % MOD 
				)
			) % MOD
		);
		std::cout << '\n';
	}
}
