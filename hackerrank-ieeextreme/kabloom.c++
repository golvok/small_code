#include <algorithm>
#include <array>
#include <bitset>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <iostream>
#include <stack>
#include <vector>
#include <stdexcept>
#include <sstream>

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

using score_type = unsigned long long;
using card_type = unsigned char;
const card_type JOKER_CARD = 14;

score_type getScoreOfMatch(card_type card1, card_type card2) {
	if (card1 != card2 && card1 != JOKER_CARD && card2 != JOKER_CARD) {
		throw std::invalid_argument([&]() -> std::string {
			std::ostringstream oss;
			oss << card1 << " and " << card2 << " aren't a match!";
			return oss.str();
		}());
	}
	switch (card1) {
		case 1: // ace
			return 20*2;
		case 11: // face cards
		case 12:
		case 13:
			return 15*2;
		case JOKER_CARD:
			if (card2 == JOKER_CARD) {
				return 50*2;
			} else {
				return getScoreOfMatch(card2, card1); // reverse order
			}
		default: // normal cards
			return card1*2;
	}
}

struct BottomTopRange : print_printable {
	size_t begin_top;
	size_t end_top;
	size_t begin_bottom;
	size_t end_bottom;

	BottomTopRange(size_t begin_top, size_t end_top, size_t begin_bottom, size_t end_bottom)
		: begin_top(begin_top)
		, end_top(end_top)
		, begin_bottom(begin_bottom)
		, end_bottom(end_bottom)
	{ }

	template<typename STREAM>
	void print(STREAM& os) const {
		os << "{ {" << this->begin_top << ',' << this->end_top << "},{" << this->begin_bottom << ',' << this->end_bottom << "} }";
	}
	bool isEmptyRange() const { return begin_top == end_top || begin_bottom == end_bottom; }
};

class MaxScoreCache {
private:
	// todo: use better datastructure.. row_size can be up to 1000!!
	std::vector<std::vector<std::vector<std::vector<score_type>>>> score_matrix;
public:
	MaxScoreCache(size_t row_size) : score_matrix() {
		score_matrix.resize(row_size+1);
		for (auto& a : score_matrix) {
			a.resize(row_size+1);
			for (auto& b : a) {
				b.resize(row_size+1);
				for (auto& c : b) {
					c.resize(row_size+1);
				}
			}
		}
	}

	void setScore(score_type s, BottomTopRange btr) {
		// std::cout << btr << " ?= " << s;
		auto& stored_score = getScore(btr);
		if (stored_score < s) {
			stored_score = s;
			// std::cout << " SET";
		}
		// std::cout << '\n';
	}

	score_type& getScore(BottomTopRange btr) {
		return score_matrix.at(btr.begin_top).at(btr.end_top).at(btr.begin_bottom).at(btr.end_bottom);
	}

	bool hasScore(BottomTopRange btr) {
		return getScore(btr) != 0;
	}
};

score_type calculate_score(
	const std::vector<card_type>& top,
	const std::vector<card_type>& bottom,
	const BottomTopRange btr,
	MaxScoreCache& cache
) {
	if (cache.hasScore(btr)) {
		// std::cout << btr << " already has a score\n";
	} else if (btr.isEmptyRange()) {
		// std::cout << btr << " is empty\n";
	} else {
		// todo: cache all possible matches? - only n^2
		for (size_t i = btr.begin_top; i < btr.end_top; ++i) {
			for (size_t j = btr.begin_bottom; j < btr.end_bottom; ++j) {
				if (top.at(i) == bottom.at(j) || top.at(i) == JOKER_CARD || bottom.at(j) == JOKER_CARD) {
					score_type new_score = getScoreOfMatch(top.at(i),bottom.at(j));
					new_score += calculate_score(top,bottom, BottomTopRange(btr.begin_top, i          , btr.begin_bottom, j             ), cache);
					new_score += calculate_score(top,bottom, BottomTopRange(i+1          , btr.end_top, j+1             , btr.end_bottom), cache);
					cache.setScore(new_score, btr);
				}
			}
		}
	}
	return cache.getScore(btr);
}

int main() {
	while (true) {
		const auto row_size = get<size_t>(std::cin);
		if (row_size == 0) {
			break;
		}

		std::array<std::vector<card_type>,2> rows;
		for (auto& row : rows) {
			for (size_t i = 0; i < row_size; ++i) {
				auto new_card = get<std::string>(std::cin);
				// std::cout << new_card;
				const auto letter2value = {
					std::make_pair('J', (char)('0' + 11)),
					std::make_pair('Q', (char)('0' + 12)),
					std::make_pair('K', (char)('0' + 13)),
					std::make_pair('A', (char)('0' +  1)),
					std::make_pair('T', (char)('0' + 10)),
					std::make_pair('R', (char)('0' + JOKER_CARD)),
				};
				for (const auto& pair : letter2value) {
					if (new_card[0] == pair.first) {
						new_card[0] = pair.second;
						break;
					}
				}
				row.push_back(new_card[0] - '0');
				// std::cout << ':' << (unsigned int)row.back();
				// std::cout << ' ';
			}
			// std::cout << '\n';
		}

		MaxScoreCache cache(row_size);
		score_type score = calculate_score(
			rows[0],
			rows[1],
			BottomTopRange {0,row_size,0,row_size},
			cache
		);

		// std::cout << "FINAL SCORE = " << score << '\n';
		std::cout << score << '\n';

		// auto& top = rows[0];
		// auto& bottom = rows[1];

		// BottomTopRange btr_of_all{0,row_size,0,row_size};

		// MaxScoreCache cache(row_size);
		// std::stack<std::pair<BottomTopRange,std::pair<bool,score_type>>> btr_stack;
		// btr_stack.emplace(btr_of_all,std::make_pair(true,0));

		// while (btr_stack.empty() == false) {
		// 	auto stack_top = btr_stack.top();
		// 	const BottomTopRange& btr = stack_top.first;

		// 	if (cache.hasScore(btr) || btr.isEmptyRange()) {
		// 		// add getScore to other child, then parent
		// 		btr_stack.pop();
		// 		stack.top().second.second += cache.getScore(btr);
		// 	} else {
		// 		// todo: cache all possible matches - only n^2
		// 		for (size_t i = btr.begin_top; i < btr.end_top; ++i) {
		// 			for (size_t j = btr.begin_bottom; j < btr.end_bottom; ++j) {
		// 				if (top.at(i) == bottom.at(j)) {
		// 					stack_top.second.second = getScoreOfMatch(top.at(i),bottom.at(j));
		// 					btr_stack.emplace(BottomTopRange{btr.begin_top, i          , btr.begin_bottom, j             },std::make_pair(true , 0));
		// 					btr_stack.emplace(BottomTopRange{i            , btr.end_top, j               , btr.end_bottom},std::make_pair(false, 0));
		// 				}
		// 			}
		// 		}
		// 	}

		// 	if (stack_top.second.first) {
		// 		cache.setScore(btr,stack_top.second.second);
		// 	}
		// }

		// std::cout << cache.getScore(btr_of_all) << '\n';
	}

	return 0;
}
