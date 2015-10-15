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
#include <unordered_map>

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

	bool operator==(const BottomTopRange& rhs) const {
		return (begin_top == rhs.begin_top) && (end_top == rhs.end_top) && (begin_bottom == rhs.begin_bottom) && (end_bottom == rhs.end_bottom);
	}
};

namespace std {
	template<>
	struct hash<BottomTopRange>{
		size_t operator()(const BottomTopRange& btr) const {
			return
				  ( std::hash<size_t>()( btr.begin_top    ) + 1000 )
				^ ( std::hash<size_t>()( btr.end_top      ) + 1000 )
				^ ( std::hash<size_t>()( btr.begin_bottom ) + 1000 )
				^ ( std::hash<size_t>()( btr.end_bottom   ) + 1000 )
			;
		}
	};
}

class MaxScoreCache {
private:
	std::unordered_map<BottomTopRange,score_type> scores;
public:
	MaxScoreCache() : scores() { }

	void setScore(score_type s, BottomTopRange btr) {
		// std::cout << btr << " ?= " << s;
		if (s == 0) {
			// std::cout << "NOSET\n";
		} else {
			auto insert_results = scores.emplace(btr,s);
			if (insert_results.second == false && insert_results.first->second < s) {
				insert_results.first->second = s;
				// std::cout << " SET";
			}
			// std::cout << '\n';
		}
	}

	score_type getScore(BottomTopRange btr) {
		auto find_results = scores.find(btr);
		if (find_results == scores.end()) {
			return 0;
		} else {
			return find_results->second;
		}
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
		return cache.getScore(btr);
	} else if (btr.isEmptyRange()) {
		// std::cout << btr << " is empty\n";
		return 0;
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
		return cache.getScore(btr);
	}
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
				auto new_card = get<char>(std::cin);
				std::cout << new_card;
				switch (new_card) {
					case 'J':
						row.push_back(11); break;
					case 'Q':
						row.push_back(12); break;
					case 'K':
						row.push_back(13); break;
					case 'A':
						row.push_back( 1); break;
					case 'T':
						row.push_back(10); break;
					case 'R':
						row.push_back(JOKER_CARD); break;
					default:
						row.push_back(new_card - '0'); break;
				}
				std::cout << ':' << (unsigned int)row.back();
				std::cout << ' ';
			}
			std::cout << '\n';
		}

		MaxScoreCache cache;
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
