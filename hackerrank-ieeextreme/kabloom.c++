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
const card_type JOKER_CARD = 'R';

score_type getScoreOfMatch(card_type card1, card_type card2) {
	if (card1 != card2 && card1 != JOKER_CARD && card2 != JOKER_CARD) {
		throw std::invalid_argument([&]() -> std::string {
			std::ostringstream oss;
			oss << card1 << " and " << card2 << " aren't a match!";
			return oss.str();
		}());
	}
	switch (card1) {
		case 'A':
			return 20*2;
		case 'T':
			return 10*2;
		case 'J':
		case 'Q':
		case 'K':
			return 15*2;
		case JOKER_CARD:
			if (card2 == JOKER_CARD) {
				return 50*2;
			} else {
				return getScoreOfMatch(card2, card1); // reverse order
			}
		default: // normal cards
			return (card1-'0')*2;
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
				  ( std::hash<size_t>()( btr.begin_top    ) + 1000*0 )
				^ ( std::hash<size_t>()( btr.end_top      ) + 1000*1 )
				^ ( std::hash<size_t>()( btr.begin_bottom ) + 1000*2 )
				^ ( std::hash<size_t>()( btr.end_bottom   ) + 1000*3 )
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
				row.push_back(get<char>(std::cin));
				// std::cout << row.back() << ' ';
			}
			// std::cout << '\n';
		}

		MaxScoreCache cache;
		score_type score = calculate_score(
			rows[0],
			rows[1],
			BottomTopRange {0,row_size,0,row_size},
			cache
		);

		std::vector<size_t> indicies;
		for (size_t i = 0; i <= row_size; ++i) {
			indicies.push_back(i);
		}

		for (auto a : indicies) {
			std::cout << "\n---" << a << "---\n";
			for (auto b : indicies) {
				std::cout << "\n-" << b << "-\n\n";
				for (auto c : indicies) {
					for (auto d :indicies) {
						std::cout << cache.getScore(BottomTopRange(a,c,b,d)) << '\t';
					}
					std::cout << '\n';
				}
			}
		}

		std::cout << "FINAL SCORE = " << score << '\n';
		// std::cout << score << '\n';

	}

	return 0;
}
