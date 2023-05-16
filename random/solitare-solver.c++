#include <algorithm>
#include <array>
#include <iostream>
#include <map>
#include <random>
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <vector>

using std::vector;
using std::ostream;
using std::cout;

using i64 = std::int64_t;

namespace {
struct App {

static int main() {
	App a;
	a.solve();
	return 0;
}

enum Colour { kRed = 0, kBlack = 1 };
enum Suite {
	kDiamonds = (0 << 1) | kRed,
	kClubs    = (1 << 1) | kBlack,
	kHearts   = (2 << 1) | kRed,
	kSpades   = (3 << 1) | kBlack,
};
enum Value { kAce = 1, kKing = 13 };

static size_t id(Suite s) { return s >> 1; }

struct Card {
	Suite _suite;
	Value _value;
	Colour colour() const { return Colour(_suite & 0b1); }
	Suite suite() const { return _suite; }
	Value value() const { return _value; }

	friend ostream& operator<<(ostream& os, Card const& c) {
		constexpr std::array<char, 4> suite_letter{'D', 'C', 'H', 'S'};
		return os << suite_letter[i64(c.suite()) >> 1] << '-' << c.value();
	}

	auto operator<=>(Card const&) const = default;
};

friend ostream& operator<<(ostream& os, vector<Card> const& vc) {
	os << '[';
	for (auto const& c : vc) {
		os << c << ' ';
	}
	return os << '(' << vc.size() << ")]";
}

friend ostream& operator<<(ostream& os, vector<vector<Card>> const& vvc) {
	os << "[\n";
	for (auto const& vc : vvc) {
		os << "  " << vc << '\n';
	}
	return os << ']';
}


struct Tableau {
	static constexpr i64 num_stacks = 7;
	static constexpr i64 num_draws = 3;

	vector<Card> draw_pile{};
	vector<Card> drawn{};
	vector<vector<Card>> hiddens{unsigned(num_stacks)};
	vector<vector<Card>> stacks{unsigned(num_stacks)};
	vector<vector<Card>> discards{4};

	auto operator<=>(Tableau const&) const = default;
};

Tableau tableau{};

i64 const& num_stacks = tableau.num_stacks;
i64 const& num_draws = tableau.num_draws;
vector<Card>& draw_pile = tableau.draw_pile;
vector<Card>& drawn = tableau.drawn;
vector<vector<Card>>& hiddens = tableau.hiddens;
vector<vector<Card>>& stacks = tableau.stacks;
vector<vector<Card>>& discards = tableau.discards;

void solve() {
	for (auto s : {kDiamonds, kClubs, kHearts, kSpades}) {
		for (auto v = kAce; v <= kKing; v = Value(v + 1)) {
			draw_pile.push_back({s, v});
		}
	}

	std::size_t seed = std::random_device{}();
	cout << "seed = " << seed << std::endl;
	auto g  = std::mt19937{seed};
	std::ranges::shuffle(draw_pile, g);

	for (i64 i_stack = 0; i_stack != num_stacks; ++i_stack) {
		for (i64 i_draw = 0; i_draw != i_stack; ++i_draw) {
			hiddens[i_stack].push_back(draw_pile.back());
			draw_pile.pop_back();
		}
		stacks[i_stack].push_back(draw_pile.back());
		draw_pile.pop_back();
	}

	dump();
	cout.flush();
	try {
		try_move();
	} catch (std::exception const& e) {
		if (std::string_view(e.what()) != "solved!") throw;
		cout << e.what() << '\n';
	}
	// dump();
}

void dump() {
	cout << "draw_pile=" << draw_pile << '\n';
	cout << "drawn=" << drawn << '\n';
	cout << "hiddens=" << hiddens << '\n';
	cout << "stacks=" << stacks << '\n';
	cout << "discards=" << discards << '\n';
}

void log(std::string_view msg1, std::string_view msg2) {
	(void)msg1, (void)msg2;
	// cout << "\n" << msg1 << msg2 << '\n';
	// dump();
}

struct TableauHasher {
	std::size_t operator()(Tableau const&) const { return 0; }
};

std::unordered_set<Tableau, TableauHasher> visited{};

void try_move(bool go = true) {
	if (not go) return;
	auto is_empty = [](auto& v) { return v.empty(); };
	if (draw_pile.empty() && drawn.empty() && std::ranges::all_of(hiddens, is_empty)) {
		cout.flush();
		throw std::runtime_error("solved!");
	}
	try_discard();
	try_transfer();
	try_play();
	try_quick_discard();
	try_draw();
}

void try_move_if_new_board(std::string_view msg) {
	if (not visited.insert(tableau).second) return;
	log("new tableau from: ", msg);
	try_move();
}

void reverted(std::string_view msg) {
	log("reverted: ", msg);
	try_move(false);
}

/// flip hidden cards into stacks
void try_flip_then_continue(i64 i_stack, std::string_view msg) {
	auto& hidden = hiddens.at(i_stack);
	auto& stack = stacks.at(i_stack);
	bool const do_flip = not hidden.empty() && stack.empty();

	if (do_flip) {
		stack.push_back(hidden.back());
		hidden.pop_back();
	}

	try_move_if_new_board(msg);

	if (do_flip) {
		hidden.push_back(stack.back());
		stack.pop_back();
	}
}

/// stack up from aces
void try_discard() {
	for (auto& s : stacks) {
		if (s.empty()) continue;

		auto tip = s.back();
		auto& dst_discard = discards.at(id(tip.suite()));
		const bool do_discard = dst_discard.empty()
			? tip.value() == kAce
			: tip.value() - 1 == dst_discard.back().value();
		if (not do_discard) continue;

		dst_discard.push_back(tip);
		s.pop_back();

		try_flip_then_continue(&s - stacks.data(), "discard from stack");

		s.push_back(dst_discard.back());
		dst_discard.pop_back();

		reverted("discard from stack");
	}
}

/// from stack to stack
void try_transfer() {
	for (i64 i_src_stack = 0; i_src_stack < num_stacks; ++i_src_stack) {
		auto& src_hidden = hiddens.at(i_src_stack);
		auto& src_stack = stacks.at(i_src_stack);
		if (src_stack.empty()) continue;
		for (i64 i_dst_stack = 0; i_dst_stack < num_stacks; ++i_dst_stack) {
			if (i_src_stack == i_dst_stack) continue;
			auto& dst_hidden = hiddens.at(i_dst_stack);
			auto& dst_stack = stacks.at(i_dst_stack);
			if (dst_stack.empty()) {
				if (src_hidden.empty() || not dst_hidden.empty() || src_stack.back().value() != kKing) continue;

				dst_stack.push_back(src_stack.back());
				src_stack.pop_back();

				try_flip_then_continue(i_src_stack, "king to empty");

				src_stack.push_back(dst_stack.back());
				dst_stack.pop_back();

				reverted("king to empty");
			} else {
				i64 src_pos = src_stack.front().value() - dst_stack.back().value() + 1;
				if (src_pos < 0 || src_pos >= ssize(src_stack)) continue;
				if (src_stack.at(src_pos).colour() == dst_stack.back().colour()) continue;

				auto num_moved = src_stack.size() - src_pos;
				dst_stack.insert(dst_stack.end(), src_stack.begin() + src_pos, src_stack.end());
				src_stack.erase(src_stack.begin() + src_pos, src_stack.end());

				try_flip_then_continue(i_src_stack, "move stack");

				src_stack.insert(src_stack.end(), dst_stack.end() - num_moved, dst_stack.end());
				dst_stack.erase(dst_stack.end() - num_moved, dst_stack.end());

				reverted("move stack");
			}
		}
	}
}

/// from drawn cards to the stacks
void try_play() {
	if (drawn.empty()) return;

	auto c = drawn.back();
	for (i64 i_stack = 0; i_stack < num_stacks; ++i_stack) {
		auto& stack = stacks.at(i_stack);
		if (c.value() == kKing) {
			if (not stack.empty()) continue;
		} else {
			if (stack.empty()) continue;
			if (stack.back().colour() == c.colour() || stack.back().value() != c.value() + 1) continue;
		}

		stack.push_back(drawn.back());
		drawn.pop_back();

		try_move_if_new_board("play from drawn");

		drawn.push_back(stack.back());
		stack.pop_back();

		reverted("play from drawn");
	}
}

/// from drawn cards to discards
void try_quick_discard() {
	if (drawn.empty()) return;
	auto c = drawn.back();

	auto& dst_discard = discards.at(id(c.suite()));
	const bool do_discard = dst_discard.empty()
		? c.value() == kAce
		: c.value() - 1 == dst_discard.back().value();
	if (not do_discard) return;
	dst_discard.push_back(drawn.back());
	drawn.pop_back();

	try_move_if_new_board("discard from drawn");

	drawn.push_back(dst_discard.back());
	dst_discard.pop_back();

	reverted("discard from drawn");
}

/// draw new cards
void try_draw() {
	if (draw_pile.empty() && drawn.empty()) return;
	bool const do_return = draw_pile.empty();
	auto msg = do_return ? "returned drawn cards" : "drew cards";

	if (do_return) {
		std::swap(draw_pile, drawn);
		std::ranges::reverse(draw_pile);
	}
	auto const num_to_draw = std::min<i64>(ssize(draw_pile), 3);
	for (i64 i = 0; i < num_to_draw; ++i) {
		drawn.push_back(draw_pile.back());
		draw_pile.pop_back();
	}

	try_move_if_new_board(msg);

	for (i64 i = 0; i < num_to_draw; ++i) {
		draw_pile.push_back(drawn.back());
		drawn.pop_back();
	}
	if (do_return) {
		std::swap(drawn, draw_pile);
		std::ranges::reverse(drawn);
	}

	reverted(msg);
}

};
}

int main() { return App::main(); }