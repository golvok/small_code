#include <algorithm>
#include <array>
#include <cassert>
#include <iostream>
#include <map>
#include <random>
#include <set>
#include <span>
#include <unordered_map>
#include <unordered_set>
#include <vector>

using std::vector;
using std::ostream;
using std::cout;

using i64 = std::int64_t;
using u64 = std::uint64_t;

namespace {
struct App {

struct Tableau;
struct Card;
enum Suit : std::uint8_t;
enum Value : std::int8_t;

static int main(std::span<std::string_view> args) {
	App app;
	u64 seed = args.size() < 2 ? std::random_device{}() : std::stoull(std::string(args[1]));
	return app.solve(seed) ? 0 : 1;
}

bool solve(u64 seed) {
	std::vector<Card> deck;
	for (auto s : {kDiamonds, kClubs, kHearts, kSpades}) {
		for (auto v = kAce; v <= kKing; v = Value(v + 1)) {
			deck.push_back({s, v});
		}
	}

	cout << "seed = " << seed << std::endl;
	auto g  = std::mt19937{seed};
	std::ranges::shuffle(deck, g);

	for (i64 i_stack = 0; i_stack != num_stacks; ++i_stack) {
		for (i64 i_draw = 0; i_draw != i_stack; ++i_draw) {
			hiddens[i_stack].push_back(deck.back());
			deck.pop_back();
		}
		stacks[i_stack].push_back(deck.back());
		deck.pop_back();
	}
	std::ranges::copy(deck, std::back_inserter(draw_pile));

	dump();
	cout.flush();
	visited.reserve(1'200'000); // eg. for seed 3660738044 with kKing = 9, num_stacks = 7

	bool solved = false;
	try {
		try_move("start", true);
	} catch (std::exception const& e) {
		if (std::string_view(e.what()) != "solved!") throw;
		solved = true;
	}

	return solved;
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
	std::size_t operator()(Tableau const& tableau) const {
		std::size_t hash = 0;
		std::size_t next_bits = 0;
		i64 num_cards_in_next_bits = 0;
		i64 next_offset = 0;
		constexpr i64 bits_in_card_data = 4 + 2;
		constexpr i64 max_cards_in_next_bits = 64 / bits_in_card_data; // 6
		constexpr i64 unused_next_bits_bits = 64 - bits_in_card_data - max_cards_in_next_bits; // 4
		auto update = [&](Card c) {
			next_bits <<= 2;
			next_bits |= id(c.suit());
			next_bits <<= 4;
			next_bits |= c.value();
			++num_cards_in_next_bits;
			if (num_cards_in_next_bits == max_cards_in_next_bits) {
				hash ^= (next_bits << next_offset);
				next_offset = (next_offset + 1) % unused_next_bits_bits;
				next_bits = 0;
				num_cards_in_next_bits = 0;
			}
		};

		std::ranges::for_each(tableau.draw_pile, update);
		std::ranges::for_each(tableau.drawn, update);
		// hash ^= tableau.drawn.size();
		for (auto& s : tableau.hiddens) std::ranges::for_each(s, update);
		for (auto& s : tableau.stacks) std::ranges::for_each(s, update);
		std::ranges::for_each(tableau.discards, update);

		hash ^= next_bits;
		return hash;
	}
};

struct TableauEqualer {
	bool operator()(Tableau const& lhs, Tableau const& rhs) const {
		// return
		// 	lhs.drawn.size() == rhs.drawn.size() &&
		// 	lhs.draw_pile == rhs.draw_pile &&
		// 	lhs.hiddens == rhs.hiddens &&
		// 	lhs.stacks == rhs.stacks &&
		// 	lhs.discards == rhs.discards;
		return lhs == rhs;
	}
};

using Visited = std::unordered_set<Tableau, TableauHasher, TableauEqualer>;
static inline Visited& visited = *new Visited{};

void try_move(std::string_view msg, bool check_unique) {
	if (check_unique) {
		if (not visited.insert(tableau).second) return;
	}
	log("new tableau from: ", msg);
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

void reverted(std::string_view msg) {
	log("reverted: ", msg);
}

/// flip hidden cards into stacks
void try_flip_then_continue(i64 i_stack, std::string_view msg, bool check_unique) {
	auto& hidden = hiddens.at(i_stack);
	auto& stack = stacks.at(i_stack);
	bool const do_flip = not hidden.empty() && stack.empty();

	if (do_flip) {
		stack.push_back(hidden.back());
		hidden.pop_back();
	}

	try_move(msg, check_unique);

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
		auto& dst_discard = discards.at(id(tip.suit()));
		if (tip.value() - 1 != dst_discard.value()) continue;

		dst_discard._value = static_cast<Value>(dst_discard._value + 1);
		s.pop_back();

		try_flip_then_continue(&s - stacks.data(), "discard from stack", true);

		s.push_back(dst_discard);
		dst_discard._value = static_cast<Value>(dst_discard._value - 1);

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

				try_flip_then_continue(i_src_stack, "king to empty", true);

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

				try_flip_then_continue(i_src_stack, "move stack", true);

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

		try_move("play from drawn", true);

		drawn.push_back(stack.back());
		stack.pop_back();

		reverted("play from drawn");
	}
}

/// from drawn cards to discards
void try_quick_discard() {
	if (drawn.empty()) return;
	auto c = drawn.back();

	auto& dst_discard = discards.at(id(c.suit()));
	if (c.value() - 1 != dst_discard.value()) return;

	dst_discard._value = static_cast<Value>(dst_discard._value + 1);
	drawn.pop_back();

	try_move("discard from drawn", true);

	drawn.push_back(dst_discard);
	dst_discard._value = static_cast<Value>(dst_discard._value - 1);

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

	try_move(msg, true);

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

enum Colour { kRed = 0, kBlack = 1 };
enum Suit : std::uint8_t {
	// even <=> red
	kDiamonds = 0,
	kClubs    = 1,
	kHearts   = 2,
	kSpades   = 3,
};
enum Value : std::int8_t { kBeforeAce = 0, kAce = 1, kKing = 13 };

static size_t id(Suit s) { return s; }

struct Card {
	Suit _suit;
	Value _value;
	Colour colour() const { return Colour(_suit & 0b1); }
	Suit suit() const { return _suit; }
	Value value() const { return _value; }

	friend ostream& operator<<(ostream& os, Card const& c) {
		constexpr std::array<char, 4> suit_letter{'D', 'C', 'H', 'S'};
		return os << suit_letter[c.suit()] << '-' << int(c.value());
	}

	auto operator<=>(Card const&) const = default;
};

template<typename T, std::size_t kMax>
struct SmallVec {
	using value_type = T;

	std::size_t _size = 0;
	std::array<T, kMax> _storage{};

	void push_back(T t) { _storage[_size] = t; ++_size; assert(_size <= kMax); }
	void pop_back() { --_size; }
	T const* data() const { return _storage.data(); }
	T*       data()       { return _storage.data(); }
	T const* begin() const { return _storage.data(); }
	T*       begin()       { return _storage.data(); }
	T const* end() const { assert(_size <= kMax); return begin() + _size; }
	T*       end()       { assert(_size <= kMax); return begin() + _size; }
	std::size_t size() const { return _size; }
	i64 ssize() const { assert(_size <= kMax); return _size; }
	bool empty() const { return _size == 0; }
	T const& front() const { return *begin(); }
	T&       front()       { return *begin(); }
	T const& back() const { assert(_size <= kMax); return *(end() - 1); }
	T&       back()       { assert(_size <= kMax); return *(end() - 1); }
	T& at(std::size_t i) { assert(i < _size); return _storage[i]; }
	T& operator[](std::size_t i) { assert(i < _size); return _storage[i]; }

	T* erase(T* from, T* to) {
		std::copy(to, end(), from);
		_size -= to - from;
		assert(_size <= kMax);
		return from + 1;
	}

	T* insert(T* here, T const* b, T const* e) {
		auto n_elem = e - b;
		std::copy(here, end(), here + n_elem);
		std::copy(b, e, here);
		_size += n_elem;
		assert(_size <= kMax);
		return here;
	}

	friend i64 ssize(const SmallVec& sv) { return sv.ssize(); }

	std::span<T const> asSpan() const { return std::span(begin(), end()); }

	bool operator==(const SmallVec& rhs) const {
		return size() == rhs.size() && std::ranges::equal(asSpan(), rhs.asSpan());
	}
};

using DrawPile = SmallVec<Card, 24>;
// using DrawPile = vector<Card>;
using Hidden = SmallVec<Card, 7>;
// using Hidden = vector<Card>;
using Stack = SmallVec<Card, kKing - kAce + 1>;
// using Stack = vector<Card>;

using Hiddens = SmallVec<Hidden, 7>;
// using Hiddens = vector<Hidden>;
using Stacks = SmallVec<Stack, 7>;
// using Stacks = vector<Stack>;
using Discards = std::array<Card, 4>;

struct Tableau {
	static constexpr i64 num_stacks = 7;
	static constexpr i64 num_draws = 3;

	DrawPile draw_pile{};
	DrawPile drawn{};
	Hiddens hiddens{unsigned(num_stacks)};
	Stacks stacks{unsigned(num_stacks)};
	Discards discards{
		Card{kDiamonds, kBeforeAce},
		Card{kClubs, kBeforeAce},
		Card{kHearts, kBeforeAce},
		Card{kSpades, kBeforeAce},
	};

	auto operator<=>(Tableau const&) const = default;
};


friend ostream& operator<<(ostream& os, vector<Card> const& vc) {
	os << '[';
	for (auto const& c : vc) {
		os << c << ' ';
	}
	return os << '(' << vc.size() << ")]";
}

template<std::size_t N>
friend ostream& operator<<(ostream& os, std::array<Card, N> const& vc) {
	os << '[';
	for (auto const& c : vc) {
		os << c << ' ';
	}
	return os << '(' << vc.size() << ")]";
}

template<std::size_t N>
friend ostream& operator<<(ostream& os, SmallVec<Card, N> const& vc) {
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

template<std::size_t N>
friend ostream& operator<<(ostream& os, vector<SmallVec<Card, N>> const& vvc) {
	os << "[\n";
	for (auto const& vc : vvc) {
		os << "  " << vc << '\n';
	}
	return os << ']';
}

template<std::size_t N, std::size_t M>
friend ostream& operator<<(ostream& os, SmallVec<SmallVec<Card, M>, N> const& vvc) {
	os << "[\n";
	for (auto const& vc : vvc) {
		os << "  " << vc << '\n';
	}
	return os << ']';
}

Tableau tableau{};

i64 const& num_stacks = tableau.num_stacks;
i64 const& num_draws = tableau.num_draws;
DrawPile& draw_pile = tableau.draw_pile;
DrawPile& drawn = tableau.drawn;
Hiddens& hiddens = tableau.hiddens;
Stacks& stacks = tableau.stacks;
Discards& discards = tableau.discards;

};
}

int main(int argc, char* argv[]) {
	std::vector<std::string_view> args;
	for (int i = 0; i < argc; ++i) {
		args.push_back(argv[i]);
	}
	return App::main(args);
}
