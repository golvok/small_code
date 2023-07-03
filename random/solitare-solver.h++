#include <algorithm>
#include <array>
#include <cassert>
#include <iostream>
#include <map>
#include <optional>
#include <random>
#include <set>
#include <span>
#include <unordered_map>
#include <unordered_set>
#include <vector>

using std::cout;
using std::optional;
using std::ostream;
using std::vector;

using i64 = std::int64_t;
using u64 = std::uint64_t;

namespace {
struct App {

struct Tableau;
struct Card;
enum Suit : std::uint8_t;
enum Value : std::int8_t;

static int main(std::span<std::string_view> args) {
	u64 seed = args.size() < 2 ? std::random_device{}() : std::stoull(std::string(args[1]));
	u64 king = args.size() < 3 ? 13 : std::stoull(std::string(args[2]));
	auto app = App(true, king);
	return app.solve(seed) ? 0 : 1;
}

Value kKing;
int verbose;

App(int verbose, int kKing)
	: kKing(static_cast<Value>(kKing))
	, verbose(verbose)
{
	if (this->kKing < kAce || this->kKing > kMaxKing) throw std::logic_error("invalid kKing");
}

bool solve(u64 seed) {
	std::vector<Card> deck;
	for (auto s : {kDiamonds, kClubs, kHearts, kSpades}) {
		for (auto v = kAce; v <= kKing; v = Value(v + 1)) {
			deck.push_back({s, v});
		}
	}

	cout << "kKing=" << (int)kKing << " ns=" << num_stacks << " nd=" << num_draws << " seed=" << seed << std::endl;
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

	if (verbose)
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

	if (verbose)
		std::cout << "no. examined unique nodes: " << visited.size() << std::endl;

	// std::unordered_map<i64, i64> visit_freqs;
	// for (auto const& v : visited) {
	// 	++visit_freqs[v.second];
	// }
	// for (auto& vf : visit_freqs) {
	// 	std::cout << vf.first << ' ' << vf.second << '\n';
	// }

	if (verbose) {
		std::cout << "max_depth=" << max_depth << " last_depth=" << curr_depth << '\n';
		std::cout << "solved=" << solved << std::endl;
	}
	return solved;
}

void dump() const { dump_tableau(tableau); }

static void dump_tableau(Tableau const& t) {
	cout << "draw_pile=" << t.draw_pile << '\n';
	cout << "drawn=" << t.drawn << '\n';
	cout << "hiddens=" << t.hiddens << '\n';
	cout << "stacks=" << t.stacks << '\n';
	cout << "discards=" << t.discards << '\n';
}

void log(std::string_view msg1, std::string_view msg2 = "") {
	(void)msg1, (void)msg2;
	// always_log(msg1, msg2);
}

void always_log(std::string_view msg1, std::string_view msg2 = "") {
	cout << "\n" << msg1 << msg2 << '\n';
	dump();
}

void dump_parents() {
	for (auto& p : parents) {
		std::cout << p.first << '\n';
		dump_tableau(*p.second);
		std::cout << '\n';
	}
}

struct TableauHasher {
	std::size_t operator()(Tableau const& tableau) const {
		std::size_t hash = 0;
		std::size_t next_bits = 0;
		i64 num_cards_in_next_bits = 0;
		i64 next_offset = 0;
		constexpr i64 bits_in_card_data = 4 + 2;
		constexpr i64 max_cards_in_next_bits = 64 / bits_in_card_data;
		constexpr i64 unused_next_bits_bits = 64 - bits_in_card_data * max_cards_in_next_bits;
		static_assert(max_cards_in_next_bits == 10, "sanity check");
		static_assert(unused_next_bits_bits == 4, "sanity check");
		auto update = [&](Card c) {
			next_bits <<= 2;
			next_bits |= id(c.suit());
			next_bits <<= 4;
			next_bits |= c.value();
			++num_cards_in_next_bits;
			if (num_cards_in_next_bits == max_cards_in_next_bits) {
				hash ^= (next_bits << next_offset);
				if constexpr (unused_next_bits_bits != 0)
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

using Visited = std::unordered_map<Tableau, i64, TableauHasher, TableauEqualer>;
Visited visited = {};

i64 max_depth = 0;
i64 curr_depth = 0;
std::vector<std::pair<std::string_view, Tableau const*>> parents = {{"base", &tableau}};

/** has a card not been played or discareded from the drawn cards since the last return to the draw pile */
bool drawn_are_fresh = true;

struct TryMoveOpts {
	bool check_unique = true;
	optional<i64> next_play_must_be_on_or_from_stack = std::nullopt;
	optional<i64> if_transfer_is_next_must_be_from_stack = std::nullopt;
};

void try_move(std::string_view msg, bool check_unique) { return try_move(msg, {.check_unique = check_unique}); }

void try_move(std::string_view msg, TryMoveOpts opts) {
	opts.next_play_must_be_on_or_from_stack = std::nullopt;
	// opts.if_transfer_is_next_must_be_from_stack = std::nullopt;
	if (opts.check_unique) {
		auto& kv = *visited.try_emplace(tableau, 0).first;
		auto const& lookup = kv.first;
		auto& num_visits = kv.second;
		// if (parents.back() == &lookup) {
			// throw std::runtime_error("loop");
		// }
		++num_visits;
		if (num_visits != 1) return;
		parents.emplace_back(msg, &lookup); // DANGER: storing string_view... for speed

	}
	if (max_depth < curr_depth) {
		max_depth = curr_depth;
		// always_log("new depth reached");
	}
	log("new tableau from: ", msg);
	auto is_empty = [](auto& v) { return v.empty(); };
	if (draw_pile.empty() && drawn.empty() && std::ranges::all_of(hiddens, is_empty)) {
		cout.flush();
		throw std::runtime_error("solved!");
	}
	++curr_depth;
	try_discard(opts.next_play_must_be_on_or_from_stack);
	try_transfer(opts.next_play_must_be_on_or_from_stack, opts.if_transfer_is_next_must_be_from_stack);
	try_play(opts.next_play_must_be_on_or_from_stack);
	if (not opts.next_play_must_be_on_or_from_stack)
		try_quick_discard();
	try_draw(opts.next_play_must_be_on_or_from_stack, opts.if_transfer_is_next_must_be_from_stack);
	--curr_depth;
	if (opts.check_unique) parents.pop_back();
}

void reverted(std::string_view msg) {
	log("reverted: ", msg);
}

void try_flip_then_continue(i64 i_stack, std::string_view msg, bool check_unique) { return try_flip_then_continue(i_stack, msg, {.check_unique = check_unique}); }

/// flip hidden cards into stacks
void try_flip_then_continue(i64 i_stack, std::string_view msg, TryMoveOpts opts) {
	auto& hidden = hiddens.at(i_stack);
	auto& stack = stacks.at(i_stack);
	bool const do_flip = not hidden.empty() && stack.empty();

	if (do_flip) {
		stack.push_back(hidden.back());
		hidden.pop_back();
	}
	if (hidden.empty())
		opts.if_transfer_is_next_must_be_from_stack = std::nullopt;

	try_move(msg, opts);

	if (do_flip) {
		hidden.push_back(stack.back());
		stack.pop_back();
	}
}


/// stack up from aces
void try_discard(optional<i64> next_play_must_be_on_or_from_stack) {
	if (next_play_must_be_on_or_from_stack)
		return try_discard_from_stack(*next_play_must_be_on_or_from_stack);

	for (i64 i_stack = 0; i_stack < num_stacks; ++i_stack) {
		try_discard_from_stack(i_stack);
	}
}

/// stack up from aces from a specific stack
void try_discard_from_stack(i64 i_stack) {
	auto& s = stacks[i_stack];
	if (s.empty()) return;

	auto tip = s.back();
	auto& dst_discard = discards.at(id(tip.suit()));
	if (tip.value() - 1 != dst_discard.value()) return;

	dst_discard._value = static_cast<Value>(dst_discard._value + 1);
	s.pop_back();

	try_flip_then_continue(&s - stacks.data(), "discard from stack", true);

	s.push_back(dst_discard);
	dst_discard._value = static_cast<Value>(dst_discard._value - 1);

	reverted("discard from stack");
}

/// from stack to stack
void try_transfer(optional<i64> next_play_must_be_on_or_from_stack, optional<i64> if_transfer_is_next_must_be_from_stack) {
	for (i64 i_src_stack = 0; i_src_stack < num_stacks; ++i_src_stack) {
		auto& src_hidden = hiddens.at(i_src_stack);
		auto& src_stack = stacks.at(i_src_stack);
		if (src_stack.empty()) continue;
		for (i64 i_dst_stack = 0; i_dst_stack < num_stacks; ++i_dst_stack) {
			if (if_transfer_is_next_must_be_from_stack && not (
				   *if_transfer_is_next_must_be_from_stack == i_src_stack
				|| *if_transfer_is_next_must_be_from_stack != i_dst_stack
			)) continue;
			if (next_play_must_be_on_or_from_stack && not (
				   *next_play_must_be_on_or_from_stack == i_src_stack
				|| *next_play_must_be_on_or_from_stack == i_dst_stack
			)) continue;
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
				break; // don't look at other empty slots -- it's pointless (symmetric)
			} else {
				i64 src_pos = src_stack.front().value() - dst_stack.back().value() + 1;
				if (src_pos < 0 || src_pos >= ssize(src_stack)) continue;
				if (src_stack.at(src_pos).colour() == dst_stack.back().colour()) continue;

				auto num_transferred = src_stack.size() - src_pos;
				dst_stack.insert(dst_stack.end(), src_stack.begin() + src_pos, src_stack.end());
				src_stack.erase(src_stack.begin() + src_pos, src_stack.end());

				try_flip_then_continue(i_src_stack, "transfer stack", {.next_play_must_be_on_or_from_stack = i_src_stack, .if_transfer_is_next_must_be_from_stack = i_src_stack});

				src_stack.insert(src_stack.end(), dst_stack.end() - num_transferred, dst_stack.end());
				dst_stack.erase(dst_stack.end() - num_transferred, dst_stack.end());

				reverted("transfer stack");
				break; // there is at most one place to transfer a stack
			}
		}
	}
}

/// from drawn cards to the stacks
void try_play(optional<i64> next_play_must_be_on_or_from_stack) {
	if (drawn.empty()) return;

	if (next_play_must_be_on_or_from_stack)
		return try_play(*next_play_must_be_on_or_from_stack);

	for (i64 i_stack = 0; i_stack < num_stacks; ++i_stack) {
		try_play(i_stack);
	}
}

// from drawn cards to a particular stack
void try_play(i64 i_stack) {
	if (drawn.empty()) return;
	auto c = drawn.back();

	auto& stack = stacks.at(i_stack);
	if (c.value() == kKing) {
		if (not stack.empty()) return;
	} else {
		if (stack.empty()) return;
		if (stack.back().colour() == c.colour() || stack.back().value() != c.value() + 1) return;
	}

	stack.push_back(drawn.back());
	drawn.pop_back();
	bool old_drawn_are_fresh = false;
	std::swap(old_drawn_are_fresh, drawn_are_fresh);

	try_move("play from drawn", true);

	std::swap(drawn_are_fresh, old_drawn_are_fresh);
	drawn.push_back(stack.back());
	stack.pop_back();

	reverted("play from drawn");
}

/// from drawn cards to discards
void try_quick_discard() {
	if (drawn.empty()) return;
	auto c = drawn.back();

	auto& dst_discard = discards.at(id(c.suit()));
	if (c.value() - 1 != dst_discard.value()) return;

	dst_discard._value = static_cast<Value>(dst_discard._value + 1);
	drawn.pop_back();
	bool old_drawn_are_fresh = false;
	std::swap(old_drawn_are_fresh, drawn_are_fresh);

	try_move("discard from drawn", true);

	std::swap(drawn_are_fresh, old_drawn_are_fresh);
	drawn.push_back(dst_discard);
	dst_discard._value = static_cast<Value>(dst_discard._value - 1);

	reverted("discard from drawn");
}

/// draw new cards
void try_draw(optional<i64> next_play_must_be_on_or_from_stack, optional<i64> if_transfer_is_next_must_be_from_stack) {
	if (draw_pile.empty() && drawn.empty()) return;
	bool const do_return = draw_pile.empty();
	auto msg = do_return ? "returned drawn cards" : "drew cards";

	bool old_drawn_are_fresh = true;
	if (do_return) {
		if (drawn_are_fresh) return; // require doing something with at least one card per return
		if (ssize(drawn) <= num_draws) return; // don't bother returning card when the only cards are already drawn
		std::swap(old_drawn_are_fresh, drawn_are_fresh);
		std::swap(draw_pile, drawn);
		std::ranges::reverse(draw_pile);
	}
	auto const num_to_draw = std::min<i64>(ssize(draw_pile), 3);
	for (i64 i = 0; i < num_to_draw; ++i) {
		drawn.push_back(draw_pile.back());
		draw_pile.pop_back();
	}

	try_move(msg, {.next_play_must_be_on_or_from_stack = next_play_must_be_on_or_from_stack, .if_transfer_is_next_must_be_from_stack = if_transfer_is_next_must_be_from_stack});

	for (i64 i = 0; i < num_to_draw; ++i) {
		draw_pile.push_back(drawn.back());
		drawn.pop_back();
	}
	if (do_return) {
		std::swap(drawn, draw_pile);
		std::ranges::reverse(drawn);
		std::swap(drawn_are_fresh, old_drawn_are_fresh);
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
enum Value : std::int8_t { kBeforeAce = 0, kAce = 1, kMaxKing = 13 };

static size_t id(Suit s) { return s; }

struct Card {
	Suit _suit;
	Value _value;
	Colour colour() const { return Colour(_suit & 0b1); }
	Suit suit() const { return _suit; }
	Value value() const { return _value; }

	friend ostream& operator<<(ostream& os, Card const& c) {
		constexpr std::array<char, 4> suit_letter{'D', 'C', 'H', 'S'};
		return os << suit_letter[c.suit()] << '_' << int(c.value());
	}

	auto operator<=>(Card const&) const = default;
};

template<typename T, std::size_t kMax>
struct SmallVec {
	using value_type = T;

	std::size_t _size = 0;
	std::array<T, kMax> _storage{};

	void push_back(T t) { assert(_size != kMax); _storage[_size] = t; ++_size; }
	void pop_back() { assert(_size != 0); --_size; }
	T const* data() const { return _storage.data(); }
	T*       data()       { return _storage.data(); }
	T const* begin() const { return _storage.data(); }
	T*       begin()       { return _storage.data(); }
	T const* end() const { assert(_size <= kMax); return begin() + _size; }
	T*       end()       { assert(_size <= kMax); return begin() + _size; }
	std::size_t size() const { return _size; }
	i64 ssize() const { return _size; }
	bool empty() const { return _size == 0; }
	T const& front() const { assert(_size != 0); return *begin(); }
	T&       front()       { assert(_size != 0); return *begin(); }
	T const& back() const { assert(_size != 0); return *(end() - 1); }
	T&       back()       { assert(_size != 0); return *(end() - 1); }
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
using Stack = SmallVec<Card, kMaxKing - kAce + 1>;
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
