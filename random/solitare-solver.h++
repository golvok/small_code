#include <fmt/format.h>

#include <algorithm>
#include <array>
#include <cassert>
#include <iostream>
#include <map>
#include <optional>
#include <random>
#include <ranges>
#include <set>
#include <span>
#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "signal.h"

using std::cout;
using std::optional;
using std::ostream;
using std::vector;

using i64 = std::int64_t;
using u64 = std::uint64_t;

namespace golvok::solitare {

auto is_empty = [](auto& v) { return v.empty(); };
auto get_size = [](auto& v) { return ssize(v); };

bool gPrintAsCxx = false;

struct App {

struct Tableau;
struct Card;
enum Suit : std::uint8_t;
enum Value : std::int8_t;

static int main(std::span<std::string_view> args) {
	u64 seed = (args.size() <= 1 || args[1] == "-") ? std::random_device{}() : std::stoull(std::string(args[1]));
	u64 king = args.size() <= 2 ? 13 : std::stoull(std::string(args[2]));
	i64 num_draws = args.size() <= 3 ? 3 : std::stoull(std::string(args[3]));
	i64 num_stacks = args.size() <= 4 ? 7 : std::stoull(std::string(args[4]));
	i64 verbose = args.size() <= 5 ? 0 : std::stoull(std::string(args[5]));
	auto app = App(verbose, king, num_draws, num_stacks);
	return app.solve(seed) ? 0 : 1;
}

i64 verbose;
Value kKing;
i64 num_draws;
i64 num_stacks;

bool enable_new_opt = true;
bool enable_new_state_code = false;
bool enable_detect_loops = false;

App(i64 verbose, int kKing, int num_draws, int num_stacks)
	: verbose(verbose)
	, kKing(static_cast<Value>(kKing))
	, num_draws(num_draws)
	, num_stacks(num_stacks)
{
	if (this->kKing < kAce || this->kKing > kMaxKing) throw std::logic_error("invalid kKing");
	if (num_stacks < 1 || num_stacks > kMaxStacks) throw std::logic_error("invalid num_stacks");
	init_strings();
}

void seed_tableau(u64 seed) {
	std::vector<Card> deck;
	for (auto s : {kDiamonds, kClubs, kHearts, kSpades}) {
		for (auto v = kAce; v <= kKing; v = Value(v + 1)) {
			deck.push_back({s, v});
		}
	}

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

	// Start with all cards drawn, so we can init `drawn_are_fresh = false`.
	// We want to prevent drawing the last few cards when they are fresh (this is always a loop)
	//   but, if we init `drawn_are_fresh = true` here, then, if we must play the last card first, we won't get the chance.
	// This also does the optimization of making drawing cards the first move, since you must do that at some point.
	std::ranges::copy(deck | std::views::reverse, std::back_inserter(drawn));
	drawn_are_fresh = false;
}

bool solve(std::optional<u64> seed) {
	fmt::println("kKing={} ns={} nd={} seed={}", (int)kKing, num_stacks, num_draws, seed ? std::to_string(*seed) : "(manual init)");
	fmt::println("enable_new_opt={} enable_new_state_code={} find_new_nodes={}", enable_new_opt, enable_new_state_code, find_new_nodes);

	if (seed)
		seed_tableau(*seed);
	if (verbose)
		dump();
	cout.flush();
	visited.back().reserve(4'000'000); // eg. for seed 424626366 with kKing = 9, num_stacks = 7, num_draws = 3

	bool solved = false;
	try {
		try_move("start", TryMoveOpts{});
	} catch (std::exception const& e) {
		if (std::string_view(e.what()) != "solved!") throw;
		solved = true;
	}

	if (verbose) {
		fmt::println("no. examined unique nodes: {}", visited.back().size());

		std::map<i64, i64> visit_freqs;
		for (auto const& layer : visited) {
			for (auto const& v : layer) ++visit_freqs[v.second];
		}
		for (auto& vf : visit_freqs) {
			fmt::println("  {:2} {}",  vf.first, vf.second);
		}

		fmt::println("max_depth={} last_depth={}", max_depth, curr_depth);
		fmt::println("solved={}", solved);
	}
	return solved;
}

struct TryMoveOpts {
	bool check_unique = true;
	optional<i64> next_play_must_be_on_or_from_stack = std::nullopt;
	optional<i64> if_transfer_is_next_must_be_from_stack = std::nullopt;
	optional<Suit> discarding_here_is_also_allowed = std::nullopt;
	bool do_king_sort = false;
};

i64 find_solutions = 1;
i64 find_new_nodes = 1;

void try_move(std::string_view msg, TryMoveOpts opts) {
	if (maybe_sort_kings_then_continue(msg, opts)) { // this branch is a bit perf-hurting
		return;
	}

	if (break_on_tableau == tableau) raise(SIGTRAP);

	if ((draw_pile.size() + drawn.size()) <= 1 && std::ranges::all_of(hiddens, is_empty)) {
		if (--find_solutions == 0) {
			if (verbose >= 3)
				dump_parents();
			log(1, "solved: ", msg);
			throw std::runtime_error("solved!");
		}
	}

	bool old_exploring_new_states = exploring_new_states;
	if (opts.check_unique) {
		auto [lookup, new_item] = visited.back().try_emplace(tableau, 0);
		auto& [key, num_visits] = *lookup;
		if (enable_new_state_code) {
			if (exploring_new_states && not new_item)
				return;
			if (not exploring_new_states && new_item) {
				// std::cout << "\n\nexploring_new_states\n";
				// dump_n_parents(5);
				std::cout << "exploring_new_states depth=" << curr_depth << '\n';
				exploring_new_states = true;
				if (--find_new_nodes == 0) {
					find_solutions = 1;
				}
			}
		}
		++num_visits;

		if (enable_detect_loops){
			const auto parent_ptrs = parents | std::views::drop(1) | std::ranges::views::transform([](auto& e) { return e.second; });
			const bool is_loop = std::ranges::find(parent_ptrs, &key) != parent_ptrs.end();
			if (is_loop) {
				dump_parents();
				log(0, "loop from: ", msg);
				throw std::runtime_error("loop");
			}
		}

		auto total_num_visits = num_visits;
		if (visited.size() != 1) {
			total_num_visits = 0;
			for (auto const& vis_map : visited) {
				auto sub_lookup = vis_map.find(tableau);
				if (sub_lookup == vis_map.end()) continue;
				total_num_visits += sub_lookup->second;
			}
		}
		if (total_num_visits > 1) return;
		parents.emplace_back(msg, &key); // DANGER: storing string_view... for speed

	}
	made_a_move.back() = true;
	made_a_move.push_back(false);
	if (max_depth < curr_depth) {
		max_depth = curr_depth;
		// log(0, "new depth reached");
	}
	auto hidden_sizes = hiddens | std::views::transform(get_size);
	auto num_hiddens = std::accumulate(hidden_sizes.begin(), hidden_sizes.end(), i64{0});
	if (num_hiddens < min_hiddens) {
		min_hiddens = num_hiddens;
		// log(0, "new min hiddens");
	}
	log(4, "new tableau from: ", msg);
	++curr_depth;
	std::exception_ptr exc;
	try {
		if (not opts.next_play_must_be_on_or_from_stack)
			try_quick_discard(opts.discarding_here_is_also_allowed);
		try_discard(opts.next_play_must_be_on_or_from_stack, opts.discarding_here_is_also_allowed);
		try_transfer(opts.next_play_must_be_on_or_from_stack, opts.if_transfer_is_next_must_be_from_stack);
		try_play(opts.next_play_must_be_on_or_from_stack);
		try_draw(opts.next_play_must_be_on_or_from_stack, opts.if_transfer_is_next_must_be_from_stack);
	} catch (...) {
		exc = std::current_exception();
	}
	--curr_depth;
	made_a_move.pop_back();
	if (opts.check_unique) {
		if (exc && exploring_new_states && not old_exploring_new_states) {
			std::cout << "\n\nsolved new states (2nd last is the new one)\n";
			dump_n_parents(7);
		}
		parents.pop_back();
		exploring_new_states = old_exploring_new_states;
	}
	if (exc) std::rethrow_exception(exc);
}

void reverted(std::string_view msg) {
	log(4, "reverted: ", msg);
}

/// flip hidden cards into stacks
void try_flip_then_continue(i64 i_stack, std::string_view msg, TryMoveOpts opts) {
	auto& hidden = hiddens.at(i_stack);
	auto& stack = stacks.at(i_stack);

	bool const do_flip = not hidden.empty() && stack.empty();
	if (do_flip) {
		stack.push_back(hidden.back());
		hidden.pop_back();
		if (hidden.empty())
			opts.if_transfer_is_next_must_be_from_stack = std::nullopt;
	}

	opts.do_king_sort |= hidden.empty() || (stack.front().value() == kKing);
	try_move(msg, opts);

	if (do_flip) {
		hidden.push_back(stack.back());
		stack.pop_back();
	}
}

/** Sort king-headed and empty stacks by suit (empty stacks last). Leaves other stacks alone. */
bool maybe_sort_kings_then_continue(std::string_view msg, TryMoveOpts opts) {
	// return false;
	if (not opts.do_king_sort) {
		return false;
	}
	opts.do_king_sort = false;  // don't want infinite recursion...

	auto king_stack_copies = SmallVec<Stack, kMaxStacks>{};
	auto sorted_king_stacks = SmallVec<Stack*, kMaxStacks>{};

	auto orig_king_indices = SmallVec<i64, kMaxStacks>{};
	for (i64 i_stack = 0; i_stack < num_stacks; ++i_stack) {
		if (not hiddens.at(i_stack).empty()) continue;
		auto& stack = stacks.at(i_stack);
		if (stack.empty() || stack.front().value() == kKing) {
			sorted_king_stacks.push_back(&stack);
			orig_king_indices.push_back(i_stack);
		}
	}

	auto const sort_by_king_suit = [](auto& lhs, auto& rhs) {  // king is front card
		return (lhs->empty() ? Suit{100} : lhs->at(0).suit()) < (rhs->empty() ? Suit{100} : rhs->at(0).suit());
	};

	std::ranges::stable_sort(sorted_king_stacks, sort_by_king_suit);

	for (auto& ks : sorted_king_stacks) {
		king_stack_copies.push_back(*ks);
	}

	for (i64 i_ks = 0; i_ks < sorted_king_stacks.ssize(); ++i_ks) {
		stacks.at(orig_king_indices[i_ks]) = king_stack_copies[i_ks];

		i64 const new_index = sorted_king_stacks[i_ks] - stacks.data();
		if (new_index == opts.if_transfer_is_next_must_be_from_stack) {
			opts.if_transfer_is_next_must_be_from_stack = orig_king_indices[i_ks];
		}
		if (new_index == opts.next_play_must_be_on_or_from_stack) {
			opts.next_play_must_be_on_or_from_stack = orig_king_indices[i_ks];
		}
	}
	try_move(msg, opts);

	for (i64 i_ks = 0; i_ks < sorted_king_stacks.ssize(); ++i_ks) {
		*sorted_king_stacks[i_ks] = king_stack_copies[i_ks];
	}

	return true;
}

/// stack up from aces
void try_discard(optional<i64> next_play_must_be_on_or_from_stack, optional<Suit> or_can_discard_here) {
	if (next_play_must_be_on_or_from_stack) {
		try_discard_from_stack(*next_play_must_be_on_or_from_stack, std::nullopt);
		if (not or_can_discard_here) return;
	}

	for (i64 i_stack = 0; i_stack < num_stacks; ++i_stack) {
		try_discard_from_stack(i_stack, or_can_discard_here);
	}
}

/// stack up from aces from a specific stack
void try_discard_from_stack(i64 i_stack, optional<Suit> must_discard_here) {
	auto& s = stacks[i_stack];
	if (s.empty()) return;

	auto tip = s.back();
	if (must_discard_here && *must_discard_here != tip.suit()) return;

	auto& dst_discard = discards.at(id(tip.suit()));
	if (tip.value() - 1 != dst_discard.value()) return;

	dst_discard._value = static_cast<Value>(dst_discard._value + 1);
	s.pop_back();

	try_flip_then_continue(&s - stacks.data(), discard_from_stack_strings[i_stack][tip], TryMoveOpts{
		.next_play_must_be_on_or_from_stack = i_stack,
		.discarding_here_is_also_allowed = tip.suit(),
	});

	s.push_back(dst_discard);
	dst_discard._value = static_cast<Value>(dst_discard._value - 1);

	reverted(discard_from_stack_strings[i_stack][tip]);
}

/// from stack to stack
void try_transfer(optional<i64> next_play_must_be_on_or_from_stack, optional<i64> if_transfer_is_next_must_be_from_stack) {
	for (i64 i_src_stack = 0; i_src_stack < num_stacks; ++i_src_stack) {
		auto& src_hidden = hiddens.at(i_src_stack);
		auto& src_stack = stacks.at(i_src_stack);
		if (src_stack.empty()) continue;
		auto const src_tip = src_stack.back(); // copy for safety

		if (src_stack.front().value() == kKing && src_hidden.empty()) continue;  // kings get stuck if hiddens is empty

		// don't transfer if the src tip can be safely discarded -- rely on some other call to try_discard to come first.
		// safely discarded: opposite-colored cards that could be placed on the tip card can be discarded (or have been).
		static_assert(kNumSuits == 4, "assumed 4 suits below");
		auto const min_opposite_color_discard = std::min(discards[(src_tip.suit() + 1) % kMaxSuit].value(), discards[(src_tip.suit() + 3) % kMaxSuit].value());
		auto const can_discard_src_tip = discards[src_tip.suit()].value() + 1 == src_tip.value();
		auto const can_safely_discard_src_tip = min_opposite_color_discard + 2 >= src_tip.value();
		if (can_discard_src_tip && can_safely_discard_src_tip) continue;

		auto is_king_and_found_empty_dst = false;
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

			auto& dst_stack = stacks.at(i_dst_stack);
			i64 src_pos;
			if (dst_stack.empty()) {
				src_pos = 0;
				if (src_stack.front().value() != kKing) continue; // can only move kings to empty stacks
				if (is_king_and_found_empty_dst) continue;
			} else {
				src_pos = src_stack.front().value() - dst_stack.back().value() + 1;
				if (src_pos < 0 || src_pos >= ssize(src_stack)) continue;
				if (src_stack.at(src_pos).colour() == dst_stack.back().colour()) continue;
			}

			auto num_transferred = src_stack.size() - src_pos;
			auto const move_head_card = src_stack.at(src_pos);
			dst_stack.insert(dst_stack.end(), src_stack.begin() + src_pos, src_stack.end());
			src_stack.erase(src_stack.begin() + src_pos, src_stack.end());

			try_flip_then_continue(i_src_stack, transfer_strings[i_src_stack][i_dst_stack][move_head_card], {
				.next_play_must_be_on_or_from_stack = (not src_hidden.empty() && src_pos == 0) ? std::nullopt : std::make_optional(i_src_stack),
				.if_transfer_is_next_must_be_from_stack = src_pos == 0 ? std::nullopt : std::make_optional(i_src_stack),
				.do_king_sort = move_head_card.value() == kKing,
			});

			src_stack.insert(src_stack.end(), dst_stack.end() - num_transferred, dst_stack.end());
			dst_stack.erase(dst_stack.end() - num_transferred, dst_stack.end());

			reverted(transfer_strings[i_src_stack][i_dst_stack][move_head_card]);

			// If we found a place to put a king, then don't consider any more empty stacks
			// This seems to be the only worth-while condition to check. For example, single-card stacks can only be moved to at most 2 places,
			//   (in general n-card stacks to n*2 places), but this doesn't seem to save time.
			if (src_pos == 0 && src_stack.front().value() == kKing)
				is_king_and_found_empty_dst = true;
		}
	}
}

/// from drawn cards to the stacks
void try_play(optional<i64> next_play_must_be_on_or_from_stack) {
	if (drawn.empty()) return;

	if (next_play_must_be_on_or_from_stack)
		return (void)try_play(*next_play_must_be_on_or_from_stack);

	for (i64 i_stack = 0; i_stack < num_stacks; ++i_stack) {
		if (not try_play(i_stack)) break;
	}
}

// from drawn cards to a particular stack
bool try_play(i64 i_stack) {
	if (drawn.empty()) return true;
	auto c = drawn.back();

	auto& stack = stacks.at(i_stack);
	if (c.value() == kKing) {
		if (not stack.empty()) return true;
	} else {
		if (stack.empty()) return true;
		if (stack.back().colour() == c.colour() || stack.back().value() != c.value() + 1) return true;
	}

	stack.push_back(drawn.back());
	drawn.pop_back();
	bool old_drawn_are_fresh = false;
	std::swap(old_drawn_are_fresh, drawn_are_fresh);

	try_move(play_strings[i_stack][c], TryMoveOpts{
		.do_king_sort = c.value() == kKing,
	});

	std::swap(drawn_are_fresh, old_drawn_are_fresh);
	drawn.push_back(stack.back());
	stack.pop_back();

	reverted(play_strings[i_stack][c]);

	return c.value() != kKing; // no need to consider further empty spaces
}

/// from drawn cards to discards
void try_quick_discard(optional<Suit> must_discard_here) {
	if (drawn.empty()) return;
	auto c = drawn.back();
	if (must_discard_here && *must_discard_here != c.suit()) return;

	auto& dst_discard = discards.at(id(c.suit()));
	if (c.value() - 1 != dst_discard.value()) return;

	dst_discard._value = static_cast<Value>(dst_discard._value + 1);
	drawn.pop_back();
	bool old_drawn_are_fresh = false;
	std::swap(old_drawn_are_fresh, drawn_are_fresh);

	try_move(quick_discard_strings[c], TryMoveOpts{});

	std::swap(drawn_are_fresh, old_drawn_are_fresh);
	drawn.push_back(dst_discard);
	dst_discard._value = static_cast<Value>(dst_discard._value - 1);

	reverted(quick_discard_strings[c]);
}

void divergence_test(bool do_test, auto base_func, auto new_func) {
	if (not do_test) {
		base_func();
		return;
	}

	auto restore = state;
	++divergence_test_count;
	visited.emplace_back();

	std::exception_ptr new_exc;
	try {
		new_func();
	} catch (...) {
		// raise(SIGTRAP);
		new_exc = std::current_exception();
		state = restore;
		visited.back().clear();
	}
	std::exception_ptr base_exc;
	try {
		base_func();
	} catch (...) {
		// raise(SIGTRAP);
		base_exc = std::current_exception();
	}

	--divergence_test_count;
	state = restore;
	visited.pop_back();

	if (bool{base_exc} != bool{new_exc}) {
		cout << "divergence!\n";
		dump_n_parents(2);
		cout.flush();
		raise(SIGTRAP);
		new_func();
	}
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
	auto const num_to_draw = std::min<i64>(ssize(draw_pile), num_draws);
	if (drawn_are_fresh && num_to_draw == ssize(draw_pile)) {
		return;
	}
	for (i64 i = 0; i < num_to_draw; ++i) {
		drawn.push_back(draw_pile.back());
		draw_pile.pop_back();
	}

	try_move(msg, {
		.next_play_must_be_on_or_from_stack = next_play_must_be_on_or_from_stack,
		.if_transfer_is_next_must_be_from_stack = if_transfer_is_next_must_be_from_stack
	});

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

static constexpr int64_t kNumSuits = 4;

enum Colour { kRed = 0, kBlack = 1 };
enum Suit : std::uint8_t {
	// even <=> red
	kMinSuit  = 0,
	kDiamonds = kMinSuit,
	kCoins    = kDiamonds,
	kClubs    = 1,
	kHearts   = 2,
	kCups     = kHearts,
	kSpades   = 3,
	kMaxSuit  = kSpades,
};
static_assert(kMaxSuit + 1 == kNumSuits);
enum Value : std::int8_t { kBeforeAce = 0, kAce = 1, kMaxKing = 13 };

static size_t id(Suit s) { return s; }

struct Card {
	Suit _suit;
	Value _value;
	Colour colour() const { return Colour(_suit & 0b1); }
	Suit suit() const { return _suit; }
	Value value() const { return _value; }

	friend ostream& operator<<(ostream& os, Card const& c) {
		if (gPrintAsCxx) {
			constexpr auto suit_str = std::array<char const*, kNumSuits>{"App::kDiamonds", "App::kClubs", "App::kHearts", "App::kSpades"};
			return os << '{' << suit_str[c.suit()] << ", App::Value{" << i64{c.value()} << "}}";
		} else {
			constexpr auto suit_letter = std::array<char, kNumSuits>{'D', 'C', 'H', 'S'};
			return os << suit_letter[c.suit()] << '_' << i64{c.value()};
		}
	}

	friend auto format_as(Card const& card) { return (std::stringstream() << card).str(); }

	auto operator<=>(Card const&) const = default;
};

template<typename T, std::size_t kMax>
class SmallVec {
public:
	using value_type = T;

	constexpr SmallVec() : SmallVec(std::size_t{0}) {}
	constexpr explicit SmallVec(std::size_t size) : _size(size) {}
	constexpr explicit SmallVec(i64 size) : _size(static_cast<std::size_t>(size)) {}
	constexpr SmallVec(std::initializer_list<T> il) : SmallVec() { for (auto& e : il) push_back(e); }

	void push_back(T t) { assert(_size != kMax); _storage[_size] = t; ++_size; }
	void pop_back() { assert(_size != 0); --_size; }
	T const* data() const { return _storage.data(); }
	T*       data()       { return _storage.data(); }
	T const* begin() const { return _storage.data(); }
	T*       begin()       { return _storage.data(); }
	T const* end() const { return begin() + _size; }
	T*       end()       { return begin() + _size; }
	std::size_t size() const { return _size; }
	i64 ssize() const { return _size; }
	bool empty() const { return _size == 0; }
	T const& front() const { assert(_size != 0); return *begin(); }
	T&       front()       { assert(_size != 0); return *begin(); }
	T const& back() const { assert(_size != 0); return *(end() - 1); }
	T&       back()       { assert(_size != 0); return *(end() - 1); }
	T&       at(std::size_t i)       { assert(i < _size); return _storage[i]; }
	T const& at(std::size_t i) const { assert(i < _size); return _storage[i]; }
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
private:
	std::size_t _size;
	std::array<T, kMax> _storage{};
};

static constexpr i64 kMaxStacks = 7;
using DrawPile = SmallVec<Card, 24>;
using Hidden = SmallVec<Card, kMaxStacks>;
using Stack = SmallVec<Card, kMaxKing - kAce + 1>;

using Hiddens = SmallVec<Hidden, kMaxStacks>;
using Stacks = SmallVec<Stack, kMaxStacks>;
using Discards = std::array<Card, kNumSuits>;

struct Tableau {
	Hiddens hiddens;
	Stacks stacks;
	DrawPile draw_pile{};
	DrawPile drawn{};
	Discards discards{
		Card{kDiamonds, kBeforeAce},
		Card{kClubs, kBeforeAce},
		Card{kHearts, kBeforeAce},
		Card{kSpades, kBeforeAce},
	};

	/** has a card not been played or discarded from the drawn cards since the last return to the draw pile */
	bool drawn_are_fresh = false;

	auto operator<=>(Tableau const&) const = default;
};


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
		hash ^= tableau.drawn_are_fresh;
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

using Visited = std::vector<std::unordered_map<Tableau, i64, TableauHasher, TableauEqualer>>;
using Parents = std::vector<std::pair<std::string_view, Tableau const*>>;

/** Not reset during exception throws -- caller must save & restore. */
struct State {
	Tableau tableau;

	i64 max_depth = 0;
	i64 curr_depth = 0;
	i64 min_hiddens = 100000;

	auto operator<=>(State const&) const = default;
};

/** Reset during exception throws -- caller does not have to save */
struct ManualState {
	Visited visited = {{}};

	Parents parents = {};
	std::vector<bool> made_a_move = {false};
	i64 divergence_test_count = 0;
	bool exploring_new_states = false;

	auto operator<=>(ManualState const&) const = default;
};

State state {
	Tableau{
		.hiddens = Hiddens(num_stacks),
		.stacks = Stacks(num_stacks),
	},
};
ManualState manual_state{};

Tableau& tableau = state.tableau;
i64& max_depth = state.max_depth;
i64& curr_depth = state.curr_depth;
i64& min_hiddens = state.min_hiddens;

bool& drawn_are_fresh = tableau.drawn_are_fresh;
DrawPile& draw_pile = tableau.draw_pile;
DrawPile& drawn = tableau.drawn;
Hiddens& hiddens = tableau.hiddens;
Stacks& stacks = tableau.stacks;
Discards& discards = tableau.discards;

Visited& visited = manual_state.visited;
Parents& parents = manual_state.parents;
std::vector<bool>& made_a_move = manual_state.made_a_move;
i64& divergence_test_count = manual_state.divergence_test_count;
bool& exploring_new_states = manual_state.exploring_new_states;

std::optional<Tableau> break_on_tableau = std::nullopt;

friend ostream& operator<<(ostream& os, vector<Card> const& vc) {
	os << '{';
	auto sep = "";
	for (auto const& c : vc) {
		os << sep << c;
		sep = gPrintAsCxx ? ", " : " ";
	}
	if (not gPrintAsCxx and vc.size() > 4)
		os << sep << '(' << vc.size() << ")";
	return os << "}";
}

template<std::size_t N>
friend ostream& operator<<(ostream& os, std::array<Card, N> const& vc) {
	os << (gPrintAsCxx ? "{{" : "{");
	auto sep = "";
	for (auto const& c : vc) {
		os << sep << c;
		sep = gPrintAsCxx ? ", " : " ";
	}
	if (not gPrintAsCxx and vc.size() > 6)
		os << sep << '(' << vc.size() << ")";
	return os << (gPrintAsCxx ? "}}" : "}");
}

template<std::size_t N>
friend ostream& operator<<(ostream& os, SmallVec<Card, N> const& vc) {
	os << '{';
	auto sep = "";
	for (auto const& c : vc) {
		os << sep << c;
		sep = gPrintAsCxx ? ", " : " ";
	}
	if (not gPrintAsCxx and vc.size() > 4)
		os << sep << '(' << vc.size() << ")";
	return os << "}";
}

friend ostream& operator<<(ostream& os, vector<vector<Card>> const& vvc) {
	os << "{";
	auto sep = "\n";
	auto const indent = gPrintAsCxx ? "\t" : "  ";
	for (auto const& vc : vvc) {
		os << sep << indent << vc;
		sep = gPrintAsCxx ? ",\n" : "\n";
	}
	return os << sep << '}';
}

template<std::size_t N>
friend ostream& operator<<(ostream& os, vector<SmallVec<Card, N>> const& vvc) {
	os << "{";
	auto sep = "\n";
	auto const indent = gPrintAsCxx ? "\t" : "  ";
	for (auto const& vc : vvc) {
		os << sep << indent << vc;
		sep = gPrintAsCxx ? ",\n" : "\n";
	}
	return os << sep << '}';
}

template<std::size_t N, std::size_t M>
friend ostream& operator<<(ostream& os, SmallVec<SmallVec<Card, M>, N> const& vvc) {
	os << "{";
	auto sep = "\n";
	auto const indent = gPrintAsCxx ? "\t" : "  ";
	int64_t i = -1;
	for (auto const& vc : vvc) {
		++i;
		auto num = gPrintAsCxx ? "" : (std::to_string(i) + ": ");
		os << sep << indent << num << vc;
		sep = gPrintAsCxx ? ",\n" : "\n";
	}
	return os << sep << '}';
}

void dump() const { dump_tableau(tableau); }

static void dump_tableau(Tableau const& t) {
	auto const sep = gPrintAsCxx ? ",\n" : "\n";
	auto const prefix = gPrintAsCxx ? "." : "";
	cout << prefix << "hiddens = " << t.hiddens << sep;
	cout << prefix << "stacks = " << t.stacks << sep;
	cout << prefix << "draw_pile = " << t.draw_pile << sep;
	cout << prefix << "drawn = " << t.drawn << sep;
	cout << prefix << "discards = " << t.discards << sep;
	cout << prefix << "drawn_are_fresh = " << t.drawn_are_fresh << sep;
}

void log(i64 log_level, std::string_view msg1, std::string_view msg2 = "") {
	if (verbose < log_level) return;
	cout << "\ndepth=" << curr_depth << " " << msg1 << msg2 << '\n';
	dump();
}

static void dump_parents(Parents const& parents) {
	for (auto& p : parents) {
		std::cout << p.first << '\n';
		dump_tableau(*p.second);
		std::cout << '\n';
	}
}
void dump_parents() const { dump_parents(parents); }
void dump_n_parents(i64 n) const { dump_parents({parents.begin() + std::max(ssize(parents), n) - n, parents.end()}); }

template<typename T>
struct CardIndexedVector {
	SmallVec<T, (kMaxKing - kBeforeAce + 1) * kNumSuits> data = {};

	T& operator[](Card c) { return data[(c.value() - kBeforeAce) * kNumSuits + +c.suit()]; }
	T const& operator[](Card c) const { return data[(c.value() - kBeforeAce) * kNumSuits + +c.suit()]; }

	template<typename F>
	static CardIndexedVector FromEachCard(F&& f) {
		CardIndexedVector result;
		for (auto value = kBeforeAce; value <= kMaxKing; value = static_cast<Value>(+value + 1)) {
			for (auto suit = kMinSuit; suit <= kMaxSuit; suit = static_cast<Suit>(+suit + 1)) {
				result.data.push_back(f(Card{suit, value}));
			}
		}
		return result;
	}
};

std::vector<std::vector<CardIndexedVector<std::string>>> transfer_strings = {};
std::vector<CardIndexedVector<std::string>> play_strings = {};
CardIndexedVector<std::string> quick_discard_strings = {};
std::vector<CardIndexedVector<std::string>> discard_from_stack_strings = {};

void init_strings() {
	for (int i_stack = 0; i_stack < num_stacks; ++i_stack) {
		transfer_strings.emplace_back();
		for (int i_dst_stack = 0; i_dst_stack < num_stacks; ++i_dst_stack) {
			transfer_strings.back().emplace_back() = transfer_strings.back().back().FromEachCard([&](Card card) {
				return fmt::format("transfer {} stack {} -> {}", card, i_stack, i_dst_stack);
			});
		}

		play_strings.emplace_back() = play_strings.back().FromEachCard([&](Card card) {
			return fmt::format("play {} from drawn to {}", card, i_stack);
		});

		discard_from_stack_strings.emplace_back() = discard_from_stack_strings.back().FromEachCard([&](Card card) {
			return fmt::format("discard {} from stack {}", card, i_stack);
		});
	}

	quick_discard_strings = quick_discard_strings.FromEachCard([](Card card) {
		return fmt::format("discard {} from drawn", card);
	});
}

};
}
