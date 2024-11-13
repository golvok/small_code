#include <memory>
#include <charconv>
#include <variant>
#include <vector>
#include <cassert>
#include <expected>

struct Parser {
	// Start     ::= Terms
	// Terms     ::= \epsilon | Term+
	// Term      ::= int | Bracketed
	// Bracketed ::= bracketU Terms bracketL
	struct Bracketed;
	struct BoxedBracketed {
		BoxedBracketed(Bracketed b) : m_data(std::make_unique<Bracketed>(std::move(b))) {}
		BoxedBracketed(BoxedBracketed const& src) : m_data(std::make_unique<Bracketed>(*src)) {}
		BoxedBracketed(BoxedBracketed&&) = default;
		BoxedBracketed& operator=(BoxedBracketed const& rhs) { **this = *rhs; return *this; }
		BoxedBracketed& operator=(BoxedBracketed&&) = default;

		std::weak_ordering operator<=>(BoxedBracketed const& rhs) const { return cmpBracketed(**this, *rhs); }
		Bracketed& operator*() { assert(m_data); return *m_data.get(); }
		Bracketed const& operator*() const { assert(m_data); return *m_data.get(); }
		Bracketed* operator->() { return &*m_data; }
		Bracketed const* operator->() const { return &*m_data; }
	private:
		std::unique_ptr<Bracketed> m_data;
	};
	using Term = std::variant<
		BoxedBracketed,
		int
	>;
	using Terms = std::vector<Term>;
	struct Bracketed {
		char bracket;
		Terms terms;

		auto operator<=>(Bracketed const&) const = default;
		friend std::weak_ordering cmpBracketed(Bracketed const& lhs, Bracketed const& rhs) {
			return lhs <=> rhs;
		}
	};

	struct Error {};

	template<typename TValue>
	struct ParseSuccess {
		int chars_consumed;
		TValue value;

		auto operator<=>(ParseSuccess const&) const = default;
	};

	template<typename TValue>
	using ParseResult = std::expected<ParseSuccess<TValue>, Error>;

	static ParseResult<Bracketed> start(std::string_view s) {
		if (auto terms = parseTerms(s))
			return ParseSuccess<Bracketed>{
				.chars_consumed = terms->chars_consumed,
				.value = {
					.bracket = 'A' - 1,
					.terms = std::move(terms->value),
				},
			};
		return std::unexpected(Error{});
	}

	static ParseResult<Terms> parseTerms(std::string_view s) {
		if (auto t1 = parseTerm(s)) {
			if (auto t_rest = parseTerms(s.substr(t1->chars_consumed))) {
				t_rest->value.insert(t_rest->value.begin(), std::move(t1->value));
				return ParseSuccess<Terms>{
					.chars_consumed = t1->chars_consumed + t_rest->chars_consumed,
					.value = std::move(t_rest->value),
				};
			}
			return ParseSuccess<Terms>{
				.chars_consumed = t1->chars_consumed,
				.value = {std::move(t1->value)},
			};
		} else {
			return ParseSuccess<Terms>{
				.chars_consumed = 0,
				.value = Terms{}
			};
		}
	}

	static ParseResult<Term> parseTerm(std::string_view s) {
		if (auto i = parseInt(s)) {
			return ParseSuccess<Term>{
				.chars_consumed = i->chars_consumed,
				.value = i->value,
			};
		} else if (auto b = parseBracketed(s)) {
			return ParseSuccess<Term>{
				.chars_consumed = b->chars_consumed,
				.value = std::move(b->value),
			};
		}
		return std::unexpected(Error{});
	}

	static ParseResult<Term> parseBracketed(std::string_view s) {
		if (s.empty()) return std::unexpected(Error{});
		if (std::isalpha(s.front())) {
			auto open = s.front();
			if (not std::isupper(open)) return std::unexpected(Error{});
			if (auto terms = parseTerms(s.substr(1))) {
				auto after_terms = s.substr(1 + terms->chars_consumed);
				if (after_terms.empty()) return std::unexpected(Error{});
				if (after_terms.front() != std::tolower(open)) return std::unexpected(Error{});
				return ParseSuccess<Term>{
					.chars_consumed = 1 + terms->chars_consumed + 1,
					.value = BoxedBracketed({
						.bracket = open,
						.terms = std::move(terms->value),
					}),
				};
			}
		}
		return std::unexpected(Error{});
	}

	static ParseResult<int> parseInt(std::string_view s) {
		ParseResult<int> r = std::unexpected(Error{});
		while (not s.empty() && std::isdigit(s.front())) {
			if (not r)
				r = ParseSuccess<int>{
					.chars_consumed = 0,
					.value = 0,
				};

			r->value *= 10;
			r->value += s.front() - '0';
			r->chars_consumed += 1;
			s = s.substr(1);
		}
		return r;
	}
};

int main() {
	Parser::start("Cc");
	Parser::start("C1c");
	Parser::start("C1Bbc");
	Parser::start("C1Bbc");
	Parser::start("");
}