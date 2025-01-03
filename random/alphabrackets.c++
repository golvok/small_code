// https://www.0de5.net/stimuli/a-reintroduction-to-programming/instructions-to-languages/grammars-parsing-and-recursive-descent

#include <memory>
#include <charconv>
#include <variant>
#include <vector>
#include <cassert>
#include <expected>

#include <catch2/catch_test_macros.hpp>

struct Parser {
	// Start     ::= Terms
	// Terms     ::= \epsilon | Term+
	// Term      ::= int | Bracketed
	// Bracketed ::= bracketU Terms bracketL
	struct Bracketed;
	using Term = std::variant<
		Bracketed,
		int
	>;
	using Terms = std::vector<Term>;
	struct Bracketed {
		char bracket;
		Terms terms;

		std::weak_ordering operator<=>(Bracketed const&) const = default;
		bool operator==(Bracketed const&) const = default;
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

	static int eval(std::string_view s) {
		auto parse_result = start(s);
		if (not parse_result) {
			throw std::runtime_error("parse fail");
		}
		return eval(parse_result->value, 0);
	}

	static int eval(Terms const& terms, int add_to_each) {
		auto const evalVisitor = [&](auto const& item) { return eval(item, add_to_each); };
		int r = 0;
		for (auto const& term : terms) {
			r += std::visit(evalVisitor, term);
		}
		return r;
	}
	static int eval(Bracketed const& b, int add_to_each) { return eval(b.terms, add_to_each + b.bracket - 'A' + 1); }
	static int eval(int i, int add_to_each) { return i + add_to_each; }

	static ParseResult<Terms> start(std::string_view s) {
		return parseTerms(s);
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
		if (auto i = parseInt(s, 1)) {
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

	static ParseResult<Bracketed> parseBracketed(std::string_view s) {
		if (s.empty()) return std::unexpected(Error{});
		if (std::isalpha(s.front())) {
			auto open = s.front();
			if (not std::isupper(open)) return std::unexpected(Error{});
			if (auto terms = parseTerms(s.substr(1))) {
				auto after_terms = s.substr(1 + terms->chars_consumed);
				if (after_terms.empty()) return std::unexpected(Error{});
				if (after_terms.front() != std::tolower(open)) return std::unexpected(Error{});
				return ParseSuccess<Bracketed>{
					.chars_consumed = 1 + terms->chars_consumed + 1,
					.value = {
						.bracket = open,
						.terms = std::move(terms->value),
					},
				};
			}
		}
		return std::unexpected(Error{});
	}

	static ParseResult<int> parseInt(std::string_view s, int max_digits) {
		ParseResult<int> r = std::unexpected(Error{});
		while (not s.empty() && std::isdigit(s.front())) {
			if (not r)
				r = ParseSuccess<int>{
					.chars_consumed = 0,
					.value = 0,
				};
			if (max_digits == r->chars_consumed)
				break;

			r->value *= 10;
			r->value += s.front() - '0';
			r->chars_consumed += 1;
			s = s.substr(1);
		}
		return r;
	}
};

TEST_CASE("simple") {
	CHECK(Parser::start("Cc") == Parser::ParseSuccess<Parser::Terms>{
		.chars_consumed=2,
		.value={
			Parser::Bracketed{.bracket='C', .terms={}},
		},
	});
	CHECK(Parser::start("C1c") == Parser::ParseSuccess<Parser::Terms>{
		.chars_consumed=3,
		.value={
			Parser::Bracketed{.bracket='C', .terms={1}},
		},
	});
	CHECK(Parser::start("C1Bbc") == Parser::ParseSuccess<Parser::Terms>{
		.chars_consumed=5,
		.value={
			Parser::Bracketed{.bracket='C', .terms={
				1,
				Parser::Bracketed{.bracket='B', .terms={}},
			}},
		},
	});
	CHECK(Parser::start("CB1bc") == Parser::ParseSuccess<Parser::Terms>{
		.chars_consumed=5,
		.value={
			Parser::Bracketed{.bracket='C', .terms={
				Parser::Bracketed{.bracket='B', .terms={1}},
			}},
		},
	});
	CHECK(Parser::start("") == Parser::ParseSuccess<Parser::Terms>{
		.chars_consumed=0,
		.value={},
	});
	CHECK(Parser::start("11") == Parser::ParseSuccess<Parser::Terms>{
		.chars_consumed=2,
		.value={1, 1},
	});

	CHECK(Parser::eval("CA5ac") == 9);
	CHECK(Parser::eval("A9a") == 10);
	CHECK(Parser::eval("ASLHRPCcpAZL98TCNOonctlzarhlsa") == 211);
	CHECK(Parser::eval("AX1AB0YY7QL4J91jlqyyb79WA2GFCcfgM9mawaxS4AVJjNG7gnvasa") == 781);
	CHECK(Parser::eval("ABK35GDB1ZZRKEekrAJMR8884E1AVA6APpIDJGDH9hLITUOCANNLPZ2MTtmzplnnacoutildgjdiHN0nhaRYC7cyravaermjaYAayzzbdgkba") == 1787);
	// CHECK(Parser::eval("AO2o47NCEW3IiABCOMCXV8UJMRrmjuvxcmocJCMmCOCJABBHQKkqhbbGFCFB6J9WWYWwywwjbfcfgajcocI2ZEMTLSQADHCAHY3OUF41SsfuoGO7MNFfnmoTY4AGOQEeVJGgJUHTBAGT0IEXX0LETAXVOovxatBLlbelx9XAGBUN2SJC8AFIVvif8MCY6DDEWNA4ZCULDRVOI5CL4CTANSDSIOoisdsnaRPCA23C5AacacprtclcVCCCG5gcccviovrdlYAVFI7GCCSRQMCQC8KZBbzkcqcmqrsccgifvayucOOXSsxoozV7GAagva2YAaynwedKAWUAG8CLLllcgauwakdycmaBRLBZ3VO2JXJ4DCE7AFW1wZAEeazfaeWW11CJjcwwcdjxjXLZzlYATJQS6XBS0sbxsqjtayxovzblMXSKCcAAaaLJ1KXESHGPGBA05IRN7HBOobIUDBNnbdLEBF9CB1Y1QPBbpqybcfbeluHZFBTALIMCBbcmi2XPCOoKDCU4ucdkcpxlatbfPpzhihnria0DOCcodbgpghsexkjlks7KTBCYJOOJ7EWQ8BXBCH8LIilhcbxb1U6AB8AXUIAHhaiuxabauqwejoojyRCPIN4O59onCNPDPCJ6B9DdbjcpdpHBA0BA2aZZN9AHBADJYLCclyjdCcabhY1T3ZSTOCXL5MTUJCcjuT1HYZ6YCBCQ9PBZGBEebgzL3YAN8XRrxnaylbpAKKkkaqcbcyzyhttmlxcoUG9ZMHhmzg7FD1XMmxdfutszPptyanzzbabIG8LIL5LJCJDUMQCcqmudjcjllilgCVKCCYE3KRBACVNVPpvnvcabrkeycckvciAahncipcrcbtkxmrbcjsnubgaxxeU8uitgabthujjvqoga3yBHhbtgyhachdaqsltmezicjbawecna") == 0x9de5);
}
