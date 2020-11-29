#include <iostream>
#include <string>
#include <vector>

#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/spirit/home/x3.hpp>
#include <boost/spirit/home/x3/support/ast/variant.hpp>

namespace x3 = boost::spirit::x3;
namespace chars = boost::spirit::x3::ascii;

namespace ast {

	struct Expression;

	struct Primary : x3::variant<
		std::string,
		x3::forward_ast<Expression>
	> {

		// GCC seems to require these to be defined. possible compiler bug.
		Primary& operator=(const Primary&) = default;
		Primary(const Primary&) = default;
		Primary() = default;

		using variant::variant;
		using variant::operator=;
	};

	using ArgumentList = std::vector<Primary>;

	struct Expression {
		std::string func_name;
		ArgumentList arguments;
		Expression() : func_name(), arguments() { }
	};

}

BOOST_FUSION_ADAPT_STRUCT(
	ast::Expression,
	func_name,
	arguments
)

namespace parser {
	x3::rule<class Primary,      ast::Primary     > Primary      = "Primary";
	x3::rule<class Expression,   ast::Expression  > Expression   = "Expression";

	const auto function_name = x3::lexeme[ +chars::alnum ];
	const auto number = x3::lexeme[ +chars::digit ];
	const auto Primary_def = number | Expression | ( x3::lit('(') > Primary > ')' );
	const auto Expression_def = function_name > +Primary;

	BOOST_SPIRIT_DEFINE(Primary, Expression)
}

struct PrefixASTPrettyPrinter {
	int level;
	std::ostream& os;

	PrefixASTPrettyPrinter(int level, std::ostream& os) : level(level), os(os) { }

	void operator()(const ast::Expression& expr) const {
		print_indent();
		os << expr.func_name << '\n';

		for (const auto& p : expr.arguments) {
			boost::apply_visitor(PrefixASTPrettyPrinter(level + 1,os), p);
		}
	}
	
	void operator()(const std::string& primary_string) const {
		print_indent();
		os << primary_string << '\n';
	}

	void print_indent() const {
		for (int i = 0; i < level; ++i) {
			os << ' ';
		}
	}
};

enum class Modes {
	PREFIX,
	COLON_SEPARATED,
};

std::string getline(std::istream& is) {
	std::string str;
	std::getline(is, str);
	return str;
}

int main() {

	const auto choice_line = getline(std::cin);
	const auto mode = [&](char choice) {
		if (choice == 'c') return Modes::COLON_SEPARATED;
		if (choice == 'p') return Modes::PREFIX;
		throw std::runtime_error("choose c or p, for COLON_SEPARATED or PREFIX, respectively, please.");
	}(choice_line.at(0));

	switch (mode) {
		case Modes::PREFIX:
			std::cout << "in mode PREFIX\n";
			break;
		case Modes::COLON_SEPARATED:
			std::cout << "in mode COLON_SEPARATED\n";
			break;
	}

	while (std::cin.good() == true) {

		const auto line = getline(std::cin);
		auto iter_in_line = begin(line);
		const auto end_of_line = end(line);

		switch (mode) {
		case Modes::PREFIX: {
			ast::Primary root_primary;

			const bool is_match = phrase_parse(iter_in_line, end_of_line,
				parser::Primary,
				chars::space,
				root_primary
			);

			if (is_match) { // } && iter_in_line == end_of_line) {
				boost::apply_visitor(PrefixASTPrettyPrinter(0, std::cout), root_primary);
			}

			} break;
		case Modes::COLON_SEPARATED: {
			const auto identifier = x3::lexeme[ !chars::char_("0-9") >> +( chars::char_("0-9a-zA-Z_") ) ];
			// also works:
			// const auto identifier = x3::raw[x3::lexeme[ +( chars::alpha | x3::char_('_') ) >> *( chars::alnum | x3::char_('_') ) ]];

			std::vector<std::string> objs;
			const bool is_match = x3::phrase_parse( iter_in_line, end_of_line,
				+( identifier >> ';' ),
				chars::space,
				objs
			);

			if (is_match) {
				for (auto& obj : objs) {
					std::cout << "obj:" << obj << '\n';
				}
			}

			} break;
		}
	}
}