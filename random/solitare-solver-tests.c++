#include "solitare-solver.h++"

#include <catch2/catch.hpp>

TEST_CASE("known seeds") {
	auto kKing = 9;
	struct P {
		i64 seed; bool solveable;
	};
	auto const tests = std::array{
		P{2666202671, false},
		P{1775214411, true},
		P{1722148151, true},
		P{127553069, true},
		P{3387246126, true},
		P{4056369853, false},
		P{523130630, false},
		P{2202261056, true},
		P{3446463334, true},
		P{1283600659, true},
		P{3886464777, true},
		P{2102469055, false},
		P{2749342037, true},
		P{3860398048, true},
		P{1304683684, true},
		P{479526959, true},
		P{2417108915, true},
		P{2581539543, true},
		P{3481201843, false},
		P{4103510507, true},
		P{1535489698, true},
		P{2229168896, false},
		P{2737320624, true},
		P{2998160689, true},
		P{4084008763, true},
	};

	for (auto const& test : tests) {
		App app(kKing);
		CHECK(app.solve(test.seed) == test.solveable);
	}
}