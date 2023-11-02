#include "solitare-solver.h++"

#include <catch2/catch.hpp>

TEST_CASE("single test") {
	char const* args_var = std::getenv("SINGLE_TEST_ARGS");
	if (not args_var) {
		SUCCEED();
		return;
	}

	std::vector<std::string_view> args;
	args.push_back("solitare-solver"); // prog name
	// split on spaces, combining. C-style.. for 'fun'
	for (char const* curr = args_var; *curr;) {
		while (*curr && *curr == ' ')
			++curr;
		auto arg_start = curr;
		while (*curr && *curr != ' ')
			++curr;
		if (*arg_start) {
			args.emplace_back(arg_start, curr - arg_start);
		}
	}

	App::main(args);
}

TEST_CASE("king-stack canonicalization") {
	auto app = App(3, 3, 1, 5);
	app.tableau = App::Tableau {
		.hiddens = {
			{},
			{},
			{{App::kSpades, App::Value{3}}},
			{{App::kHearts, App::Value{3}}},
			{},
		},
		.stacks =  {
			{},
			{{App::kDiamonds, App::Value{3}}},
			{{App::kDiamonds, App::Value{2}}},
			{{App::kHearts, App::Value{2}}},
			{},
		},
		.draw_pile = {{App::kHearts, App::Value{6}}},
		.drawn = {},
		.discards = {{
			{App::kDiamonds, App::Value{1}},
			{App::kClubs, App::Value{3}},
			{App::kHearts, App::Value{1}},
			{App::kSpades, App::Value{2}},
		}},
	};


	app.enable_new_opt = true;
	CHECK(app.solve(std::nullopt));
}

TEST_CASE("known seeds") {
	auto kKing = 9;
	auto num_draws = 3;
	auto num_stacks = 7;
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
		P{523130630, true},
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
		P{2588666873, true},
		P{1736492770, true},
		P{1855606254, false},
		P{4002761587, true},
		P{404880233, true},
		P{951565461, true},
		P{2436201585, true},
		P{2530822378, true},
		P{3729536514, false},
		P{1031335289, true},
		P{3438837208, true},
		P{1848988431, true},
		P{44449863, true},
		P{1860381114, true},
		P{4270170060, true},
		P{2559051306, true},
		P{3482059390, true},
		P{3539763307, true},
		P{4138387265, false},
		P{2805736436, false},
		P{123803911, true},
		P{2751335957, true},
		P{66806515, false},
		P{3839093309, true},
		P{1807416663, true},
		P{878236126, false},
		P{1774459184, true},
		P{1126380761, false},
		P{492877971, true},
		P{3497816086, true},
		P{1137821295, true},
		P{3915577236, false},
		P{2987851817, true},
		P{2755249545, true},
		P{995862560, true},
		P{770131165, true},
		P{6318243, false},
		P{1171949367, false},
		P{3405431933, true},
		P{734874366, true},
		P{2422620042, false},
		P{3567987200, true},
		P{4186175811, false},
		P{2879808721, true},
		P{1531278541, true},
		P{1204947057, true},
		P{1273716065, true},
		P{621893820, false},
		P{1498510505, true},
		P{270519824, false},
		P{199704550, false},
		P{2452329633, false},
		P{570641825, true},
		P{2489300905, true},
		P{2295276590, true},
		P{529527685, true},
		P{662627631, true}, // ref data said unsolvable, but solution checks out
		P{1502826548, true},
		P{3604988811, true},
		P{2840218531, false},
		P{3891259777, false},
		P{841170194, true},
		P{3079848152, false},
		P{2635799374, true},
		P{1569146925, true},
		P{1326750147, false},
		P{1592535874, true},
		P{3551108118, true},
		P{780810771, true},
		P{341515073, true},
		P{2736250806, true},
		P{373581916, true},
		P{1155005635, false},
		P{2948703850, true},
		P{2645767828, true},
		P{947046604, false},
		P{4149763088, true},
		P{3086260904, true},
		P{4166405817, true},
		P{3365327026, true},
		P{4027369171, true},
		P{1383654926, true},
		P{815786869, true},
		P{1519984203, false},
		P{470041533, false},
		P{1070904146, false},
		P{3450840801, false},
		P{3435312903, false},
		P{657007104, true},
		P{2126732655, true},
		P{720413696, false},
		P{2139915373, true},
		P{1169256844, true},
		P{4626581, true},
		P{3259493865, true},
		P{2961294640, true},
		P{3492032682, true},
		P{1693707493, true},
		P{3311172364, true},
		P{631647572, true},
		P{2465848241, true},
		P{1487941069, true},
		P{4231426531, false},
		P{2072463795, true},
		P{793465435, true},
		P{139706714, false},
		P{1056694444, true},
		P{2380075234, true},
		P{1266679021, true},
		P{3501506564, false},
		P{4187813663, false},
		P{2699165452, true},
		P{3115812345, true},
		P{3125468079, true},
		P{3244930766, true},
		P{2599873592, false},
		P{1541636361, true},
		P{3864411488, false},
		P{519863112, false},
		P{3915383323, true},
		P{3574666364, true},
		P{1130489614, true},
		P{3445043447, true},
		P{2328986456, false},
		P{1163055420, true},
		P{3014089307, true},
		P{626151084, false},
		P{20456398, false},
		P{2650038383, false},
		P{424626366, false}, // big
		P{1063134063, true},
		P{643952142, false},
		P{3555435236, true},
		P{3556036429, true}, // big, ref data said unsolvable, but solution checks out
		P{2136481655, true},
		P{1468096988, false},
		P{2427493840, true},
		P{3347806600, true},
		P{4293197769, true},
		P{362462603, false},
		P{4189922425, false},
		P{210463051, true},
		P{1240251650, false},
		P{3287078413, true},
		P{1664325137, true},
	};

	for (auto const& test : tests) {
		App app(false, kKing, num_draws, num_stacks);
		CHECK(app.solve(test.seed) == test.solveable);
	}
}
