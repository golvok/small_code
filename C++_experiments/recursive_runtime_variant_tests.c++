#include "recursive_runtime_variant.h++"

#include <catch2/catch.hpp>

#include <iostream>
#include <sstream>

/*
	Goals:
		Two separate uses:
			"metadata"
				- tagging arbitrary, nested, type-erased data onto things
				- debug info, metadata
				- wants to pull types out as they are... except vector, map, etc?
					- ie NodeConcrete \is NodeValue
				- scalar types are good enough (don't care about others)
			"configuration"
				- nested, type-erased configuration
				- config files, function parameters
				- wants access into everything
					- ie NodeConcrete \in { NodeValue, NodeReference, NodeIndirectAccess, ... }
				- doesn't want scalar types. Could add a bool template param to Node, which propagates down to requiring !Scalar<T>
		Assign-through
			- prefer copy-assignment over copy-construction
			- more like assigning to data members of an object
			- some references *won't* be invalidated
				- invalidates? Assign with dict over dict: not root, but yes to all non-matching sub-objects
					- we assign through for matching keys
				- invalidates? Assign with dict over type: yes
				- invalidates? Assign with same type: no (different from std::any)
					- any uses swap for exception-safe self-assignment-safe assignment
				- invalidates? Assign with different type: yes
	Next:
		- avoid member lookup when iterating by passing returned MemberIterator to rrvMember?
			- types probably shouldn't store actual iterators to avoid invalidation? or say screw it, and just use existing rules for type(s)
		- non-string_view arguments for operator[]?
			- maybe take a variant of a couple stdlib types - long long, string_view
			- YAML does it by taking a Node as as argument... could do this too
				- getMember will have to convert -- make sure to catch exceptions from it to add context
			- Is it possible to use an overload set of rrvMember? - library automatically converts int<->string if only one
		- setMember. Allows returning const& from getMember(s)?
		   - can't: Node::get returns a plain reference
		     - return NodeConcrete<T>&?
		- Convert NodeValue, RodeReference NodeIndirectAcess to static polymorphism
		- make classes final
		- 'at' throw if not exist
		- member not found error constant
		- Catch exceptions to add context. Eg. what member names are being accessed
		- moving from a NodeReference seems sketchy (tryAssign, clone)
		- can cut-down vtables by having a single virtual method with a dispatch enum
			- hard to avoid completely, since destructor doesn't know type -- unless a 'deleter' is stored
				- NodeConcreteBase would be more unique_ptr wrapper?
*/

using rrv::Node;
using rrv::Dict;

namespace {

struct LifetimeTracer {
	struct Stats {
		std::string name;

		int copy_constructions = 0;
		int move_constructions = 0;
		int copy_assignments_to = 0;
		int move_assignments_to = 0;
		int copy_assignments_from = 0;
		int move_assignments_from = 0;
		int destructions = 0;
		int destructions_from_moved = 0;

		bool noisy = false;
	};

	using DataHandle = std::shared_ptr<Stats>;

	DataHandle actual;
	Stats expected = {};

	LifetimeTracer() : LifetimeTracer("") {}
	explicit LifetimeTracer(std::string name) : actual(std::make_shared<Stats>(Stats{std::move(name)})) {}
	LifetimeTracer(const LifetimeTracer&) = delete;
	LifetimeTracer& operator=(const LifetimeTracer&) = delete;
	~LifetimeTracer() { actual = nullptr; }

	int numAlive() const { return actual.use_count() - 1; }

	std::string compare(int expectedNumAlive) {
		std::ostringstream errs;
		auto compare_var = [&errs](const auto& exp, const auto& act, const auto& name) {
			if (exp != act) errs << "expected " << exp << " " << name << ", but saw " << act << "\n";
		};
		compare_var(expectedNumAlive, numAlive(), "numAlive");
		compare_var(expected.copy_constructions, actual->copy_constructions, "copy_constructions");
		compare_var(expected.move_constructions, actual->move_constructions, "move_constructions");
		compare_var(expected.copy_assignments_to, actual->copy_assignments_to, "copy_assignments_to");
		compare_var(expected.move_assignments_to, actual->move_assignments_to, "move_assignments_to");
		compare_var(expected.copy_assignments_from, actual->copy_assignments_from, "copy_assignments_from");
		compare_var(expected.move_assignments_from, actual->move_assignments_from, "move_assignments_from");
		compare_var(expected.destructions, actual->destructions, "destructions");
		compare_var(expected.destructions_from_moved, actual->destructions_from_moved, "destructions_from_moved");
		return errs.str();
	}

	struct Instance {
		Instance(DataHandle stats) : stats(std::move(stats)) {}

		Instance(const Instance& src) : stats(src.stats) {
			++stats->copy_constructions;
			if (stats->noisy) std::cout << stats->name << ": copy_construction\n";
		}
		Instance(Instance&& src) : stats(src.stats) {
			++stats->move_constructions;
			if (stats->noisy) std::cout << stats->name << ": move_construction\n";
			src.moved_from = true;
		}
		Instance& operator=(const Instance& rhs) {
			++stats->copy_assignments_to;
			if (stats->noisy) std::cout << stats->name << ": copy-assignment to\n";
			stats = rhs.stats;
			++stats->copy_assignments_from;
			if (stats->noisy) std::cout << stats->name << ": copy-assignment from\n";
			moved_from = false;
			return *this;
		}
		Instance& operator=(Instance&& rhs) {
			++stats->move_assignments_to;
			if (stats->noisy) std::cout << stats->name << ": move-assignment to\n";
			stats = rhs.stats;
			++stats->move_assignments_from;
			if (stats->noisy) std::cout << stats->name << ": move-assignment from\n";
			moved_from = false;
			rhs.moved_from = true;
			return *this;
		}
		~Instance() {
			if (moved_from) {
				++stats->destructions_from_moved;
				if (stats->noisy) std::cout << stats->name << ": destructed from moved\n";
			} else {
				++stats->destructions;
				if (stats->noisy) std::cout << stats->name << ": destructed\n";
			}
		}

		DataHandle stats;
		bool moved_from = false;
	};

	Instance makeInstance() { return Instance(actual); }
};

struct StructWithFriendConversions {
	int i;
	friend auto rrvMembers(StructWithFriendConversions& s) {
		return std::make_tuple(
			std::make_pair("i", &s.i)
		);
	}
};

struct StructWithDynamicMembersViaMember {
	int i, j;
	std::variant<int*> rrvMember(std::string_view key) {
		if (key == "i") return &i; else return &j;
	}
	constexpr static auto names = std::array{"i", "j"};
	auto rrvBegin() { return names.begin(); }
	auto rrvEnd() { return names.end(); }
};

struct StructWithDynamicMembersViaFriend {
	int i, j;
	friend std::variant<int*> rrvMember(StructWithDynamicMembersViaFriend& s, std::string_view key) {
		if (key == "i") return &s.i; else return &s.j;
	}
	constexpr static auto names = std::array{"i", "j"};
	friend auto rrvBegin(StructWithDynamicMembersViaFriend&) { return names.begin(); }
	friend auto rrvEnd(StructWithDynamicMembersViaFriend&) { return names.end(); }
};

struct TwoTypeVector {
	std::vector<int> vecInt;
	std::vector<float> vecFloat;
	struct MemberIter {
		bool inVecInt;
		std::size_t i = 0;
		auto operator<=>(const MemberIter&) const = default;
		void increment(const TwoTypeVector& mv) { ++i; if (inVecInt and i == mv.vecInt.size()) { i = 0; inVecInt = false; } }
		// return pair<string, unique_ptr<Node>> ?
		//   not clear how this avoids the O(n*n) iteration...
		auto dereference(const TwoTypeVector&) { return (inVecInt ? "i" : "f") + std::to_string(i); }
		bool equals(const MemberIter& rhs, const TwoTypeVector&) const { return *this == rhs; }
	};
	friend std::variant<int*, float*, std::monostate> rrvMember(TwoTypeVector& s, std::string_view key) {
		auto index = std::stoi(std::string(key.substr(1)));
		if (key[0] == 'i') if ((std::size_t)index < s.vecInt.size())   return &s.vecInt.at(index);   else return std::monostate{};
		              else if ((std::size_t)index < s.vecFloat.size()) return &s.vecFloat.at(index); else return std::monostate{};
	}
	friend auto rrvBegin(TwoTypeVector& s) { return MemberIter{s.vecInt.empty(), 0}; }
	friend auto rrvEnd(TwoTypeVector& s) { return MemberIter{false, s.vecFloat.size()}; }
};

struct TheNested {
	int ii;
	auto operator<=>(const TheNested&) const = default;

	auto rrvMembers() {
		return std::make_tuple(
			std::make_pair("ii", &ii)
		);
	}
};
struct HasNested {
	int i;
	TheNested m;
	auto operator<=>(const HasNested&) const = default;

	auto rrvMembers() {
		return std::make_tuple(
			std::make_pair("i", &i),
			std::make_pair("m", &m)
		);
	}
};

}

TEST_CASE("basic use") {
	SECTION("default init -- empty dict") {
		Node e;
		auto& as_dict = e.get<Dict>();
		REQUIRE(as_dict.empty());
	}
	SECTION("init with int") {
		Node l = 4;
		REQUIRE(l.get<int>() == 4);
		REQUIRE(l.get<const int>() == 4);
	}
	SECTION("set and get int") {
		Node d;
		d["k"] = 7;
		REQUIRE(d["k"].get<int>() == 7);
		REQUIRE(d["k"].get<const int>() == 7);
	}
	SECTION("assign to const int") {
		Node d;
		const int i = 7;
		d["k"] = i;
		REQUIRE(d["k"].get<int>() == 7);
		REQUIRE(d["k"].get<const int>() == 7);
	}
	SECTION("check const access") {
		const Node d = 7;
		auto& non_const_get = d.get<int>();
		static_assert(std::is_same_v<decltype(non_const_get), const int&>, "const access should return const");
		REQUIRE(d.get<const int>() == 7);
	}
	SECTION("access string key with integer keys") {
		Node d;
		d["1"] = 7;
		d["2"] = 8;
		CHECK(d[(signed char)       1].get<int>() == 7);
		CHECK(d[(short)             1].get<int>() == 7);
		CHECK(d[(int)               1].get<int>() == 7);
		CHECK(d[(long)              1].get<int>() == 7);
		CHECK(d[(long long)         1].get<int>() == 7);
		CHECK(d[(unsigned char)     1].get<int>() == 7);
		CHECK(d[(unsigned short)    1].get<int>() == 7);
		CHECK(d[(unsigned int)      1].get<int>() == 7);
		CHECK(d[(unsigned long)     1].get<int>() == 7);
		CHECK(d[(unsigned long long)1].get<int>() == 7);
		CHECK(d[(std::size_t)       1].get<int>() == 7);
		CHECK(d[(std::ptrdiff_t)    1].get<int>() == 7);
		CHECK_THROWS_WITH(d[(std::size_t)-1].get<int>(), "Signed cast would loose data");
	}
	SECTION("access integer key with string key") {
		Node d;
		d[1] = 7;
		d[2] = 8;
		CHECK(d["1"].get<int>() == 7);
	}
	SECTION("dict with dict with int") {
		Node root;
		auto& child = root["j"];
		child["k"] = 8;
		REQUIRE(root["j"]["k"].get<int>() == 8);
		REQUIRE(root["j"]["k"].get<const int>() == 8);
	}
	SECTION("assign empty rvalue node") {
		Node root;
		root["j"] = Node();
		REQUIRE(root["j"].get<Dict>().empty());
		REQUIRE(root["j"].get<const Dict>().empty());
	}
	SECTION("move-assign nested node") {
		Node root;
		Node child;
		child["k"] = 44;
		root["j"] = std::move(child);
		REQUIRE(root["j"]["k"].get<int>() == 44);
		REQUIRE(root["j"]["k"].get<const int>() == 44);
	}
	SECTION("copy-assign nested node - scalar") {
		Node root;
		Node child;
		child["k"] = 44;
		REQUIRE(child["k"].get<int>() == 44);
		REQUIRE(child["k"].get<const int>() == 44);
		root["j"] = child;
		REQUIRE(root["j"]["k"].get<int>() == 44);
		REQUIRE(root["j"]["k"].get<const int>() == 44);
		SECTION("copies are independent") {
			child["k"] = 33;
			REQUIRE(child["k"].get<int>() == 33);
			REQUIRE(child["k"].get<const int>() == 33);
			REQUIRE(root["j"]["k"].get<int>() == 44);
			REQUIRE(root["j"]["k"].get<const int>() == 44);
		}
	}
	SECTION("copy-assign nested node - static members") {
		struct S {int i; auto rrvMembers() { return std::make_tuple(std::make_pair("i", &i)); }};
		Node root;
		Node child;
		child["k"] = S{44};
		REQUIRE(child["k"]["i"].get<int>() == 44);
		REQUIRE(child["k"]["i"].get<const int>() == 44);
		root["j"] = child;
		REQUIRE(root["j"]["k"]["i"].get<int>() == 44);
		REQUIRE(root["j"]["k"]["i"].get<const int>() == 44);
		SECTION("copies are independent") {
			child["k"]["i"].get<int>() = 33;
			REQUIRE(child["k"]["i"].get<int>() == 33);
			REQUIRE(child["k"]["i"].get<const int>() == 33);
			REQUIRE(root["j"]["k"]["i"].get<int>() == 44);
			REQUIRE(root["j"]["k"]["i"].get<const int>() == 44);
		}
	}
	SECTION("copy-assign nested node - dynamic members") {
		constexpr static auto names = std::array{"i"};
		struct D {
			int i;
			std::variant<int*> rrvMember(std::string_view) { return &i; }
			auto rrvBegin() const { return names.begin(); }
			auto rrvEnd() const { return names.end(); }
		};
		Node root;
		Node child;
		child["k"] = D{44};
		REQUIRE(child["k"]["i"].get<int>() == 44);
		REQUIRE(child["k"]["i"].get<const int>() == 44);
		root["j"] = child;
		REQUIRE(root["j"]["k"]["i"].get<int>() == 44);
		REQUIRE(root["j"]["k"]["i"].get<const int>() == 44);
		SECTION("copies are independent") {
			child["k"]["i"].get<int>() = 33;
			REQUIRE(child["k"]["i"].get<int>() == 33);
			REQUIRE(child["k"]["i"].get<const int>() == 33);
			REQUIRE(root["j"]["k"]["i"].get<int>() == 44);
			REQUIRE(root["j"]["k"]["i"].get<const int>() == 44);
		}
	}
}

TEST_CASE("conversion to Scalars") {
	SECTION("convert 1-level Node Dict to Scalars") {
		Node n;
		n["a"] = 4;
		n["b"] = 5;
		const auto scalared = n.toScalars();
		CHECK(scalared.at("a").get<int>() == 4);
		CHECK(scalared.at("b").get<int>() == 5);
	}
	SECTION("convert Node Dict to Scalars") {
		Node n;
		n["a"]["aa"] = 4;
		n["b"]["bb"] = 5;
		const auto scalared = n.toScalars();
		CHECK(scalared.at("a").at("aa").get<int>() == 4);
		CHECK(scalared.at("b").at("bb").get<int>() == 5);
	}
	SECTION("convert a simple struct -- friend conversion function") {
		Node n = StructWithFriendConversions{3};
		const auto scalared = n.toScalars();
		CHECK(scalared.at("i").get<int>() == 3);
	}
	SECTION("convert a simple struct -- member conversion function") {
		SECTION("just one") {
			Node n = HasNested{44, {444}};
			const auto scalared = n.toScalars();
			CHECK(scalared.at("i").get<int>() == 44);
			CHECK(scalared.at("m").at("ii").get<int>() == 444);
		}
		SECTION("a vector") {
			Node n = std::vector{HasNested{55, {555}}, HasNested{66, {666}}, HasNested{77, {777}}};
			const auto scalared = n.toScalars();
			CHECK(scalared.at("0").at("i").get<int>() == 55);
			CHECK(scalared.at("0").at("m").at("ii").get<int>() == 555);
			CHECK(scalared.at("1").at("i").get<int>() == 66);
			CHECK(scalared.at("1").at("m").at("ii").get<int>() == 666);
			CHECK(scalared.at("2").at("i").get<int>() == 77);
			CHECK(scalared.at("2").at("m").at("ii").get<int>() == 777);
		}
	}
}

TEST_CASE("iteration") {
	SECTION("basic on Dict") {
		Dict d;
		d["k"] = 4;
		for (const auto& [k, v] : d) {
			REQUIRE(k == "k");
			REQUIRE(v->get<int>() == 4);
		}
	}
	SECTION("basic on Node") {
		Node no;
		no["k"] = 4;
		for (const auto& [k, v] : no) {
			REQUIRE(k == "k");
			REQUIRE(v->get<int>() == 4);
		}
	}
	SECTION("just one") {
		Node n = HasNested{44, {444}};
		int saw_m = 0, saw_i = 0;
		for (const auto& [k, v] : n) {
			if (k == "m") { ++saw_m; CHECK(v->get<TheNested>() == TheNested{444}); }
			if (k == "i") { ++saw_i; CHECK(v->get<int>() == 44); }
		}
		CHECK(saw_m == 1);
		CHECK(saw_i == 1);
	}
	SECTION("vector<int>") {
		Node n = std::vector{1, 2};
		std::map<rrv::Key, int> saw_it;
		for (const auto& [k, v] : n) {
			++saw_it[k];
			CHECK((k == "0" || k == "1"));
			if (k == "0") CHECK(v->get<int>() == 1);
			if (k == "1") CHECK(v->get<int>() == 2);
		}
		CHECK(saw_it.at("0") == 1);
		CHECK(saw_it.at("1") == 1);
		CHECK(saw_it.size() == 2);
	}
	SECTION("custom dynamic type - via member") {
		Node n = StructWithDynamicMembersViaMember{11,22};
		std::map<rrv::Key, int> saw_it;
		for (const auto& [k, v] : n) {
			++saw_it[k];
			CHECK((k == "i" || k == "j"));
			if (k == "i") CHECK(v->get<int>() == 11);
			if (k == "j") CHECK(v->get<int>() == 22);
		}
		CHECK(saw_it.at("i") == 1);;
		CHECK(saw_it.at("j") == 1);;
		CHECK(saw_it.size() == 2);
	}
	SECTION("custom dynamic type - via friend") {
		Node n = StructWithDynamicMembersViaFriend{11,22};
		std::map<rrv::Key, int> saw_it;
		for (const auto& [k, v] : n) {
			++saw_it[k];
			CHECK((k == "i" || k == "j"));
			if (k == "i") CHECK(v->get<int>() == 11);
			if (k == "j") CHECK(v->get<int>() == 22);
		}
		CHECK(saw_it.at("i") == 1);;
		CHECK(saw_it.at("j") == 1);;
		CHECK(saw_it.size() == 2);
	}
}

struct StructWithStaticMembersViaMemberIntAccess {
	int i, j;
	auto rrvMembers() {
		return std::make_tuple(
			std::make_pair(0, &i),
			std::make_pair(1, &j)
		);
	}
};

struct StructWithDynamicMembersViaMemberIntAccess {
	int i, j;
	std::variant<int*> rrvMember(long long key) {
		if (key == 0) return &i; else return &j;
	}
	constexpr static auto names = std::array{0, 1};
	auto rrvBegin() { return names.begin(); }
	auto rrvEnd() { return names.end(); }
};

struct StructWithStaticMembersViaFriendIntAccess {
	int i, j;
	friend auto rrvMembers(StructWithStaticMembersViaFriendIntAccess& s) {
		return std::make_tuple(
			std::make_pair(0, &s.i),
			std::make_pair(1, &s.j)
		);
	}
};

struct StructWithDynamicMembersViaFriendIntAccess {
	int i, j;
	friend std::variant<int*> rrvMember(StructWithDynamicMembersViaFriendIntAccess& s, long long key) {
		if (key == 0) return &s.i; else return &s.j;
	}
	constexpr static auto names = std::array{0, 1};
	friend auto rrvBegin(StructWithDynamicMembersViaFriendIntAccess&) { return names.begin(); }
	friend auto rrvEnd(StructWithDynamicMembersViaFriendIntAccess&) { return names.end(); }
};

TEST_CASE("member access") {
	SECTION("multi-type") {
		Node ttv_node = TwoTypeVector{{1, 2}, {3.0f, 4.0f}};
		CHECK(ttv_node["i0"].as<int>() == 1);
		CHECK(ttv_node["i1"].as<int>() == 2);
		CHECK(ttv_node["f0"].as<float>() == 3.0f);
		CHECK(ttv_node["f1"].as<float>() == 4.0f);

		CHECK_THROWS_WITH(ttv_node["i2"].as<int>(), "Member not found");
		ttv_node.get<TwoTypeVector>().vecInt.push_back(3);
		CHECK(ttv_node["i2"].as<int>() == 3);
	}
	SECTION("a vector - member access") {
		Node n = std::vector{HasNested{55, {555}}, HasNested{66, {666}}, HasNested{77, {777}}};
		CHECK(n.at("0").at("i").get<int>() == 55);
		CHECK(n.at("0").at("m").at("ii").get<int>() == 555);
		CHECK(n.at("1").at("i").get<int>() == 66);
		CHECK(n.at("1").at("m").at("ii").get<int>() == 666);
		CHECK(n.at("2").at("i").get<int>() == 77);
		CHECK(n.at("2").at("m").at("ii").get<int>() == 777);

		auto& vec = n.get<std::vector<HasNested>>();
		CHECK(n.at("0").at("i").get<int>() == 55);
		vec.at(0).i = 111;
		CHECK(n.at("0").at("i").get<int>() == 111);
		n.at("0").at("i").get<int>() = 222;
		CHECK(vec.at(0).i == 222);
	}
	SECTION("just one - member access") {
		Node n = HasNested{44, {444}};
		CHECK(n.at("i").get<int>() == 44);
		CHECK(n.at("m").at("ii").get<int>() == 444);
	}
	SECTION("assign Node = member") {
		Node n = HasNested{44, {444}};
		Node n2 = n.at("i");
		n.get<HasNested>().i = 55;
		CHECK(n2.get<int>() == 44);
	}
	SECTION("custom dynamic type - via member") {
		Node n = StructWithDynamicMembersViaMemberIntAccess{11,22};
		CHECK(n.at(0).get<int>() == 11);
		CHECK(n.at("0").get<int>() == 11);
		CHECK(n.at(1).get<int>() == 22);
		CHECK(n.at("1").get<int>() == 22);
		CHECK(n.at(7).get<int>() == 22);
		CHECK(n.at("7").get<int>() == 22);
	}
	SECTION("custom dynamic type - via member") {
		Node n = StructWithStaticMembersViaMemberIntAccess{11,22};
		CHECK(n.at(0).get<int>() == 11);
		CHECK(n.at("0").get<int>() == 11);
		CHECK(n.at(1).get<int>() == 22);
		CHECK(n.at("1").get<int>() == 22);
	}
	SECTION("custom dynamic type - via friend") {
		Node n = StructWithDynamicMembersViaFriendIntAccess{11,22};
		CHECK(n.at(0).get<int>() == 11);
		CHECK(n.at("0").get<int>() == 11);
		CHECK(n.at(1).get<int>() == 22);
		CHECK(n.at("1").get<int>() == 22);
		CHECK(n.at(7).get<int>() == 22);
		CHECK(n.at("7").get<int>() == 22);
	}
	SECTION("custom dynamic type - via friend") {
		Node n = StructWithStaticMembersViaFriendIntAccess{11,22};
		CHECK(n.at(0).get<int>() == 11);
		CHECK(n.at("0").get<int>() == 11);
		CHECK(n.at(1).get<int>() == 22);
		CHECK(n.at("1").get<int>() == 22);
	}
	SECTION("custom dynamic type - via friend") {
		Node n = StructWithDynamicMembersViaFriend{11,22};
		CHECK(n.at("i").get<int>() == 11);
		CHECK(n.at("j").get<int>() == 22);
		CHECK(n.at("e").get<int>() == 22);
	}
}

TEST_CASE("assignment/construction behaviour") {
	auto tracer1 = LifetimeTracer("t1");
	auto tracer2 = LifetimeTracer("t2");

	WHEN("assign to rvalue") {
		{
			auto i1 = tracer1.makeInstance();
			{
				Node n1 = std::move(i1);
				++tracer1.expected.move_constructions;
				CHECK(tracer1.compare(2) == "");
			}
			++tracer1.expected.destructions;
			CHECK(tracer1.compare(1) == "");
		}
		++tracer1.expected.destructions_from_moved;
		CHECK(tracer1.compare(0) == "");
	}

	WHEN("assign to lvalue") {
		{
			auto i1 = tracer1.makeInstance();
			{
				Node n1 = i1;
				++tracer1.expected.copy_constructions;
				CHECK(tracer1.compare(2) == "");
			}
			++tracer1.expected.destructions;
			CHECK(tracer1.compare(1) == "");
		}
		++tracer1.expected.destructions;
		CHECK(tracer1.compare(0) == "");
	}

	WHEN("assign to const lvalue") {
		{
			const auto i1 = tracer1.makeInstance();
			{
				Node n1 = i1;
				++tracer1.expected.copy_constructions;
				CHECK(tracer1.compare(2) == "");
			}
			++tracer1.expected.destructions;
			CHECK(tracer1.compare(1) == "");
		}
		++tracer1.expected.destructions;
		CHECK(tracer1.compare(0) == "");
	}

	WHEN("making a copy") {
		{
			Node n1 = tracer1.makeInstance();
			++tracer1.expected.move_constructions;
			++tracer1.expected.destructions_from_moved;
			CHECK(tracer1.compare(1) == "");

			{
				Node n2 = n1;
				++tracer1.expected.copy_constructions;
				CHECK(tracer1.compare(2) == "");
			}
			++tracer1.expected.destructions;
			CHECK(tracer1.compare(1) == "");
		}
		++tracer1.expected.destructions;
		CHECK(tracer1.compare(0) == "");
	}

	WHEN("making a copy from const") {
		{
			Node n1 = tracer1.makeInstance();
			++tracer1.expected.move_constructions;
			++tracer1.expected.destructions_from_moved;
			CHECK(tracer1.compare(1) == "");

			{
				const Node n2 = n1;
				++tracer1.expected.copy_constructions;
				CHECK(tracer1.compare(2) == "");
			}
			++tracer1.expected.destructions;
			CHECK(tracer1.compare(1) == "");
		}
		++tracer1.expected.destructions;
		CHECK(tracer1.compare(0) == "");
	}

	WHEN("moving a node") {
		{
			Node n1 = tracer1.makeInstance();
			++tracer1.expected.move_constructions;
			++tracer1.expected.destructions_from_moved;
			CHECK(tracer1.compare(1) == "");

			{
				Node n2 = std::move(n1);
				CHECK(tracer1.compare(1) == "");
			}
			++tracer1.expected.destructions;
			CHECK(tracer1.compare(0) == "");
		}
		CHECK(tracer1.compare(0) == "");
	}

	WHEN("assign to rvalue, non-root") {
		{
			auto i1 = tracer1.makeInstance();
			{
				Node n1;
				n1["k"] = std::move(i1);
				++tracer1.expected.move_constructions;
				CHECK(tracer1.compare(2) == "");
			}
			++tracer1.expected.destructions;
			CHECK(tracer1.compare(1) == "");
		}
		++tracer1.expected.destructions_from_moved;
		CHECK(tracer1.compare(0) == "");
	}

	WHEN("assign to lvalue, non-root") {
		{
			auto i1 = tracer1.makeInstance();
			{
				Node n1;
				n1["k"] = i1;
				++tracer1.expected.copy_constructions;
				CHECK(tracer1.compare(2) == "");
			}
			++tracer1.expected.destructions;
			CHECK(tracer1.compare(1) == "");
		}
		++tracer1.expected.destructions;
		CHECK(tracer1.compare(0) == "");
	}

	WHEN("assign to const lvalue, non-root") {
		{
			const auto i1 = tracer1.makeInstance();
			{
				Node n1;
				n1["k"] = i1;
				++tracer1.expected.copy_constructions;
				CHECK(tracer1.compare(2) == "");
			}
			++tracer1.expected.destructions;
			CHECK(tracer1.compare(1) == "");
		}
		++tracer1.expected.destructions;
		CHECK(tracer1.compare(0) == "");
	}

	WHEN("making a copy, non-root") {
		{
			Node n1;
			n1["k"] = tracer1.makeInstance();
			++tracer1.expected.move_constructions;
			++tracer1.expected.destructions_from_moved;
			CHECK(tracer1.compare(1) == "");

			{
				Node n2 = n1;
				++tracer1.expected.copy_constructions;
				CHECK(tracer1.compare(2) == "");
			}
			++tracer1.expected.destructions;
			CHECK(tracer1.compare(1) == "");
		}
		++tracer1.expected.destructions;
		CHECK(tracer1.compare(0) == "");
	}

	WHEN("making a copy from const, non-root") {
		{
			Node n1;
			n1["k"] = tracer1.makeInstance();
			++tracer1.expected.move_constructions;
			++tracer1.expected.destructions_from_moved;
			CHECK(tracer1.compare(1) == "");

			{
				const Node n2 = n1;
				++tracer1.expected.copy_constructions;
				CHECK(tracer1.compare(2) == "");
			}
			++tracer1.expected.destructions;
			CHECK(tracer1.compare(1) == "");
		}
		++tracer1.expected.destructions;
		CHECK(tracer1.compare(0) == "");
	}
}

TEST_CASE("Assign-through behaviour - single nodes") {
	auto tracer1 = LifetimeTracer("t1");
	auto tracer2 = LifetimeTracer("t2");

	WHEN("assign with different type") {
		Node n1 = tracer1.makeInstance();
		++tracer1.expected.move_constructions;
		++tracer1.expected.destructions_from_moved;
		CHECK(tracer1.compare(1) == "");

		n1 = 4;
		++tracer1.expected.destructions;
		CHECK(tracer1.compare(0) == "");
	}

	WHEN("assign with same type rvalue") {
		{
			Node n1 = tracer1.makeInstance();
			++tracer1.expected.move_constructions;
			++tracer1.expected.destructions_from_moved;
			CHECK(tracer1.compare(1) == "");

			n1 = tracer2.makeInstance();
			++tracer1.expected.move_assignments_to;
			++tracer2.expected.move_assignments_from;
			++tracer2.expected.destructions_from_moved;
			CHECK(tracer1.compare(0) == "");
			CHECK(tracer2.compare(1) == "");
		}
		++tracer2.expected.destructions;
		CHECK(tracer1.compare(0) == "");
		CHECK(tracer2.compare(0) == "");
	}

	WHEN("assign with same type lvalue") {
		{
			Node n1 = tracer1.makeInstance();
			++tracer1.expected.move_constructions;
			++tracer1.expected.destructions_from_moved;
			CHECK(tracer1.compare(1) == "");

			{
				auto i2 = tracer2.makeInstance();
				CHECK(tracer2.compare(1) == "");

				n1 = i2;
				++tracer1.expected.copy_assignments_to;
				++tracer2.expected.copy_assignments_from;
				CHECK(tracer1.compare(0) == "");
				CHECK(tracer2.compare(2) == "");
			}
			++tracer2.expected.destructions;
			CHECK(tracer2.compare(1) == "");
		}
		++tracer2.expected.destructions;
		CHECK(tracer1.compare(0) == "");
		CHECK(tracer2.compare(0) == "");
	}

	WHEN("assign with node rvalue holding same type") {
		{
			Node n1 = tracer1.makeInstance();
			++tracer1.expected.move_constructions;
			++tracer1.expected.destructions_from_moved;
			CHECK(tracer1.compare(1) == "");
			{
				Node n2 = tracer2.makeInstance();
				++tracer2.expected.move_constructions;
				++tracer2.expected.destructions_from_moved;
				CHECK(tracer2.compare(1) == "");

				n1 = std::move(n2);
				++tracer1.expected.move_assignments_to;
				++tracer2.expected.move_assignments_from;
				CHECK(tracer1.compare(0) == "");
				CHECK(tracer2.compare(2) == "");
			}
			++tracer2.expected.destructions_from_moved;
			CHECK(tracer2.compare(1) == "");
		}
		++tracer2.expected.destructions;
		CHECK(tracer1.compare(0) == "");
		CHECK(tracer2.compare(0) == "");
	}

	WHEN("assign with node lvalue holding same type") {
		{
			Node n1 = tracer1.makeInstance();
			++tracer1.expected.move_constructions;
			++tracer1.expected.destructions_from_moved;
			CHECK(tracer1.compare(1) == "");
			{
				Node n2 = tracer2.makeInstance();
				++tracer2.expected.move_constructions;
				++tracer2.expected.destructions_from_moved;
				CHECK(tracer2.compare(1) == "");

				n1 = n2;
				++tracer1.expected.copy_assignments_to;
				++tracer2.expected.copy_assignments_from;
				CHECK(tracer1.compare(0) == "");
				CHECK(tracer2.compare(2) == "");
			}
			++tracer2.expected.destructions;
			CHECK(tracer2.compare(1) == "");
		}
		++tracer2.expected.destructions;
		CHECK(tracer1.compare(0) == "");
		CHECK(tracer2.compare(0) == "");
	}
}

TEST_CASE("Assign-through behaviour - hierarchies") {
	auto tracer1 = LifetimeTracer("t1");
	auto tracer2 = LifetimeTracer("t2");
	auto tracer3 = LifetimeTracer("t3");
	auto tracer4 = LifetimeTracer("t4");
	auto tracer5 = LifetimeTracer("t5");
	auto tracer6 = LifetimeTracer("t6");

	WHEN("assigning same key") {
		Node h1;
		h1["k1"] = tracer1.makeInstance();
		++tracer1.expected.move_constructions;
		++tracer1.expected.destructions_from_moved;
		CHECK(tracer1.compare(1) == "");

		Node h2;
		h2["k1"] = tracer2.makeInstance();
		++tracer2.expected.move_constructions;
		++tracer2.expected.destructions_from_moved;
		CHECK(tracer2.compare(1) == "");

		h1 = h2;
		++tracer1.expected.copy_assignments_to;
		++tracer2.expected.copy_assignments_from;
		CHECK(tracer1.compare(0) == "");
		CHECK(tracer2.compare(2) == "");
	}

	WHEN("assigning same key, both nested") {
		Node h1;
		h1["k1"]["kk1"] = tracer1.makeInstance();
		++tracer1.expected.move_constructions;
		++tracer1.expected.destructions_from_moved;
		CHECK(tracer1.compare(1) == "");

		Node h2;
		h2["k1"]["kk1"] = tracer2.makeInstance();
		++tracer2.expected.move_constructions;
		++tracer2.expected.destructions_from_moved;
		CHECK(tracer2.compare(1) == "");

		h1 = h2;
		++tracer1.expected.copy_assignments_to;
		++tracer2.expected.copy_assignments_from;
		CHECK(tracer1.compare(0) == "");
		CHECK(tracer2.compare(2) == "");
	}

	WHEN("assigning same key, lhs nested") {
		Node h1;
		h1["k1"]["kk1"] = tracer1.makeInstance();
		++tracer1.expected.move_constructions;
		++tracer1.expected.destructions_from_moved;
		CHECK(tracer1.compare(1) == "");

		Node h2;
		h2["kk1"] = tracer2.makeInstance();
		++tracer2.expected.move_constructions;
		++tracer2.expected.destructions_from_moved;
		CHECK(tracer2.compare(1) == "");

		h1["k1"] = h2;
		++tracer1.expected.copy_assignments_to;
		++tracer2.expected.copy_assignments_from;
		CHECK(tracer1.compare(0) == "");
		CHECK(tracer2.compare(2) == "");
	}

	WHEN("assigning same key, mixed, extra node") {
		Node h1;
		h1["k1"]["kk1"] = tracer1.makeInstance();
		++tracer1.expected.move_constructions;
		++tracer1.expected.destructions_from_moved;
		CHECK(tracer1.compare(1) == "");
		h1["k2"] = tracer2.makeInstance();
		++tracer2.expected.move_constructions;
		++tracer2.expected.destructions_from_moved;
		CHECK(tracer2.compare(1) == "");
		h1["k3"]["k-copy"] = tracer5.makeInstance();
		++tracer5.expected.move_constructions;
		++tracer5.expected.destructions_from_moved;
		CHECK(tracer5.compare(1) == "");

		Node h2;
		h2["k1"]["kk1"] = tracer3.makeInstance();
		++tracer3.expected.move_constructions;
		++tracer3.expected.destructions_from_moved;
		CHECK(tracer3.compare(1) == "");
		h2["k2"] = tracer4.makeInstance();
		++tracer4.expected.move_constructions;
		++tracer4.expected.destructions_from_moved;
		CHECK(tracer4.compare(1) == "");
		h2["k3"]["k-drop"] = tracer6.makeInstance();
		++tracer6.expected.move_constructions;
		++tracer6.expected.destructions_from_moved;
		CHECK(tracer6.compare(1) == "");

		h1 = h2;
		++tracer1.expected.copy_assignments_to;
		++tracer2.expected.copy_assignments_to;
		++tracer3.expected.copy_assignments_from;
		++tracer4.expected.copy_assignments_from;
		++tracer5.expected.destructions;
		++tracer6.expected.copy_constructions;
		CHECK(tracer1.compare(0) == "");
		CHECK(tracer2.compare(0) == "");
		CHECK(tracer3.compare(2) == "");
		CHECK(tracer4.compare(2) == "");
		CHECK(tracer5.compare(0) == "");
		CHECK(tracer6.compare(2) == "");
	}
}

TEST_CASE("pathSubscript") {
	Node root;
	root["a"]["aa"]["aaa"] = 4;
	SECTION("access 1 level") {
		const auto rv = pathSubscript(root, "a");
		CHECK(rv != nullptr);
	}
	SECTION("access 2 levels") {
		const auto rv = pathSubscript(root, "a.aa");
		CHECK(rv != nullptr);
	}
	SECTION("access 3 levels") {
		const auto rv = pathSubscript(root, "a.aa.aaa");
		CHECK(rv != nullptr);
		CHECK(rv->get<int>() == 4);
	}
	SECTION("access 3 levels, sep=/") {
		const auto rv = pathSubscript(root, "a/aa/aaa", '/');
		CHECK(rv != nullptr);
		CHECK(rv->get<int>() == 4);
	}
	SECTION("access wrong level 1") {
		const auto& rv = pathSubscript(root, "b");
		CHECK(rv != nullptr);
		CHECK(rv->get<Dict>().size() == 0);
	}
	SECTION("access wrong level 2") {
		const auto& rv = pathSubscript(root, "a.bb");
		CHECK(rv != nullptr);
		CHECK(rv->get<Dict>().size() == 0);
	}
	SECTION("access wrong level 3") {
		const auto& rv = pathSubscript(root, "a.aa.bbb");
		CHECK(rv != nullptr);
		CHECK(rv->get<Dict>().size() == 0);
	}
	SECTION("access wrong sep") {
		const auto& rv = pathSubscript(root, "a.aa.aaa", '/');
		CHECK(rv != nullptr);
		CHECK(rv->get<Dict>().size() == 0);
	}
}

TEST_CASE("pathSubscript const") {
	const Node root = []() {
		Node root;
		root["a"]["aa"]["aaa"] = 4;
		return root;
	}();

	SECTION("access 1 level") {
		const auto rv = pathSubscript(root, "a");
		CHECK(rv != nullptr);
	}
	SECTION("access 2 levels") {
		const auto rv = pathSubscript(root, "a.aa");
		CHECK(rv != nullptr);
	}
	SECTION("access 3 levels") {
		const auto rv = pathSubscript(root, "a.aa.aaa");
		CHECK(rv != nullptr);
		CHECK(rv->get<int>() == 4);
	}
	SECTION("access 3 levels, sep=/") {
		const auto rv = pathSubscript(root, "a/aa/aaa", '/');
		CHECK(rv != nullptr);
		CHECK(rv->get<int>() == 4);
	}
	SECTION("access wrong level 1") {
		CHECK_THROWS(pathSubscript(root, "b"));
	}
	SECTION("access wrong level 2") {
		CHECK_THROWS(pathSubscript(root, "a.bb"));
	}
	SECTION("access wrong level 3") {
		CHECK_THROWS(pathSubscript(root, "a.aa.bbb"));
	}
	SECTION("access wrong sep") {
		CHECK_THROWS(pathSubscript(root, "a.aa.aaa", '/'));
	}
}

TEST_CASE("error paths") {
	SECTION("access Node that is a Dict as object") {
		Node no;
		CHECK_THROWS_WITH(no.get<int>(), std::string(rrv::errors::kAccessNodeAsObjectButIsDict));
	}
	SECTION("access member of an object in a Node") {
		Node no = 4;
		CHECK_THROWS_WITH(no["k"], std::string(rrv::errors::kAccessMemberOfScalarType));
	}
	SECTION("access member of an object in a Node") {
		struct ScalarStruct { int i; };
		Node no = ScalarStruct{};
		CHECK_THROWS_WITH(no["i"], std::string(rrv::errors::kAccessMemberOfScalarType));
	}
	SECTION("iterate object") {
		Node no = 4;
		const Node& const_no = no;
		CHECK_THROWS_WITH(no.begin(), std::string(rrv::errors::kAccessMemberOfScalarType));
		CHECK_THROWS_WITH(no.end(), std::string(rrv::errors::kAccessMemberOfScalarType));
		CHECK_THROWS_WITH(no.cbegin(), std::string(rrv::errors::kAccessMemberOfScalarType));
		CHECK_THROWS_WITH(no.cend(), std::string(rrv::errors::kAccessMemberOfScalarType));
		CHECK_THROWS_WITH(const_no.begin(), std::string(rrv::errors::kAccessMemberOfScalarType));
		CHECK_THROWS_WITH(const_no.end(), std::string(rrv::errors::kAccessMemberOfScalarType));
	}
	SECTION("access with wrong type") {
		Node no = 4;
		CHECK_THROWS_WITH(no.get<float>(), std::string(rrv::errors::kAccessWithWrongType));
	}
	SECTION("access as wrong type") {
		Node no = 4;
		CHECK_THROWS_WITH(no.as<float>(), std::string(rrv::errors::kAsCannotAccessOrConvert));
	}
	SECTION("non-copyable") {
		// struct NoCopy {
		// 	NoCopy() {}
		// 	NoCopy(NoCopy&&) = default;
		// 	NoCopy& operator=(NoCopy&&) = default;
		// 	NoCopy(const NoCopy&) = delete;
		// 	NoCopy& operator=(const NoCopy&) = delete;
		// };
		// auto no = Node(NoCopy{}); (void)no;
	}
}

TEST_CASE("basic dict operations") {
	Dict d;
	d["k"] = 1;
	SECTION("assign elements") {
		d["l"] = 2;
		CHECK(d.size() == 2);
		CHECK(d["k"].get<int>() == 1);
		CHECK(d["l"].get<int>() == 2);
	}
	SECTION("copy-construct") {
		Dict cpy = d;
		CHECK(cpy.size() == 1);
		CHECK(cpy["k"].get<int>() == 1);
		CHECK(d.size() == 1);
		CHECK(d["k"].get<int>() == 1);
	}
	SECTION("copy-assign") {
		Dict cpy;
		cpy = d;
		CHECK(cpy.size() == 1);
		CHECK(cpy["k"].get<int>() == 1);
		CHECK(d.size() == 1);
		CHECK(d["k"].get<int>() == 1);
	}
	SECTION("move-construct") {
		Dict cpy = std::move(d);
		CHECK(cpy.size() == 1);
		CHECK(cpy["k"].get<int>() == 1);
		CHECK(d.size() == 0);
	}
	SECTION("move-assign") {
		Dict cpy;
		cpy = std::move(d);
		CHECK(cpy.size() == 1);
		CHECK(cpy["k"].get<int>() == 1);
		CHECK(d.size() == 0);
	}
}

TEST_CASE("Node <- Dict interactions") {
	Node no;
	SECTION("default-constucted Node = Dict") {
		no = Dict{};
		no.get<Dict>();
	}
	SECTION("Node with element = Dict") {
		no["k"] = 1;
		CHECK(no.get<Dict>().size() == 1);
		no = Dict{};
		CHECK(no.get<Dict>().size() == 0);
	}
}

namespace {
struct SmallTestProgramData {
	int i, j, k;
	friend auto rrvMembers(SmallTestProgramData& s) {
		return std::make_tuple(
			std::make_pair("i", &s.i),
			std::make_pair("j", &s.j),
			std::make_pair("k", &s.k)
		);
	}
};
}

TEST_CASE("small test program") {
	using Data = SmallTestProgramData;
	const auto producer = [](int a, int b) {
		Node data;
		data["alpha"] = Data{a, a+b, b};
		data["beta"] = std::vector<Data>{Data{1,2,3}, Data{a,5,b}};
		return data;
	};
	const auto modifyer = [](Node& data) {
		data["alpha"].get<Data>().i = 44;
		data["beta"].get<std::vector<Data>>().at(1).k += 6;
		data["gamma"] = 4;
	};
	const auto consumer = [](const Node& data) {
		const Data& alpha = data["alpha"].get<Data>();
		const std::vector<Data>& beta = data["beta"].get<std::vector<Data>>();
		CHECK(alpha.i == 44);
		CHECK(alpha.j == 11+22);
		CHECK(alpha.k == 22);
		CHECK(beta.at(0).i == 1);
		CHECK(beta.at(0).j == 2);
		CHECK(beta.at(0).k == 3);
		CHECK(beta.at(1).i == 11);
		CHECK(beta.at(1).j == 5);
		CHECK(beta.at(1).k == 22+6);
		CHECK(data["gamma"].get<int>() == 4);
	};
	auto data = producer(11, 22);
	modifyer(data);
	const auto& data_const = data;
	consumer(data_const);
}
