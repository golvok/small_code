#define CATCH_CONFIG_MAIN  // Tell catch to provide main()
#include <catch2/catch.hpp>

#include "recursive_runtime_variant.h++"

/*
 	need string-based member access, but don't want to require hanafication
 		dot is special char?
 		Object case:
 			don't use std::any, use a class with virtual accessor that throws if not hanafied.
 				ie. member is variant<Dict, unique_ptr<Node>>
 					Or, Dict inherits from Node too?
 					hmm... copying... clone pattern I guess
 						std::any requires all types to be copyable!
 							I guess so there are no runtime surprises...
 				or have a std::function member that does the cast & get or throws if not hanafied -- worse: has heap alloc
 		this needs to be able to return a string/scalar (or long, or bool)?
 			something similar to as? (pass in the type)
 		"dictify" -- recursively (or not) convert to a dict type
 			can use virtual func method again
 			note: YAML's .as is the reverse of this
 		handle references? return them? -- std::reference_wrapper special handling?
	Want to be able to consturct a Node{}... but this is the base class...
		different use base for parameters, and NodeConcrete for local vars?
			that is weird, but makes it very obvious what is going on
		Make common class that is uninitialized, and deterlmined upon assignment?
*/

using rrv::NodeOwning;
using rrv::NodeBase;
using rrv::NodeConcrete;
using rrv::Dict;

TEST_CASE("basic use") {
	SECTION("default init -- empty dict") {
		NodeOwning e;
		auto& as_dict = e.get<Dict>();
		REQUIRE(as_dict.empty());
	}
	SECTION("init with int") {
		NodeOwning l = 4;
		REQUIRE(l.get<int>() == 4);
	}
	SECTION("set and get int") {
		NodeOwning d;
		d["k"] = 7;
		REQUIRE(d["k"].get<int>() == 7);
	}
	SECTION("dict with dict with int") {
		NodeOwning root;
		auto& child = root["j"];
		child["k"] = 8;
		REQUIRE(root["j"]["k"].get<int>() == 8);
	}
	SECTION("assign empty rvalue node") {
		NodeOwning root;
		root["j"] = NodeOwning();
		REQUIRE(root["j"].get<Dict>().empty());
	}
	SECTION("move-assign nested node") {
		NodeOwning root;
		NodeOwning child;
		child["k"] = 44;
		root["j"] = std::move(child);
		REQUIRE(root["j"]["k"].get<int>() == 44);
	}
	SECTION("copy-assign nested node") {
		NodeOwning root;
		NodeOwning child;
		child["k"] = 44;
		root["j"] = child;
		REQUIRE(root["j"]["k"].get<int>() == 44);
	}
}


TEST_CASE("error paths") {
	SECTION("access NodeOwning that is a Dict as object") {
		NodeOwning no;
		CHECK_THROWS_WITH(no.get<int>(), std::string(rrv::errors::kAccessNodeOwningAsObjectButIsDict));
	}
	SECTION("access member of an object in a NodeOwning") {
		NodeOwning no = 4;
		CHECK_THROWS_WITH(no["k"], std::string(rrv::errors::kAccessMemberOfConcreteType));
	}
	SECTION("non-copyable") {
		// struct NoCopy {
		// 	NoCopy() {}
		// 	NoCopy(NoCopy&&) = default;
		// 	NoCopy& operator=(NoCopy&&) = default;
		// 	NoCopy(const NoCopy&) = delete;
		// 	NoCopy& operator=(const NoCopy&) = delete;
		// };
		// auto no = NodeOwning(NoCopy{}); (void)no;
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

TEST_CASE("Dict <- NodeOwning interactions") {
	Dict d;
	SECTION("Dict = NodeOwning that is Dict") {
		NodeOwning no;
		no["k"] = 1;
		d = no;
		CHECK(d.size() == 1);
		CHECK(d["k"].get<int>() == 1);
	}
	SECTION("Dict = NodeOwning that is object") {
		NodeOwning no = 1;
		CHECK_THROWS_WITH(d = no, std::string(rrv::errors::kAssignToObjectInNodeOwningToDict));
	}
	SECTION("Dict = NodeOwning that is Dict via NodeBase") {
		NodeOwning no;
		no["k"] = 1;
		d = *(NodeBase*)&no;
		CHECK(d.size() == 1);
		CHECK(d["k"].get<int>() == 1);
	}
	SECTION("Dict = NodeOwning that is object via NodeBase") {
		NodeOwning no = 1;
		CHECK_THROWS_WITH(d = *(NodeBase*)&no, std::string(rrv::errors::kAssignToObjectInNodeOwningToDict));
	}
}

TEST_CASE("NodeOwning <- Dict interactions") {
	NodeOwning no;
	SECTION("default-constucted NodeOwning = Dict") {
		no = Dict{};
		no.get<Dict>();
	}
	SECTION("NodeOwning with element = Dict") {
		no["k"] = 1;
		CHECK(no.get<Dict>().size() == 1);
		no = Dict{};
		CHECK(no.get<Dict>().size() == 0);
	}
}

TEST_CASE("small test program") {
	struct Data {
		int i, j, k;
	};
	const auto producer = [](int a, int b) {
		NodeOwning data;
		data["alpha"] = Data{a, a+b, b};
		data["beta"] = std::vector<Data>{Data{1,2,3}, Data{a,5,b}};
		return data;
	};
	const auto modifyer = [](NodeBase& data) {
		data["alpha"].get<Data>().i = 44;
		data["beta"].get<std::vector<Data>>().at(1).k += 6;
		data["gamma"] = 4;
	};
	const auto consumer = [](const NodeBase& data) {
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