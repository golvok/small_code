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

TEST_CASE("basic use", "") {
	SECTION("default init -- empty dict") {
		NodeOwning e;
		auto& as_dict = e.get<rrv::Dict>();
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
		REQUIRE(root["j"].get<rrv::Dict>().empty());
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