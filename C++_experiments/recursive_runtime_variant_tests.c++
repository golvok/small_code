#define CATCH_CONFIG_MAIN  // Tell catch to provide main()
#include <catch2/catch.hpp>

#include "recursive_runtime_variant.h++"

/*
	need string-based member access, but don't want to require hanafication
		this needs to be able to return a string/scalar (or long, or bool)?
			something similar to as? (pass in the type)
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

TEST_CASE("iteration") {
	SECTION("basic on Dict") {
		Dict d;
		d["k"] = 4;
		for (const auto& [k, v] : d) {
			REQUIRE(k == "k");
			REQUIRE(v->get<int>() == 4);
		}
	}
	SECTION("basic on NodeOwning") {
		NodeOwning no;
		no["k"] = 4;
		for (const auto& [k, v] : no) {
			REQUIRE(k == "k");
			REQUIRE(v->get<int>() == 4);
		}
	}
}

TEST_CASE("pathSubscript") {
	NodeOwning root;
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
	const NodeOwning root = []() {
		NodeOwning root;
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
	SECTION("access NodeOwning that is a Dict as object") {
		NodeOwning no;
		CHECK_THROWS_WITH(no.get<int>(), std::string(rrv::errors::kAccessNodeOwningAsObjectButIsDict));
	}
	SECTION("access member of an object in a NodeOwning") {
		NodeOwning no = 4;
		CHECK_THROWS_WITH(no["k"], std::string(rrv::errors::kAccessMemberOfConcreteType));
	}
	SECTION("assign object to Dict from a NodeOwning") {
		Dict d;
		CHECK_THROWS_WITH(d = NodeOwning{4}, std::string(rrv::errors::kAssignObjectInNodeOwningToDict));
	}
	SECTION("assign object to Dict from a NodeBase") {
		Dict d;
		auto nc = NodeConcrete<int>{4}; // constructing a NodeOwning doesn't work -- it tries that downcast
		CHECK_THROWS_WITH(d = nc, std::string(rrv::errors::kAssignObjectToDict));
	}
	SECTION("iterate object") {
		NodeOwning no = 4;
		const NodeOwning& const_no = no;
		CHECK_THROWS_WITH(no.begin(), std::string(rrv::errors::kAccessMemberOfConcreteType));
		CHECK_THROWS_WITH(no.end(), std::string(rrv::errors::kAccessMemberOfConcreteType));
		CHECK_THROWS_WITH(no.cbegin(), std::string(rrv::errors::kAccessMemberOfConcreteType));
		CHECK_THROWS_WITH(no.cend(), std::string(rrv::errors::kAccessMemberOfConcreteType));
		CHECK_THROWS_WITH(const_no.begin(), std::string(rrv::errors::kAccessMemberOfConcreteType));
		CHECK_THROWS_WITH(const_no.end(), std::string(rrv::errors::kAccessMemberOfConcreteType));
	}
	SECTION("access with wrong type") {
		NodeOwning no = 4;
		CHECK_THROWS_WITH(no.get<float>(), std::string(rrv::errors::kAccessWithWrongType));
	}
	SECTION("access as wrong type") {
		NodeOwning no = 4;
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
		CHECK_THROWS_WITH(d = no, std::string(rrv::errors::kAssignObjectInNodeOwningToDict));
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
		CHECK_THROWS_WITH(d = *(NodeBase*)&no, std::string(rrv::errors::kAssignObjectInNodeOwningToDict));
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