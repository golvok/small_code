#include "recursive_runtime_variant.h++"

#include <catch2/catch.hpp>

/*
	Goals:
		Want to be able to construct a Node{}... but this is the base class...
			use different base for parameters, and NodeConcrete for local vars?
				that is weird, but makes it very obvious what is going on
			Make common class that is uninitialized, and deterlmined upon assignment?
		Two separate uses:
			"metadata"
				- tagging arbitrary, nested, type-erased data onto things
				- debug info, metadata
			"configuration"
			    - nested, type-erased configuration
			    - config files, function params
	Next:
		- make 'Scalar' types possible -- don't have member access/iteration methods
		- avoid member lookup when iterating by passing returned MemberIterator to rrvMember?
			- types probably shouldn't store actual iterators to avoid invalidation? or say screw it, and just use existing rules for type(s)
		- non-string_view arguments for operator[]?
			- maybe take a variant of a couple stdlib types - long long, string_view
			- YAML does it by taking a Node as as argument... could do this too
				- getMember will have to convert -- make sure to catch exceptions from it to add context
			- Is it possible to use an overload set of rrvMember? - library automatically converts int<->string if only one
		- setMember. Allows returning const& from getMember(s)?
		   - can't: NodeBase::get returns a plain reference
		     - return NodeReference<T>?
		- Clean up assignment code, especially dict
		- Dict::operator[] emplace_hint suspicious
		- extract static member names into array from tuple via apply + CTAD?
		- convert most of hierarchy to static polymorphism - don't need runtime, except for type-erased storage
			- single (templated) derived class with variant of Owning, Bose, Concrete, Reference, Value, Dict?
			- may resolve the "want to be able to construct a Node" problem, by defaulting to NodeOwning
		- Catch exceptions to add context. Eg. what member names are being accessed
*/

using rrv::NodeOwning;
using rrv::NodeBase;
using rrv::NodeConcrete;
using rrv::NodeReference;
using rrv::NodeValue;
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
		REQUIRE(l.get<const int>() == 4);
	}
	SECTION("set and get int") {
		NodeOwning d;
		d["k"] = 7;
		REQUIRE(d["k"].get<int>() == 7);
		REQUIRE(d["k"].get<const int>() == 7);
	}
	SECTION("check const access") {
		const NodeOwning d = 7;
		auto& non_const_get = d.get<int>();
		static_assert(std::is_same_v<decltype(non_const_get), const int&>, "const access should return const");
		REQUIRE(d.get<const int>() == 7);
	}
	SECTION("dict with dict with int") {
		NodeOwning root;
		auto& child = root["j"];
		child["k"] = 8;
		REQUIRE(root["j"]["k"].get<int>() == 8);
		REQUIRE(root["j"]["k"].get<const int>() == 8);
	}
	SECTION("assign empty rvalue node") {
		NodeOwning root;
		root["j"] = NodeOwning();
		REQUIRE(root["j"].get<Dict>().empty());
		REQUIRE(root["j"].get<const Dict>().empty());
	}
	SECTION("move-assign nested node") {
		NodeOwning root;
		NodeOwning child;
		child["k"] = 44;
		root["j"] = std::move(child);
		REQUIRE(root["j"]["k"].get<int>() == 44);
		REQUIRE(root["j"]["k"].get<const int>() == 44);
	}
	SECTION("copy-assign nested node") {
		NodeOwning root;
		NodeOwning child;
		child["k"] = 44;
		root["j"] = child;
		REQUIRE(root["j"]["k"].get<int>() == 44);
		REQUIRE(root["j"]["k"].get<const int>() == 44);
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

namespace {
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
		// return pair<string, unique_ptr<NodeBase>> ?
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

}

TEST_CASE("Multi-type member iteration") {
	SECTION("TwoTypeVector") {
		NodeOwning ttv_node = TwoTypeVector{{1, 2}, {3.0f, 4.0f}};
		CHECK(ttv_node["i0"].as<int>() == 1);
		CHECK(ttv_node["i1"].as<int>() == 2);
		CHECK(ttv_node["f0"].as<float>() == 3.0f);
		CHECK(ttv_node["f1"].as<float>() == 4.0f);

		CHECK_THROWS_WITH(ttv_node["i2"].as<int>(), "Member not found");
		ttv_node.get<TwoTypeVector>().vecInt.push_back(3);
		CHECK(ttv_node["i2"].as<int>() == 3);
	}
}

TEST_CASE("conversion to Scalars") {
	NodeOwning root;
	SECTION("convert 1-level NodeOwning Dict to Scalars") {
		root["a"] = 4;
		root["b"] = 5;
		const auto scalared = root.toScalars();
		CHECK(scalared.at("a").get<int>() == 4);
		CHECK(scalared.at("b").get<int>() == 5);
	}
	SECTION("convert NodeOwning Dict to Scalars") {
		root["a"]["aa"] = 4;
		root["b"]["bb"] = 5;
		const auto scalared = root.toScalars();
		CHECK(scalared.at("a").at("aa").get<int>() == 4);
		CHECK(scalared.at("b").at("bb").get<int>() == 5);
	}
	SECTION("convert a simple struct -- friend conversion function") {
		root = StructWithFriendConversions{3};
		const auto scalared = root.toScalars();
		CHECK(scalared.at("i").get<int>() == 3);
	}
	SECTION("convert a simple struct -- member conversion function") {
		struct Mm {
			int ii;
			auto operator<=>(const Mm&) const = default;

			auto rrvMembers() {
				return std::make_tuple(
					std::make_pair("ii", &ii)
				);
			}
		};
		struct M {
			int i;
			Mm m;
			auto operator<=>(const M&) const = default;

			auto rrvMembers() {
				return std::make_tuple(
					std::make_pair("i", &i),
					std::make_pair("m", &m)
				);
			}
		};
		SECTION("just one - scalarize") {
			NodeOwning n = M{44, {444}};
			const auto scalared = n.toScalars();
			CHECK(scalared.at("i").get<int>() == 44);
			CHECK(scalared.at("m").at("ii").get<int>() == 444);
		}
		SECTION("a vector - scalarize") {
			NodeOwning n = std::vector{M{55, {555}}, M{66, {666}}, M{77, {777}}};
			const auto scalared = n.toScalars();
			CHECK(scalared.at("0").at("i").get<int>() == 55);
			CHECK(scalared.at("0").at("m").at("ii").get<int>() == 555);
			CHECK(scalared.at("1").at("i").get<int>() == 66);
			CHECK(scalared.at("1").at("m").at("ii").get<int>() == 666);
			CHECK(scalared.at("2").at("i").get<int>() == 77);
			CHECK(scalared.at("2").at("m").at("ii").get<int>() == 777);
		}
		SECTION("a vector - member access") {
			NodeOwning n = std::vector{M{55, {555}}, M{66, {666}}, M{77, {777}}};
			CHECK(n.at("0").at("i").get<int>() == 55);
			CHECK(n.at("0").at("m").at("ii").get<int>() == 555);
			CHECK(n.at("1").at("i").get<int>() == 66);
			CHECK(n.at("1").at("m").at("ii").get<int>() == 666);
			CHECK(n.at("2").at("i").get<int>() == 77);
			CHECK(n.at("2").at("m").at("ii").get<int>() == 777);

			auto& vec = n.get<std::vector<M>>();
			CHECK(n.at("0").at("i").get<int>() == 55);
			vec.at(0).i = 111;
			CHECK(n.at("0").at("i").get<int>() == 111);
			n.at("0").at("i").get<int>() = 222;
			CHECK(vec.at(0).i == 222);
		}
		SECTION("just one - member access") {
			NodeOwning n = M{44, {444}};
			CHECK(n.at("i").get<int>() == 44);
			CHECK(n.at("m").at("ii").get<int>() == 444);
		}
		SECTION("just one - member iter") {
			NodeOwning n = M{44, {444}};
			int saw_m = 0, saw_i = 0;
			for (const auto& [k, v] : n) {
				if (k == "m") { ++saw_m; CHECK(v->get<Mm>() == Mm{444}); }
				if (k == "i") { ++saw_i; CHECK(v->get<int>() == 44); }
			}
			CHECK(saw_m == 1);
			CHECK(saw_i == 1);
		}
		SECTION("vector<int> - member iter") {
			NodeOwning n = std::vector{1, 2, 3, 4};
			std::vector<int> saw_it(4);
			for (const auto& [k, v] : n) {
				auto i = stoi(k);
				++saw_it.at(i);
				CHECK(v->get<int>() == i+1);
			}
			for (auto saw : saw_it) {
				CHECK(saw == 1);
			}
		}
		SECTION("assign NodeOwning = member") {
			NodeOwning n = M{44, {444}};
			NodeOwning n2 = n.at("i");
			n.get<M>().i = 55;
			CHECK(n2.get<int>() == 44);
		}
		SECTION("custom dynamic type - via member") {
			NodeOwning n = StructWithDynamicMembersViaMember{11,22};
			CHECK(n.at("i").get<int>() == 11);
			CHECK(n.at("j").get<int>() == 22);
			CHECK(n.at("e").get<int>() == 22);
		}
		SECTION("custom dynamic type - via friend") {
			NodeOwning n = StructWithDynamicMembersViaFriend{11,22};
			CHECK(n.at("i").get<int>() == 11);
			CHECK(n.at("j").get<int>() == 22);
			CHECK(n.at("e").get<int>() == 22);
		}
		SECTION("custom dynamic type - via member") {
			NodeOwning n = StructWithDynamicMembersViaMember{11,22};
			std::map<std::string, int> saw_it;
			for (const auto& [k, v] : n) {
				++saw_it[k];
				CHECK((k == "i" || k == "j"));
				if (k == "i") CHECK(v->get<int>() == 11);
				if (k == "j") CHECK(v->get<int>() == 22);
			}
			for (const auto& [k, count] : saw_it) {
				CHECK(count == 1);
			}
			CHECK(saw_it.size() == 2);
		}
		SECTION("custom dynamic type - via friend") {
			NodeOwning n = StructWithDynamicMembersViaFriend{11,22};
			std::map<std::string, int> saw_it;
			for (const auto& [k, v] : n) {
				++saw_it[k];
				CHECK((k == "i" || k == "j"));
				if (k == "i") CHECK(v->get<int>() == 11);
				if (k == "j") CHECK(v->get<int>() == 22);
			}
			for (const auto& [k, count] : saw_it) {
				CHECK(count == 1); (void)k;
			}
			CHECK(saw_it.size() == 2);
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
		CHECK_THROWS_WITH(no["k"], std::string(rrv::errors::kAccessMemberOfScalarType));
	}
	SECTION("assign object to Dict from a NodeOwning") {
		Dict d;
		CHECK_THROWS_WITH(d = NodeOwning{4}, std::string(rrv::errors::kAssignObjectInNodeOwningToDict));
	}
	SECTION("assign object to Dict from a NodeBase") {
		Dict d;
		auto nc = NodeValue<int>{4}; // constructing a NodeOwning doesn't work -- it tries that downcast
		CHECK_THROWS_WITH(d = nc, std::string(rrv::errors::kAssignObjectToDict));
	}
	SECTION("iterate object") {
		NodeOwning no = 4;
		const NodeOwning& const_no = no;
		CHECK_THROWS_WITH(no.begin(), std::string(rrv::errors::kAccessMemberOfScalarType));
		CHECK_THROWS_WITH(no.end(), std::string(rrv::errors::kAccessMemberOfScalarType));
		CHECK_THROWS_WITH(no.cbegin(), std::string(rrv::errors::kAccessMemberOfScalarType));
		CHECK_THROWS_WITH(no.cend(), std::string(rrv::errors::kAccessMemberOfScalarType));
		CHECK_THROWS_WITH(const_no.begin(), std::string(rrv::errors::kAccessMemberOfScalarType));
		CHECK_THROWS_WITH(const_no.end(), std::string(rrv::errors::kAccessMemberOfScalarType));
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
