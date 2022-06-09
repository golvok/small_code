#pragma once

#include <charconv>
#include <limits>
#include <map>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <variant>
#include <vector>

namespace rrv {

struct KeyType {
	KeyType(const char* s) : KeyType(std::string_view(s)) {}
	KeyType(std::string_view sv) : impl(sv) {}
	KeyType(int i) : impl(i) {}
	KeyType(long long i) : impl(i) {}
	KeyType(float d) = delete;
	KeyType(double d) = delete;

	KeyType(const KeyType&) = default;
	KeyType(KeyType&&) = default;
	KeyType& operator=(const KeyType&) = default;
	KeyType& operator=(KeyType&&) = default;

	std::strong_ordering operator<=>(const KeyType&) const = default;

	template<typename F> decltype(auto) visit(F f)       { return std::visit(std::forward<F>(f), impl); }
	template<typename F> decltype(auto) visit(F f) const { return std::visit(std::forward<F>(f), impl); }

	friend std::string to_string(KeyType key) {
		return key.visit([](auto& k) {
			if constexpr(std::is_same_v<decltype(k), std::string_view&>) {
				return std::string(k);
			} else {
				return std::to_string(k);
			}
		});
	}

// private:
	std::variant<std::string_view, long long> impl;
};

struct KeyTypeOwning {
	KeyTypeOwning(const char* s) : KeyTypeOwning(std::string(s)) {}
	KeyTypeOwning(std::string s) : impl(s) {}
	KeyTypeOwning(int i) : impl(i) {}
	KeyTypeOwning(long long i) : impl(i) {}
	KeyTypeOwning(float d) = delete;
	KeyTypeOwning(double d) = delete;

	explicit KeyTypeOwning(const KeyType& src) : impl(0) {
		struct V {
			KeyTypeOwning* self;
			void operator()(long long i) { self->impl = i; }
			void operator()(std::string_view s) { self->impl = std::string(s); }
		};
		src.visit(V{this});
	}

	KeyTypeOwning(const KeyTypeOwning&) = default;
	KeyTypeOwning(KeyTypeOwning&&) = default;
	KeyTypeOwning& operator=(const KeyTypeOwning&) = default;
	KeyTypeOwning& operator=(KeyTypeOwning&&) = default;

	std::strong_ordering operator<=>(const KeyTypeOwning&) const = default;

	template<typename F> decltype(auto) visit(F f)       { return std::visit(std::forward<F>(f), impl); }
	template<typename F> decltype(auto) visit(F f) const { return std::visit(std::forward<F>(f), impl); }

	operator KeyType() const { return visit([](auto& e) { return KeyType(e); }); }
	bool operator<(const KeyType& rhs) const { return (bool)((KeyType)(*this) < rhs); }

// private:
	std::variant<std::string, long long> impl;
};

namespace detail {}
using namespace detail;

struct Node;
struct NodeConcreteBase;
template<typename T> struct NodeConcrete;
template<typename T> struct NodeReference;
template<typename T> struct NodeValue;
template <typename Container, typename T> struct NodeIndirectAcess;
struct Dict;
template<bool> struct MemberIterator;
template<bool> struct MemberIteratorImplBase;

template<typename Obj, typename = void> constexpr static bool kIsDynamicMemberType = false;
template<typename Obj, typename = void> constexpr static bool kIsStaticMemberType = false;
template<typename Obj> constexpr static bool kIsScalarType = not kIsStaticMemberType<Obj> and not kIsDynamicMemberType<Obj>;

namespace errors {
	using namespace std::literals::string_view_literals;
	constexpr auto kAccessMemberOfScalarType = "Accessing member of a scalar type"sv;
	constexpr auto kAccessNodeAsObjectButIsDict = "Trying to access a Node as an object, but it is a Dict"sv;
	constexpr auto kAccessWithWrongType = "Accessing with wrong type"sv;
	constexpr auto kAsCannotAccessOrConvert = ".as is unable to access as or convert to the type requested"sv;
	constexpr auto kCloneNotImplemented_ConstRef = "clone const& not implemented"sv;
	constexpr auto kCloneNotImplemented_RvalRef = "clone && not implemented"sv;
	constexpr auto kAccessMemberOfConcreteType = "can't get member of concrete type"sv;
}

template<typename Source, typename Target>
using SameConstAs = std::conditional_t<std::is_const_v<Source>, const Target, Target>;

using DictBase = std::map<KeyTypeOwning, std::unique_ptr<Node>, std::less<>>;

template<bool const_iter>
struct MemberIteratorImplBase {
	using OwningPtr = std::unique_ptr<MemberIteratorImplBase>;
	using reference = std::conditional_t<const_iter, DictBase::const_reference, DictBase::reference>;
	using value_type = std::conditional_t<const_iter, const DictBase::value_type, DictBase::value_type>;

	virtual ~MemberIteratorImplBase() = default;
	virtual value_type* deref() = 0;
	virtual bool equalTo(const MemberIteratorImplBase&) const = 0;
	virtual void advance() = 0;
	virtual OwningPtr clone() const& = 0;
};

template<bool const_iter>
struct MemberIterator {
	using ImplBase = MemberIteratorImplBase<const_iter>;
	using OwningPtr = typename ImplBase::OwningPtr;
	using value_type = typename ImplBase::value_type;
	using reference = typename ImplBase::reference;

	MemberIterator(OwningPtr impl) : _impl(std::move(impl)) { }
	MemberIterator(const MemberIterator& src) : _impl(src.clone_impl()) {}
	MemberIterator(MemberIterator&& src) = default;
	MemberIterator& operator=(const MemberIterator& src) { _impl = src.clone_impl(); }
	MemberIterator& operator=(MemberIterator&& src) = default;

	reference operator*() { return *_impl->deref(); }
	reference operator*() const { return *_impl->deref(); }
	MemberIterator& operator++() { return _impl->advance(), *this; }

	template<bool const_iter_>
	bool operator==(const MemberIterator<const_iter_>& rhs) const { return _impl->equalTo(*rhs._impl); }
	template<bool const_iter_>
	bool operator!=(const MemberIterator<const_iter_>& rhs) const { return !(*this == rhs); }

private:
	OwningPtr _impl;

	OwningPtr clone_impl() const { return _impl->clone(); }
};

template<bool const_iter>
using MemberIteratorPair = std::pair<MemberIterator<const_iter>, MemberIterator<const_iter>>;

struct Dict  {
	Dict() : impl() {}
	Dict(const Dict& src) : impl() { synchronizeKeys(src.impl); }
	Dict(Dict&&) = default;
	Dict& operator=(const Dict& rhs) { synchronizeKeys(rhs.impl); return *this; }
	Dict& operator=(Dict&&) = default;

	Node toScalars() const;
	const Node& operator[](KeyType key) const {
		auto lookup = impl.find(key);
		if (lookup == impl.end()) throw std::logic_error("Cannot find member of Dict: " + to_string(key));
		return *lookup->second;
	}
	Node& operator[](KeyType key);

	bool empty() const { return impl.empty(); }
	std::size_t size() const { return impl.size(); }

	template<bool const_iter>
	struct Iter : MemberIteratorImplBase<const_iter> {
		using ImplBase = MemberIteratorImplBase<const_iter>;
		using OwningPtr = typename ImplBase::OwningPtr;
		using value_type = typename ImplBase::value_type;
		using Impl = std::conditional_t<const_iter, DictBase::const_iterator, DictBase::iterator>;
		Impl impl;
		Iter(Impl impl_) : impl(impl_) {}
		Iter(const Iter&) = default;
		Iter(Iter&&) = default;
		// static auto create(Impl impl) { return MemberIterator<const_iter>{OwningPtr{new Iter{impl}}}; }
		value_type* deref() override { return &*impl; }
		void advance() override { ++impl; }
		OwningPtr clone() const& override { return OwningPtr{new Iter(*this)}; }
		bool equalTo(const ImplBase& rhs) const override { auto* downcasted = dynamic_cast<const Iter*>(&rhs); return downcasted && (this->impl == downcasted->impl); }
	};

	MemberIterator<false> begin()       { return MemberIteratorImplBase<false>::OwningPtr{new Iter<false>{impl.begin()}}; }
	MemberIterator<true>  begin() const { return MemberIteratorImplBase<true>:: OwningPtr{new Iter<true> {impl.begin()}}; }
	MemberIterator<false> end()         { return MemberIteratorImplBase<false>::OwningPtr{new Iter<false>{impl.end()}}; }
	MemberIterator<true>  end()   const { return MemberIteratorImplBase<true>:: OwningPtr{new Iter<true> {impl.end()}}; }

private:
	DictBase impl;

	void synchronizeKeys(const DictBase& src);
};

using NodeConcreteMemberInfo = std::unique_ptr<Node>;
using NodeConcreteMemberCache = std::map<KeyTypeOwning, NodeConcreteMemberInfo, std::less<>>;

struct NodeConcreteBase {
	virtual const Node& operator[](KeyType key) const = 0;
	virtual       Node& operator[](KeyType key) = 0;
	virtual Node toScalars() const = 0;
	MemberIterator<false> begin()       { return memberIteratorPair().first; }
	MemberIterator<true>  begin() const { return memberIteratorPair().first; }
	MemberIterator<false> end()         { return memberIteratorPair().second; }
	MemberIterator<true>  end()   const { return memberIteratorPair().second; }
	virtual std::unique_ptr<NodeConcreteBase> clone() const& = 0;
	virtual std::unique_ptr<NodeConcreteBase> clone() && = 0;
	virtual bool tryAssign(const NodeConcreteBase&  rhs) = 0;
	virtual bool tryAssign(      NodeConcreteBase&& rhs) = 0;
	virtual ~NodeConcreteBase() = default;
protected:
	virtual MemberIteratorPair<true>  memberIteratorPair() const = 0;
	virtual MemberIteratorPair<false> memberIteratorPair() = 0;
};

template <typename T>
struct NodeConcrete : NodeConcreteBase {
	static_assert(not std::is_reference_v<T>, "Use with a value type instead of a reference");
	static_assert(std::is_copy_constructible_v<T>, "Require copy-constructible types so cloning will generally work");
	static_assert(not std::is_const_v<T>, "NodeConcrete enforces constness itself");
	static_assert(kIsDynamicMemberType<T> + kIsStaticMemberType<T> <= 1, "Types should define at most one of rrvMember and rrvMembers");

	using ContainedType = T;

	NodeConcrete() = default;
	NodeConcrete(const NodeConcrete&) = delete;
	NodeConcrete(NodeConcrete&&) = default;
	NodeConcrete& operator=(const NodeConcrete&) = default;
	NodeConcrete& operator=(NodeConcrete&&) = default;

	const Node& operator[](KeyType key) const override { return *getMember(key).second; }
	      Node& operator[](KeyType key)       override { return *getMember(key).second; }

	Node toScalars() const override;

	MemberIteratorPair<true>  memberIteratorPair() const override { return nodeConcreteGetIterators(*this); }
	MemberIteratorPair<false> memberIteratorPair()       override { return nodeConcreteGetIterators(*this); }

	std::unique_ptr<NodeConcreteBase> clone() const& override { return std::unique_ptr<NodeConcreteBase>(new NodeValue<T>(getObj())); }
	std::unique_ptr<NodeConcreteBase> clone() &&     override { return std::unique_ptr<NodeConcreteBase>(new NodeValue<T>(std::move(getObj()))); }

	virtual bool tryAssign(const NodeConcreteBase& rhs) {
		auto downcasted = dynamic_cast<const NodeConcrete<T>*>(&rhs);
		if (downcasted) getObject() = downcasted->getObject();
		return downcasted;
	}
	virtual bool tryAssign(NodeConcreteBase&& rhs) {
		auto downcasted = dynamic_cast<NodeConcrete<T>*>(&rhs);
		if (downcasted) getObject() = std::move(*downcasted).getObject();
		return downcasted;
	}

	T& getObject() const& { return getObj(); }
	T&& getObject() && { return std::move(getObj()); }

protected:
	virtual T& getObj() const = 0;
	// std::variant<T, T*> obj;
	// T& getObj()       { if (auto* t = std::get_if<      T>(&obj)) return t; else return *std::get<T*>(obj); }
	// T& getObj() const { if (auto* t = std::get_if<const T>(&obj)) return t; else return *std::get<T*>(obj); }
	template<bool const_iter, typename Impl, typename Self>
	friend struct NodeConcreteDynamicMemberIter;

	mutable NodeConcreteMemberCache member_cache = {};

	NodeConcreteMemberCache::reference getMember(KeyType key) const;
	template<typename TT = T> auto initMemberCacheForStaticMembers() const -> std::enable_if_t<kIsStaticMemberType<TT>>;
	template<typename Self>
	MemberIteratorPair<std::is_const_v<Self>> friend nodeConcreteGetIterators(Self& self);
};

template <typename T>
struct NodeValue : NodeConcrete<T> {
	NodeValue(const NodeValue&) = default;
	NodeValue(NodeValue&&) = default;
	NodeValue& operator=(const NodeValue&) = default;
	NodeValue& operator=(NodeValue&&) = default;
	explicit NodeValue(const T& obj_) : obj(obj_) {}
	explicit NodeValue(T&& obj_) : obj(std::move(obj_)) {}

protected:
	T& getObj() const override { return obj; }

private:
	mutable T obj;
};

template <typename T>
struct NodeReference : NodeConcrete<T> {
	NodeReference(const NodeReference&) = default;
	NodeReference(NodeReference&&) = default;
	NodeReference& operator=(const NodeReference&) = default;
	NodeReference& operator=(NodeReference&&) = default;
	explicit NodeReference(T* obj_) : obj(obj_) {}

protected:
	T& getObj() const override { return *obj; }

private:
	T* obj;
};

struct Node {
public:
	using DictImpl = Dict;
	using ObjectImpl = std::unique_ptr<NodeConcreteBase>;
	using Impl = std::variant<DictImpl, ObjectImpl>;

	template<bool unwrap_obj_impl = true, typename Self, typename DictF, typename ObjF>
	friend decltype(auto) visitImpl(Self& self, DictF&& dict_f, ObjF&& obj_f) {
		static_assert(std::variant_size_v<Impl> == 2);
		auto* dict_impl = std::get_if<DictImpl>(&self.impl);
		if (dict_impl) {
			return std::forward<DictF>(dict_f)(*dict_impl);
		} else {
			auto& obj_impl = *std::get_if<ObjectImpl>(&self.impl);
			if constexpr (unwrap_obj_impl) {
				using ObjectPtr = std::conditional_t<std::is_const_v<Self>, const NodeConcreteBase*, NodeConcreteBase*>;
				ObjectPtr obj = obj_impl.get();
				return std::forward<ObjF>(obj_f)(*obj);
			} else {
				return std::forward<ObjF>(obj_f)(obj_impl);
			}
		}
	}

	Node() : impl(DictImpl()) {}
	Node(const Node&  src) : Node() { assign(src); }
	Node(      Node&& src) : Node() { assign(std::move(src)); }
	explicit Node(ObjectImpl ri) : impl(std::move(ri)) {}
	explicit Node(DictImpl   di) : impl(std::move(di)) {}
	Node(const NodeConcreteBase&  nb) : Node() { assign(nb); }
	Node(      NodeConcreteBase&& nb) : Node() { assign(std::move(nb)); }

	template<typename T>
	Node(T&& t) : Node() { assign(std::forward<T>(t)); }

	template<typename T>
	Node& operator=(T&& rhs) { assign(std::forward<T>(rhs)); return *this; }

	template<typename T>
	void assign(T&& rhs) {
		using PlainT = std::remove_cvref_t<T>;
		const bool is_copy_assignment = std::is_reference_v<T>;
		const auto fwd = [](auto& a) -> decltype(auto) {
			if constexpr (is_copy_assignment) {
				return a;
			} else {
				return std::move(a);
			}
		};

		if constexpr (std::is_base_of_v<Node, PlainT>) {
			if constexpr (is_copy_assignment) {
				visitImpl(rhs,
					[this](auto& rhs_dict) { impl = rhs_dict; }, // just copy the dict (may assign through)
					[this](auto& rhs_obj)  { *this = rhs_obj; } // recurse on held object
				);
			} else {
				visitImpl<false>(rhs,
					[this](auto& rhs_dict) { impl = std::move(rhs_dict); }, // just move the dict (may assign through)
					[this](auto& rhs_obj)  {
						visitImpl<false>(*this,
							[&, this](auto& /*this_dict*/) { impl = std::move(rhs_obj); }, // move rhs into us
							[&, this](auto& this_obj) {
								if (not this_obj->tryAssign(std::move(*rhs_obj))) {
									this_obj = std::move(rhs_obj);
								}
							}
						);
					}
				);
			}
		} else if constexpr (std::is_base_of_v<Dict, PlainT>) {
			impl = fwd(rhs); // (variant assigns through if same alternative)
		} else if constexpr (std::is_base_of_v<NodeConcreteBase, PlainT>) {
			visitImpl<false>(*this,
				[&, this](auto&& /*dict*/) { impl = fwd(rhs).clone(); }, // just overwrite the dict
				[&, this](auto&& obj) {
					if (not obj->tryAssign(fwd(rhs))) { // try to assign through
						obj = fwd(rhs).clone(); // different type? overwrite it
					}
				}
			);
		} else {
			visitImpl<false>(*this,
				[&](auto& /*dict*/) {
					// have a Dict; overwrite with object impl by making a new node
					impl = ObjectImpl(new NodeValue<PlainT>(fwd(rhs)));
				},
				[&, this](auto& obj) {
					// don't have a NodeConcreteBase for rhs, so can't use tryAssign as-is
					if (auto downcast = dynamic_cast<NodeConcrete<PlainT>*>(obj.get())) {
						// type matches. Do an assignment to the underlying object
						downcast->getObject() = fwd(rhs);
					} else {
						// type does not match. Make a new node
						obj = ObjectImpl(new NodeValue<PlainT>(fwd(rhs)));
					}
				}
			);
		}
	}

	template<typename T> const T* get_if() const { return get_if_impl<const T>(*this); }
	template<typename T>       T* get_if()       { return get_if_impl<      T>(*this); }
	template<typename T> const T& get()    const { return getImpl<const T>(*this); }
	template<typename T>       T& get()          { return getImpl<      T>(*this); }

	template<typename T>
	T as() const {
		if (auto ptr = get_if<T>()) {
			return *ptr;
		}
		// if constexpr ( can convert T from Dict ) {
		// 	if (auto ptr = get_if<Dict>()) {
		// 		return DictConversion<T>::convert(*ptr);
		// 	} else {
		// 		return DictConversion<T>::convert(toDict());
		// 	}
		// }
		throw std::logic_error(std::string(errors::kAsCannotAccessOrConvert));
	}

	template<typename T> explicit operator const T&() const { return get<T>(); }

	const Node& operator[](KeyType key) const { auto l = [&](auto& obj_or_dict) -> const Node& { return obj_or_dict[key]; }; return visitImpl(*this, l, l); }
	      Node& operator[](KeyType key)       { auto l = [&](auto& obj_or_dict) ->       Node& { return obj_or_dict[key]; }; return visitImpl(*this, l, l); }
	const Node& at(KeyType key) const { return (*this)[key]; }
	      Node& at(KeyType key)       { return (*this)[key]; }

	Node toScalars() const { auto l = [](auto& obj_or_dict) { return obj_or_dict.toScalars(); }; return visitImpl(*this, l, l); }

	MemberIterator<false> begin()       { auto l = [](auto& obj_or_dict) { return obj_or_dict.begin(); }; return visitImpl(*this, l, l); }
	MemberIterator<true>  begin() const { auto l = [](auto& obj_or_dict) { return obj_or_dict.begin(); }; return visitImpl(*this, l, l); }
	MemberIterator<true> cbegin() const { return begin(); }
	MemberIterator<false> end()         { auto l = [](auto& obj_or_dict) { return obj_or_dict.end();   }; return visitImpl(*this, l, l); }
	MemberIterator<true>  end()   const { auto l = [](auto& obj_or_dict) { return obj_or_dict.end();   }; return visitImpl(*this, l, l); }
	MemberIterator<true> cend()   const { return end(); }

private:
	Impl impl;

	template<typename T, typename Self>
	static T* get_if_impl(Self&);

	template<typename T, typename Self>
	static T& getImpl(Self& self) {
		if (auto ptr = self.template get_if<T>()) {
			return *ptr;
		} else {
			throw std::logic_error(std::string(errors::kAccessWithWrongType));
		}
	}
};

// Customization points
template<typename Obj> auto rrvMembers(Obj& t) -> decltype(t.rrvMembers()) { return t.rrvMembers(); }
template<typename Obj> auto rrvMember(Obj& obj, std::string_view key) -> decltype(obj.rrvMember(key)) { return obj.rrvMember(key); }
template<typename Obj> auto rrvMember(Obj& obj, long long key) -> decltype(obj.rrvMember(key)) { return obj.rrvMember(key); }
template<typename Obj> auto rrvBegin(Obj& t) -> decltype(t.rrvBegin()) { return t.rrvBegin(); }
template<typename Obj> auto rrvEnd(Obj& t) -> decltype(t.rrvEnd()) { return t.rrvEnd(); }

// Specializations (must be before static/dynamic tests)
// std::vector
struct IntIter {
	int i;
	IntIter& operator++() { ++i; return *this; }
	auto operator*() { return std::to_string(i); }
	int operator<=>(const IntIter&) const = default;
};
template<typename T> std::variant<T*, std::monostate> rrvMember(std::vector<T>& obj, std::string_view key) {
	const auto i = std::stoi(std::string(key));
	if (i>=0 && (size_t)i<obj.size()) return &obj.at(i); else return std::monostate{};
}
template<typename T> auto rrvBegin(std::vector<T>&) { return IntIter{0}; }
template<typename T> auto rrvEnd(std::vector<T>& v) { return IntIter{(int)v.size()}; }

// Dynamic member test
template<typename Obj> auto rrvMemberTestString() -> decltype(rrvMember(std::declval<Obj&>(), std::declval<std::string_view>()), void()) {}
template<typename Obj, typename = void> constexpr static bool kIsDynamicMemberTypeByString = false;
template<typename Obj> constexpr static bool kIsDynamicMemberTypeByString<Obj, decltype(rrvMemberTestString<Obj>())> = true;

template<typename Obj> auto rrvMemberTestInt() -> decltype(rrvMember(std::declval<Obj&>(), std::declval<long long>()), void()) {}
template<typename Obj, typename = void> constexpr static bool kIsDynamicMemberTypeByInt = false;
template<typename Obj> constexpr static bool kIsDynamicMemberTypeByInt<Obj, decltype(rrvMemberTestInt<Obj>())> = true;

template<typename Obj> constexpr static bool kIsDynamicMemberType<Obj, std::enable_if_t<kIsDynamicMemberTypeByString<Obj> || kIsDynamicMemberTypeByInt<Obj>>> = true;

// Static member test
template<typename Obj> auto rrvMembersTest() -> decltype(rrvMembers(std::declval<Obj&>()), void()) {}
template<typename Obj> constexpr static bool kIsStaticMemberType<Obj, decltype(rrvMembersTest<Obj>())> = true;


// Internal dispatch
namespace detail {

template <typename F>
decltype(auto) callWithString(F&& f, const KeyType& key) {
	struct V {
		F& f;
		decltype(auto) operator()(std::string_view s) {
			return std::forward<F>(f)(s);
		}
		decltype(auto) operator()(long long i) {
			// -2**63 = -9223372036854775808
			static_assert(std::numeric_limits<long long>::digits10 <= 19);
			std::array<char, 19 + 1> s; // +1 for the sign
			auto [ptr, ec] = std::to_chars(s.data(), s.data() + s.size(), i);
			if (ec == std::errc()) {
				return (*this)({s.data(), ptr});
			} else {
				throw std::logic_error("failed to convert integer to string");
			}
		}
	};
	return std::visit(V{f}, key.impl);
}

template <typename F>
decltype(auto) callWithInt(F&& f, const KeyType& key) {
	struct V {
		F& f;
		decltype(auto) operator()(long long i) {
			return std::forward<F>(f)(i);
		}
		decltype(auto) operator()(std::string_view s) {
			long long i;
			auto [ptr, ec] = std::from_chars(s.data(), s.data() + s.size(), i);
			if (ec == std::errc() && ptr == s.end()) {
				return (*this)(i);
			} else {
				throw std::logic_error("Could not convert " + std::string(s) + " to an integer, and this type does not accept strings for rrvMember");
			}
		}
	};
	return std::visit(V{f}, key.impl);
}

template<typename Obj> auto rrvMemberFromKey(Obj& obj, const KeyType& key) -> decltype(rrvMember(obj, std::declval<std::string_view>())) {
	return callWithString([&](std::string_view sv) { return rrvMember(obj, sv); }, key);
}
template<typename Obj> auto rrvMemberFromKey(Obj& obj, const KeyType& key) -> decltype(rrvMember(obj, std::declval<long long>())) {
	return callWithInt([&](long long i) { return rrvMember(obj, i); }, key);
}

}

template<typename T, typename Self>
T* Node::get_if_impl(Self& self) {
	using PlainT = std::remove_cv_t<T>;

	return visitImpl(self,
		[](auto&& dict) -> T* {
			if constexpr (std::is_same_v<PlainT, Dict>) {
				return &dict;
			} else {
				throw std::logic_error(std::string(errors::kAccessNodeAsObjectButIsDict));
			}
		},
		[](auto&& obj) -> T* {
			constexpr auto self_is_const = std::is_const_v<Self>;
			using NodeConcreteSameConst = std::conditional_t<self_is_const, const NodeConcrete<PlainT>, NodeConcrete<PlainT>>;
			if (auto downcasted = dynamic_cast<NodeConcreteSameConst*>(&obj)) {
				return &downcasted->getObject();
			} else {
				return nullptr; // access with wrong type case
			}
		}
	);
}

void Dict::synchronizeKeys(const DictBase& src) {
	DictBase new_impl;
	for (const auto& [k, src_v] : src) {
		if (auto old_node = impl.extract(k)) {
			*old_node.mapped() = *src_v;
			new_impl.insert(std::move(old_node));
		} else {
			new_impl.emplace(k, DictBase::mapped_type(new Node(*src_v)));
		}
	}
	impl = std::move(new_impl);
}

Node Dict::toScalars() const { return Node(*this); }

Node& Dict::operator[](KeyType key) {
	auto lookup = impl.lower_bound(key);
	if (lookup == impl.end() or lookup->first != key) {
		lookup = impl.emplace_hint(lookup, to_string(key), DictBase::mapped_type{new Node()});
	}
	return *lookup->second;
}

template<typename T>
Node NodeConcrete<T>::toScalars() const {
	Node r;
	for (const auto& name_and_member : *this) {
		r[name_and_member.first] = *name_and_member.second;
	}
	return r;
}

template <typename Container, typename T>
struct NodeIndirectAcess : NodeConcrete<T> {
	NodeIndirectAcess(const NodeIndirectAcess&) = default;
	NodeIndirectAcess(NodeIndirectAcess&&) = default;
	NodeIndirectAcess& operator=(const NodeIndirectAcess&) = default;
	NodeIndirectAcess& operator=(NodeIndirectAcess&&) = default;
	explicit NodeIndirectAcess(Container* container_, KeyType key_) : container(container_), key(key_) {}

protected:
	T& getObj() const override {
		return *std::get<T*>(rrvMemberFromKey(*container, key));
	}

private:
	Container* container;
	KeyTypeOwning key;
};

template<typename T>
NodeConcreteMemberCache::reference NodeConcrete<T>::getMember(KeyType key) const {
	static_assert(kIsScalarType<T> + kIsDynamicMemberType<T> + kIsStaticMemberType<T> == 1, "Internal error");
	if constexpr (kIsDynamicMemberType<T>) {
		return std::visit(
			[this, &key](auto member) -> NodeConcreteMemberCache::reference {
				if constexpr (std::is_same_v<decltype(member), std::monostate>) {
					throw std::logic_error("Member not found");
				} else {
					using Member = std::remove_pointer_t<decltype(member)>;
					auto [lookup, is_new] = this->member_cache.emplace(key, NodeConcreteMemberInfo{});
					auto& member_info = lookup->second;
					if (is_new) {
						member_info = NodeConcreteMemberInfo(new Node(Node::ObjectImpl(new NodeIndirectAcess<T, Member>(&getObj(), key))));
					}
					return *lookup;
				}
			},
			rrvMemberFromKey(getObj(), key)
		);
	} else if constexpr (kIsStaticMemberType<T>) {
		initMemberCacheForStaticMembers();
		auto lookup = member_cache.find(key);
		if (lookup == member_cache.end()) {
			auto set_lookup = [&lookup, this](auto e) mutable {
				lookup = member_cache.find(KeyType(e));
			};
			if (std::holds_alternative<long long>(key.impl)) {
				callWithString(set_lookup, key);
			}
			if (std::holds_alternative<std::string_view>(key.impl)) {
				callWithInt(set_lookup, key);
			}
		}
		if (lookup == member_cache.end()) {
			throw std::logic_error("Member not found");
		}
		return *lookup;
	} else if constexpr (kIsScalarType<T>) {
		throw std::logic_error(std::string(errors::kAccessMemberOfScalarType));
	}
}

template<typename T> template<typename TT>
auto NodeConcrete<T>::initMemberCacheForStaticMembers() const -> std::enable_if_t<kIsStaticMemberType<TT>> {
	if (not member_cache.empty()) return;

	const auto add_elem = [this](auto&& elem) {
		using MemberType = std::remove_reference_t<decltype(*elem.second)>;
		static_assert(not std::is_const_v<MemberType>, "rrvMembers should not return pointers to const. Perhaps 'this' is const in the implementation of rrvMembers.");
		this->member_cache.emplace(elem.first, NodeConcreteMemberInfo(new Node(Node::ObjectImpl(new NodeReference<MemberType>(elem.second)))));
	};

	const auto add_all = [&add_elem](auto&&... elems) {
		(add_elem(std::forward<decltype(elems)>(elems)), ...);
	};

	std::apply(add_all, rrvMembers(this->getObj()));
}

template<bool const_iter>
struct NodeConcreteStaticMemberIter : MemberIteratorImplBase<const_iter> {
	using BaseClass = MemberIteratorImplBase<const_iter>;
	using OwningPtr = typename BaseClass::OwningPtr;
	using value_type = typename BaseClass::value_type;
	using Impl = std::conditional_t<const_iter, NodeConcreteMemberCache::const_iterator, NodeConcreteMemberCache::iterator>;
	Impl impl;
	NodeConcreteStaticMemberIter(Impl impl_) : impl(impl_) {}
	NodeConcreteStaticMemberIter(const NodeConcreteStaticMemberIter&) = default;
	NodeConcreteStaticMemberIter(NodeConcreteStaticMemberIter&&) = default;
	value_type* deref() override { return &*impl; }
	void advance() override { ++impl; }
	OwningPtr clone() const& override { return OwningPtr{new NodeConcreteStaticMemberIter(*this)}; }
	bool equalTo(const BaseClass& rhs) const override { auto* downcasted = dynamic_cast<const NodeConcreteStaticMemberIter*>(&rhs); return downcasted && (this->impl == downcasted->impl); }
};

template<bool const_iter, typename Impl, typename Self>
struct NodeConcreteDynamicMemberIter : MemberIteratorImplBase<const_iter> {
	using BaseClass = MemberIteratorImplBase<const_iter>;
	using OwningPtr = typename BaseClass::OwningPtr;
	using value_type = typename BaseClass::value_type;
	Impl impl;
	Self* self;
	NodeConcreteDynamicMemberIter(Impl impl_, Self* self_) : impl(impl_), self(self_) {}
	NodeConcreteDynamicMemberIter(const NodeConcreteDynamicMemberIter&) = default;
	NodeConcreteDynamicMemberIter(NodeConcreteDynamicMemberIter&&) = default;
	NodeConcreteDynamicMemberIter& operator=(const NodeConcreteDynamicMemberIter&) = default;
	NodeConcreteDynamicMemberIter& operator=(NodeConcreteDynamicMemberIter&&) = default;

	value_type* deref() override { return &self->getMember(KeyTypeOwning(derefImpl<Impl>())); }
	void advance() override { incrementImpl<Impl>(); }
	OwningPtr clone() const& override { return OwningPtr{new NodeConcreteDynamicMemberIter(*this)}; }
	bool equalTo(const BaseClass& rhs) const override { auto* downcasted = dynamic_cast<const NodeConcreteDynamicMemberIter*>(&rhs); return downcasted && equalImpl(downcasted->impl); }

	// These have to be templates to allow SFINAE to work
	template<typename TImpl> auto incrementImpl() -> decltype(std::declval<TImpl&>().increment(self->getObject())) { return impl.increment(self->getObject()); }
	template<typename TImpl> auto incrementImpl() -> decltype(++std::declval<TImpl&>()) { return ++impl; }
	template<typename TImpl> auto derefImpl() -> decltype(std::declval<TImpl&>().dereference(self->getObject())) { return impl.dereference(self->getObject()); }
	template<typename TImpl> auto derefImpl() -> decltype(*std::declval<TImpl&>()) { return *impl; }
	template<typename TImpl> auto equalImpl(const TImpl& rhs) const -> decltype(std::declval<TImpl&>().equals(rhs, *self)) { return impl.equals(rhs, *self); }
	template<typename TImpl> auto equalImpl(const TImpl& rhs) const -> decltype(std::declval<TImpl&>() == rhs) { return impl == rhs; }
};

template<typename Self>
std::pair<MemberIterator<std::is_const_v<Self>>, MemberIterator<std::is_const_v<Self>>> nodeConcreteGetIterators(Self& self) {
	constexpr bool self_is_const = std::is_const_v<Self>;
	using T = Self::ContainedType;

	static_assert(kIsScalarType<T> + kIsDynamicMemberType<T> + kIsStaticMemberType<T> == 1, "Internal error");
	if constexpr (kIsDynamicMemberType<T>) {
		return {
			// TODO: avoid the allocation somehow?
			typename MemberIteratorImplBase<self_is_const>::OwningPtr{new NodeConcreteDynamicMemberIter<self_is_const, decltype(rrvBegin(self.getObj())), Self>{rrvBegin(self.getObj()), &self}},
			typename MemberIteratorImplBase<self_is_const>::OwningPtr{new NodeConcreteDynamicMemberIter<self_is_const, decltype(rrvEnd(self.getObj())), Self>{rrvEnd(self.getObj()), &self}},
		};
	} else if constexpr (kIsStaticMemberType<T>) {
		self.initMemberCacheForStaticMembers();
		return {
			// TODO: avoid the allocation somehow?
			typename MemberIteratorImplBase<self_is_const>::OwningPtr{new NodeConcreteStaticMemberIter<self_is_const>{self.member_cache.begin()}},
			typename MemberIteratorImplBase<self_is_const>::OwningPtr{new NodeConcreteStaticMemberIter<self_is_const>{self.member_cache.end()}},
		};
	} else if constexpr (kIsScalarType<T>) {
		throw std::logic_error(std::string(errors::kAccessMemberOfScalarType));
	}
}

template <typename Nb>
Nb* pathSubscript_impl(Nb& n, std::string_view path, char sep) {
	std::size_t spos = 0;
	Nb* curr = &n;
	for (;;) {
		const std::size_t epos_maybe_npos = path.find_first_of(sep, spos);
		const auto epos = epos_maybe_npos == path.npos ? path.size() : epos_maybe_npos;
		const auto path_elem = path.substr(spos, epos - spos);
		if (path_elem.size() < 1) {
			throw std::logic_error(std::string("Empty path element in path with sep='") + sep + "': " + std::string(path));
		}
		// version that returns nullptr if this fails? rn, this creates a new node, if it's a Node underneath.
		curr = &(*curr)[path_elem];
		if (epos_maybe_npos == path.npos) {
			return curr;
		}
		spos = epos + 1;
	}
}

Node* pathSubscript(Node& n, std::string_view path, char sep = '.') { return pathSubscript_impl(n, path, sep); }
const Node* pathSubscript(const Node& n, std::string_view path, char sep = '.') { return pathSubscript_impl(n, path, sep); }


}  // namespace rrv