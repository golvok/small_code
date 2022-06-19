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

namespace detail {}
using namespace detail;

template <typename F, typename TInt>
decltype(auto) callWithString(F&& f, TInt i) {
	// -2**63 = -9223372036854775808
	std::array<char, std::numeric_limits<TInt>::digits10 + 1> s; // +1 for the sign
	auto [ptr, ec] = std::to_chars(s.data(), s.data() + s.size(), i);
	if (ec == std::errc()) {
		return std::forward<F>(f)({s.data(), ptr});
	} else {
		throw std::logic_error("failed to convert integer to string");
	}
}

std::string asString(long long i) {
	return callWithString([](std::string_view s) { return std::string(s); }, i);
}

std::optional<long long> asInt(std::string_view s) {
	long long i;
	auto [ptr, ec] = std::from_chars(s.data(), s.data() + s.size(), i);
	if (ec == std::errc() && ptr == s.end()) {
		return i;
	} else {
		return std::nullopt;
	}
}

struct KeyReference;
struct Key {
	explicit Key(long long i) : i(i), s(asString(i)) {}
	explicit Key(std::string s) : i(asInt(s)), s(std::move(s)) {}
	explicit Key(const KeyReference&);

	std::optional<long long> i;
	std::string s;
};

struct KeyReference {
	explicit KeyReference(long long i) = delete;
	explicit KeyReference(std::string_view s) : i(asInt(s)), s(s) {}
	KeyReference(const Key& k) : i(k.i), s(k.s) {}

	std::optional<long long> i;
	std::string_view s;

	bool operator<(const KeyReference& rhs) const { if (this->i || rhs.i) return this->i < rhs.i; else return this->s < rhs.s; }
	bool operator==(const KeyReference& rhs) const { if (this->i || rhs.i) return this->i == rhs.i; else return this->s == rhs.s; }
};

Key::Key(const KeyReference& k) : i(k.i), s(std::string(k.s)) {}

/** @brief A type for function parameters. Accepts many implicit conversions and easily forms non-owning references. */
struct InterfaceKey {
	InterfaceKey(const Key&   k) : InterfaceKey(KeyReference(k)) {}
	InterfaceKey(Key&&        k) : impl(std::move(k)) {}
	InterfaceKey(KeyReference k) : impl(k) {}

	InterfaceKey(std::string&&      s) : impl(Key(std::move(s))) {}
	InterfaceKey(const std::string& s) : impl(KeyReference(s)) {}
	InterfaceKey(std::string_view   s) : impl(KeyReference(s)) {}
	InterfaceKey(const char*        s) : impl(KeyReference(s)) {}

	InterfaceKey(int       i) : InterfaceKey(static_cast<long long>(i)) {}
	InterfaceKey(long      i) : InterfaceKey(static_cast<long long>(i)) {}
	InterfaceKey(long long i) : impl(Key(i)) {}

	// questionable, but convenient
	InterfaceKey(unsigned int       i) : InterfaceKey(static_cast<unsigned long>(i)) {}
	InterfaceKey(unsigned long      i) : InterfaceKey(static_cast<unsigned long long>(i)) {}
	InterfaceKey(unsigned long long i) : InterfaceKey(static_cast<long long>(i)) { if (std::numeric_limits<long long>::max() < i) { throw std::runtime_error("Signed cast would loose data"); } }

	// ban implicit conversions from floating point types
	InterfaceKey(float  d) = delete;
	InterfaceKey(double d) = delete;

	InterfaceKey(const InterfaceKey&) = default;
	InterfaceKey(InterfaceKey&&) = default;
	InterfaceKey& operator=(const InterfaceKey&) = default;
	InterfaceKey& operator=(InterfaceKey&&) = default;

	operator KeyReference() const& {
		if (auto kr = std::get_if<KeyReference>(&impl)) { return *kr; }
		else return std::get<Key>(impl);
	}

	operator Key() && {
		if (auto kr = std::get_if<KeyReference>(&impl)) { return Key(*kr); }
		else return std::move(std::get<Key>(impl));
	}

	explicit operator Key() const& {
		if (auto kr = std::get_if<KeyReference>(&impl)) { return Key(*kr); }
		else return std::get<Key>(impl);
	}

private:
	std::variant<Key, KeyReference> impl;
};

bool operator==(const InterfaceKey& lhs, const InterfaceKey& rhs) { return KeyReference(lhs) == KeyReference(rhs); };
bool operator<(const InterfaceKey& lhs, const InterfaceKey& rhs) { return KeyReference(lhs) < KeyReference(rhs); };

struct Node;
struct NodeConcreteBase;
template<typename T> struct NodeConcrete;
template<typename T> struct NodeReference;
template<typename T> struct NodeValue;
template<typename Container, typename T> struct NodeIndirectAcess;
struct Dict;
template<bool> struct MemberIterator;
template<bool> struct MemberIteratorImplBase;

template<typename Obj, typename = void> constexpr static bool kIsDynamicMemberTypeByString = false;
template<typename Obj, typename = void> constexpr static bool kIsDynamicMemberTypeByInt = false;
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
	constexpr auto kAccessMemberOfConcreteType = "Can't get member of concrete type"sv;
}

template<typename Source, typename Target>
using SameConstAs = std::conditional_t<std::is_const_v<Source>, const Target, Target>;

using DictBase = std::map<Key, Node, std::less<>>;

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
	const Node& operator[](InterfaceKey key) const;
	Node& operator[](InterfaceKey key);

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

using NodeConcreteMemberCache = std::map<Key, Node, std::less<>>;
struct NodeConcreteBase {
	virtual const Node& operator[](InterfaceKey key) const = 0;
	virtual       Node& operator[](InterfaceKey key) = 0;
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
	static_assert(kIsDynamicMemberTypeByString<T> + kIsDynamicMemberTypeByInt<T> <= 1, "Types should define at most one rrvMember overload");

	using ContainedType = T;

	NodeConcrete() = default;
	NodeConcrete(const NodeConcrete&) = delete;
	NodeConcrete(NodeConcrete&&) = default;
	NodeConcrete& operator=(const NodeConcrete&) = default;
	NodeConcrete& operator=(NodeConcrete&&) = default;

	const Node& operator[](InterfaceKey key) const override { return getMember(key).second; }
	      Node& operator[](InterfaceKey key)       override { return getMember(key).second; }

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

	NodeConcreteMemberCache::reference getMember(InterfaceKey key) const;
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
	using DictImpl = std::unique_ptr<Dict>;
	using ObjectImpl = std::unique_ptr<NodeConcreteBase>;
	using Impl = std::variant<DictImpl, ObjectImpl>;

	template<bool unwrap_impl = true, typename Self, typename DictF, typename ObjF>
	friend decltype(auto) visitImpl(Self& self, DictF&& dict_f, ObjF&& obj_f) {
		static_assert(std::variant_size_v<Impl> == 2);
		auto* dict_impl = std::get_if<DictImpl>(&self.impl);
		if (dict_impl) {
			if constexpr (unwrap_impl) {
				using DictPtr = std::conditional_t<std::is_const_v<Self>, const Dict*, Dict*>;
				return std::forward<DictF>(dict_f)(*DictPtr(dict_impl->get()));
			} else {
				return std::forward<DictF>(dict_f)(*dict_impl);
			}
		} else {
			auto* obj_impl = std::get_if<ObjectImpl>(&self.impl);
			if constexpr (unwrap_impl) {
				using ObjectPtr = std::conditional_t<std::is_const_v<Self>, const NodeConcreteBase*, NodeConcreteBase*>;
				return std::forward<ObjF>(obj_f)(*ObjectPtr(obj_impl->get()));
			} else {
				return std::forward<ObjF>(obj_f)(*obj_impl);
			}
		}
	}

	Node() : impl(DictImpl(new Dict())) {}
	Node(const Node&  src) : Node() { assign(src); }
	Node(      Node&& src) : Node() { assign(std::move(src)); }
	explicit Node(ObjectImpl oi) : impl(std::move(oi)) {}
	explicit Node(DictImpl   di) : impl(std::move(di)) {}
	Node(const NodeConcreteBase&  nb) : Node() { assign(nb); }
	Node(      NodeConcreteBase&& nb) : Node() { assign(std::move(nb)); }

	template<typename T>
	Node(T&& t) : Node() { assign(std::forward<T>(t)); }

	Node& operator=(const Node& rhs) { assign(rhs); return *this; }
	Node& operator=(Node&& rhs     ) { assign(std::move(rhs)); return *this; }
	Node& operator=(ObjectImpl&& oi) { impl = std::move(oi); return *this; }
	Node& operator=(DictImpl&&   di) { impl = std::move(di); return *this; }

	Node& operator=(const Node*      ) = delete; // don't select general case
	Node& operator=(const ObjectImpl&) = delete; // don't select general case
	Node& operator=(const DictImpl&  ) = delete; // don't select general case

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
					[this](auto& rhs_dict) {
						visitImpl(*this,
							[&, this](auto& this_dict) { this_dict = rhs_dict; }, // move underlying dict into us
							[&, this](auto& /*this_obj*/) { impl = DictImpl(new Dict(rhs_dict)); } // copy the dict
						);
					},
					[this](auto& rhs_obj)  { *this = rhs_obj; } // recurse on held object
				);
			} else {
				visitImpl<false>(rhs,
					[this](auto& rhs_dict) {
						visitImpl(*this,
							[&, this](auto& this_dict) { this_dict = std::move(*rhs_dict); }, // move underlying dict into us
							[&, this](auto& /*this_obj*/) { impl = std::move(rhs_dict); } // move dict ptr into us
						);
					},
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
			impl = DictImpl(new Dict(fwd(rhs))); // (variant assigns through if same alternative)
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

	const Node& operator[](InterfaceKey key) const { auto l = [&](auto& obj_or_dict) -> const Node& { return obj_or_dict[key]; }; return visitImpl(*this, l, l); }
	      Node& operator[](InterfaceKey key)       { auto l = [&](auto& obj_or_dict) ->       Node& { return obj_or_dict[key]; }; return visitImpl(*this, l, l); }
	const Node& at(InterfaceKey key) const { return (*this)[key]; }
	      Node& at(InterfaceKey key)       { return (*this)[key]; }

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
struct SizeTypeIter {
	std::size_t i;
	SizeTypeIter& operator++() { ++i; return *this; }
	auto operator*() { return i; }
	std::strong_ordering operator<=>(const SizeTypeIter&) const = default;
};
template<typename T> std::variant<T*, std::monostate> rrvMember(std::vector<T>& obj, long long key) {
	if (key>=0 && (size_t)key<obj.size()) return &obj.at(key); else return std::monostate{};
}
template<typename T> auto rrvBegin(std::vector<T>&) { return SizeTypeIter{0}; }
template<typename T> auto rrvEnd(std::vector<T>& v) { return SizeTypeIter{v.size()}; }

// Dynamic member test
template<typename Obj> auto rrvMemberTestString() -> decltype(rrvMember(std::declval<Obj&>(), std::declval<std::string_view>()), void()) {}
template<typename Obj> auto rrvMemberTestInt() -> decltype(rrvMember(std::declval<Obj&>(), std::declval<long long>()), void()) {}
template<typename Obj> constexpr static bool kIsDynamicMemberTypeByString<Obj, decltype(rrvMemberTestString<Obj>())> = true;
template<typename Obj> constexpr static bool kIsDynamicMemberTypeByInt<Obj, decltype(rrvMemberTestInt<Obj>())> = true;
template<typename Obj> constexpr static bool kIsDynamicMemberType<Obj, std::enable_if_t<kIsDynamicMemberTypeByString<Obj> || kIsDynamicMemberTypeByInt<Obj>>> = true;

// Static member test
template<typename Obj> auto rrvMembersTest() -> decltype(rrvMembers(std::declval<Obj&>()), void()) {}
template<typename Obj> constexpr static bool kIsStaticMemberType<Obj, decltype(rrvMembersTest<Obj>())> = true;


// Internal dispatch
namespace detail {


template<typename Obj> auto rrvMemberFromKey(Obj& obj, const KeyReference& key) -> decltype(rrvMember(obj, std::declval<std::string_view>())) {
	return rrvMember(obj, key.s);
}
template<typename Obj> auto rrvMemberFromKey(Obj& obj, const KeyReference& key) -> decltype(rrvMember(obj, std::declval<long long>())) {
	auto maybe_int = key.i;
	if (not maybe_int) throw std::logic_error("Could not convert " + std::string(key.s) + " to an integer, and this type does not accept strings for rrvMember");
	return rrvMember(obj, *maybe_int);
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
			old_node.mapped() = src_v;
			new_impl.insert(std::move(old_node));
		} else {
			new_impl.emplace(k, src_v);
		}
	}
	impl = std::move(new_impl);
}

Node Dict::toScalars() const { return Node(*this); }

const Node& Dict::operator[](InterfaceKey key) const {
	auto lookup = impl.find(key);
	if (lookup == impl.end()) throw std::logic_error("Cannot find member of Dict: " + Key(key).s);
	return lookup->second;
}

Node& Dict::operator[](InterfaceKey key) {
	auto lookup = impl.lower_bound(key);
	if (lookup == impl.end() or lookup->first != key) {
		lookup = impl.emplace_hint(lookup, Key(std::move(key)), Node{});
	}
	return lookup->second;
}

template<typename T>
Node NodeConcrete<T>::toScalars() const {
	Node r;
	for (const auto& name_and_member : *this) {
		r[Key(name_and_member.first)] = name_and_member.second;
	}
	return r;
}

template <typename Container, typename T>
struct NodeIndirectAcess : NodeConcrete<T> {
	NodeIndirectAcess(const NodeIndirectAcess&) = default;
	NodeIndirectAcess(NodeIndirectAcess&&) = default;
	NodeIndirectAcess& operator=(const NodeIndirectAcess&) = default;
	NodeIndirectAcess& operator=(NodeIndirectAcess&&) = default;
	explicit NodeIndirectAcess(Container* container_, Key key_) : container(container_), key(key_) {}

protected:
	T& getObj() const override {
		return *std::get<T*>(rrvMemberFromKey(*container, key));
	}

private:
	Container* container;
	Key key;
};

template<typename T>
NodeConcreteMemberCache::reference NodeConcrete<T>::getMember(InterfaceKey key) const {
	static_assert(kIsScalarType<T> + kIsDynamicMemberType<T> + kIsStaticMemberType<T> == 1, "Internal error");
	if constexpr (kIsDynamicMemberType<T>) {
		return std::visit(
			[this, &key](auto member) -> NodeConcreteMemberCache::reference {
				if constexpr (std::is_same_v<decltype(member), std::monostate>) {
					throw std::logic_error("Member not found");
				} else {
					using Member = std::remove_pointer_t<decltype(member)>;
					auto [lookup, is_new] = this->member_cache.emplace(std::move(key), Node{});
					auto& member_info = lookup->second;
					if (is_new) {
						member_info = Node::ObjectImpl(new NodeIndirectAcess<T, Member>(&getObj(), lookup->first));
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
		this->member_cache.emplace(InterfaceKey(elem.first), Node::ObjectImpl(new NodeReference<MemberType>(elem.second)));
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

	value_type* deref() override { return &self->getMember(derefImpl<Impl>()); }
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