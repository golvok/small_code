#pragma once

#include <map>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <variant>
#include <vector>

namespace rrv {

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
	constexpr auto kAssignObjectToDict = "Trying to assign an object to a Dict (via a Node)"sv;
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

using DictBase = std::map<std::string, std::unique_ptr<Node>, std::less<>>;

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

// TODO: use composition instead....
struct Dict  {
	Dict() : impl() {}
	Dict(const Dict& src) : impl() { *this = src; }
	Dict(Dict&&) = default;
	Dict(const DictBase& src) : impl() { *this = src; }
	Dict(DictBase&& src) : impl() { *this = std::move(src); }

	Dict& operator=(const DictBase& rhs);
	Dict& operator=(const Dict& rhs) { return *this = rhs.impl; }
	Dict& operator=(Dict&&) = default;
	Dict& operator=(const Node& rhs);
	Dict& operator=(Node&& rhs);

	Node toScalars() const;
	const Node& operator[](std::string_view sv) const {
		auto lookup = impl.find(sv);
		if (lookup == impl.end()) throw std::logic_error("Cannot find member of Dict: " + std::string(sv));
		return *lookup->second;
	}
	Node& operator[](std::string_view sv);

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
};

using NodeConcreteMemberInfo = std::unique_ptr<Node>;
using NodeConcreteMemberCache = std::map<std::string, NodeConcreteMemberInfo, std::less<>>;

struct NodeConcreteBase {
	virtual const Node& operator[](std::string_view key) const = 0;
	virtual       Node& operator[](std::string_view key) = 0;
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

	const Node& operator[](std::string_view key) const override { return *getMember(key).second; }
	      Node& operator[](std::string_view key)       override { return *getMember(key).second; }

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

	NodeConcreteMemberCache::reference getMember(std::string_view key) const;
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

	const Node& operator[](std::string_view sv) const { auto l = [&](auto& obj_or_dict) -> const Node& { return obj_or_dict[sv]; }; return visitImpl(*this, l, l); }
	      Node& operator[](std::string_view sv)       { auto l = [&](auto& obj_or_dict) ->       Node& { return obj_or_dict[sv]; }; return visitImpl(*this, l, l); }
	const Node& at(std::string_view sv) const { return (*this)[sv]; }
	      Node& at(std::string_view sv)       { return (*this)[sv]; }

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
template<typename Obj> auto rrvMemberTest() -> decltype(rrvMember(std::declval<Obj&>(), std::declval<std::string_view>()), void()) {}
template<typename Obj> constexpr static bool kIsDynamicMemberType<Obj, decltype(rrvMemberTest<Obj>())> = true;

// Static member test
template<typename Obj> auto rrvMembersTest() -> decltype(rrvMembers(std::declval<Obj&>()), void()) {}
template<typename Obj> constexpr static bool kIsStaticMemberType<Obj, decltype(rrvMembersTest<Obj>())> = true;

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

Dict& Dict::operator=(const DictBase& rhs) {
	impl.clear();
	for (const auto& [k, v] : rhs) {
		impl.emplace(k, DictBase::mapped_type(new Node(*v)));
	}
	return *this;
}
Dict& Dict::operator=(const Node& rhs) {
	return visitImpl(rhs,
		[this](auto&& dict) -> Dict& { return *this = dict; },
		[this](auto&& /* obj */) -> Dict& {
			throw std::logic_error(std::string(errors::kAssignObjectToDict));
		}
	);
}
Dict& Dict::operator=(Node&& rhs) {
	return visitImpl(rhs,
		[this](auto&& dict) -> Dict& { return *this = std::move(dict); },
		[this](auto&& /* obj */) -> Dict& {
			throw std::logic_error(std::string(errors::kAssignObjectToDict));
		}
	);
}

Node Dict::toScalars() const { return Node(*this); }

Node& Dict::operator[](std::string_view sv) {
	auto lookup = impl.lower_bound(sv);
	if (lookup == impl.end() or lookup->first != sv) {
		lookup = impl.emplace_hint(lookup, std::string(sv), DictBase::mapped_type{new Node()});
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
	explicit NodeIndirectAcess(Container* container_, std::string_view key_) : container(container_), key(key_) {}

protected:
	T& getObj() const override {
		return *std::get<T*>(rrvMember(*container, key));
	}

private:
	Container* container;
	std::string key;
};

template<typename T>
NodeConcreteMemberCache::reference NodeConcrete<T>::getMember(std::string_view key) const {
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
			rrvMember(getObj(), key)
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