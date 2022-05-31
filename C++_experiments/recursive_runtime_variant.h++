#pragma once

#include <map>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <variant>
#include <vector>

namespace rrv {

class NodeBase;
template<typename T> class NodeConcrete;
template<typename T> class NodeReference;
template<typename T> class NodeValue;
struct Dict;
class NodeOwning;
template<bool>
class MemberIterator;
template<bool>
class MemberIteratorImplBase;

template<typename Obj, typename = void> constexpr static bool kIsDynamicMemberType = false;
template<typename Obj, typename = void> constexpr static bool kIsStaticMemberType = false;
template<typename Obj> constexpr static bool kIsScalarType = not kIsStaticMemberType<Obj> and not kIsDynamicMemberType<Obj>;

namespace errors {
	using namespace std::literals::string_view_literals;
	constexpr auto kAssignObjectInNodeOwningToDict = "Trying to assign an object to a Dict (via a NodeOwning)"sv;
	constexpr auto kAccessMemberOfScalarType = "Accessing member of a scalar type"sv;
	constexpr auto kAccessNodeOwningAsObjectButIsDict = "Trying to access a NodeOwning as an object, but it is a Dict"sv;
	constexpr auto kAccessWithWrongType = "Accessing with wrong type"sv;
	constexpr auto kAsCannotAccessOrConvert = ".as is unable to access as or convert to the type requested"sv;
	constexpr auto kCloneNotImplemented_ConstRef = "clone const& not implemented"sv;
	constexpr auto kCloneNotImplemented_RvalRef = "clone && not implemented"sv;
	constexpr auto kAccessMemberOfConcreteType = "can't get member of concrete type"sv;
	constexpr auto kAssignObjectToDict = "Trying to assign an object to a Dict"sv;
}

template<typename Source, typename Target>
using SameConstAs = std::conditional_t<std::is_const_v<Source>, const Target, Target>;

using DictBase = std::map<std::string, std::unique_ptr<NodeBase>, std::less<>>;

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
class MemberIterator {
	using ImplBase = MemberIteratorImplBase<const_iter>;
	using OwningPtr = typename ImplBase::OwningPtr;
	OwningPtr _impl;

	OwningPtr clone_impl() const { return _impl->clone(); }

public:
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
};

class NodeBase {
public:
	template <typename T>
	NodeBase& operator=(T&& rhs);
	virtual ~NodeBase() = default;

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

	virtual const NodeBase& operator[](std::string_view sv) const = 0;
	virtual NodeBase& operator[](std::string_view sv) = 0;
	const NodeBase& at(std::string_view sv) const { return (*this)[sv]; }
	NodeBase& at(std::string_view sv) { return (*this)[sv]; }

	virtual NodeOwning toScalars() const = 0;

	virtual MemberIterator<false> begin() = 0;
	virtual MemberIterator<true> begin() const = 0;
	MemberIterator<true> cbegin() const { return begin(); }
	virtual MemberIterator<false> end() = 0;
	virtual MemberIterator<true> end() const = 0;
	MemberIterator<true> cend() const { return end(); }

	virtual std::unique_ptr<NodeBase> clone() const& = 0;
	virtual std::unique_ptr<NodeBase> clone() && = 0;

private:
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

// TODO: use composition instead....
struct Dict : NodeBase, private DictBase {
public:
	Dict() {}
	Dict(const Dict& src) : DictBase() { *this = src; }
	Dict(Dict&&) = default;
	Dict(const DictBase& src) : DictBase() { *this = src; }
	Dict(DictBase&& src) : DictBase(std::move(src)) {}

	Dict& operator=(const DictBase& rhs) {
		clear();
		for (const auto& [k, v] : rhs) {
			emplace(k, v->clone());
		}
		return *this;
	}
	Dict& operator=(const Dict& rhs) { // need this to override NodeBase's
		return *this = static_cast<const DictBase&>(rhs);
	}
	Dict& operator=(Dict&& rhs) { // need this to override NodeBase's
		this->DictBase::operator=(std::move(rhs));
		return *this;
	}
	Dict& operator=(const NodeOwning& rhs);
	Dict& operator=(const NodeBase& rhs);
	Dict& operator=(NodeOwning&& rhs);
	Dict& operator=(NodeBase&& rhs);

	using DictBase::find;
	using DictBase::empty;
	using DictBase::size;
	using DictBase::clear;
	using DictBase::insert;
	using DictBase::emplace;
	using NodeBase::at;

	virtual NodeOwning toScalars() const override;
	// TODO: don't make a string (use transparent find)
	virtual const NodeBase& operator[](std::string_view sv) const override { return *DictBase::at(std::string(sv)); };
	virtual NodeBase& operator[](std::string_view sv) override;

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

	MemberIterator<false> begin()       override { return MemberIteratorImplBase<false>::OwningPtr{new Iter<false>{this->DictBase::begin()}}; }
	MemberIterator<true>  begin() const override { return MemberIteratorImplBase<true>:: OwningPtr{new Iter<true> {this->DictBase::begin()}}; }
	MemberIterator<false> end()         override { return MemberIteratorImplBase<false>::OwningPtr{new Iter<false>{this->DictBase::end()}}; }
	MemberIterator<true>  end()   const override { return MemberIteratorImplBase<true>:: OwningPtr{new Iter<true> {this->DictBase::end()}}; }

	std::unique_ptr<NodeBase> clone() const& override { return std::unique_ptr<NodeBase>(new Dict(*this)); }
	std::unique_ptr<NodeBase> clone() &&     override { return std::unique_ptr<NodeBase>(new Dict(std::move(*this))); }
};

using NodeConcreteMemberInfo = std::unique_ptr<NodeBase>;
using NodeConcreteMemberCache = std::map<std::string, NodeConcreteMemberInfo, std::less<void>>;

template <typename T>
struct NodeConcrete : NodeBase {
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

	const NodeBase& operator[](std::string_view key) const override { return *getMember(key).second; }
	      NodeBase& operator[](std::string_view key)       override { return *getMember(key).second; }

	NodeOwning toScalars() const override;

	MemberIterator<false> begin()       override { return nodeConcreteGetIterators(*this).first; }
	MemberIterator<true>  begin() const override { return nodeConcreteGetIterators(*this).first; }
	MemberIterator<false> end()         override { return nodeConcreteGetIterators(*this).second; }
	MemberIterator<true>  end()   const override { return nodeConcreteGetIterators(*this).second; }

	std::unique_ptr<NodeBase> clone() const& override { return std::unique_ptr<NodeBase>(new NodeValue<T>(getObj())); }
	std::unique_ptr<NodeBase> clone() &&     override { return std::unique_ptr<NodeBase>(new NodeValue<T>(std::move(getObj()))); }

	T& getObject() const& { return getObj(); }
	T&& getObject() && { return std::move(getObj()); }

protected:
	virtual T& getObj() const = 0;
	template<bool const_iter, typename Impl, typename Self>
	friend struct NodeConcreteDynamicMemberIter;

	mutable NodeConcreteMemberCache member_cache = {};

	NodeConcreteMemberCache::reference getMember(std::string_view key) const;
	template<typename TT = T> auto initMemberCacheForStaticMembers() const -> std::enable_if_t<kIsStaticMemberType<TT>>;
	template<typename Self>
	std::pair<MemberIterator<std::is_const_v<Self>>, MemberIterator<std::is_const_v<Self>>> friend nodeConcreteGetIterators(Self& self);
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


class NodeOwning : public NodeBase {
public:
	using DictImpl = Dict;
	using ObjectImpl = std::unique_ptr<NodeBase>;
	using Impl = std::variant<DictImpl, ObjectImpl>;

	template<typename Self, typename DictF, typename ObjF>
	friend decltype(auto) visitImpl(Self& self, DictF&& dict_f, ObjF&& obj_f) {
		static_assert(std::variant_size_v<Impl> == 2);
		auto* dict_impl = std::get_if<DictImpl>(&self.impl);
		if (dict_impl) {
			return std::forward<DictF>(dict_f)(*dict_impl);
		} else {
			using ObjectPtr = std::conditional_t<std::is_const_v<Self>, const NodeBase*, NodeBase*>;
			ObjectPtr obj_impl = std::get_if<ObjectImpl>(&self.impl)->get();
			return std::forward<ObjF>(obj_f)(*obj_impl);
		}
	}

	NodeOwning() : impl(DictImpl()) {}
	NodeOwning(const NodeOwning& src) : impl() { *this = src; }
	NodeOwning(NodeOwning&& src) : impl() { *this = std::move(src); }
	NodeOwning& operator=(const NodeOwning& src) { auto l = [this](auto& obj_or_dict) { *this = obj_or_dict; };            visitImpl(src, l, l); return *this; }
	NodeOwning& operator=(     NodeOwning&& src) { auto l = [this](auto& obj_or_dict) { *this = std::move(obj_or_dict); }; visitImpl(src, l, l); return *this; }
	explicit NodeOwning(ObjectImpl ri) : impl(std::move(ri)) {}
	explicit NodeOwning(DictImpl di) : impl(std::move(di)) {}
	NodeOwning(NodeBase& nb) : NodeOwning(std::move(nb).clone()) {}
	NodeOwning(NodeBase&& nb) : NodeOwning(std::move(nb).clone()) {}
	NodeOwning(const NodeBase& nb) : NodeOwning(nb.clone()) {}

	template<typename T>
	NodeOwning(T&& t) : impl(ObjectImpl(new NodeValue<std::remove_cvref_t<T>>(std::forward<T>(t)))) {}

	template<typename T>
	NodeOwning& operator=(T&& rhs) {
		using PlainT = std::remove_cvref_t<T>;
		const bool is_copy_assignment = std::is_reference_v<T>;
		const bool rhs_is_const = std::is_const_v<std::remove_reference_t<T>>;
		using RhsNodeOwning = std::conditional_t<rhs_is_const, const NodeOwning, NodeOwning>;
		using RhsDict = std::conditional_t<rhs_is_const, const Dict, Dict>;
		using ForwardingRhsNodeOwning = std::conditional_t<is_copy_assignment, RhsNodeOwning&, RhsNodeOwning&&>;
		using ForwardingRhsDict = std::conditional_t<is_copy_assignment, RhsDict&, RhsDict&&>;
		const auto fwd_rhs = [&rhs]() -> decltype(auto) { return std::forward<T>(rhs); };

		if constexpr (std::is_same_v<NodeOwning, PlainT>) {
			if constexpr (is_copy_assignment) {
				visitImpl(rhs,
					[this](auto& rhs_dict) { impl = rhs_dict; }, // just copy the dict
					[this](auto& rhs_obj)  { *this = rhs_obj; } // recurse on held object
				);
			} else {
				this->impl = fwd_rhs().impl;
			}
		} else if constexpr (std::is_same_v<Dict, PlainT>) {
			// want to avoid impl being a ObjctImpl(Dict*)
			impl = fwd_rhs();
		} else if constexpr (std::is_base_of_v<NodeBase, PlainT>) {
			if (auto rhs_as_owning = dynamic_cast<RhsNodeOwning*>(&rhs)) {
				// want to avoid impl being a ObjectImpl(NodeOwning*). Recurse and use NodeOwning case
				*this = static_cast<ForwardingRhsNodeOwning>(*rhs_as_owning);
			} else if (auto rhs_as_dict = dynamic_cast<RhsDict*>(&rhs)) {
				// want to avoid impl being a ObjectImpl(Dict*). Recurse and use Dict case
				*this = static_cast<ForwardingRhsDict>(*rhs_as_dict);
			} else {
				// some other NodeBase... overwrite impl with it
				impl = NodeOwning::ObjectImpl(fwd_rhs().clone());
			}
		} else {
			// T is some type outside of the NodeBase hierarchy. Copy/move into a node
			visitImpl(*this,
				[&](auto& /*dict*/) {
					// have a Dict; overwrite with object impl by making a new node
					impl = NodeOwning::ObjectImpl(new NodeValue<PlainT>(fwd_rhs()));
				},
				[&, this](auto& obj) {
					if (auto obj_impl = obj.template get_if<PlainT>()) {
						// type matches. Do an assignment to the underlying object
						*obj_impl = fwd_rhs();
					} else {
						// type does not match. Make a new node
						std::get_if<ObjectImpl>(&impl)->reset(new NodeValue<PlainT>(fwd_rhs()));
					}
				}
			);
		}
		return *this;
	}

	const NodeBase& operator[](std::string_view sv) const override { auto l = [&](auto& obj_or_dict) -> const NodeBase& { return obj_or_dict[sv]; }; return visitImpl(*this, l, l); }
	      NodeBase& operator[](std::string_view sv)       override { auto l = [&](auto& obj_or_dict) ->       NodeBase& { return obj_or_dict[sv]; }; return visitImpl(*this, l, l); }

	NodeOwning toScalars() const override { auto l = [](auto& obj_or_dict) { return obj_or_dict.toScalars(); }; return visitImpl(*this, l, l); }

	std::unique_ptr<NodeBase> clone() &&     override { return std::unique_ptr<NodeBase>(new NodeOwning(std::move(*this))); }
	std::unique_ptr<NodeBase> clone() const& override { return std::unique_ptr<NodeBase>(new NodeOwning(*this)); }
	
	MemberIterator<false> begin()       override { auto l = [](auto& obj_or_dict) { return obj_or_dict.begin(); }; return visitImpl(*this, l, l); }
	MemberIterator<true>  begin() const override { auto l = [](auto& obj_or_dict) { return obj_or_dict.begin(); }; return visitImpl(*this, l, l); }
	MemberIterator<false> end()         override { auto l = [](auto& obj_or_dict) { return obj_or_dict.end();   }; return visitImpl(*this, l, l); }
	MemberIterator<true>  end()   const override { auto l = [](auto& obj_or_dict) { return obj_or_dict.end();   }; return visitImpl(*this, l, l); }

private:
	friend NodeBase;
	Impl impl;
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
T* NodeBase::get_if_impl(Self& self) {
	constexpr auto self_is_const = std::is_const_v<Self>;
	using PlainT = std::remove_cv_t<T>;
	using NodeConcreteSameConst = std::conditional_t<self_is_const, const NodeConcrete<PlainT>, NodeConcrete<PlainT>>;
	if (auto downcasted = dynamic_cast<NodeConcreteSameConst*>(&self)) {
		return &downcasted->getObject();
	}
	using NodeOwningSameConst = std::conditional_t<self_is_const, const NodeOwning, NodeOwning>;
	if (auto downcasted = dynamic_cast<NodeOwningSameConst*>(&self)) {
		if constexpr (std::is_same_v<PlainT, Dict>) {
			return std::get_if<NodeOwning::DictImpl>(&downcasted->impl);
		} else {
			return visitImpl(*downcasted,
				[](auto&& /*dict*/) -> T* { throw std::logic_error(std::string(errors::kAccessNodeOwningAsObjectButIsDict)); },
				[](auto&& obj) -> T* { return obj.template get_if<T>(); }
			);
		}
	}
	return nullptr;
}

template <typename Rhs>
NodeBase& NodeBase::operator=(Rhs&& rhs) {
	using PlainRhs = std::remove_cvref_t<Rhs>;
	constexpr auto t_derives_nodebase = std::is_base_of_v<NodeBase, PlainRhs>;

	if constexpr (t_derives_nodebase) {
		if (auto downcasted = dynamic_cast<Dict*>(this)) {
			*downcasted = std::forward<Rhs>(rhs);
		}
	} else {
		if (auto downcasted = dynamic_cast<NodeConcrete<PlainRhs>*>(this)) {
			downcasted->getObject() = std::forward<Rhs>(rhs);
			return *this;
		}
	}
	if (auto downcasted = dynamic_cast<NodeOwning*>(this)) {
		*downcasted = std::forward<Rhs>(rhs);
		return *this;
	}
	throw std::logic_error("TODO: make operator=(const NodeBase&) and operator=(NodeBase&) virtual??");
}

Dict& Dict::operator=(const NodeOwning& rhs) {
	return visitImpl(rhs,
		[this](auto&& dict) -> Dict& { return *this = dict; },
		[this](auto&& /* obj */) -> Dict& {
			throw std::logic_error(std::string(errors::kAssignObjectInNodeOwningToDict));
		}
	);
}
Dict& Dict::operator=(const NodeBase& rhs) {
	if (auto* rhs_as_dict = dynamic_cast<const Dict*>(&rhs)) {
		return *this = *rhs_as_dict;
	} else if (auto* rhs_as_owning = dynamic_cast<const NodeOwning*>(&rhs)) {
		return *this = *rhs_as_owning;
	} else { // must be a NodeConcrete
		throw std::logic_error(std::string(errors::kAssignObjectToDict));
	}
}
Dict& Dict::operator=(NodeOwning&& rhs) {
	return visitImpl(rhs,
		[this](auto&& dict) -> Dict& { return *this = std::move(dict); },
		[this](auto&& /* obj */) -> Dict& {
			throw std::logic_error(std::string(errors::kAssignObjectInNodeOwningToDict));
		}
	);
}
Dict& Dict::operator=(NodeBase&& rhs) {
	if (auto* rhs_as_dict = dynamic_cast<Dict*>(&rhs)) {
		return *this = std::move(*rhs_as_dict);
	} else if (auto* rhs_as_owning = dynamic_cast<NodeOwning*>(&rhs)) {
		return *this = std::move(*rhs_as_owning);
	} else {
		throw std::logic_error(std::string(errors::kAssignObjectToDict));
	}
}

NodeOwning Dict::toScalars() const { return NodeOwning(*this); }

NodeBase& Dict::operator[](std::string_view sv) {
	auto lookup = lower_bound(sv);
	if (lookup == DictBase::end() or lookup->first != sv) {
		auto storage = mapped_type{new NodeOwning()};
		auto& ref = *storage;
		emplace_hint(lookup, std::string(sv), std::move(storage));
		return ref;
	} else {
		return *lookup->second;
	}
}

template<typename T>
NodeOwning NodeConcrete<T>::toScalars() const {
	NodeOwning r;
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
						member_info = NodeConcreteMemberInfo{
							.node = std::unique_ptr<NodeBase>(
								new NodeIndirectAcess<T, Member>(&getObj(), key)
							),
						};
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
		this->member_cache.emplace(elem.first, std::unique_ptr<NodeBase>(new NodeReference<MemberType>(elem.second)));
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
		// version that returns nullptr if this fails? rn, this creates a new node, if it's a NodeOwning underneath.
		curr = &(*curr)[path_elem];
		if (epos_maybe_npos == path.npos) {
			return curr;
		}
		spos = epos + 1;
	}
}

NodeBase* pathSubscript(NodeBase& n, std::string_view path, char sep = '.') { return pathSubscript_impl(n, path, sep); }
const NodeBase* pathSubscript(const NodeBase& n, std::string_view path, char sep = '.') { return pathSubscript_impl(n, path, sep); }


}  // namespace rrv