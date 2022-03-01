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
// struct IsSclarar {};

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
	using reference = std::conditional_t<const_iter, const DictBase::reference, DictBase::reference>;
	using value_type = std::conditional_t<const_iter, const DictBase::value_type, DictBase::value_type>;

	virtual ~MemberIteratorImplBase() = default;
	virtual value_type* deref() = 0;
	virtual void advance() = 0;
	virtual OwningPtr clone() const& = 0;
};

template<bool const_iter>
class MemberIterator {
	using ImplBase = MemberIteratorImplBase<const_iter>;
	using OwningPtr = typename ImplBase::OwningPtr;
	OwningPtr _impl;
	OwningPtr clone_impl() const {
		// _impl ? _impl->clone() : nullptr;
		return _impl->clone();
	}
public:
	using value_type = typename ImplBase::value_type;
	using reference = typename ImplBase::reference;

	MemberIterator(OwningPtr impl) : _impl(std::move(impl)) { }
	MemberIterator(const MemberIterator& src) : _impl(src.clone_impl()) {}
	MemberIterator(MemberIterator&& src) = default;
	MemberIterator& operator=(const MemberIterator& src) { _impl = src.clone_impl(); }
	MemberIterator& operator=(MemberIterator&& src) = default;

	// void check_done() { if (_impl && _impl->done()) impl.reset(); }
	reference operator*() { return *_impl->deref(); }
	const reference operator*() const { return *_impl->deref(); }
	MemberIterator& operator++() { return _impl->advance(), *this; }
	template<bool const_iter_>
	bool operator==(const MemberIterator<const_iter_>& rhs) const {
		// if (not _impl && not rhs._impl) return true;
		// if (not _impl || not rhs._impl) return false;
		return _impl->deref() == rhs._impl->deref();
	}
	template<bool const_iter_>
	bool operator!=(const MemberIterator<const_iter_>& rhs) const { return !(*this == rhs); }
};

class NodeBase {
public:
	template <typename T>
	NodeBase& operator=(T&& rhs);
	virtual ~NodeBase() = default;

	template<typename T>
	const T* get_if() const { return get_if_impl<const T>(*this); }
	template<typename T>
	T* get_if() { return get_if_impl<T>(*this); }

	template<typename T>
	const T& get() const { return getImpl<const T>(*this); }
	template<typename T>
	T& get() { return getImpl<T>(*this); }

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

	template<typename T>
	explicit operator const T&() const {
		return get<T>();
	}

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
	};

	MemberIterator<false> begin()       override { return MemberIteratorImplBase<false>::OwningPtr{new Iter<false>{this->DictBase::begin()}}; }
	MemberIterator<true>  begin() const override { return MemberIteratorImplBase<true>:: OwningPtr{new Iter<true> {this->DictBase::begin()}}; }
	MemberIterator<false> end()         override { return MemberIteratorImplBase<false>::OwningPtr{new Iter<false>{this->DictBase::end()}}; }
	MemberIterator<true>  end()   const override { return MemberIteratorImplBase<true>:: OwningPtr{new Iter<true> {this->DictBase::end()}}; }

	std::unique_ptr<NodeBase> clone() const& override { return std::unique_ptr<NodeBase>(new Dict(*this)); }
	std::unique_ptr<NodeBase> clone() &&     override { return std::unique_ptr<NodeBase>(new Dict(std::move(*this))); }
};

template <typename T>
NodeOwning scalarizeImpl(const T& t);

template <typename T>
struct NodeConcrete : NodeBase {
	static_assert(not std::is_reference_v<T>, "Use with a value type instead of a reference");
	static_assert(std::is_copy_constructible_v<T>, "Require copy-constructible types so cloning will generally work");
	static_assert(not std::is_const_v<T>, "NodeConcrete enforces constness itself");

	NodeConcrete() = default;
	NodeConcrete(const NodeConcrete&) = default;
	NodeConcrete(NodeConcrete&&) = default;
	NodeConcrete& operator=(const NodeConcrete&) = default;
	NodeConcrete& operator=(NodeConcrete&&) = default;

	const NodeBase& operator[](std::string_view key) const override { return *getMember(key).node; }
	NodeBase& operator[](std::string_view key) override { return  *getMember(key).node; }

	NodeOwning toScalars() const override;

	MemberIterator<false> begin()       override { throw std::logic_error(std::string(rrv::errors::kAccessMemberOfConcreteType)); }
	MemberIterator<true>  begin() const override { throw std::logic_error(std::string(rrv::errors::kAccessMemberOfConcreteType)); }
	MemberIterator<false> end()         override { throw std::logic_error(std::string(rrv::errors::kAccessMemberOfConcreteType)); }
	MemberIterator<true>  end()   const override { throw std::logic_error(std::string(rrv::errors::kAccessMemberOfConcreteType)); }

	std::unique_ptr<NodeBase> clone() const& override { return std::unique_ptr<NodeBase>(new NodeValue<T>(getObj())); }
	std::unique_ptr<NodeBase> clone() &&     override { return std::unique_ptr<NodeBase>(new NodeValue<T>(std::move(getObj()))); }

	T& getObject() const& { return getObj(); }
	T&& getObject() && { return std::move(getObj()); }

protected:
	virtual T& getObj() const = 0;

	struct MemberInfo {
		std::unique_ptr<NodeBase> node;
	};
	mutable std::map<std::string, MemberInfo, std::less<void>> member_cache = {};

	MemberInfo& getMember(std::string_view key) const;
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

// rename to Root?
class NodeOwning : public NodeBase {
public:
	using DictImpl = Dict;
	// using ObjectImpl = std::unique_ptr<NodeOwning>;
	// using DictImpl = NodeValue<Dict>;
	// using Impl = std::unique_ptr<NodeOwning>;
	using ObjectImpl = std::unique_ptr<NodeBase>;
	using Impl = std::variant<DictImpl, ObjectImpl>;
	Impl impl;

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
	NodeOwning& operator=(const NodeOwning& src) {
		visitImpl(src,
			[this](auto& dict) { *this = dict; },
			[this](auto& obj)  { *this = obj; }
		);
		return *this;
	}
	NodeOwning& operator=(NodeOwning&& src) {
		visitImpl(src,
			[this](auto& dict) { *this = std::move(dict); },
			[this](auto& obj)  { *this = std::move(obj); }
		);
		return *this;
	}
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

	// deduce *this via friend if it gets more complicated
	const NodeBase& operator[](std::string_view sv) const override {
		return visitImpl(*this,
			[&](auto& dict) -> const NodeBase& { return dict[sv]; },
			[&](auto& obj) -> const NodeBase& { return obj[sv]; }
		);
	}
	NodeBase& operator[](std::string_view sv) override {
		return visitImpl(*this,
			[&](auto& dict) -> NodeBase& { return dict[sv]; },
			[&](auto& obj) -> NodeBase& { return obj[sv]; }
		);
	}
	NodeOwning toScalars() const override {
		auto l = [](auto& obj_or_dict) { return obj_or_dict.toScalars(); };
		return visitImpl(*this, l, l);
	}
	std::unique_ptr<NodeBase> clone() && override {
		return std::unique_ptr<NodeBase>(new NodeOwning(std::move(*this)));
	}
	std::unique_ptr<NodeBase> clone() const& override {
		return std::unique_ptr<NodeBase>(new NodeOwning(*this));
	}

	MemberIterator<false> begin() override {
		return visitImpl(*this,
			[&](auto& dict) { return dict.begin(); },
			[&](auto& obj) { return obj.begin(); }
		);
	}
	MemberIterator<true> begin() const override {
		return visitImpl(*this,
			[&](auto& dict) { return dict.begin(); },
			[&](auto& obj) { return obj.begin(); }
		);
	}
	MemberIterator<false> end() override {
		return visitImpl(*this,
			[&](auto& dict) { return dict.end(); },
			[&](auto& obj) { return obj.end(); }
		);
	}
	MemberIterator<true> end() const override {
		return visitImpl(*this,
			[&](auto& dict) { return dict.end(); },
			[&](auto& obj) { return obj.end(); }
		);
	}

	// NodeOwning operator[](std::string_view sv) { return getImpl()[sv]; }
	// template<typename T>       T& get()       { return getImpl().get<T>(); }
	// template<typename T> const T& get() const { return getImpl().get<T>(); }
	// template<typename T> T as() const { return getImpl().as<T>(); }
	// NodeOwning toScalars() const { return getImpl().toScalars(); }
private:
	// template<typename Self>
	// friend NodeOwning& getImpl_(Self& self) {
	// 	return std::visit([](auto& impl_choice) -> auto& {
	// 		using Impl = std::remove_cv_t<std::remove_reference_t<decltype(impl_choice)>>;
	// 		if constexpr (std::is_same_v<DictImpl, Impl>) {
	// 			return impl_choice;
	// 		} else {
	// 			return *impl_choice;
	// 		}
	// 	}, self.impl);
	// }
	// NodeOwning& getImpl() { return getImpl_(*this); }
	// NodeOwning& getImpl() const { return getImpl_(*this); }
};

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
	auto lookup = find(sv);
	if (lookup == this->DictBase::end()) {
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
	return scalarizeImpl(getObj());
}

template<typename Obj> std::enable_if_t<std::is_arithmetic_v<Obj>, std::tuple<>> rrvMembers(Obj&) { return {}; }
template<typename Obj> auto rrvMembers(Obj& t) -> decltype(t.rrvMembers()) { return t.rrvMembers(); }

template<typename Obj> auto rrvMember(Obj& obj, std::string_view key) -> std::enable_if_t<false, Obj*>;
template<typename Obj> auto rrvMember(Obj& obj, std::string_view key) -> decltype(obj.rrvMember(key)) { return obj.rrvMember(key); }

// stdlib specializations
template<typename T> decltype(auto) rrvMember(std::vector<T>& obj, std::string_view key) { return obj.at(std::stoi(std::string(key))); }

// Dynamic member test
template<typename Obj> auto rrvMemberTest() -> decltype(rrvMember(std::declval<Obj&>(), std::declval<std::string_view>()), void()) {}
template<typename Obj, typename = void> constexpr static bool kIsDynamicMemberType = false;
template<typename Obj> constexpr static bool kIsDynamicMemberType<Obj, decltype(rrvMemberTest<Obj>())> = true;

// Static member test
template<typename Obj> auto rrvMembersTest() -> decltype(rrvMembers(std::declval<Obj&>()), void()) {}
template<typename Obj, typename = void> constexpr static bool kIsStaticMemberType = false;
template<typename Obj> constexpr static bool kIsStaticMemberType<Obj, decltype(rrvMembersTest<Obj>())> = true;

// need this so all branches of NodeConcrete::getMember are well-formed...
template<typename Obj> std::enable_if_t<kIsDynamicMemberType<Obj>, std::tuple<>> rrvMembers(Obj&) { static_assert(!sizeof(Obj*), "should never be instantiated"); return {}; }

template <typename Container, typename T>
struct NodeIndirectAcess : NodeConcrete<T> {
	NodeIndirectAcess(const NodeIndirectAcess&) = default;
	NodeIndirectAcess(NodeIndirectAcess&&) = default;
	NodeIndirectAcess& operator=(const NodeIndirectAcess&) = default;
	NodeIndirectAcess& operator=(NodeIndirectAcess&&) = default;
	explicit NodeIndirectAcess(Container* container_, std::string_view key_) : container(container_), key(key_) {}

protected:
	T& getObj() const override { return rrvMember(*container, key); }

private:
	Container* container;
	std::string key;
};

template<typename T>
auto NodeConcrete<T>::getMember(std::string_view key) const -> MemberInfo& {
	static_assert(kIsDynamicMemberType<T> != kIsStaticMemberType<T>, "types should only define one of getMember and getMembers");
	if constexpr (kIsDynamicMemberType<T>) {
		using Member = std::remove_reference_t<decltype(rrvMember(getObj(), key))>;
		auto [lookup, is_new] = member_cache.emplace(key, MemberInfo{});
		auto& member_info = lookup->second;
		if (is_new) {
			member_info = MemberInfo{
				.node = std::unique_ptr<NodeBase>(
					new NodeIndirectAcess<T, Member>(&getObj(), key)
				),
			};
		}
		return member_info;
	} else {
		if (member_cache.empty()) {
			const auto add_elem = [this](auto&& elem) {
				using MemberType = std::remove_reference_t<decltype(*elem.second)>;
				if constexpr (not std::is_const_v<MemberType>) {
					this->member_cache.emplace(elem.first, std::unique_ptr<NodeBase>(new NodeReference<MemberType>(elem.second)));
				} else {
					static_assert(not sizeof(T*), "rrvMembers should not return pointers to const. Perhaps 'this' is const in the implementation of rrvMembers.");
				}
			};

			const auto add_all = [&add_elem](auto&&... elems) {
				(add_elem(std::forward<decltype(elems)>(elems)), ...);
			};

			std::apply(add_all, rrvMembers(this->getObj()));

			if (member_cache.empty()) {
				throw std::logic_error(std::string(errors::kAccessMemberOfScalarType));
			}
		}

		auto lookup = member_cache.find(key);
		if (lookup == member_cache.end()) {
			throw std::logic_error("Member not found");
		}
		return lookup->second;
	}
}

template<typename T> std::enable_if_t<std::is_arithmetic_v<T>, NodeOwning> rrvScalarize(const T& t) { return t; }
template<typename T> auto rrvScalarize(const T& t) -> decltype(t.rrvScalarize()) { return t.rrvScalarize(); }

template<typename T>
NodeOwning rrvScalarize(const std::vector<T>& arg) {
	NodeOwning n;
	for (int i = 0; i < (int)arg.size(); ++i) {
		// TODO: don't want to convert the index to string here?
		using ::std::to_string;
		using ::rrv::rrvScalarize;
		n[to_string(i)] = rrvScalarize(arg[i]);
	}
	return n;
}

template <typename T>
NodeOwning scalarizeImpl(const T& t) {
	using ::rrv::rrvScalarize;
	return rrvScalarize(t);
// 	auto scalarized = rrvScalarize(t);
// 	using Scalarized = std::remove_cvref_t<decltype(scalarized)>;
// 	if constexpr (std::is_same_v<Scalarized, IsSclarar>) {
// 		return NodeOwning(t);
// 	} else {
// 		return scalarized;
// 	}
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