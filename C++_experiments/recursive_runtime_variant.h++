#pragma once

#include <map>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <variant>

namespace rrv {

class NodeBase;
template<typename T, typename> class NodeConcrete;
struct Dict;
class NodeOwning;
template<bool>
class MemberIterator;
template<bool>
class MemberIteratorImplBase;

namespace errors {
	constexpr auto kAssignObjectInNodeOwningToDict = std::string_view("Trying to assign an object to a Dict (via a NodeOwning)");
	constexpr auto kAccessNodeOwningAsObjectButIsDict = std::string_view("Trying to access a NodeOwning as an object, but it is a Dict");
	constexpr auto kAccessWithWrongType = std::string_view("Accessing with wrong type");
	constexpr auto kAsCannotAccessOrConvert = std::string_view(".as is unable to access as or convert to the type requested");
	constexpr auto kCloneNotImplemented_ConstRef = std::string_view("clone const& not implemented");
	constexpr auto kCloneNotImplemented_RvalRef = std::string_view("clone && not implemented");
	constexpr auto kAccessMemberOfConcreteType = std::string_view("can't get member of concrete type");
	constexpr auto kAssignObjectToDict = std::string_view("Trying to assign an object to a Dict");
}

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
	MemberIteratorImplBase<const_iter>::OwningPtr _impl;
	MemberIteratorImplBase<const_iter>::OwningPtr clone_impl() const {
		// _impl ? _impl->clone() : nullptr;
		return _impl->clone();
	}
public:
	using value_type = MemberIteratorImplBase<const_iter>::value_type;
	using reference = MemberIteratorImplBase<const_iter>::reference;

	MemberIterator(MemberIteratorImplBase<const_iter>::OwningPtr impl) : _impl(std::move(impl)) { }
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
	const T* get_if() const;
	template<typename T>
	T* get_if() { return const_cast<T*>(static_cast<const NodeBase*>(this)->get_if<T>()); }

	template<typename T>
	const T& get() const {
		if (auto ptr = get_if<T>()) {
			return *ptr;
		} else {
			throw std::logic_error(std::string(errors::kAccessWithWrongType));
		}
	}
	template<typename T>
	T& get() { return const_cast<T&>(static_cast<const NodeBase*>(this)->get<T>()); }

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

	virtual Dict toDict() const = 0;

	virtual MemberIterator<false> begin() = 0;
	virtual MemberIterator<true> begin() const = 0;
	MemberIterator<true> cbegin() const { return begin(); }
	virtual MemberIterator<false> end() = 0;
	virtual MemberIterator<true> end() const = 0;
	MemberIterator<true> cend() const { return end(); }

	virtual std::unique_ptr<NodeBase> clone() const& {
		throw std::logic_error(std::string(errors::kCloneNotImplemented_ConstRef));
	}

	virtual std::unique_ptr<NodeBase> clone() && {
		throw std::logic_error(std::string(errors::kCloneNotImplemented_RvalRef));
	}
};

// TODO: use composition instead....
struct Dict : NodeBase, private DictBase {
public:
	Dict() {}
	Dict(const Dict& src) : DictBase() { *this = src; }
	Dict(Dict&&) = default;

	Dict& operator=(const Dict& rhs) { // need this to override NodeBase's
		clear();
		for (const auto& [k, v] : static_cast<const DictBase&>(rhs)) {
			emplace(k, v->clone());
		}
		return *this;
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

	virtual Dict toDict() const override { return *this; }
	// TODO: don't make a string (use transparent find)
	virtual const NodeBase& operator[](std::string_view sv) const override { return *at(std::string(sv)); };
	virtual NodeBase& operator[](std::string_view sv) override;

	template<bool const_iter>
	struct Iter : MemberIteratorImplBase<const_iter> {
		using OwningPtr = MemberIteratorImplBase<const_iter>::OwningPtr;
		using value_type = MemberIteratorImplBase<const_iter>::value_type;
		using Impl = std::conditional_t<const_iter, DictBase::const_iterator, DictBase::iterator>;
		Impl impl;
		Iter(Impl impl_) : impl(impl_) {}
		Iter(const Iter&) = default;
		Iter(Iter&&) = default;
		value_type* deref() override { return &*impl; }
		void advance() override { ++impl; }
		OwningPtr clone() const& override { return OwningPtr{new Iter(*this)}; }
	};

	MemberIterator<false> begin() override {
		return MemberIteratorImplBase<false>::OwningPtr{new Iter<false>{this->DictBase::begin()}};
	}
	MemberIterator<true> begin() const override {
		return MemberIteratorImplBase<true>::OwningPtr{new Iter<true>{this->DictBase::begin()}};
	}
	MemberIterator<false> end() override {
		return MemberIteratorImplBase<false>::OwningPtr{new Iter<false>{this->DictBase::end()}};
	}
	MemberIterator<true> end() const override {
		return MemberIteratorImplBase<true>::OwningPtr{new Iter<true>{this->DictBase::end()}};
	}
};

template <typename T>
Dict convertToDict(const T&) {
	throw std::logic_error("TODO: implement Dict conversion");
}

template <typename T>
class NodeMemberRef : public NodeBase {
public:
	T* member;

	virtual Dict toDict() const override { return convertToDict(*member); }
	virtual const NodeBase& operator[](std::string_view sv) const override {
		throw std::logic_error("can't get member of member (const)");
	};
	virtual NodeBase& operator[](std::string_view sv) override {
		throw std::logic_error("can't get member of member");
	}
};

template <typename T, typename = void>
struct NodeConcrete : NodeBase {
	static_assert(std::is_copy_constructible_v<T>, "Require copy-constructible types so cloning will generally work");
	NodeConcrete() = default;
	NodeConcrete(const NodeConcrete&) = default;
	NodeConcrete(NodeConcrete&&) = default;
	NodeConcrete& operator=(const NodeConcrete&) = default;
	NodeConcrete& operator=(NodeConcrete&&) = default;
	explicit NodeConcrete(const T& obj_) : obj(obj_) {}
	explicit NodeConcrete(T&& obj_) : obj(std::move(obj_)) {}

	const NodeBase& operator[](std::string_view) const override {
		// if constexpr (boost::hana::is_struct<T>::value) {
		// 	search members
		// 	what to return here, though? No NodeBase to return by referenece!
		// } else {
		// 	static_asert(!sizeof(T), "no known member access method");
		// }
		throw std::logic_error(std::string(errors::kAccessMemberOfConcreteType));
	}
	NodeBase& operator[](std::string_view) override {
		throw std::logic_error(std::string(errors::kAccessMemberOfConcreteType));
	}

	Dict toDict() const override {
		return convertToDict(obj);
	}

	std::unique_ptr<NodeBase> clone() const& override {
		return std::unique_ptr<NodeBase>(new NodeConcrete<T>(obj));
	}

	std::unique_ptr<NodeBase> clone() && override {
		return std::unique_ptr<NodeBase>(new NodeConcrete<T>(std::move(obj)));
	}

	MemberIterator<false> begin() override {
		throw std::logic_error(std::string(errors::kAccessMemberOfConcreteType));
	}
	MemberIterator<true> begin() const override {
		throw std::logic_error(std::string(errors::kAccessMemberOfConcreteType));
	}
	MemberIterator<false> end() override {
		throw std::logic_error(std::string(errors::kAccessMemberOfConcreteType));
	}
	MemberIterator<true> end() const override {
		throw std::logic_error(std::string(errors::kAccessMemberOfConcreteType));
	}

	T& getObj() & { return obj; }
	const T& getObj() const& { return obj; }

	// maybe?
	// operator T&() { return obj; }
	// operator const T&() const { return obj; }
	// operator T() && { return obj; }
private:
	friend NodeBase;
	T obj;
};

// rename to Root?
class NodeOwning : public NodeBase {
public:
	using DictImpl = Dict;
	// using ObjectImpl = std::unique_ptr<NodeOwning>;
	// using DictImpl = NodeConcrete<Dict>;
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

	template<typename T>
	NodeOwning(T&& t) : impl(ObjectImpl(new NodeConcrete<std::remove_cvref_t<T>>(std::forward<T>(t)))) {}

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
					impl = NodeOwning::ObjectImpl(new NodeConcrete<PlainT>(fwd_rhs()));
				},
				[&, this](auto& obj) {
					if (auto obj_impl = obj.template get_if<PlainT>()) {
						// type matches. Do an assignment to the underlying object
						*obj_impl = fwd_rhs();
					} else {
						// type does not match. Make a new node
						std::get_if<ObjectImpl>(&impl)->reset(new NodeConcrete<PlainT>(fwd_rhs()));
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
	Dict toDict() const override {
		return visitImpl(*this,
			[](auto& dict) { return dict.toDict(); },
			[](auto& obj) { return convertToDict(obj); }
		);
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
	// Dict toDict() const { return getImpl().toDict(); }
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

template<typename T>
const T* NodeBase::get_if() const {
	using PlainT = std::remove_cv_t<T>;
	if (auto downcasted = dynamic_cast<const NodeConcrete<PlainT>*>(this)) {
		return &downcasted->obj;
	}
	if (auto downcasted = dynamic_cast<const NodeOwning*>(this)) {
		if constexpr (std::is_same_v<PlainT, Dict>) {
			return std::get_if<NodeOwning::DictImpl>(&downcasted->impl);
		} else {
			return visitImpl(*downcasted,
				[](auto&& /*dict*/) -> const T* { throw std::logic_error(std::string(errors::kAccessNodeOwningAsObjectButIsDict)); },
				[](auto&& obj) -> const T* { return obj.template get_if<const T>(); }
			);
		}
	}
	return nullptr;
}

template <typename T>
NodeBase& NodeBase::operator=(T&& rhs) {
	using PlainT = std::remove_cvref_t<T>;
	constexpr auto t_derives_nodebase = std::is_base_of_v<NodeBase, PlainT>;

	if constexpr (t_derives_nodebase) {
		if (auto downcasted = dynamic_cast<Dict*>(this)) {
			*downcasted = std::forward<T>(rhs);
		}
	} else {
		if (auto downcasted = dynamic_cast<NodeConcrete<PlainT>*>(this)) {
			downcasted->obj = std::forward<T>(rhs);
			return *this;
		}
	}
	if (auto downcasted = dynamic_cast<NodeOwning*>(this)) {
		*downcasted = std::forward<T>(rhs);
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




// // only needed if NodeOwning doesn't inherit from NodeBase
// class NodeRef {
// public:
// 	using Impl = std::variant<NodeOwning::DictImpl*, NodeBase*>;
// 	// using Impl = NodeBase*;
// 	Impl impl;

// 	explicit NodeRef(Impl impl_) : impl(std::move(impl_)) {}
// };

// struct SimpleNode {
// 	using Impl std::variant<std::unique_ptr<SimpleNode>, std::any>;
// 	Impl impl;
// };

NodeBase* pathSubscript(NodeBase& n, std::string_view path, char sep = '.') {
	std::size_t spos = 0;
	NodeBase* curr = &n;
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

}  // namespace rrv