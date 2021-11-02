#pragma once

#include <map>
#include <memory>
#include <variant>
#include <string>
#include <stdexcept>

namespace rrv {

class NodeBase;
template<typename T> class NodeConcrete;
struct Dict;
class NodeOwning;

class NodeBase {
public:
	virtual const NodeBase& operator[](std::string_view sv) const = 0;
	virtual NodeBase& operator[](std::string_view sv) = 0;

	template<typename T>
	const T& get() const {
		if (auto ptr = get_if<T>()) {
			return *ptr;
		} else {
			throw std::logic_error("Accessing with wrong type");
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
		throw std::logic_error("can't convert or access it");
	}

	template <typename T>
	NodeBase& operator=(T&& rhs);
	virtual ~NodeBase() = default;

	virtual Dict toDict() const = 0;

	virtual std::unique_ptr<NodeBase> clone() const& {
		throw std::logic_error("clone const& not implemented");
	}

	virtual std::unique_ptr<NodeBase> clone() && {
		throw std::logic_error("clone && not implemented");
	}

	template<typename T>
	const T* get_if() const;
	template<typename T>
	T* get_if() { return const_cast<T*>(static_cast<const NodeBase*>(this)->get_if<T>()); }
};

using DictBase = std::map<std::string, std::unique_ptr<NodeBase>, std::less<>>;
struct Dict : NodeBase, private DictBase {
public:
	Dict() {}
	Dict(const Dict&) = delete;
	Dict& operator=(const Dict&) = delete;
	Dict(Dict&&) = default;
	Dict& operator=(Dict&&) = default;

	using DictBase::find;
	using DictBase::empty;
	using DictBase::size;

	virtual Dict toDict() const override { throw std::logic_error("TODO: implement clone operation on NodeBase"); }
	// todo: don't make a string (use transparent find)
	virtual const NodeBase& operator[](std::string_view sv) const override { return *at(std::string(sv)); };
	virtual NodeBase& operator[](std::string_view sv) override;
};

template <typename T>
Dict convertToDict(const T&) {
	throw std::logic_error("todo: implement Dict conversion");
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

template <typename T>
struct NodeConcrete : NodeBase {
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
		throw std::logic_error("can't get member of concrete type (const)");
	}
	NodeBase& operator[](std::string_view) override {
		throw std::logic_error("can't get member of concrete type");
	}

	Dict toDict() const override {
		return convertToDict(obj);
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
	using ObjectImpl = std::unique_ptr<NodeBase>;
	using Impl = std::variant<DictImpl, ObjectImpl>;
	Impl impl;

	NodeOwning() : impl(DictImpl()) {}
	NodeOwning(const NodeOwning&) = delete;
	NodeOwning(NodeOwning&&) = default;
	explicit NodeOwning(ObjectImpl ri) : impl(std::move(ri)) {}
	explicit NodeOwning(DictImpl di) : impl(std::move(di)) {}
	NodeOwning& operator=(const NodeOwning&) = delete;
	NodeOwning& operator=(NodeOwning&&) = default;

	template<typename T>
	// NodeOwning(T&& t) : impl(std::any(std::forward<T>(t))) {}
	NodeOwning(T&& t) : impl(ObjectImpl(new NodeConcrete<T>(std::forward<T>(t)))) {}
	template<typename Self, typename DictF, typename ObjF>
	friend decltype(auto) visitImpl(Self& self, DictF&& dict_f, ObjF&& obj_f) {
		static_assert(std::variant_size_v<Impl> == 2);
		auto* dict_impl = std::get_if<DictImpl>(&self.impl);
		if (dict_impl) {
			return std::forward<DictF>(dict_f)(*dict_impl);
		} else {
			auto* obj_impl = std::get_if<ObjectImpl>(&self.impl);
			return std::forward<ObjF>(obj_f)(**obj_impl);
		}
	}

	NodeOwning& assign(NodeOwning&& rhs) {
		this->impl = std::move(rhs.impl);
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
			return std::get<NodeOwning::ObjectImpl>(downcasted->impl)->get_if<const T>();
		}
	}
	return nullptr;
}

template <typename T>
NodeBase& NodeBase::operator=(T&& rhs) {
	using PlainT = std::remove_cv_t<T>;
	constexpr auto t_derives_nodebase = std::is_base_of_v<NodeBase, PlainT>;

	if constexpr (not t_derives_nodebase) {
		if (auto downcasted = dynamic_cast<NodeConcrete<PlainT>*>(this)) {
			downcasted->obj = std::forward<T>(rhs);
		}
	}
	if (auto downcasted = dynamic_cast<NodeOwning*>(this)) {
		// TODO: move this code into NodeOwning
		if constexpr (std::is_same_v<PlainT, Dict>) {
			if (auto dict = std::get_if<NodeOwning::DictImpl>(&downcasted->impl)) {
				*dict = std::forward<T>(rhs);
			} else {
				downcasted->impl = std::forward<T>(rhs);
			}
		} else {
			if (auto obj = std::get_if<NodeOwning::ObjectImpl>(&downcasted->impl)) {
				if constexpr (t_derives_nodebase) {
					if (auto rhs_as_owning = dynamic_cast<NodeOwning*>(&rhs)) {
						downcasted->assign(static_cast<std::conditional_t<std::is_reference_v<PlainT>, NodeOwning&, NodeOwning&&>>(*rhs_as_owning));
					} else {
						downcasted->impl = NodeOwning::ObjectImpl(std::forward<T>(rhs).clone());
					}
				} else {
					if (auto downcasted_obj_impl = dynamic_cast<NodeConcrete<PlainT>*>(obj->get())) {
						downcasted_obj_impl->obj = std::forward<T>(rhs);
					} else {
						downcasted->impl = NodeOwning::ObjectImpl(new NodeConcrete<T>(std::forward<T>(rhs)));
					}
				}
			} else {
				if constexpr (t_derives_nodebase) {
					if (auto rhs_as_owning = dynamic_cast<NodeOwning*>(&rhs)) {
						downcasted->assign(static_cast<std::conditional_t<std::is_reference_v<PlainT>, NodeOwning&, NodeOwning&&>>(*rhs_as_owning));
					} else {
						downcasted->impl = NodeOwning::ObjectImpl(std::forward<T>(rhs).clone());
					}
				} else {
					downcasted->impl = NodeOwning::ObjectImpl(new NodeConcrete<T>(std::forward<T>(rhs)));
				}
			}
		}
	}
	return *this;
}

NodeBase& Dict::operator[](std::string_view sv) {
	auto lookup = find(sv);
	if (lookup == end()) {
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

}  // namespace rrv