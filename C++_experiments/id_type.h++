
#include <iosfwd>
#include <type_traits>

template<typename id_type, typename TAG>
class ID {
	id_type value;
protected:
	explicit ID(const id_type& value) : value(value) { }

	template<typename ID_TYPE, typename... ARGS>
	friend auto make_id(ARGS... args) -> std::enable_if_t<
		std::is_base_of<typename ID_TYPE::ThisIdType,ID_TYPE>::value,
		ID_TYPE
	>;
public:
	using IdType = id_type;
	using ThisIdType = ID<id_type,TAG>;
	const static id_type DEFAULT_VALUE = TAG::DEFAULT_VALUE;

	ID() : value(TAG::DEFAULT_VALUE) { }
	explicit operator id_type() const { return value; }
	id_type getValue() const { return value; }
	void print(std::ostream& os) { os << value; }
};

template<typename ID_TYPE, typename... ARGS>
auto make_id(ARGS... args) -> std::enable_if_t<
	std::is_base_of<typename ID_TYPE::ThisIdType,ID_TYPE>::value,
	ID_TYPE
> {
	return ID_TYPE(args...);
}

template<typename id_type, typename TAG>
bool operator==(const ID<id_type,TAG>& lhs, const ID<id_type,TAG>& rhs) {
	return (id_type)lhs == (id_type)rhs;
}

template<typename id_type, typename TAG>
bool operator!=(const ID<id_type,TAG>& lhs, const ID<id_type,TAG>& rhs) {
	return !(lhs == rhs);
}

// namespace std {
// 	template<typename id_type, typename TAG>
// 	struct hash<ID<id_type,TAG>> {
// 		size_t operator()(const ID<id_type,TAG>& id) const {
// 			return std::hash<decltype(id.getValue())>()(id.getValue());
// 		}
// 	};
// }
