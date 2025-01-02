#include <list>
#include <map>

namespace rrv::detail {

template<typename TKey, typename TMapped, typename... TMapArgs>
class InsertionOrderMap {
public:
	using mapped_type = TMapped;
	using key_type = TKey;
	using value_type = std::pair<const key_type, mapped_type>;
	using reference = value_type&;
	using const_reference = const value_type&;

private:
	using moveable_value_type = std::pair<key_type, mapped_type>;
	using Order = std::list<value_type>;
	using Map   = std::map<key_type, typename Order::iterator, TMapArgs...>;

	Order order;
	Map map;

public:
	using iterator = Order::iterator;
	using const_iterator = Order::const_iterator;
	struct node_type {
		mutable Order order_handle;
		typename Map::node_type map_handle;

		operator bool() const { return not order_handle.empty(); }
		mapped_type& mapped() const { return order_handle.front().second; }
		// key_type& key() const { order_handle.front().first; } // non-const access!
	};

	InsertionOrderMap() : order(), map() {}
	InsertionOrderMap(InsertionOrderMap&&) = default; // std::list move constructor doesn't invalidate
	InsertionOrderMap& operator=(InsertionOrderMap&&) = default; // std::list move constructor doesn't invalidate
	InsertionOrderMap(InsertionOrderMap const& src) : order(src.order), map() {
		for (auto it = order.begin(); it != order.end(); ++it) {
			map.emplace(it->first, it);
		}
	}
	InsertionOrderMap& operator=(InsertionOrderMap const& rhs) {
		*this = InsertionOrderMap{*rhs}; // move-assign a copy
	}

	template<typename TKeyComparable>
	mapped_type& at(TKeyComparable&& key) { return map.at(std::forward<TKeyComparable>(key))->second; }
	template<typename TKeyComparable>
	mapped_type const& at(TKeyComparable&& key) const { return map.at(std::forward<TKeyComparable>(key))->second; }

	template<typename TKeyComparable>
	iterator find(TKeyComparable&& key) {
		auto map_it = map.find(std::forward<TKeyComparable>(key));
		if (map_it == map.end()) return order.end();
		return map_it->second;
	}
	template<typename TKeyComparable>
	const_iterator find(TKeyComparable&& key) const {
		auto map_it = map.find(std::forward<TKeyComparable>(key));
		if (map_it == map.end()) return order.end();
		return map_it->second;
	}

	template<typename TKeyConvertible>
	mapped_type& operator[](TKeyConvertible&& key) {
		auto it = find(key);
		if (it != end()) return it->second;
		return emplace(key_type(key), mapped_type{}).first->second;
	}

	iterator begin() { return order.begin(); }
	const_iterator begin() const { return order.begin(); }
	iterator end() { return order.end(); }
	const_iterator end() const { return order.end(); }

	bool empty() const { return order.empty(); }
	std::size_t size() const { return order.size(); }

	template<typename... TArgs>
	std::pair<iterator, bool> emplace(TArgs&&... args) {
		// note: stdlib emplace always consumes
		auto new_value = moveable_value_type(std::forward<TArgs>(args)...);
		auto [map_it, is_new_key] = map.emplace(new_value.first, order.end()); // insert a dummy node
		if (is_new_key) { // overwrite dummy node
			auto new_iter = order.emplace(order.end(), std::move(new_value));
			map_it->second = new_iter;
			return {new_iter, true};
		} else { // or drop the new value and return the existing node
			auto existing_iter = map_it->second;
			return {existing_iter, false};
		}
	}

	iterator erase(const_iterator it) {
		map.erase(it->first);
		return order.erase(it);
	}

	template<typename TKeyComparable>
	node_type extract(TKeyComparable&& key) {
		Order order_handle;
		auto target = find(key);
		if (target == end()) return node_type{};
		order_handle.splice(order_handle.begin(), order, target); // does not invalidate iterator in map
		return node_type{std::move(order_handle), map.extract(key)}; 
	}

	struct insert_return_type
	{
	    iterator  position;
	    bool      inserted;
	    node_type node;
	};
	insert_return_type insert(node_type&& nh) {
		if (nh.order_handle.empty()) {
			return {end(), false, std::move(nh)};
		}

		auto [map_it, is_new_key, existing_map_handle] = map.insert(std::move(nh.map_handle));
		if (is_new_key) { // add it to order
			order.splice(order.end(), nh.order_handle);
			return {map_it->second, true, node_type{}};
		} else { // return everything back to the caller
			auto existing_iter = map_it->second;
			nh.map_handle = std::move(existing_map_handle);
			return {existing_iter, false, std::move(nh)};
		}
	}
};

}