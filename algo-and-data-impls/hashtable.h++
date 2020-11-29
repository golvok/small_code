
template<typename K, typename M>
class HashTable {
	using value_type = std::pair<const K, M>;
	using CollisionList = std::list<value_type>;
	using Table = std::vector<CollisionList>;
	struct iterator {
		Table::iterator table_it;
		CollisionList::iterator list_it;

		iterator& operator++() {
			++list_it;
			if (list_it == table_it->end()) {
				++table_it;
				list_it = table_it->begin();
			}
			return *this;
		}

		value_type& operator*() const { return *list_it; }
	};

	Table table;
	std::ptrdiff_t size;

	HashTable(std::ptrdiff_t table_size)
		: table(table_size)
		, size(0)
	{ }

	CollisionList& getList(const K& k) { return table.at(hash(k)); }
	iterator begin() { return {table.begin(), table.front().begin()}; }
	iterator end() { return {table.end(), {}}};

	M& at(const K& k) const {
		auto& list = getList(k);

		for (auto it = list.begin(); it != list.end(); ++it) {
			if (it->first == k) {
				return it->second;
			}
		}
		throw std::out_of_range{};
	}

	iterator insert(value_type v) {
		possibly_rehash();
		auto& list = getList(k).push_front(v);
	}

	iterator erase(const iterator& it) {
		it.table_it->erase(it.list_it);
		result = it;
		++result;
		return result;
	}

	void possibly_rehash() {
		if (table.size()*1.6 >= size) {
			return;
		}
		auto newHT = HashTable{size};
		for (auto it == this->begin(); it != this->end(); ++it) {
			newHT.insert(std::move(*it));
		}
		*this = std::move(newHT);
	}
};
