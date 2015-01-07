#include <iostream>
#include <cstdlib>

#include <functional>
#include <algorithm>

#include <limits>
#include <numeric>

#include <array>
#include <vector>
#include <map>
#include <unordered_map>
#include <set>
#include <unordered_set>
#include <queue>
#include <deque>

#include <cstring>
#include <cmath>

#include <cassert>

typedef std::pair<size_t,size_t> Point;
typedef std::pair<std::ptrdiff_t,std::ptrdiff_t> Direction;

typedef unsigned char uchar;
typedef unsigned long long int ullong;

template<typename T, typename U>
std::ostream& operator<<(std::ostream& os, std::pair<T,U> p) {
	os << '{' << p.first << ',' << p.second << '}';
	return os;
}

template<typename T, typename U, typename V, typename W>
void operator+=(std::pair<T,U>& p1, std::pair<V,W>& p2) {
	p1.first += p2.first;
	p1.second += p2.second;
}

template<typename T, typename U, typename V, typename W>
std::pair<T,U> operator+(std::pair<T,U> p1, const std::pair<V,W>& p2) {
	p1.first += p2.first;
	p1.second += p2.second;
	return p1;
}

template<typename T, typename U, typename MULT_TYPE>
std::pair<T,U> operator*(std::pair<T,U> p, MULT_TYPE m) {
	p.first *= m;
	p.second *= m;
	return p;
}

template<typename TwoDeeArrayType>
auto arrayGet(const TwoDeeArrayType& a, const Point& location) -> decltype(a[location.second][location.first]) {
	return a[location.second][location.first];
}

template<typename TwoDeeArrayType>
auto arrayGet(TwoDeeArrayType& a, const Point& location) -> decltype(a[location.second][location.first]) {
	return a[location.second][location.first];
}

template<typename T>
T abs(T num) {
	return (num < 0) ? (-num) : (num);
}

template<typename InputIter, typename InOutIter, typename BinaryOp>
void combineInto(InputIter first1, InputIter last1, InOutIter first2, BinaryOp op) {
	while (first1 != last1) {
		*first2 = op(*first1,*first2);
		++first1;
		++first2;
	}
}

template<typename InputIter, typename InOutIter>
void addTo(InputIter first1, InputIter last1, InOutIter first2) {
	combineInto(first1,last1,first2, std::plus< typename std::remove_reference<decltype(*first1)>::type>());
}

template<typename TwoDeeArrayType>
class ColIter {
public:
	typedef std::forward_iterator_tag iterator_category;
	typedef typename TwoDeeArrayType::value_type::value_type value_type;
	typedef typename TwoDeeArrayType::difference_type difference_type;
	typedef value_type* pointer;
	typedef value_type& reference;

	typedef typename TwoDeeArrayType::value_type::size_type col_size_type;
	typedef typename TwoDeeArrayType::size_type row_size_type;

	static const col_size_type END_VAL = -1;

	ColIter(TwoDeeArrayType& backing_array_, col_size_type column_index_, row_size_type start_row = END_VAL)
		: backing_array(backing_array_)
		, column_index(column_index_)
		, current_index(start_row)
		{}

	ColIter(const ColIter& src)
		: backing_array(src.backing_array)
		, column_index(src.column_index)
		, current_index(src.current_index)
		{}

	value_type& operator*() {
		return backing_array[current_index][column_index];
	}

	template<typename IntType>
	ColIter operator+(IntType delta) {
		ColIter result(*this);
		result += delta;
		return result;
	}

	difference_type operator-(const ColIter& other) const {
		return this->current_index - other.current_index;
	}

	template<typename IntType>
	ColIter& operator+=(IntType delta) {
		if(current_index != END_VAL) {
			current_index += delta;
			if (current_index >= backing_array.size()) {
				current_index = END_VAL;
			}
		}
		return *this;
	}

	ColIter& operator++() {
		*this += 1;
		return *this;
	}

	bool operator==(const ColIter& other) {
		return
			(&backing_array == &other.backing_array)
			&& (column_index == other.column_index)
			&& (current_index == other.current_index)
		;
	}

	bool operator!=(const ColIter& other) {
		return !(*this == other);
	}

private:
	TwoDeeArrayType& backing_array;
	col_size_type column_index;
	row_size_type current_index;
};

template<typename TwoDeeArrayType>
ColIter<TwoDeeArrayType> make_col_iter(TwoDeeArrayType& backing_array, size_t column_index) {
	return ColIter<TwoDeeArrayType>(backing_array, column_index);
}

template<typename TwoDeeArrayType>
ColIter<TwoDeeArrayType> make_col_iter(TwoDeeArrayType& backing_array, size_t column_index, size_t start_row) {
	return ColIter<TwoDeeArrayType>(backing_array, column_index, start_row);
}

template <class Key, class Compare>
std::set<Key, Compare> make_set_with_compare(Compare compare) {
     return std::set<Key, Compare> (compare);
}
