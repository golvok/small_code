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
typename TwoDeeArrayType::value_type::reference arrayGet(TwoDeeArrayType& a, Point location) {
	return a[location.second][location.first];
}

template<typename TwoDeeArrayType>
typename TwoDeeArrayType::value_type::value_type arrayGet(const TwoDeeArrayType& a, Point location) {
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
