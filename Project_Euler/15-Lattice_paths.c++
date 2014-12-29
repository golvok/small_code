#include <iostream>
#include <unordered_map>

const size_t SIDE_LENGTH = 20;

typedef unsigned long long int uintType;
typedef std::pair<uintType,uintType> Rectangle;
typedef std::unordered_map<Rectangle,uintType> PathCountCache;

namespace std {
	template<>
	struct hash<Rectangle> {
		size_t operator() (const Rectangle& r) const {
			auto uint_hasher = std::hash<uintType>();
			return uint_hasher(r.first) ^ uint_hasher(r.second);
		}
	};
}

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

uintType calculatePathCount(Rectangle r, PathCountCache& dim2path_count);

uintType getFromCacheOrCalculate(Rectangle r, PathCountCache& dim2path_count) {
	auto cached_value = dim2path_count.find(r);
	if (cached_value == dim2path_count.end()) {
		return calculatePathCount(r, dim2path_count);
	} else {
		return (*cached_value).second;
	}
}

uintType calculatePathCount(Rectangle r, PathCountCache& dim2path_count) {
	uintType sum = 0;
	if (r.first != 0) {
		sum += getFromCacheOrCalculate(r + std::make_pair(-1,0), dim2path_count);
	}
	if (r.second != 0) {
		sum += getFromCacheOrCalculate(r + std::make_pair(0,-1), dim2path_count);
	}
	std::cout << r << " has " << sum << std::endl;
	dim2path_count.insert({r,sum});
	dim2path_count.insert({{r.second,r.first},sum});
	return sum;
}

int main() {
	PathCountCache dim2path_count;
	dim2path_count.insert({{0,0},1});
	calculatePathCount({SIDE_LENGTH,SIDE_LENGTH}, dim2path_count);
	return 0;
}
