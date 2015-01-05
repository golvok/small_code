#include "common.h"

const size_t SIDE_LENGTH = 20;

typedef std::pair<ullong,ullong> Rectangle;
typedef std::unordered_map<Rectangle,ullong> PathCountCache;

namespace std {
	template<>
	struct hash<Rectangle> {
		size_t operator() (const Rectangle& r) const {
			auto uint_hasher = std::hash<ullong>();
			return uint_hasher(r.first) ^ uint_hasher(r.second);
		}
	};
}

ullong calculatePathCount(Rectangle r, PathCountCache& dim2path_count);

ullong getFromCacheOrCalculate(Rectangle r, PathCountCache& dim2path_count) {
	auto cached_value = dim2path_count.find(r);
	if (cached_value == dim2path_count.end()) {
		return calculatePathCount(r, dim2path_count);
	} else {
		return (*cached_value).second;
	}
}

ullong calculatePathCount(Rectangle r, PathCountCache& dim2path_count) {
	ullong sum = 0;
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
