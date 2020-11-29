
#include <vector>

template<typename RandIt1, typename RandIt2, typename Compare>
void mergesort_helper(RandIt1 src_begin, RandIt1 src_end, RandIt2 dest_begin, RandIt2 dest_end, Compare& comp) {
	auto range_size = std::distance(src_begin, src_end);
	if (range_size == 1) {
		return;
	}

	auto half_size = range_size/2;
	auto src_midpoint = std::next(src_begin, half_size);
	auto dest_midpoint = std::next(dest_begin, half_size);

	mergesort_helper(dest_begin, dest_midpoint, src_begin, src_midpoint, comp);
	mergesort_helper(dest_midpoint, dest_end, src_midpoint, src_end, comp);
	std::merge(src_begin, src_midpoint, src_midpoint, src_end, dest_begin, comp);
}

template<typename RandIt, typename Compare = std::less<>>
void mergesort(RandIt b, RandIt e, Compare comp = {}) {
	std::vector<std::remove_reference_t<decltype(*b)>> data_copy(b, e);
	mergesort_helper(data_copy.begin(), data_copy.end(), b, e, comp);
}
