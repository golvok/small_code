#include "../Project_Euler/common.h"

template<typename ContainerType>
void print_oneindexed_array(
	const ContainerType& c,
	typename ContainerType::size_type start_index,
	typename ContainerType::size_type end_index,
	std::ostream& os
) {
	for (auto iter = c.begin()+start_index+1; iter != c.begin()+(end_index+1); ++iter) {
		os << *iter << ' ';
	}
}

template<typename ContainerType>
void print_oneindexed_array(const ContainerType& c, std::ostream& os = std::cout) {
	print_oneindexed_array(c,0,c.size()-1,os);
}

template<typename ContainerType>
void print_oneindexed_array(const ContainerType& c, typename ContainerType::size_type start_index, std::ostream& os = std::cout) {
	print_oneindexed_array(c,start_index,c.size()-1,os);
}

int main(int arc, char** argv) {
	assert(arc == 2 && "1 argument please");

	std::vector<uint> nums;

	for (char c : std::string(argv[1])) {
		nums.push_back(c - '0');
	}

	// first is length, second is highest num
	std::vector<std::vector<std::pair<uint,uint>>> subseq_info(nums.size()+2,std::vector<std::pair<uint,uint>>(nums.size()+1,{0,0}));

	for (size_t len = 1; len <= nums.size(); ++len) {
		size_t j;
		for (size_t i = 1; (j = i + len-1) <= nums.size(); ++i) {
			uint current_num = nums[j-1]; // note: zero indexed
			uint start_num = nums[i-1]; // note: zero indexed
			if (i == j) {
				subseq_info[i][j] = {1, current_num};
			} else {
				std::cout << std::make_pair(i,j) << " (" << nums[j-1] << ") : ";
				auto& below = subseq_info[i+1][j];
				auto& left = subseq_info[i][j-1];

				if (below.first > left.first) {
					std::cout << "take from below\n";
					subseq_info[i][j] = below;
				} else if (left.second < current_num) {
					std::cout << "continue from left\n";
					subseq_info[i][j] = {left.first+1, current_num};
				} else if (start_num < current_num) {
					uint num_less = 0;
					uint max_so_far = 0;
					for (size_t k = i; k+1 < j; ++k) {
						if (nums[k-1] > max_so_far) {
							if (nums[k-1] < current_num) {
								max_so_far = nums[k-1];
								num_less += 1;
							} else {
								break;
							}
						}
					}
					std::cout << "starting over, num less = " << num_less << std::endl;
					subseq_info[i][j] = {num_less+1,current_num};
					// std::cout << "starting over\n";
					// subseq_info[i][j] = {below.first+1,current_num};
				} else {
					std::cout << "take from left\n";
					subseq_info[i][j] = left;
				}
			}
		}
	}

	/**
	 * Print out final array.
	 */

	{ // new scope
		std::cout << "     |  ";
		for (size_t i = 1; i <= nums.size(); ++i) {
			std::cout << i << ((i>=10) ? "      " : "       ");
		}
		std::cout << std::endl;

		std::cout << "     |  ";
		for (size_t i = 1; i <= nums.size(); ++i) {
			std::cout << nums[i-1] << "       ";
		}
		std::cout << std::endl;

		std::cout << "-----+--";
		for (size_t i = 0; i < nums.size(); ++i) {
			std::cout << "--------";
		}
		std::cout << std::endl;

		for (size_t i = 1; i <= nums.size(); ++i) {
			std::cout << i << ((i>=10) ? " " : "  ") << nums[i-1] << " | ";
			for (size_t j = 1; j <= nums.size(); ++j) {
				if (j < i) {
					std::cout << "        ";
				} else {
					std::cout << subseq_info[i][j] << ' ';
					size_t extra_spaces = 2;
					if (subseq_info[i][j].first >= 10) {
						extra_spaces -= 1;
					}
					if (subseq_info[i][j].second >= 10) {
						extra_spaces -= 1;
					}
					for (size_t k = 0; k < extra_spaces; ++k) {
						std::cout << ' ';
					}
				}
			}
			std::cout << std::endl;
		}
	}

}
