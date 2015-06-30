#include "../Project_Euler/common.h"

template<typename ContainerType>
void print_oneindexed_array(
	const ContainerType& c,
	typename ContainerType::size_type start_index,
	typename ContainerType::size_type end_index,
	std::ostream& os
) {
	for (auto iter = c.begin()+start_index+1; iter != c.begin()+(end_index+1); ++iter) {
		os << *iter << ((*iter>=10) ? " " : "  ");
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

	std::string s(argv[1]);
	std::vector<std::vector<int>> palsubs(s.length()+1,std::vector<int>(s.length()+1,0));

	for (size_t len = 1; len <= s.length(); ++len) {
		size_t end;
		for (size_t start = 1; (end = start + len-1) <= s.length(); ++start) {
			// std::cout << "{start,end} = " << std::make_pair(start,end) << ": substr is " << s.substr(start-1,end-start+1);
			if (start == end) {
				// std::cout << " - is 1 char";
				palsubs[start][end] = 1;
			} else if (s[start-1] == s[end-1]) {
				// std::cout << " - first and last same";
				palsubs[start][end] = palsubs[start+1][end-1] + 2;
			} else {
				// std::cout << " - nothing doing";
				palsubs[start][end] = std::max (
					palsubs[start+1][end],
					palsubs[start][end-1]
				);
			}
			// std::cout << ", set to " << palsubs[start][end] << std::endl;
		}
	}

	/**
	 * Print out final array.
	 */

	{ // new scope
		std::cout << "       ";
		for (size_t i = 1; i <= s.length(); ++i) {
			std::cout << i << ((i>=10) ? " " : "  ");
		}
		std::cout << std::endl;

		std::cout << "       ";
		for (size_t i = 1; i <= s.length(); ++i) {
			std::cout << s[i-1] << "  ";
		}
		std::cout << std::endl;

		std::cout << "---";
		for (size_t i = 0; i <= s.length(); ++i) {
			std::cout << "---";
		}
		std::cout << std::endl;

		for (size_t i = 1; i <= s.length(); ++i) {
			std::cout << i << ((i>=10) ? " " : "  ") << s[i-1] << " | ";
			for (size_t j = 1; j < i; ++j) {
				std::cout << "   ";
			}
			print_oneindexed_array(palsubs[i],i-1);
			std::cout << std::endl;
		}
	}

	std::cout << "\n         string was: " << s << std::endl;

	/**
	 * Find indexes of the chars
	 */

	Point current = {s.length(),1};
	std::deque<size_t> char_indexes;
	while (true) {
		int current_value = arrayGet(palsubs,current);
		if (current_value <= 1) {
			break;
		}

		/**
		 * Try to find the "corner" of a number's region
		 */

		// go diagonal as far as you can
		Point diagonal = current+Point(-1,+1);
		if (arrayGet(palsubs,diagonal) == current_value) {
			current = diagonal;
			continue;
		}

		// when there are ties,
		// left then down favours palindromes starting earlier
		// down then left favours palindromes ending later

		// go left as far as you can
		Point left = current+Point(-1,0);
		if (arrayGet(palsubs,left) == current_value) {
			current = left;
			continue;
		}

		// go down as far as you can
		Point down = current+Point(0,+1);
		if (arrayGet(palsubs,down) == current_value) {
			current = down;
			continue;
		}

		// found corner, so recording indicies
		char_indexes.push_back(current.first);
		char_indexes.push_front(current.second);

		// then go diagonal, through it.
		current += Point(-1,+1);
	}

	/**
	 * Print out info about answer, as well as the answer.
	 */

	{ // marker string scope
		std::string marker_string(s.length()+1,' ');
		for (auto& index : char_indexes) {
			if (index <= current.first) {
				marker_string[index-1] = '`';
			} else {
				marker_string[index-1] = '^';
			}
		}
		std::cout << "chars in palindrome: " << marker_string << std::endl;
	}

	{ // printing the palindrome scope
		std::cout << "palindrome is: ";
		std::stack<char> reverse;

		// print first half
		while (!char_indexes.empty() && char_indexes.front() <= current.first) {
			reverse.push(s[char_indexes.front()-1]);
			char_indexes.pop_front();
		}
		while (reverse.empty() == false) {
			std::cout << reverse.top();
			reverse.pop();
		}

		// print possible "middle" characters
		for (;arrayGet(palsubs,current) != 0; --current.first) {
			reverse.push(s[current.first-1]);
		}
		if (reverse.size() <= 1) {
			if (reverse.size() == 1) {
				std::cout << reverse.top();
				reverse.pop();
			}
		} else {
			std::cout << '{';
			while (true) {
				std::cout << reverse.top();
				reverse.pop();
				if (reverse.empty()) {
					break;
				} else {
					std::cout << ',';
				}
			}
			std::cout << '}';
		}

		// print second half
		while (char_indexes.empty() == false) {
			std::cout << s[char_indexes.back()-1];
			char_indexes.pop_back();
		}

		std::cout << " (len=" << palsubs[1][s.length()] << ")\n";
	}
}

// pallindrome_subsequences(String s) begin
// 	Array2D palsubs
// 	for (len from 1 to s.length()) do
// 		for (i from 1 to s.length()) do
// 		index j = i + len;
// 			if (s[i] == s[j]) then
// 				palsubs[i][j] = palsubs[i+1][j-1]
// 			else
// 				palsubs[i][j] = max (
// 					palsubs[i+1][j],
// 					palsubs[i][j-1]
// 				)
// 		endfor
// 	endfor
// end

