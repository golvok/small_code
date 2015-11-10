#include "print_with.h++"

#include <iostream>

struct my_type {
	int i;
	int f(int j) {
		return i + j;
	}
};

struct my_type2 {
	int j;
};

std::ostream& operator<<(std::pair<std::ostream&,my_type&> pair, my_type2& mt2) {
	pair.first << "1: " << pair.second.f(mt2.j) << ' ' << mt2.j << '\n';
	return pair.first;
}

std::ostream& operator<<(std::ostream& os, my_type2& mt2) {
	os << "2: " << mt2.j << '\n';
	return os;
}

int main() {
	my_type mt { 2 };
	my_type2 mt2 { 3 };

	std::cout << "test1:\n";
	std::cout << with(mt) << mt2;
	std::cout << "test1 done\n\n";

	std::cout << "test2:\n" << with(mt) << mt2 << "test 2 done\n\n";
}
