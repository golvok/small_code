#include <iostream>
#include <string_view>

int main(int argc, char const** argv) {
	const int up_to = argc < 2 ? 3*5*7 : std::atoi(argv[1]);
	static constexpr auto storage = std::string_view("FizzBuzzNozzJazz");
	for (int i = 1; i <= up_to; ++i) {
		const auto b = i %  3 ?  4 : 0;
		const auto e = i %  5 ?  4 : 8;
		const auto f = i %  7 ? 12 : 8;
		const auto g = i % 11 ? 12 : 16;
		std::cout << i << " ";
		if (e == f) {
			std::cout << storage.substr(b, g - b) << '\n';
		} else {
			std::cout << storage.substr(b, e - b) << storage.substr(f, g - f) << '\n';
		}
	}
}
