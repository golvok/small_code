#include <iostream>

template<unsigned NUM> struct fib;

template<>
struct fib<0> {
	static const unsigned value = 1;
};

template<>
struct fib<1> {
	static const unsigned value = 1;
};

template<unsigned NUM>
struct fib {
	static const unsigned value = fib<NUM-1>::value + fib<NUM-2>::value;
};

int main() {
	std::cout << "0: " << fib<0>::value << "\n";
	std::cout << "1: " << fib<1>::value << "\n";
	std::cout << "2: " << fib<2>::value << "\n";
	std::cout << "3: " << fib<3>::value << "\n";
	std::cout << "4: " << fib<4>::value << "\n";
	std::cout << "5: " << fib<5>::value << "\n";
	std::cout << "6: " << fib<6>::value << "\n";
	std::cout << "7: " << fib<7>::value << "\n";
	std::cout << "8: " << fib<8>::value << "\n";
	std::cout << "9: " << fib<9>::value << "\n";
	std::cout << "10: " << fib<10>::value << "\n";
	std::cout << "11: " << fib<11>::value << "\n";
	std::cout << "12: " << fib<12>::value << "\n";
	std::cout << "13: " << fib<13>::value << "\n";
	std::cout << "14: " << fib<14>::value << "\n";
	std::cout << "15: " << fib<15>::value << "\n";
	std::cout << "16: " << fib<16>::value << "\n";
	std::cout << "17: " << fib<17>::value << "\n";
	std::cout << "77: " << fib<77>::value << "\n";
	std::cout << "100: " << fib<100>::value << "\n";
	std::cout << "200: " << fib<200>::value << "\n";
	return 0;
}
