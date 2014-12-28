#include <iostream>

const unsigned long int MAX = 100;
const unsigned long int SQUARE_OF_SUM = ((MAX*(MAX+1))/2)*((MAX*(MAX+1))/2);

int main() {
	unsigned long int sumOfSquares = 0;
	for (unsigned long int i = 1; i <= MAX; ++i) {
		sumOfSquares += i*i;
	}
	std::cout << SQUARE_OF_SUM - sumOfSquares << std::endl;
	return 0;
}
