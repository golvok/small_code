#include "common.h"

const ullong MAX = 100;
const ullong SQUARE_OF_SUM = ((MAX*(MAX+1))/2)*((MAX*(MAX+1))/2);

int main() {
	ullong sumOfSquares = 0;
	for (ullong i = 1; i <= MAX; ++i) {
		sumOfSquares += i*i;
	}
	std::cout << SQUARE_OF_SUM - sumOfSquares << std::endl;
	return 0;
}
