#include "fifo.h"


DEFINE_LINKED_LIST(size_t)

DEFINE_FIFO(size_t)

int main() {
	size_t_fifo* fifo = size_t_fifo_construct();
	assert(fifo);

	size_t_fifo_insert(fifo,1);

	putchar('{');
	while (size_t_fifo_is_empty(fifo) == false) {
		size_t num;
		assert(size_t_fifo_pop(fifo,&num) == 0);
		printf("%lu, ", num);
	}
	puts("}\n");

	return 0;
}