#include "linked_list.h"

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>
#include <errno.h>
#include <math.h>
#include <time.h>
#include <setjmp.h>
#include <signal.h>
#include <termios.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <pthread.h>
#include <stdint.h>

pthread_mutex_t* init_normal_robust_mutex(pthread_mutex_t* mutex_to_init) {
	pthread_mutexattr_t lock_attrs;

	if (
	    pthread_mutexattr_init(&lock_attrs) != 0 ||
	    pthread_mutexattr_settype(&lock_attrs, PTHREAD_MUTEX_NORMAL) != 0 ||
	    pthread_mutexattr_setrobust(&lock_attrs, PTHREAD_MUTEX_ROBUST) != 0 ||
	    pthread_mutex_init(mutex_to_init, &lock_attrs) != 0
	) {

		return NULL;
	} else {
		return mutex_to_init;
	}
}

#define DEFINE_FIFO(fifo_type) \
 \
typedef struct { \
	fifo_type##_linked_list* items; \
	pthread_mutex_t mutex; \
	pthread_cond_t reader_wait_cond; \
} fifo_type##_fifo; \
 \
fifo_type##_fifo* fifo_type##_fifo_construct() { \
	fifo_type##_fifo* fifo = malloc(sizeof(fifo_type##_fifo)); \
	fifo->items = fifo_type##_ll_construct(); \
	if (init_normal_robust_mutex(&fifo->mutex) != &fifo->mutex) { \
		free(fifo); \
		return NULL; \
	} \
	if (pthread_cond_init(&fifo->reader_wait_cond, NULL) != 0) { \
		free(fifo); \
		return NULL; \
	} \
	return fifo; \
} \
 \
void fifo_type##_fifo_insert(fifo_type##_fifo* fifo, fifo_type item) { \
	pthread_mutex_lock(&fifo->mutex); \
	fifo_type##_ll_push_back(fifo->items, item); \
	pthread_cond_broadcast(&fifo->reader_wait_cond); \
	pthread_mutex_unlock(&fifo->mutex); \
} \
 \
int fifo_type##_fifo_pop(fifo_type##_fifo* fifo, fifo_type* put_item_here) { \
	pthread_mutex_lock(&fifo->mutex); \
	while (fifo_type##_ll_is_empty(fifo->items)) { \
		pthread_cond_wait(&fifo->reader_wait_cond, &fifo->mutex); \
	} \
	fifo_type##_ll_pop_front(fifo->items, put_item_here); \
	pthread_mutex_unlock(&fifo->mutex); \
	return 0; \
} \
 \
bool fifo_type##_fifo_is_empty(fifo_type##_fifo* fifo) { \
	pthread_mutex_lock(&fifo->mutex); \
	bool retval = fifo_type##_ll_is_empty(fifo->items); \
	pthread_mutex_unlock(&fifo->mutex); \
	return retval; \
}
