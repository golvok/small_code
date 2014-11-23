#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <stdio.h>

#define ll_next(ll_node) ll_node->next
#define ll_get_data(ll_node) ll_node->data
#define ll_begin(ll) ll->pre_head->next
#define ll_front(ll) ll_begin(ll)
#define ll_back(ll) ll->pre_tail->next
#define ll_end(ll) 0
#define ll_increment(ll_node) ll_node = ll_next(ll_node)
#define ll_is_empty(ll) (ll_next(ll->pre_head) == 0)

#define ll_FOR_EACH(type, var, ll, code) \
{type var = ll_get_data(ll_begin(ll)) for(;var != ll_end(ll); ll_increment(var)) { code }}

#define DEFINE_LINKED_LIST(type) \
typedef type type##_ll_node_data; \
 \
typedef struct type##_ll_node_s { \
	type##_ll_node_data data; \
	struct type##_ll_node_s* next; \
} type##_ll_node; \
 \
typedef struct type##_linked_list_s { \
	type##_ll_node* pre_head; \
	type##_ll_node* pre_tail; \
} type##_linked_list; \
 \
type##_ll_node* type##_ll_next(type##_ll_node* node) { return ll_next(node); } \
type##_ll_node_data* type##_ll_get_data(type##_ll_node* node) { return &ll_get_data(node); } \
bool type##_ll_is_empty(type##_linked_list* ll) { return ll_is_empty(ll); } \
int type##_ll_insert_after(type##_linked_list* ll, type##_ll_node* node, type##_ll_node* new_next); \
int type##_ll_remove_next(type##_linked_list* ll, type##_ll_node* node); \
int type##_ll_remove_next_any(type##_linked_list* ll, type##_ll_node* prev_prev, type##_ll_node* prev); \
int type##_ll_pop_next(type##_linked_list* ll, type##_ll_node* node, type##_ll_node_data* ret); \
int type##_ll_pop_next_any(type##_linked_list* ll, type##_ll_node* prev_prev, type##_ll_node* prev, type##_ll_node_data* ret); \
 \
type##_ll_node* type##_ll_node_init(type##_ll_node_data data); \
void type##_ll_node_destroy(type##_ll_node* node); \
bool type##_ll_node_has_next(type##_ll_node* node) { return ll_next(node) != 0; } \
 \
 \
type##_linked_list* type##_ll_init() { \
	type##_linked_list* ll; \
 \
	ll = (type##_linked_list *)malloc(sizeof(type##_linked_list)); \
	assert(ll); \
 \
	ll->pre_head = type##_ll_node_init(0); \
	ll->pre_tail = ll->pre_head; \
 \
	return ll; \
} \
 \
void type##_ll_destroy(type##_linked_list* ll) { \
	type##_ll_node* pre_head = ll->pre_head; \
	while (!type##_ll_is_empty(ll)) { \
		type##_ll_remove_next(ll, pre_head); \
	} \
	type##_ll_node_destroy(pre_head); \
	ll->pre_head = 0; \
	ll->pre_tail = 0; \
	free(ll); \
} \
 \
int type##_ll_push_front(type##_linked_list* ll, type##_ll_node* to_be_inserted) { \
	return type##_ll_insert_after(ll, ll->pre_head, to_be_inserted); \
} \
 \
int type##_ll_push_back(type##_linked_list* ll, type##_ll_node* to_be_inserted) { \
	if (type##_ll_is_empty(ll)) { \
		return type##_ll_insert_after(ll, ll->pre_head, to_be_inserted); \
	} else { \
		return type##_ll_insert_after(ll, ll->ll_next(pre_tail), to_be_inserted); \
	} \
} \
 \
int type##_ll_pop_front(type##_linked_list* ll, type##_ll_node_data *ret) { \
	return type##_ll_pop_next(ll,ll->pre_head,ret); \
} \
 \
int type##_ll_pop_back(type##_linked_list* ll, type##_ll_node_data *ret) { \
	return type##_ll_pop_next(ll,ll->pre_tail,ret); \
} \
 \
int type##_ll_remove_by_index(type##_linked_list* ll, int index, type##_ll_node_data *ret) { \
	type##_ll_node* iter = ll->pre_head; \
	int current_index = 0; \
	while (true) { \
		if ( \
			ll_next(iter) == 0 \
			|| current_index == index \
		) { \
			return type##_ll_pop_next(ll,iter,ret); \
		} \
		ll_increment(iter); \
		++current_index; \
	} \
} \
 \
int type##_ll_remove_by_value(type##_linked_list* ll, type##_ll_node_data val) { \
	if (ll_is_empty(ll)) { return 1; } \
	type##_ll_node* prev_prev = 0; \
	type##_ll_node* prev = ll_begin(ll); \
	for (;ll_next(prev) != ll_end(ll); ll_increment(prev)) { \
		if (ll_get_data(ll_next(prev)) == val) { \
			break; \
		} \
	} \
	if (ll_end(ll) == ll_next(prev)) { return 1; } \
	return type##_ll_remove_next_any(ll,prev_prev,prev); \
} \
 \
int type##_ll_insert_after(type##_linked_list* ll, type##_ll_node* node, type##_ll_node* new_next) { \
	if (new_next == 0 || node == 0) { return 1; } \
 \
	if (node == ll->pre_tail && !type##_ll_is_empty(ll)) { \
		ll->pre_tail = new_next;	 \
	} else if (node == ll_next(ll->pre_tail)) { \
		ll->pre_tail = node; \
	} \
	ll_next(new_next) = ll_next(node); \
	ll_next(node) = new_next; \
	return 0; \
} \
 \
int type##_ll_remove_next(type##_linked_list* ll, type##_ll_node* prev) { \
	return type##_ll_pop_next(ll,prev,0); \
} \
int type##_ll_remove_next_any(type##_linked_list* ll, type##_ll_node* prev_prev, type##_ll_node* prev) { \
	return type##_ll_pop_next_any(ll,prev_prev,prev,0); \
} \
 \
int type##_ll_pop_next(type##_linked_list* ll, type##_ll_node* prev, type##_ll_node_data* ret) { \
	return type##_ll_pop_next_any(ll,0,prev,ret); \
} \
 \
int type##_ll_pop_next_any(type##_linked_list* ll, type##_ll_node* prev_prev, type##_ll_node* prev, type##_ll_node_data* ret) { \
	if (prev == 0) { return 1; } \
 \
	if (prev != ll->pre_head) { \
		if (prev_prev == 0) { \
			if (ll->pre_tail == prev && !ll_is_empty(ll)) { \
				assert(0 && "need the prev_prev to remove the tail"); \
			} \
		} else if (ll_next(prev_prev) != prev) { \
			assert(0 && "prev_prev isn't the node before prev"); \
		} \
	} \
 \
	type##_ll_node* old_next = ll_next(prev); \
	if (old_next == 0) { return 1; } \
	 \
	ll_next(prev) = ll_next(old_next); \
	if (ret != 0) { \
		*ret = *type##_ll_get_data(old_next); \
	} \
	if (ll->pre_tail == old_next) { \
		ll->pre_tail = prev; \
	} else if (ll->pre_tail == prev && !ll_is_empty(ll)) { \
		ll->pre_tail = prev_prev; \
	} \
	type##_ll_node_destroy(old_next); \
	return 0; \
} \
 \
void type##_ll_node_destroy(type##_ll_node* node) { \
	free(node); \
} \
 \
type##_ll_node* type##_ll_node_init(type##_ll_node_data data) { \
	type##_ll_node* new_node = calloc(sizeof(type##_ll_node),1); \
	ll_get_data(new_node) = data; \
	return new_node; \
} \
void type##__YOU_FORGOT_A_SEMICOLON()



#define PRINT_LL(type,fmt,function,ll) do { \
	type##_ll_node* node = ll_begin(ll); \
	for (; node != ll_end(ll); ll_increment(node)) { \
		printf(fmt", ", function(*type##_ll_get_data(node))); \
	} \
	putchar('\n'); \
} while (0)

DEFINE_LINKED_LIST(int);
DEFINE_LINKED_LIST(float);

int main() {
	{
		int_linked_list* ll = int_ll_init();
		for (size_t i = 1; i < 11; ++i) {
			int_ll_push_front(ll, int_ll_node_init(i));
		}

		for (int_ll_node* node = ll_begin(ll); node != ll_end(ll); ll_increment(node)) {
			printf("%d, ", *int_ll_get_data(node));
		}
		printf("\n pt = %d\n", *int_ll_get_data(ll->pre_tail));

		for (size_t i = 0; i < 5; ++i) {
			int_ll_pop_front(ll, 0);
		}

		for (size_t i = 15; i > 10; --i) {
			int_ll_push_back(ll, int_ll_node_init(i));
		}
		
		for (int_ll_node* node = ll_begin(ll); node != ll_end(ll); ll_increment(node)) {
			printf("%d, ", *int_ll_get_data(node));
		}
		printf("\n pt = %d\n", *int_ll_get_data(ll->pre_tail));

		while (!int_ll_is_empty(ll)) {
			puts("poping front!");
			int_ll_pop_front(ll, NULL);
			puts("list is now:");
			for (int_ll_node* node = ll_begin(ll); node != ll_end(ll); ll_increment(node)) {
				printf("%d, ", *int_ll_get_data(node));
			}
			printf("\n pt = %d\n", *int_ll_get_data(ll->pre_tail));	
		}

		for (size_t i = 1; i < 6; ++i) {
			int_ll_push_front(ll, int_ll_node_init(i));
		}

		int_ll_destroy(ll);
	}

	{
		float_linked_list* ll = float_ll_init();
		for (size_t i = 1; i < 11; ++i) {
			float_ll_push_front(ll, float_ll_node_init(i));
		}

		PRINT_LL(float,"%lf",,ll);

		for (size_t i = 0; i < 5; ++i) {
			float_ll_pop_front(ll, 0);
		}

		for (size_t i = 15; i > 10; --i) {
			float_ll_push_back(ll, float_ll_node_init(i));
		}
		
		PRINT_LL(float,"%lf",,ll);

		puts("removing 11.0!");
		float_ll_remove_by_value(ll,11);

		PRINT_LL(float,"%lf",,ll);

		while (!float_ll_is_empty(ll)) {
			puts("poping front!");
			float_ll_pop_front(ll, NULL);
			puts("list is now:");
			PRINT_LL(float,"%lf",,ll);
		}

		float_ll_destroy(ll);
	}

	return 0;
}