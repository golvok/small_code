#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <stdio.h>

#define ll_next(ll_node) ll_node->next
#define ll_prev(dll_node) (dll_node->prev)
#define ll_get_data(ll_node) ll_node->data
#define ll_begin(ll) ll->pre_head->next
#define ll_front(ll) ll_begin(ll)
#define ll_back(ll) ll->pre_tail->next
#define ll_end(ll) 0
#define ll_increment(ll_node) ll_node = ll_next(ll_node)
#define ll_is_empty(ll) (ll_next(ll->pre_head) == 0)

#define ll_FOR_EACH(type, var, ll, code) \
{type var = ll_get_data(ll_begin(ll)) for(;var != ll_end(ll); ll_increment(var)) { code }}

#define TOK_CAT__(t1,t2) t1##t2
#define TOK_CAT(t1,t2) TOK_CAT__(t1,t2)

#define DEFINE_LINKED_LIST_COMMON__(prefix) \
prefix##ll_node* prefix##ll_next(prefix##ll_node* node) { return ll_next(node); } \
prefix##ll_node_data* prefix##ll_get_data(prefix##ll_node* node) { return &ll_get_data(node); } \
bool prefix##ll_is_empty(prefix##linked_list* ll) { return ll_is_empty(ll); } \
int prefix##ll_insert_after(prefix##linked_list* ll, prefix##ll_node* node, prefix##ll_node* new_next); \
int prefix##ll_remove_next(prefix##linked_list* ll, prefix##ll_node* node); \
int prefix##ll_pop_next(prefix##linked_list* ll, prefix##ll_node* node, prefix##ll_node_data* ret); \
int prefix##ll_pop_next_any(prefix##linked_list* ll, prefix##ll_node* prev_prev, prefix##ll_node* prev, prefix##ll_node_data* ret); \
 \
prefix##ll_node* prefix##ll_node_init(prefix##ll_node_data data); \
void prefix##ll_node_destroy(prefix##ll_node* node); \
bool prefix##ll_node_has_next(prefix##ll_node* node) { return ll_next(node) != 0; } \
 \
void prefix##ll_extra_insert_code_(prefix##ll_node* node, prefix##ll_node* new_next); \
 \
prefix##linked_list* prefix##ll_init() { \
	prefix##linked_list* ll; \
 \
	ll = (prefix##linked_list *)malloc(sizeof(prefix##linked_list)); \
	assert(ll); \
 \
	ll->pre_head = prefix##ll_node_init(0); \
	ll->pre_tail = ll->pre_head; \
 \
	return ll; \
} \
 \
void prefix##ll_destroy(prefix##linked_list* ll) { \
	prefix##ll_node* pre_head = ll->pre_head; \
	while (!prefix##ll_is_empty(ll)) { \
		prefix##ll_remove_next(ll, pre_head); \
	} \
	prefix##ll_node_destroy(pre_head); \
	ll->pre_head = 0; \
	ll->pre_tail = 0; \
	free(ll); \
} \
 \
int prefix##ll_push_front(prefix##linked_list* ll, prefix##ll_node* to_be_inserted) { \
	return prefix##ll_insert_after(ll, ll->pre_head, to_be_inserted); \
} \
 \
int prefix##ll_push_back(prefix##linked_list* ll, prefix##ll_node* to_be_inserted) { \
	if (prefix##ll_is_empty(ll)) { \
		return prefix##ll_insert_after(ll, ll->pre_head, to_be_inserted); \
	} else { \
		return prefix##ll_insert_after(ll, ll->ll_next(pre_tail), to_be_inserted); \
	} \
} \
 \
int prefix##ll_pop_front(prefix##linked_list* ll, prefix##ll_node_data *ret) { \
	return prefix##ll_pop_next(ll,ll->pre_head,ret); \
} \
 \
int prefix##ll_pop_back(prefix##linked_list* ll, prefix##ll_node_data *ret) { \
	return prefix##ll_pop_next(ll,ll->pre_tail,ret); \
} \
 \
int prefix##ll_remove_by_index(prefix##linked_list* ll, int index, prefix##ll_node_data *ret) { \
	prefix##ll_node* iter = ll->pre_head; \
	int current_index = 0; \
	while (true) { \
		if ( \
			ll_next(iter) == 0 \
			|| current_index == index \
		) { \
			return prefix##ll_pop_next(ll,iter,ret); \
		} \
		ll_increment(iter); \
		++current_index; \
	} \
} \
 \
int prefix##ll_insert_after(prefix##linked_list* ll, prefix##ll_node* node, prefix##ll_node* new_next) { \
	if (new_next == 0 || node == 0) { return 1; } \
 \
	if (node == ll->pre_tail && !prefix##ll_is_empty(ll)) { \
		ll->pre_tail = new_next;	 \
	} else if (node == ll_next(ll->pre_tail)) { \
		ll->pre_tail = node; \
	} \
	ll_next(new_next) = ll_next(node); \
	prefix##ll_extra_insert_code_(node,new_next); \
	ll_next(node) = new_next; \
	return 0; \
} \
 \
int prefix##ll_pop_next_any(prefix##linked_list* ll, prefix##ll_node* prev_prev, prefix##ll_node* prev, prefix##ll_node_data* ret) { \
	if (prev == 0) { return 1; } \
 \
	prefix##ll_pop_next_any_checks_(ll,prev,prev_prev); \
 \
	prefix##ll_node* old_next = ll_next(prev); \
	if (old_next == 0) { return 1; } \
	 \
	ll_next(prev) = ll_next(old_next); \
	prefix##ll_pop_next_extra_code_(old_next); \
	if (ret != 0) { \
		*ret = *prefix##ll_get_data(old_next); \
	} \
	if (ll->pre_tail == old_next) { \
		ll->pre_tail = prev; \
	} else if (ll->pre_tail == prev && !ll_is_empty(ll)) { \
		ll->pre_tail = prev_prev; \
	} \
	prefix##ll_node_destroy(old_next); \
	return 0; \
} \
 \
void prefix##ll_node_destroy(prefix##ll_node* node) { \
	free(node); \
} \
 \
prefix##ll_node* prefix##ll_node_init(prefix##ll_node_data data) { \
	prefix##ll_node* new_node = calloc(sizeof(prefix##ll_node),1); \
	ll_get_data(new_node) = data; \
	return new_node; \
}

#define DEFINE_LINKED_LIST_COMMON(prefix) DEFINE_LINKED_LIST_COMMON__(prefix)


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
int type##_ll_remove_next_any(type##_linked_list* ll, type##_ll_node* prev_prev, type##_ll_node* prev); \
 \
void type##_ll_extra_insert_code_(type##_ll_node* node, type##_ll_node* new_next) { \
	(void)node; (void)new_next; \
} \
void type##_ll_pop_next_any_checks_(type##_linked_list* ll, type##_ll_node* prev, type##_ll_node* prev_prev) { \
	if (prev_prev == 0) { \
		if (ll->pre_tail == prev && prev != ll->pre_head) { \
			assert(0 && "need the prev_prev to remove the tail"); \
		} \
	} else if (ll_next(prev_prev) != prev) { \
		assert(0 && "prev_prev isn't the node before prev"); \
	} \
} \
void type##_ll_pop_next_extra_code_(type##_ll_node* old_next) { \
	(void)old_next; \
} \
 \
DEFINE_LINKED_LIST_COMMON(TOK_CAT(type,_)) \
 \
int type##_ll_remove_next_any(type##_linked_list* ll, type##_ll_node* prev_prev, type##_ll_node* prev) { \
	return type##_ll_pop_next_any(ll,prev_prev,prev,0); \
} \
 \
int type##_ll_remove_next(type##_linked_list* ll, type##_ll_node* prev) { \
	return type##_ll_pop_next(ll,prev,0); \
} \
 \
int type##_ll_pop_next(type##_linked_list* ll, type##_ll_node* prev, type##_ll_node_data* ret) { \
	return type##_ll_pop_next_any(ll,0,prev,ret); \
} \
 \
int type##_ll_remove_by_value(type##_linked_list* ll, type##_ll_node_data val) { \
	if (ll_is_empty(ll)) { return 1; } \
	type##_ll_node* prev_prev = 0; \
	type##_ll_node* prev = ll_begin(ll); \
	for (;ll_next(prev) != ll_end(ll); prev_prev = prev, ll_increment(prev)) { \
		if (ll_get_data(ll_next(prev)) == val) { \
			break; \
		} \
	} \
	if (ll_end(ll) == ll_next(prev)) { return 1; } \
	return type##_ll_remove_next_any(ll,prev_prev,prev); \
} \
void type##_YOU_FORGOT_A_SEMICOLON()


#define DEFINE_DBL_LINKED_LIST(type) \
typedef type type##_dll_node_data; \
 \
typedef struct type##_dll_node_s { \
	type##_dll_node_data data; \
	struct type##_dll_node_s* next; \
	struct type##_dll_node_s* prev; \
} type##_dll_node; \
 \
typedef struct type##_dlinked_list_s { \
	type##_dll_node* pre_head; \
	type##_dll_node* pre_tail; \
} type##_dlinked_list; \
 \
int type##_dll_remove(type##_dlinked_list* ll, type##_dll_node* node); \
int type##_dll_pop(type##_dlinked_list* ll, type##_dll_node* node, type##_dll_node_data* ret); \
 \
void type##_dll_extra_insert_code_(type##_dll_node* node, type##_dll_node* new_next) { \
	ll_prev(new_next) = node; \
	if (ll_next(new_next) != 0) { \
		ll_prev(ll_next(new_next)) = new_next; \
	} \
}\
 \
void type##_dll_pop_next_any_checks_(type##_dlinked_list* ll, type##_dll_node* prev, type##_dll_node* prev_prev) { \
	if (prev != ll->pre_head) { \
		if (ll_next(prev_prev) != prev) { \
			assert(0 && "prev_prev isn't the node before prev"); \
		} \
	} \
} \
 \
void type##_dll_pop_next_extra_code_(type##_dll_node* old_next) { \
	if (ll_next(old_next)) { \
		ll_prev(ll_next(old_next)) = ll_prev(old_next); \
	} \
} \
 \
DEFINE_LINKED_LIST_COMMON(TOK_CAT(type,_d)) \
 \
int type##_dll_remove(type##_dlinked_list* ll, type##_dll_node* node) { \
	return type##_dll_pop_next_any(ll,node->prev->prev,node->prev,0); \
} \
int type##_dll_pop(type##_dlinked_list* ll, type##_dll_node* node, type##_dll_node_data* ret) { \
	return type##_dll_pop_next(ll,node->prev,ret); \
} \
 \
int type##_dll_remove_next(type##_dlinked_list* ll, type##_dll_node* prev) { \
	return type##_dll_pop_next_any(ll,prev->prev,prev,0); \
} \
 \
int type##_dll_pop_next(type##_dlinked_list* ll, type##_dll_node* prev, type##_dll_node_data* ret) { \
	return type##_dll_pop_next_any(ll,prev->prev,prev,ret); \
} \
 \
int type##_dll_remove_by_value(type##_dlinked_list* ll, type##_dll_node_data val) { \
	if (ll_is_empty(ll)) { return 1; } \
	type##_dll_node* prev = ll_begin(ll); \
	for (;ll_next(prev) != ll_end(ll); ll_increment(prev)) { \
		if (ll_get_data(ll_next(prev)) == val) { \
			break; \
		} \
	} \
	if (ll_end(ll) == ll_next(prev)) { return 1; } \
	return type##_dll_remove_next(ll,prev); \
} \
void type##_YOU_FORGOT_A_SEMICOLON()


#define PRINT_XLL__(prefix,fmt,function,ll) do { \
	prefix##ll_node* node = ll_begin(ll); \
	for (; node != ll_end(ll); ll_increment(node)) { \
		printf(fmt", ", function(*prefix##ll_get_data(node))); \
	} \
	putchar('\n'); \
} while (0)

#define PRINT_XLL(prefix,fmt,function,ll) PRINT_XLL__(prefix,fmt,function,ll)

#define PRINT_DLL(type,fmt,function,ll) PRINT_XLL(TOK_CAT(type,_d),fmt,function,ll)
#define PRINT_LL(type,fmt,function,ll) PRINT_XLL(TOK_CAT(type,_),fmt,function,ll)

DEFINE_DBL_LINKED_LIST(int);
DEFINE_LINKED_LIST(int);
DEFINE_LINKED_LIST(float);

int main() {
	{
		puts("-------singly linked test-------");
		int_linked_list* ll = int_ll_init();
		for (size_t i = 1; i < 11; ++i) {
			int_ll_push_front(ll, int_ll_node_init(i));
		}

		PRINT_LL(int, "%d", , ll);
		printf(" pt = %d\n\n", *int_ll_get_data(ll->pre_tail));

		for (size_t i = 0; i < 5; ++i) {
			int_ll_pop_front(ll, 0);
		}

		for (size_t i = 15; i > 10; --i) {
			int_ll_push_back(ll, int_ll_node_init(i));
		}

		// int_ll_pop_back(ll,0); // should fail

		PRINT_LL(int, "%d", , ll);
		printf(" pt = %d\n\n", *int_ll_get_data(ll->pre_tail));

		while (!int_ll_is_empty(ll)) {
			puts("poping front!");
			int_ll_pop_front(ll, NULL);
			puts("list is now:");
			PRINT_LL(int, "%d", , ll);
			printf(" pt = %d\n\n", *int_ll_get_data(ll->pre_tail));
		}

		for (size_t i = 1; i < 6; ++i) {
			int_ll_push_front(ll, int_ll_node_init(i));
		}

		int_ll_destroy(ll);
	}

	{
		puts("-------other type test-------");
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

	{
		puts("-------doubly likned list test-------");
		int_dlinked_list* ll = int_dll_init();
		for (size_t i = 1; i < 11; ++i) {
			int_dll_push_front(ll, int_dll_node_init(i));
		}

		PRINT_DLL(int, "%d", , ll);
		printf(" pt = %d\n\n", *int_dll_get_data(ll->pre_tail));

		for (size_t i = 1; i < 4; ++i) {
			puts("removing last. List is now:");
			int_dll_pop_back(ll,0);

			PRINT_DLL(int, "%d", , ll);
			printf(" pt = %d\n\n", *int_dll_get_data(ll->pre_tail));
		}

		puts("removing '4'. List is now:");
		int_dll_remove_by_value(ll,4);
		PRINT_DLL(int, "%d", , ll);
		printf(" pt = %d\n\n", *int_dll_get_data(ll->pre_tail));

		for (size_t i = 1; i < 3; ++i) {
			puts("removing middle. List is now:");
			int_dll_remove(ll,ll->pre_head->next->next->next->next);

			PRINT_DLL(int, "%d", , ll);
			printf(" pt = %d\n\n", *int_dll_get_data(ll->pre_tail));
		}
		puts("(pt should have changed to 8)\n");

		while (ll_is_empty(ll) == false) {
			puts("removing front. List is now:");
			int_dll_pop_front(ll,0);

			PRINT_DLL(int, "%d", , ll);
			printf(" pt = %d\n\n", *int_dll_get_data(ll->pre_tail));
		}

		int_dll_destroy(ll);
	}

	return 0;
}