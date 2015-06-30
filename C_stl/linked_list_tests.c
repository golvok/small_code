#include "linked_list.h"

DEFINE_DBL_LINKED_LIST(int)
DEFINE_LINKED_LIST(int)
DEFINE_LINKED_LIST(float)

int main() {
	{
		puts("-------singly linked test-------");
		int_linked_list* ll = int_ll_construct();
		for (size_t i = 1; i < 11; ++i) {
			int_ll_push_front(ll, i);
		}

		PRINT_LL(int, "%d", , ll);
		printf(" pt = %d\n\n", *int_ll_get_data(ll->pre_tail));

		for (size_t i = 0; i < 5; ++i) {
			int_ll_pop_front(ll, 0);
		}

		for (size_t i = 15; i > 10; --i) {
			int_ll_push_back(ll, i);
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
			int_ll_push_front(ll, i);
		}

		int_ll_destroy(ll);
	}

	{
		puts("-------other type test-------");
		float_linked_list* ll = float_ll_construct();
		for (size_t i = 1; i < 11; ++i) {
			float_ll_push_front(ll, i);
		}

		PRINT_LL(float,"%lf",,ll);

		for (size_t i = 0; i < 5; ++i) {
			float_ll_pop_front(ll, 0);
		}

		for (size_t i = 15; i > 10; --i) {
			float_ll_push_back(ll, i);
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
		int_dlinked_list* ll = int_dll_construct();
		for (size_t i = 1; i < 11; ++i) {
			int_dll_push_front(ll, i);
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
