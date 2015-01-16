// t_hash_table* ht_init() {
// 	t_hash_table* ht = (t_hash_table*)calloc(sizeof(t_hash_table),1);

// 	ht->table_size = 0;
// 	ht->used_indicies = 0;
// 	ht->n_used_indicies = 0;
// 	ht_resize_table(ht,SUBTABLE_SIZE);

// 	return ht;
// }

// void ht_destroy(t_hash_table* wc) {
// 	{size_t i = 0; for (; i < wc->table_size; ++i) {
// 		t_table_item* table_item = wc->table[i];
// 		if (table_item != 0) {
// 			free(table_item);
// 		}
// 	}}
// 	free(wc->table);
// 	free(wc);
// }

// int ht_insert(t_hash_table* ht, t_key_type* key) {
// 	size_t index_of_key = ht_get(ht,key);
// 	t_table_item* found_item = ht->table[index_of_key];
// 	if (found_item == 0) {
// 		found_item = ti_init(key);
// 		if (found_item == 0) {
// 			return 0;
// 		}
// 		ht->table[index_of_key] = found_item;
// 		ht->used_indicies[ht->n_used_indicies] = index_of_key;
// 		ht->n_used_indicies += 1;
// 	}

// 	DEBUG_CODE(HTBL,printf("successful insert of %s\n", found_item->key););
// 	found_item->value += 1;

// 	return 1;
// }

// size_t ht_get(t_hash_table* ht, t_key_type* key) {
// 	size_t attempts = 0;
// 	size_t ideal_index = hash(key);

// 	t_table_item* result = 0;
// 	while (true) {
// 		size_t index = (ideal_index + attempts*attempts) % ht->table_size;
// 		result = ht->table[index];
// 		if (result == 0 || ht_key_equal(&result->key,key)) {
// 			return index;
// 		}
// 		++attempts;
// 	}
// }

// bool ht_key_equal(t_key_type* k1, t_key_type* k2) {
// 	return strcmp(*k1, *k2) == 0;
// }

// void ht_resize_table(t_hash_table* wc, size_t new_size) {
// 	if (wc->table_size == new_size) { return; }

// 	t_table_item** new_table = calloc(sizeof(t_table_item*),new_size);
// 	size_t* new_used_indicies = (size_t*)calloc(sizeof(size_t),new_size);
// 	if (wc->table_size != 0) {
// 		free(wc->table);
// 		// TODO re-insert things...
// 		// TODO re-add used indicies
// 		assert(0 && "actual resizing is unspported");
// 	}
// 	wc->table_size = new_size;
// 	wc->table = new_table;
// 	wc->used_indicies = new_used_indicies;
// 	DEBUG_CODE(HTBL,printf("successful resize to %lu\n", new_size););
// }

// //table item init
// t_table_item* ti_init(t_key_type* key) {
// 	t_table_item* ti = calloc(sizeof(t_table_item),1);
// 	ti->key = strdup(*key);
// 	ti->value = 0;
// 	return ti;
// }

int main() {
	return 0;
}