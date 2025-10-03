#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"

#define TABLE_MAX_LOAD 0.75

void initTable(Table* table) {
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

void freeTable(Table* table) {
    FREE_ARRAY(Entry, table->entries, table->capacity);
    initTable(table);
}

// Return pointer to key's bucket
static Entry* findEntry(Entry* entries, int capacity, ObjString* key) {
    uint32_t index = key->hash % capacity;
    // First time we pass a tombstone, we store it in thie local cariable
    Entry* tombstone = NULL;
    for (;;) {
        Entry* entry = &entries[index];
        if (entry->key == NULL) {
            if (IS_NIL(entry->value)) {
                // Empty entry
                // If we have passed a tombstone previously we return its entry so
                // if we call in order to insert a node that let's us treat the 
                // tombstone as empty and reuse it for the new entry
                return tombstone != NULL ? tombstone : entry;
            } else {
                // We found a tombstone
                // We set the local var to the first tombstone
                if (tombstone == NULL) tombstone = entry;
            }
        } else if (entry->key == key) {
            // We found the key
            return entry;
        }

        index = (index + 1) % capacity;
    }
}

// Return if table contains key and point value pointer to it's corresponding value
bool tableGet(Table* table, ObjString* key, Value* value) {
    if (table->count == 0) return false;

    Entry* entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL) return false;

    *value = entry->value;
    return true;
}

// When we are adjusting capacity we are discarding the tombstones since we are rebulding
// the whole table
static void adjustCapacity(Table* table, int capacity) {
    Entry* entries = ALLOCATE(Entry, capacity);
    for (int i = 0; i < capacity; i++) {
        entries[i].key = NULL;
        entries[i].value = NIL_VAL;
    }

    // Since we place the entries modulo the capacity we have to rebuild the hash table 
    // from scratch
    table->count = 0;
    for (int i = 0; i < table->capacity; i++) {
        Entry* entry = &table->entries[i];
        if (entry->key == NULL) continue;

        Entry* dest = findEntry(entries, capacity, entry->key);
        dest->key = entry->key;
        dest->value = entry->value;
        // each time we find a non-tombstone entry, we increment count
        table->count++;
    }

    FREE_ARRAY(Entry, table->entries, table->capacity);
    table->entries = entries;
    table->capacity = capacity;
}

// Set value in table corresponding to the key
bool tableSet(Table* table, ObjString* key, Value value) {
    if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
        int capacity = GROW_CAPACITY(table->capacity);
        adjustCapacity(table, capacity);
    }

    Entry* entry = findEntry(table->entries, table->capacity, key);
    bool isNewKey = entry->key == NULL;
    // We treat tombstones as full entries so we only increment for entirely empty buckets
    if (isNewKey && IS_NIL(entry->value)) table->count++;

    entry->key = key;
    entry->value = value;
    return isNewKey;
}

// Delete entry from table
// We are not decreasing the count because in order to avoid infinite loop in `findEntry()`
// we are treating tombstones as full
bool tableDelete(Table* table, ObjString* key) {
    if (table->count == 0) return false;

    // Find the entry
    Entry* entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL) return false;

    // Place a tombstone in the entry
    entry->key = NULL;
    entry->value = BOOL_VAL(true);
    return true;
}

void tableAddAll(Table* from, Table* to) {
    for (int i = 0; i < from->capacity; i++) {
        Entry* entry = &from->entries[i];
        if (entry->key != NULL) {
            tableSet(to, entry->key, entry->value);
        }
    }
}

// This is the one place in the VM where we actually test strings for textual equality. 
// We do it here to deduplicate strings and then the rest of the VM can take for granted 
// that any two strings at different addresses in memory must have different contents.
ObjString* tableFindString(Table* table, const char* chars, int length, uint32_t hash) {
    if (table->count == 0) return NULL;

    uint32_t index = hash % table->capacity;
    for (;;) {
        Entry* entry = &table->entries[index];
        if (entry->key == NULL) {
            // Stop if we find an empty non-tombstonre entry
            if (IS_NIL(entry->value)) return NULL;
        } else if (entry->key->length == length && entry->key->hash == hash && memcmp(entry->key->chars, chars, length) == 0) {
            // We found it
            return entry->key;
        }

        index = (index + 1) % table->capacity;
    }
}