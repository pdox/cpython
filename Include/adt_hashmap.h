/* Hash table implementation

   Each bucket is an independent linked list of entries.
   Each entry stores a key and value.
   Max load factor 0.5, before forcing resize.
   Resize increases number of buckets by 2x.
   Automatic shrinking not implemented. (although it can be resized with adt_hashmap_resize)

 */

#define ADT_HASHMAP_START_SIZE          8
#define ADT_HASHMAP_MAX_LOAD_FACTOR     0.5

/* Guaranteed alignment for both keys and values.
   (this is not effective if larger than malloc alignment) */
#define ADT_HASHMAP_ALIGNMENT  8


typedef int (*adt_hashmap_eqfunc)(void*,void*);
typedef size_t (*adt_hashmap_hashfunc)(void*);

struct adt_hashmap_entry_t;
typedef struct adt_hashmap_entry_t adt_hashmap_entry_t;
typedef adt_hashmap_entry_t* adt_hashmap_entry;
struct adt_hashmap_entry_t {
    adt_hashmap_entry next;
    /* Key and value follow */
};

typedef struct {
    size_t nentries;
    size_t nbuckets;
    size_t key_size;
    size_t value_size;
    size_t key_offset; /* in entry */
    size_t value_offset; /* in entry */
    adt_hashmap_eqfunc keyeq;
    adt_hashmap_hashfunc keyhash;
    adt_hashmap_entry *buckets;
} adt_hashmap_t;
typedef adt_hashmap_t* adt_hashmap;

static inline adt_hashmap_entry*
_adt_hashmap_make_buckets(size_t nbuckets) {
    assert(nbuckets > 0);
    size_t buckets_bytes = nbuckets * sizeof(adt_hashmap_entry);
    adt_hashmap_entry *buckets = (adt_hashmap_entry*)malloc(buckets_bytes);
    memset(buckets, 0, buckets_bytes);
    return buckets;
}

static inline void*
_adt_hashmap_entry_key(adt_hashmap m, adt_hashmap_entry e) {
    return ((char*)e) + m->key_offset;
}

static inline void*
_adt_hashmap_entry_value(adt_hashmap m, adt_hashmap_entry e) {
    return ((char*)e) + m->value_offset;
}

static inline adt_hashmap_entry
_adt_hashmap_entry_new(adt_hashmap m) {
    return (adt_hashmap_entry)malloc(m->value_offset + m->value_size);
}

static inline adt_hashmap
adt_hashmap_new(
        size_t key_size,
        size_t value_size,
        adt_hashmap_eqfunc keyeq,
        adt_hashmap_hashfunc keyhash)
{
    adt_hashmap m = (adt_hashmap)malloc(sizeof(adt_hashmap_t));
    m->nentries = 0;
    m->nbuckets = ADT_HASHMAP_START_SIZE;
    m->key_size = key_size;
    m->value_size = value_size;
    m->key_offset = ADT_ROUND_UP(sizeof(adt_hashmap_entry_t), ADT_HASHMAP_ALIGNMENT);
    m->value_offset = ADT_ROUND_UP(m->key_offset + m->key_size, ADT_HASHMAP_ALIGNMENT);
    m->keyeq = keyeq;
    m->keyhash = keyhash;
    m->buckets = _adt_hashmap_make_buckets(m->nbuckets);
    return m;
}

static inline void
adt_hashmap_delete(adt_hashmap m) {
    free(m->buckets);
    free(m);
}

static inline adt_hashmap_entry
_adt_hashmap_find_entry(adt_hashmap m, void *key)
{
    size_t hash = m->keyhash(key);
    size_t index = hash % m->nbuckets;
    adt_hashmap_entry cursor = m->buckets[index];
    while (cursor != NULL) {
        void *entry_key = _adt_hashmap_entry_key(m, cursor);
        if (m->keyeq(key, entry_key)) {
            return cursor;
        }
        cursor = cursor->next;
    }
    return NULL;
}

static inline void
_adt_hashmap_insert_entry(adt_hashmap m, adt_hashmap_entry entry) {
    size_t hash = m->keyhash(_adt_hashmap_entry_key(m, entry));
    size_t index = hash % m->nbuckets;
    entry->next = m->buckets[index];
    m->buckets[index] = entry;
    m->nentries++;
}

static inline int
adt_hashmap_get(adt_hashmap m, void *key_in, void *value_out)
{
    adt_hashmap_entry entry = _adt_hashmap_find_entry(m, key_in);
    if (entry) {
        void *entry_value = _adt_hashmap_entry_value(m, entry);
        memcpy(value_out, entry_value, m->value_size);
        return 1;
    }
    return 0;
}

static inline void
adt_hashmap_resize(adt_hashmap m, size_t nbuckets) {
    size_t old_nentries = m->nentries;
    size_t old_nbuckets = m->nbuckets;
    adt_hashmap_entry *old_buckets = m->buckets;
    m->nentries = 0;
    m->nbuckets = nbuckets;
    m->buckets = _adt_hashmap_make_buckets(nbuckets);

    /* Re-insert all entries */
    for (size_t i = 0; i < old_nbuckets; i++) {
        adt_hashmap_entry cursor = old_buckets[i];
        adt_hashmap_entry next;
        while (cursor != NULL) {
            next = cursor->next;
            _adt_hashmap_insert_entry(m, cursor);
            cursor = next;
        }
    }
    assert(m->nentries == old_nentries);
    free(old_buckets);
}

/* Insert into the map.
   On success, 1 is returned.
 . If this key already exists, the insertion does not happen, and 0 is returned.
 */
static inline int
adt_hashmap_insert(adt_hashmap m, void *key, void *value)
{
    float load_factor = (float)(m->nentries + 1)/m->nbuckets;
    if (load_factor > ADT_HASHMAP_MAX_LOAD_FACTOR)
        adt_hashmap_resize(m, m->nbuckets * 2);

    adt_hashmap_entry entry = _adt_hashmap_find_entry(m, key);
    if (entry != NULL)
        return 0;

    entry = _adt_hashmap_entry_new(m);
    memcpy(_adt_hashmap_entry_key(m, entry), key, m->key_size);
    memcpy(_adt_hashmap_entry_value(m, entry), value, m->value_size);
    _adt_hashmap_insert_entry(m, entry);
    return 1;
}
