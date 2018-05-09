typedef unsigned int adt_bitset_word;

#define ADT_BITSET_WORD_BITS  (sizeof(adt_bitset_word) * 8)
#define ADT_NWORDS(nbits)  (((nbits) + ADT_BITSET_WORD_BITS - 1)/ADT_BITSET_WORD_BITS)

typedef struct {
    size_t nbits;
    adt_bitset_word data[0];
} adt_bitset_t;
typedef adt_bitset_t *adt_bitset;

static inline adt_bitset_word _adt_bitset_end_mask(size_t nbits) {
    size_t nwords = ADT_NWORDS(nbits);
    size_t unused_bits = nwords * ADT_BITSET_WORD_BITS - nbits;
    if (unused_bits) {
        size_t mask_bits = ADT_BITSET_WORD_BITS - unused_bits;
        assert(mask_bits > 0 && mask_bits < ADT_BITSET_WORD_BITS);
        return (((adt_bitset_word)1) << mask_bits) - 1;
    }
    return ~(adt_bitset_word)0;
}

static inline adt_bitset adt_bitset_new(size_t nbits) {
    size_t nwords = ADT_NWORDS(nbits);
    adt_bitset ret = (adt_bitset)malloc(sizeof(adt_bitset_t) + nwords * sizeof(adt_bitset_word));
    ret->nbits = nbits;
    return ret;
}

static inline void adt_bitset_delete(adt_bitset bs) {
    free(bs);
}

/* Reset all bits to 0 or 1 */
static inline void adt_bitset_setall(adt_bitset bs, int value) {
    size_t nwords = ADT_NWORDS(bs->nbits);
    memset(&bs->data[0], value ? ~(adt_bitset_word)0 : 0, nwords * sizeof(adt_bitset_word));
}

/* Set a single bit */
static inline void adt_bitset_setbit(adt_bitset bs, size_t index, int value) {
    assert(index < bs->nbits);
    size_t word_index = index / ADT_BITSET_WORD_BITS;
    size_t bit_index = index % ADT_BITSET_WORD_BITS;
    if (value) {
        bs->data[word_index] |= ((adt_bitset_word)1) << bit_index;
    } else {
        bs->data[word_index] &= ~(((adt_bitset_word)1) << bit_index);
    }
}

/* Get a single bit */
static inline int adt_bitset_getbit(adt_bitset bs, size_t index) {
    assert(index < bs->nbits);
    size_t word_index = index / ADT_BITSET_WORD_BITS;
    size_t bit_index = index % ADT_BITSET_WORD_BITS;
    return (bs->data[word_index] >> bit_index) & 1;
}

/* AND two bitsets of the same size. 'dest' may alias 's1' or 's2'. */
static inline void adt_bitset_and(adt_bitset dest, adt_bitset s1, adt_bitset s2) {
    assert(dest->nbits == s1->nbits);
    assert(s1->nbits == s2->nbits);
    size_t nwords = ADT_NWORDS(dest->nbits);
    for (size_t i = 0; i < nwords; i++) {
        dest->data[i] = s1->data[i] & s2->data[i];
    }
}

/* OR two bitsets of the same size. 'dest' may alias 's1' or 's2'. */
static inline void adt_bitset_or(adt_bitset dest, adt_bitset s1, adt_bitset s2) {
    assert(dest->nbits == s1->nbits);
    assert(s1->nbits == s2->nbits);
    size_t nwords = ADT_NWORDS(dest->nbits);
    for (size_t i = 0; i < nwords; i++) {
        dest->data[i] = s1->data[i] | s2->data[i];
    }
}

/* XOR two bitsets of the same size. 'dest' may alias 's1' or 's2'. */
static inline void adt_bitset_xor(adt_bitset dest, adt_bitset s1, adt_bitset s2) {
    assert(dest->nbits == s1->nbits);
    assert(s1->nbits == s2->nbits);
    size_t nwords = ADT_NWORDS(dest->nbits);
    for (size_t i = 0; i < nwords; i++) {
        dest->data[i] = s1->data[i] ^ s2->data[i];
    }
}

/* Returns true if two bitsets are equal */
static inline int adt_bitset_eq(adt_bitset bs1, adt_bitset bs2) {
    assert(bs1->nbits == bs2->nbits);
    size_t nbits = bs1->nbits;
    size_t nwords = ADT_NWORDS(nbits);
    for (size_t i = 0; i < nwords; i++) {
        if (bs1->data[i] != bs2->data[i]) {
            if (i == nwords - 1) {
                /* Unused bits in the last word are undefined and should be ignored */
                adt_bitset_word mask = _adt_bitset_end_mask(nbits);
                return (mask & bs1->data[i]) == (mask & bs2->data[i]);
            }
            return 0;
        }
    }
    return 1;
}

static inline void adt_bitset_copy(adt_bitset dest, adt_bitset src) {
    assert(dest->nbits == src->nbits);
    size_t nbits = dest->nbits;
    size_t nwords = ADT_NWORDS(nbits);
    memcpy(&dest->data[0], &src->data[0], nwords * sizeof(adt_bitset_word));
}

static inline int adt_bitset_empty(adt_bitset bs) {
    size_t nbits = bs->nbits;
    size_t nwords = ADT_NWORDS(nbits);
    for (size_t i = 0; i < nwords; i++) {
        if (bs->data[i] != 0) {
            if (i == nwords - 1) {
                /* Unused bits in the last word are undefined and should be ignored */
                adt_bitset_word mask = _adt_bitset_end_mask(nbits);
                return (mask & bs->data[i]) == 0;
            }
            return 0;
        }
    }
    return 1;
}

static inline char* adt_bitset_repr(char *p, adt_bitset bs) {
    for (size_t i = 0; i < bs->nbits; i++) {
        *p++ = adt_bitset_getbit(bs, i) ? '1' : '0';
    }
    *p = '\0';
    return p;
}

static inline void adt_bitset_dump(adt_bitset bs) {
    for (size_t i = 0; i < bs->nbits; i++) {
        fputc(adt_bitset_getbit(bs, i) ? '1' : '0', stderr);
    }
}
