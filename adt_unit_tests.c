#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include "Include/adt.h"

#define EXPECT_TRUE(v)  (assert(v))
#define EXPECT_FALSE(v) (assert(!v))
#define EXPECT_EQ(a, b) (assert((a) == (b)))

int char_eq(void *pa, void *pb) {
    char a = *((char*)pa);
    char b = *((char*)pb);
    return a == b;
}

size_t char_hash(void *pa) {
    char a = *((char*)pa);
    return (size_t)a;
}

void test_hashmap_char_short(void) {
    adt_hashmap m = adt_hashmap_new(1, sizeof(short), char_eq, char_hash);

    /* Should start out empty */
    char key;
    short value;
    EXPECT_FALSE(adt_hashmap_get(m, "a", &value));
    EXPECT_FALSE(adt_hashmap_get(m, "z", &value));

    /* Insert test elements */
    for (key = 'a'; key <= 'z'; key++) {
        value = 1000 + key;
        EXPECT_TRUE(adt_hashmap_insert(m, &key, &value));
    }

    /* Verify that re-insertion fails */
    for (key = 'a'; key <= 'z'; key++) {
        value = 2000 + key;
        EXPECT_FALSE(adt_hashmap_insert(m, &key, &value));
    }

    /* Verify test elements */
    for (key = 'a'; key <= 'z'; key++) {
        value = 0;
        EXPECT_TRUE(adt_hashmap_get(m, &key, &value));
        EXPECT_EQ(value, 1000 + key);
    }

    /* Test resize (everything into 1 bucket!) */
    adt_hashmap_resize(m, 1);
    EXPECT_EQ(m->nbuckets, 1);

    /* Verify test elements */
    for (key = 'a'; key <= 'z'; key++) {
        value = 0;
        EXPECT_TRUE(adt_hashmap_get(m, &key, &value));
        EXPECT_EQ(value, 1000 + key);
    }

    /* Test resize (everything into 2 buckets) */
    adt_hashmap_resize(m, 2);
    EXPECT_EQ(m->nbuckets, 2);

    /* Verify test elements */
    for (key = 'a'; key <= 'z'; key++) {
        value = 0;
        EXPECT_TRUE(adt_hashmap_get(m, &key, &value));
        EXPECT_EQ(value, 1000 + key);
    }

    adt_hashmap_delete(m);
}

int main() {
    test_hashmap_char_short();
    printf("Tests OK\n");
    return 0;
}
