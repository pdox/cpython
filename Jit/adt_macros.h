/* Round down size "n" to be a multiple of "a". */
#define ADT_ROUND_DOWN(n, a) ((size_t)(n) & ~(size_t)((a) - 1))

/* Round up size "n" to be a multiple of "a". */
#define ADT_ROUND_UP(n, a) (((size_t)(n) + (size_t)((a) - 1)) & ~(size_t)((a) - 1))

/* Insert 'b' at the front of a doubly-linked list */
#define ADT_LL_INSERT_FIRST(head, tail, b) do { \
    b->prev = NULL; \
    b->next = head; \
    head = b; \
    if (b->next) { \
        b->next->prev = b; \
    } else { \
        tail = b; \
    } \
} while (0)

/* Insert 'b' into a linked list after existing node 'a'.
   If 'a' is NULL, inserts at front of list.

   head, tail, a, b must be *distinct* lvalues. (not expressions)
   For example, do not do: ADT_LL_INSERT_AFTER(head, tail, tail, b)
 */
#define ADT_LL_INSERT_AFTER(head, tail, a, b) do { \
    b->prev = a; \
    if (a) { \
        b->next = a->next; \
        a->next = b; \
    } else { \
        b->next = head; \
        head = b; \
    } \
    if (b->next) { \
        b->next->prev = b; \
    } else { \
        tail = b; \
    } \
} while (0)

/* Remove 'a' from doubly-linked list */
#define ADT_LL_REMOVE(head, tail, a) do { \
    if (a->prev) { \
        a->prev->next = a->next; \
    } else { \
        head = a->next; \
    } \
    if (a->next) { \
        a->next->prev = a->prev; \
    } else { \
        tail = a->prev; \
    } \
    a->prev = a->next = NULL; \
} while (0)

/* Insert 'b' at the end of a linked list */
#define ADT_LL_INSERT_LAST(head, tail, b) do { \
    b->prev = tail; \
    b->next = NULL; \
    if (b->prev) { \
        b->prev->next = b; \
    } else { \
        head = b; \
    } \
    tail = b; \
} while (0)

/* Detach the chain going from 'a' to 'b' from a doubly-linked list
   When this finishes, all the nodes from a to b (inclusive) will be
   removed from the list. a->prev will be NULL, and b->next will be
   NULL. a, b may refer to the same node. (but cannot be NULL)
 */
#define ADT_LL_DETACH_CHAIN(head, tail, a, b) do { \
    if (a->prev) { \
        a->prev->next = b->next; \
    } else { \
        head = b->next; \
    } \
    if (b->next) { \
        b->next->prev = a->prev; \
    } else { \
        tail = a->prev; \
    } \
    a->prev = NULL; \
    b->next = NULL; \
} while (0)


/* Attach the chain going from 'a' to 'b' (detached)
   after the node 'c' (which must be in the list head -> tail)
   (c cannot be NULL, and c cannot be a reference to head or tail)
 */
#define ADT_LL_ATTACH_CHAIN(head, tail, a, b, c) do { \
    a->prev = c; \
    b->next = c->next; \
    c->next = a; \
    if (b->next) { \
        b->next->prev = b; \
    } else { \
        tail = b; \
    } \
} while (0)
