#define _alignof(t)     __alignof__(t)
#define ALIGN_UP(p, a) ((void *)(((uintptr_t)(p) + (uintptr_t)((a) - 1)) & ~(uintptr_t)((a) - 1)))
