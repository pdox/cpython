#ifndef Py_JIT_MACROS_H
#define Py_JIT_MACROS_H
#ifdef __cplusplus
extern "C" {
#endif

#define always_assert(x) do { \
    if (!(x)) _always_assert_fail(#x, __FILE__, __LINE__); \
} while (0)

void _always_assert_fail(const char *expr, const char *file, long line);

void PyJIT_FatalError(const char *msg);

#ifdef __cplusplus
}
#endif
#endif /* !Py_JIT_MACROS_H */
