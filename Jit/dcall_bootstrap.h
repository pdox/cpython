#ifndef Py_DCALL_BOOTSTRAP_H
#define Py_DCALL_BOOTSTRAP_H
#ifdef __cplusplus
extern "C" {
#endif

void *PyJIT_DCall_MakeBootstrap_X86_64(void *target);

#ifdef __cplusplus
}
#endif
#endif /* !Py_DCALL_BOOTSTRAP_H */
