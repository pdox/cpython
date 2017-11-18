#ifndef Py_JIT_H
#define Py_JIT_H
#ifdef __cplusplus
extern "C" {
#endif

struct _EvalContext;
typedef struct _EvalContext EvalContext;

PyAPI_FUNC(int) _PyJIT_Execute(struct _EvalContext *ctx);
PyAPI_FUNC(void) _PyJIT_JUMP(EvalContext *ctx);
PyAPI_FUNC(void) _PyJIT_GOTO_NEXT_OPCODE(EvalContext *ctx);
PyAPI_FUNC(void) _PyJIT_GOTO_FAST_YIELD(EvalContext *ctx);
PyAPI_FUNC(void) _PyJIT_GOTO_ERROR(EvalContext *ctx);
PyAPI_FUNC(void) _PyJIT_GOTO_DISPATCH_OPCODE(EvalContext *ctx);
PyAPI_FUNC(void) _PyJIT_GOTO_FAST_BLOCK_END(EvalContext *ctx);
PyAPI_FUNC(void) _PyJIT_GOTO_UNWIND_CLEANUP(EvalContext *ctx);
PyAPI_FUNC(void) _PyJIT_GOTO_EXIT_EVAL_FRAME(EvalContext *ctx);

#ifdef __cplusplus
}
#endif
#endif /* !Py_JIT_H */
