#ifndef Py_JIT_OPTIONS_H
#define Py_JIT_OPTIONS_H
#ifdef __cplusplus
extern "C" {
#endif

extern int Py_JITFlag;
extern int Py_JITDebugFlag;
extern char *Py_JITDebugFunc;
extern char *Py_JITDebugFile;
extern int Py_JITEvalBreaks;
extern int Py_JITPatchpoint;
extern int Py_JITNoExc;
extern int Py_JITNoSuper;
extern int Py_JITAsserts;

#define JIT_ATTRCACHE_OFF     0
#define JIT_ATTRCACHE_ON      1
#define JIT_ATTRCACHE_VERIFY  2
extern int Py_JITAttrCache;

void PyJIT_InitOptions(const char *config);

const char* PyJIT_ReadDefaultOptions(void);

#ifdef __cplusplus
}
#endif
#endif /* !Py_JIT_H */

