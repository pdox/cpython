#ifndef Py_LIMITED_API
#ifndef Py_JIT_ATTRCACHE_H
#define Py_JIT_ATTRCACHE_H
#ifdef __cplusplus
extern "C" {
#endif

extern int Py_JITAttrCache;

struct _PyJITAttrCache;
typedef struct _PyJITAttrCache PyJITAttrCache;

PyJITAttrCache* PyJITAttrCache_New(PyObject *attrname);

typedef PyObject* (*GetAttrSignature)(PyObject *, PyJITAttrCache *);
typedef PyObject* (*GetMethodSignature)(PyObject *, PyJITAttrCache *, int*);

PyObject* _PyJITAttrCache_GetAttr(PyObject *receiver, PyJITAttrCache *ic);
PyObject* _PyJITAttrCache_GetMethod(PyObject *receiver, PyJITAttrCache *ic, int *method_found);

PyObject* _PyJITAttrCache_GetAttrAndVerify(PyObject *receiver, PyJITAttrCache *ic);
PyObject* _PyJITAttrCache_GetMethodAndVerify(PyObject *receiver, PyJITAttrCache *ic, int *method_found);

static inline
GetAttrSignature PyJITAttrCache_GetAttrFunc(void) {
  switch (Py_JITAttrCache) {
  case 0: return NULL;
  case 1: return _PyJITAttrCache_GetAttr;
  case 2: return _PyJITAttrCache_GetAttrAndVerify;
  default: abort();
  }
}

static inline
GetMethodSignature PyJITAttrCache_GetMethodFunc(void) {
  switch (Py_JITAttrCache) {
  case 0: return NULL;
  case 1: return _PyJITAttrCache_GetMethod;
  case 2: return _PyJITAttrCache_GetMethodAndVerify;
  default: abort();
  }
}

void _PyJITAttrCache_Notify_TypeModified(PyTypeObject *tp);
void _PyJITAttrCache_Notify_TypeDealloc(PyTypeObject *tp);
void _PyJITAttrCache_Notify_SetCachedKeys(PyTypeObject *tp, PyDictKeysObject *dk);
void _PyJITAttrCache_Notify_AddKey(PyDictKeysObject *dk, PyObject *attrkey, Py_hash_t attrhash);

#ifdef __cplusplus
}
#endif
#endif /* !Py_JIT_ATTRCACHE_H */
#endif /* Py_LIMITED_API */

