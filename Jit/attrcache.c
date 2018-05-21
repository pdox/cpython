#define ATTRCACHE_SOURCE
#include "Python.h"
#include "Objects/stringlib/eq.h"
#include "Jit/macros.h"
#include "adt.h"
#include "Jit/attrcache.h"
#include "Jit/options.h"

#if defined(__GNUC__) && (__GNUC__ > 2) && defined(__OPTIMIZE__)
#  define UNLIKELY(value) __builtin_expect((value), 0)
#else
#  define UNLIKELY(value) (value)
#endif

extern int _PyObject_GetMethod(PyObject *, PyObject *, PyObject **);
extern int assign_version_tag(PyTypeObject *);

typedef struct {
    int is_load_method;
    PyObject *attrname;
    Py_hash_t attrhash;
} PyJITAttrCacheStubKey;

struct _PyJITAttrCacheStub {
    void *handler;
    PyObject *tp_value;            /* Reference borrowed from PyTypeObject */
    Py_ssize_t tp_dictoffset;
    PyDictKeysObject *cached_keys; /* Reference borrowed from PyTypeObject */
    Py_ssize_t dk_index;

    /* This holds a strong reference to 'attrname' */
    PyJITAttrCacheStubKey key;
    uint32_t refcnt;
};

PyJITAttrCacheStub*
_PyJITAttrCache_GetStub(PyTypeObject *tp, PyObject *name, int is_load_method);

static void _update_stub(PyTypeObject *tp, PyJITAttrCacheStub *stub);

#define TP_VALUE_NULL         0
#define TP_VALUE_NOT_NULL     1
#define TP_VALUE_DESCR        2
#define TP_VALUE_METHOD       3

#define NODICT              0
#define TP_DICTOFFSET      -1

/* Each automatic handler is a call to _fetch where the first three arguments
   are known at compile-time. In theory, this will allow the compiler to propagate
   the constants, inline, and optimize aggressively to produce an optimal version
   of _fetch() specialized for each stub type.

   If it turns out not all compilers do the right thing here, it may be necessary
   to generate the stubs directly (e.g. using asmjit).
 */
static inline PyObject* _Py_HOT_FUNCTION
_fetch(
    int tp_value_mode,
    Py_ssize_t dictoffset,
    int *side_effects,
    PyObject *receiver,
    PyJITAttrCacheStub *stub,
    int *method_found)
{
    assert(!_PyErr_OCCURRED());
    PyObject *descr = NULL;
    descrgetfunc f = NULL;
    if (tp_value_mode != TP_VALUE_NULL) {
        /* Need to hold strong reference to descr while calling
           tp_descr_get or doing PyDict_GetItem */
        descr = stub->tp_value;
        Py_INCREF(descr);
        if (tp_value_mode == TP_VALUE_DESCR) {
            f = descr->ob_type->tp_descr_get;
        }
    }
    /* Find the dictionary */
    PyDictObject *dict = NULL;
    if (dictoffset == TP_DICTOFFSET) {
        dictoffset = stub->tp_dictoffset;
    }
    if (dictoffset != 0) {
        if (dictoffset < 0) {
            Py_ssize_t tsize;
            size_t size;

            tsize = ((PyVarObject *)receiver)->ob_size;
            if (tsize < 0)
                tsize = -tsize;
            size = _PyObject_VAR_SIZE(Py_TYPE(receiver), tsize);
            assert(size <= PY_SSIZE_T_MAX);

            dictoffset += (Py_ssize_t)size;
            assert(dictoffset > 0);
            assert(dictoffset % SIZEOF_VOID_P == 0);
        }
        PyDictObject **dictptr;
        dictptr = (PyDictObject **) ((char *)receiver + dictoffset);
        dict = *dictptr;
    }
    if (dict != NULL) {
        PyObject *res;
        if (dict->ma_keys == stub->cached_keys) {
            res = (stub->dk_index >= 0) ? dict->ma_values[stub->dk_index] : NULL;
        } else {
            if (side_effects) *side_effects = 1;
            res = PyDict_GetItem((PyObject*)dict, stub->key.attrname);
        }
        if (res != NULL) {
            Py_INCREF(res);
            if (tp_value_mode != TP_VALUE_NULL) Py_DECREF(descr);
            assert(!_PyErr_OCCURRED());
            return res;
        }
    }
    if (tp_value_mode == TP_VALUE_METHOD) {
        *method_found = 1;
        return descr;
    }
    if (tp_value_mode == TP_VALUE_DESCR) {
        PyObject *res = f(descr, receiver, (PyObject*)Py_TYPE(receiver));
        if (side_effects) *side_effects = 1;
        Py_DECREF(descr);
        return res;
    }
    if (tp_value_mode == TP_VALUE_NULL) {
        PyErr_Format(PyExc_AttributeError,
                     "'%.50s' object has no attribute '%U'",
                     Py_TYPE(receiver)->tp_name, stub->key.attrname);
        if (side_effects) *side_effects = 1;
        return NULL;
    }
    return descr;
}

#define HANDLER_PARAMS    PyObject *receiver, PyJITAttrCacheStub *stub, PyJITAttrCache *ic, int *method_found
#define HANDLER_ARGS      receiver, stub, ic, method_found

typedef PyObject* (*handler_t)(HANDLER_PARAMS);

#define _HANDLER(name)              _handler_ ## name
#define _HANDLER_WITH_VERIFY(name)  _handler_ ## name ## _with_verify

#define DEFINE_HANDLER(name) \
  static PyObject* _Py_HOT_FUNCTION _HANDLER(name) (HANDLER_PARAMS)

#define DEFINE_HANDLER_WITH_VERIFY(name) \
  static PyObject* _Py_HOT_FUNCTION _HANDLER_WITH_VERIFY(name) (HANDLER_PARAMS)

#define DEFINE_HANDLER_WITH_VERIFY_AS_SAME(name) \
    DEFINE_HANDLER_WITH_VERIFY(name) { \
        return _HANDLER(name)(HANDLER_ARGS); \
    }

#define GET_HANDLER(name) \
    (Py_JITAttrCache == JIT_ATTRCACHE_VERIFY ? \
     _HANDLER_WITH_VERIFY(name) : \
     _HANDLER(name))

#define DEFINE_AUTO(name, tp_value_mode, dictoffset) \
  static PyObject* _Py_HOT_FUNCTION _HANDLER(name) (HANDLER_PARAMS) { \
    return _fetch((tp_value_mode), (dictoffset), NULL, receiver, stub, method_found); \
  } \
  static PyObject* _Py_HOT_FUNCTION _HANDLER_WITH_VERIFY(name) (HANDLER_PARAMS) { \
    always_assert(!_PyErr_OCCURRED()); \
    int side_effects = 0; \
    int mfound = 0; \
    int *mfoundp = ic->is_load_method ? &mfound : NULL; \
    PyObject* result = _fetch((tp_value_mode), (dictoffset), &side_effects, receiver, stub, mfoundp); \
    return _verify(result, mfound, side_effects, ic, receiver, method_found); \
  }

static PyObject* _verify(PyObject *result, int mfound, int side_effects, PyJITAttrCache *ic, PyObject *receiver, int *method_found) {
    if (result == NULL) {
        always_assert(_PyErr_OCCURRED());
    } else {
        always_assert(!_PyErr_OCCURRED());
    }
    if (ic->is_load_method) {
        *method_found = mfound;
    } else {
        always_assert(mfound == 0);
    }
    if (!side_effects) {
        if (ic->is_load_method) {
            PyObject *meth = NULL;
            int tmp = _PyObject_GetMethod(receiver, ic->attrname, &meth);
            always_assert(meth == result);
            always_assert(tmp == mfound);
            Py_XDECREF(meth);
        } else {
            PyObject *obj = PyObject_GetAttr(receiver, ic->attrname);
            always_assert(obj == result);
            Py_XDECREF(obj);
        }
    }
    return result;
}

/* Handlers automatically generated from _fetch */
DEFINE_AUTO(null,           TP_VALUE_NULL,       NODICT)
DEFINE_AUTO(value,          TP_VALUE_NOT_NULL,   NODICT)
DEFINE_AUTO(descr,          TP_VALUE_DESCR,      NODICT)
DEFINE_AUTO(method,         TP_VALUE_METHOD,     NODICT)

DEFINE_AUTO(dict,           TP_VALUE_NULL,       TP_DICTOFFSET)
DEFINE_AUTO(dict_32,        TP_VALUE_NULL,       32)
DEFINE_AUTO(value_dict,     TP_VALUE_NOT_NULL,   TP_DICTOFFSET)
DEFINE_AUTO(descr_dict,     TP_VALUE_DESCR,      TP_DICTOFFSET)
DEFINE_AUTO(method_dict,    TP_VALUE_METHOD,     TP_DICTOFFSET)

DEFINE_HANDLER(getattr) {
    return PyObject_GetAttr(receiver, ic->attrname);
}
DEFINE_HANDLER_WITH_VERIFY_AS_SAME(getattr);

DEFINE_HANDLER(getmethod) {
    /* NOTE: Cannot dereference 'stub', as it may be NULL */
    PyObject *meth = NULL;
    *method_found = _PyObject_GetMethod(receiver, ic->attrname, &meth);
    return meth;
}
DEFINE_HANDLER_WITH_VERIFY_AS_SAME(getmethod);

DEFINE_HANDLER(stale) {
    _update_stub(Py_TYPE(receiver), stub);
    return ((handler_t)stub->handler)(HANDLER_ARGS);
}
DEFINE_HANDLER_WITH_VERIFY_AS_SAME(stale);

static inline size_t _find_entry(PyJITAttrCache *ic, PyTypeObject *tp) {
    size_t i = 0;
    while (i < ATTRCACHE_ENTRY_COUNT && ic->entries[i].tp != tp) i++;
    return i;
}

DEFINE_HANDLER(miss) {
    size_t i = _find_entry(ic, NULL);
    if (i < ATTRCACHE_ENTRY_COUNT) {
        PyTypeObject *tp = Py_TYPE(receiver);
        stub = _PyJITAttrCache_GetStub(tp, ic->attrname, ic->is_load_method);
        stub->refcnt++;
        ic->entries[i].tp = tp;
        ic->entries[i].stub = stub;
        return ((handler_t)(stub->handler))(HANDLER_ARGS);
    }
    stub = NULL;
    handler_t handler =
        ic->is_load_method ? GET_HANDLER(getmethod) : GET_HANDLER(getattr);
    return handler(HANDLER_ARGS);
}
DEFINE_HANDLER_WITH_VERIFY_AS_SAME(miss);

PyJITAttrCacheStub PyJITAttrCache_MissStub = { _HANDLER(miss) };


DEFINE_HANDLER(orphaned) {
    /* Wait a minute!! This stub is orphaned, which means that its PyTypeObject
       was deallocated. So then, how are we being called to do attribute lookup
       on an object of that type?

       This happens when a new, different PyTypeObject is created, and happens
       to live at exactly the same address in memory as the old type. This is
       actually quite common, since types may be created and destroyed quickly.

       We could technically re-use the orphaned stub, but it is easier to just
       discard it and create a new one.
     */
    size_t i = _find_entry(ic, Py_TYPE(receiver));
    assert(i < ATTRCACHE_ENTRY_COUNT);
    ic->entries[i].tp = NULL;
    stub = NULL;
    return GET_HANDLER(miss)(HANDLER_ARGS);
}
DEFINE_HANDLER_WITH_VERIFY_AS_SAME(orphaned);

static void _update_stub(PyTypeObject *tp, PyJITAttrCacheStub *stub) {
    if (!(tp->tp_flags & Py_TPFLAGS_READY) ||
        tp->tp_getattro != PyObject_GenericGetAttr ||
        !assign_version_tag(tp)) {
        stub->handler = stub->key.is_load_method ? GET_HANDLER(getmethod) : GET_HANDLER(getattr);
    } else {
        PyObject *descr = _PyType_Lookup(tp, stub->key.attrname);
        Py_ssize_t dictoffset = tp->tp_dictoffset;
        stub->tp_value = descr;
        if (descr != NULL) {
            descrgetfunc f = descr->ob_type->tp_descr_get;
            if (f) {
                /* tp_value is a descriptor */
                if (stub->key.is_load_method &&
                    (PyFunction_Check(descr) ||
                     Py_TYPE(descr) == &PyMethodDescr_Type)) {
                    stub->handler = dictoffset ? GET_HANDLER(method_dict) : GET_HANDLER(method);
                } else if (PyDescr_IsData(descr) || dictoffset == 0) {
                    stub->handler = GET_HANDLER(descr);
                    dictoffset = 0; /* data descriptor takes precedence over dictionary */
                } else {
                    stub->handler = GET_HANDLER(descr_dict);
                }
            } else {
                /* tp_value is not a descriptor */
                stub->handler = dictoffset ? GET_HANDLER(value_dict) : GET_HANDLER(value);
            }
        } else {
            /* tp_value is NULL */
            if (dictoffset == 32) {
                stub->handler = GET_HANDLER(dict_32);
            } else if (dictoffset == 0) {
                stub->handler = GET_HANDLER(null);
            } else {
                stub->handler = GET_HANDLER(dict);
            }
        }

        /* Fill in the dictionary info, if needed */
        if (dictoffset != 0) {
            stub->tp_dictoffset = dictoffset;
            PyDictKeysObject *dk = NULL;
            if (tp->tp_flags & Py_TPFLAGS_HEAPTYPE) {
                PyHeapTypeObject *et = (PyHeapTypeObject*)tp;
                dk = et->ht_cached_keys;
            }
            stub->cached_keys = dk;
            stub->dk_index = (dk != NULL) ? lookdict_split_for_jit(dk, stub->key.attrname, stub->key.attrhash) : -1;
        }
    }
}

static void
_PyJITAttrCache_DeleteStub(PyJITAttrCacheStub *stub) {
    assert(stub->refcnt == 0);
    Py_DECREF(stub->key.attrname);
    free(stub);
}

/* PyTypeObject* -> TypeInfo* */
static adt_hashmap _tp_to_type_info;

/* PyDictKeysObject* -> TypeInfo* */
static adt_hashmap _dk_to_type_info;

typedef struct {
    PyTypeObject *tp;
    PyDictKeysObject *dk;

    /*  PyJITAttrCacheStubKey -> PyJITAttrCacheStub*  */
    adt_hashmap stub_map;
} TypeInfo;

static int stub_key_eq(void *ap, void *bp) {
    PyJITAttrCacheStubKey *a = (PyJITAttrCacheStubKey*)ap;
    PyJITAttrCacheStubKey *b = (PyJITAttrCacheStubKey*)bp;
    if (a->attrhash != b->attrhash) return 0;
    if (a->is_load_method != b->is_load_method) return 0;
    return unicode_eq(a->attrname, b->attrname);
}

static size_t stub_key_hash(void *ap) {
    PyJITAttrCacheStubKey *a = (PyJITAttrCacheStubKey*)ap;
    return (size_t)a->attrhash + a->is_load_method;
}

static int ptr_eq(void *ap, void *bp) {
    void *a = *(void**)ap;
    void *b = *(void**)bp;
    return a == b;
}

static size_t ptr_hash(void *ap) {
    void *a = *(void**)ap;
    return (size_t)a;
}

static void
_init_maps(void) {
    if (_tp_to_type_info == NULL) {
        _tp_to_type_info = adt_hashmap_new(
            sizeof(PyTypeObject*),
            sizeof(TypeInfo*),
            ptr_eq,
            ptr_hash);
        _dk_to_type_info = adt_hashmap_new(
            sizeof(PyDictKeysObject*),
            sizeof(TypeInfo*),
            ptr_eq,
            ptr_hash);
    }
}

static TypeInfo*
_PyJITAttrCache_CreateTypeInfo(PyTypeObject *tp) {
    _init_maps();
    PyDictKeysObject *dk = NULL;
    if (tp->tp_flags & Py_TPFLAGS_HEAPTYPE) {
        dk = ((PyHeapTypeObject*)tp)->ht_cached_keys;
    }
    TypeInfo *ti = (TypeInfo*)malloc(sizeof(TypeInfo));
    ti->tp = tp;
    ti->dk = dk;
    ti->stub_map = adt_hashmap_new(
        sizeof(PyJITAttrCacheStubKey),
        sizeof(PyJITAttrCacheStub*),
        stub_key_eq,
        stub_key_hash);
    always_assert(adt_hashmap_insert(_tp_to_type_info, &tp, &ti));
    if (dk != NULL) {
        always_assert(adt_hashmap_insert(_dk_to_type_info, &dk, &ti));
    }
    return ti;
}

static TypeInfo*
_PyJITAttrCache_GetTypeInfo(PyTypeObject *tp, int should_create) {
    _init_maps();
    TypeInfo *ti;
    if (adt_hashmap_get(_tp_to_type_info, &tp, &ti)) {
        return ti;
    }
    if (!should_create)
        return NULL;
    return _PyJITAttrCache_CreateTypeInfo(tp);
}

static TypeInfo*
_PyJITAttrCache_GetTypeInfoFromDictKeys(PyDictKeysObject *dk) {
    if (_dk_to_type_info == NULL)
        return NULL;
    TypeInfo *ti;
    if (adt_hashmap_get(_dk_to_type_info, &dk, &ti))
        return ti;
    return NULL;
}

static void _set_all_handlers(TypeInfo *ti, handler_t handler) {
    PyJITAttrCacheStubKey key;
    PyJITAttrCacheStub *stub;
    adt_hashmap_cursor_t cursor;
    adt_hashmap_iter_begin(ti->stub_map, &cursor);
    while (adt_hashmap_iter_next(&cursor, &key, &stub)) {
        stub->handler = handler;
    }
}

static void mark_all_stubs_stale(TypeInfo *ti) {
    _set_all_handlers(ti, GET_HANDLER(stale));
}

static void release_all_stubs(TypeInfo *ti) {
    PyJITAttrCacheStubKey key;
    PyJITAttrCacheStub *stub;
    adt_hashmap_cursor_t cursor;
    adt_hashmap_iter_begin(ti->stub_map, &cursor);
    while (adt_hashmap_iter_next(&cursor, &key, &stub)) {
        stub->handler = GET_HANDLER(orphaned);
        stub->refcnt--;
        if (stub->refcnt == 0) {
            _PyJITAttrCache_DeleteStub(stub);
        }
    }
    adt_hashmap_delete(ti->stub_map);
    ti->stub_map = NULL;
}

static void
_PyJITAttrCache_DeleteTypeInfo(TypeInfo *ti) {
    PyTypeObject *tp = ti->tp;
    PyDictKeysObject *dk = ti->dk;
    TypeInfo *v;
    if (dk != NULL) {
        always_assert(adt_hashmap_remove(_dk_to_type_info, &dk, &v));
        always_assert(ti == v);
    }
    /* Remove type info from hashmap */
    always_assert(adt_hashmap_remove(_tp_to_type_info, &tp, &v));
    always_assert(ti == v);

    release_all_stubs(ti);
    free(ti);
}

PyJITAttrCacheStub*
_PyJITAttrCache_GetStub(PyTypeObject *tp, PyObject *name, int is_load_method) {
    /* See if there is already a stub for this (tp, name) pair */
    TypeInfo *ti = _PyJITAttrCache_GetTypeInfo(tp, 1);
    PyJITAttrCacheStubKey key;
    PyJITAttrCacheStub *stub;
    key.is_load_method = is_load_method;
    key.attrname = name;
    key.attrhash = PyObject_Hash(name);
    if (adt_hashmap_get(ti->stub_map, &key, &stub)) {
        return stub;
    }
    /* Make the stub and insert it into the stub_map */
    stub = (PyJITAttrCacheStub*)malloc(sizeof(PyJITAttrCacheStub));
    Py_INCREF(key.attrname);
    stub->key = key;
    stub->refcnt = 1; /* Reference owned by stub_map */
    _update_stub(tp, stub);
    always_assert(adt_hashmap_insert(ti->stub_map, &key, &stub));
    return stub;
}

void _PyJITAttrCache_Notify_TypeModified(PyTypeObject *tp) {
    TypeInfo *ti = _PyJITAttrCache_GetTypeInfo(tp, 0);
    if (ti == NULL)
        return;

    /* Mark all stubs as being stale */
    mark_all_stubs_stale(ti);
}

void _PyJITAttrCache_Notify_TypeDealloc(PyTypeObject *tp) {
    TypeInfo *ti = _PyJITAttrCache_GetTypeInfo(tp, 0);
    if (ti == NULL)
        return;
    _PyJITAttrCache_DeleteTypeInfo(ti);
}

void _PyJITAttrCache_Notify_SetCachedKeys(PyTypeObject *tp, PyDictKeysObject *dk) {
    TypeInfo *ti = _PyJITAttrCache_GetTypeInfo(tp, 0);
    if (ti == NULL)
        return;

    /* Mark all stubs as stale */
    mark_all_stubs_stale(ti);

    /* Change 'dk' */
    if (ti->dk != NULL) {
        TypeInfo *v;
        PyDictKeysObject *old_dk = ti->dk;
        int r = adt_hashmap_remove(_dk_to_type_info, &old_dk, &v);
        always_assert(r);
        always_assert(v == ti);
    }
    ti->dk = dk;
    if (dk != NULL) {
        int r = adt_hashmap_insert(_dk_to_type_info, &dk, &ti);
        always_assert(r);
    }
}

void _PyJITAttrCache_Notify_AddKey(PyDictKeysObject *dk, PyObject *attrkey, Py_hash_t attrhash) {
    assert(dk != NULL);
    TypeInfo *ti = _PyJITAttrCache_GetTypeInfoFromDictKeys(dk);
    if (ti == NULL)
        return;
    assert(ti->dk == dk);

    /* There may be an existing stub for this key (with a dk_index of -1)
       which is no longer valid. Mark all stubs with this hash invalid. */
    PyJITAttrCacheStubKey key;
    PyJITAttrCacheStub *stub;
    adt_hashmap_cursor_t cursor;
    adt_hashmap_iter_begin(ti->stub_map, &cursor);
    while (adt_hashmap_iter_next(&cursor, &key, &stub)) {
        if (key.attrhash == attrhash) {
            stub->handler = GET_HANDLER(stale);
        }
    }
}

PyJITAttrCache*
PyJITAttrCache_New(PyObject *attrname, int is_load_method) {
    assert(PyUnicode_CheckExact(attrname));
    PyJITAttrCache *ic = (PyJITAttrCache*)malloc(sizeof(PyJITAttrCache));
    /* Zero-out both entries. The first use of this cache will trigger the
       fallback handler, and fill in the first stub. */
    memset(ic, 0, sizeof(PyJITAttrCache));
    ic->attrname = attrname;
    ic->is_load_method = is_load_method;
    return ic;
}
