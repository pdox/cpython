#include "Python.h"
#include "Objects/stringlib/eq.h"
#include "jit_attrcache.h"
#include "adt.h"

#if defined(__GNUC__) && (__GNUC__ > 2) && defined(__OPTIMIZE__)
#  define UNLIKELY(value) __builtin_expect((value), 0)
#else
#  define UNLIKELY(value) (value)
#endif

extern int _PyObject_GetMethod(PyObject *, PyObject *, PyObject **);
extern int assign_version_tag(PyTypeObject *);

typedef struct {
    PyObject *attrname;
    Py_hash_t attrhash;
} PyJITAttrCacheStubKey;

typedef struct {
    uint32_t refcnt;
    uint32_t mode;
    PyObject *tp_value;            /* Reference borrowed from PyTypeObject */
    PyDictKeysObject *cached_keys; /* Reference borrowed from PyTypeObject */
    Py_ssize_t dk_index;
    // Key part
    PyObject *attrname;            /* Strong reference to attrname */
    Py_hash_t attrhash;
} PyJITAttrCacheStub;

/* Everything is perfect, use the fast path */
#define ATTRCACHE_STUB_FAST           0
/* Should always use fallback mechanism */
#define ATTRCACHE_STUB_FALLBACK       1
/* PyTypeObject was deallocated. Stub still exists until refcnt falls to 0. */
#define ATTRCACHE_STUB_ORPHANED       2
/* PyTypeObject has been modified, and stub needs to be updated. */
#define ATTRCACHE_STUB_STALE          3

static void _update_stub(PyTypeObject *tp, PyJITAttrCacheStub *stub) {
    if (!(tp->tp_flags & Py_TPFLAGS_READY) ||
        tp->tp_getattro != PyObject_GenericGetAttr ||
        !assign_version_tag(tp)) {
        stub->mode = ATTRCACHE_STUB_FALLBACK;
        stub->tp_value = NULL;
        stub->cached_keys = NULL;
        stub->dk_index = -1;
    } else {
        stub->mode = ATTRCACHE_STUB_FAST;
        stub->tp_value = _PyType_Lookup(tp, stub->attrname);
        PyDictKeysObject *dk = NULL;
        if (tp->tp_flags & Py_TPFLAGS_HEAPTYPE) {
            PyHeapTypeObject *et = (PyHeapTypeObject*)tp;
            dk = et->ht_cached_keys;
        }
        stub->cached_keys = dk;
        stub->dk_index = (dk != NULL) ? lookdict_split_for_jit(dk, stub->attrname, stub->attrhash) : -1;
    }
}

static PyJITAttrCacheStub*
_PyJITAttrCache_MakeStub(PyTypeObject *tp, PyObject *attrname, Py_hash_t attrhash) {
    PyJITAttrCacheStub *stub;
    stub = (PyJITAttrCacheStub*)malloc(sizeof(PyJITAttrCacheStub));
    stub->refcnt = 1;
    Py_INCREF(attrname);
    stub->attrname = attrname;
    stub->attrhash = attrhash;
    _update_stub(tp, stub);
    return stub;
}

static void
_PyJITAttrCache_DeleteStub(PyJITAttrCacheStub *stub) {
    assert(stub->refcnt == 0);
    assert(stub->mode == ATTRCACHE_STUB_ORPHANED);
    Py_DECREF(stub->attrname);
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
    return unicode_eq(a->attrname, b->attrname);
}

static size_t stub_key_hash(void *ap) {
    PyJITAttrCacheStubKey *a = (PyJITAttrCacheStubKey*)ap;
    return (size_t)a->attrhash;
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

static void mark_all_stubs_stale(TypeInfo *ti) {
    adt_hashmap_cursor_t cursor;
    adt_hashmap_iter_begin(ti->stub_map, &cursor);
    PyJITAttrCacheStubKey key;
    PyJITAttrCacheStub *stub;
    while (adt_hashmap_iter_next(&cursor, &key, &stub)) {
        stub->mode = ATTRCACHE_STUB_STALE;
    }
}

static void mark_all_stubs_orphaned(TypeInfo *ti) {
    adt_hashmap_cursor_t cursor;
    adt_hashmap_iter_begin(ti->stub_map, &cursor);
    PyJITAttrCacheStubKey key;
    PyJITAttrCacheStub *stub;
    while (adt_hashmap_iter_next(&cursor, &key, &stub)) {
        stub->mode = ATTRCACHE_STUB_ORPHANED;
        if (stub->refcnt == 0) {
            _PyJITAttrCache_DeleteStub(stub);
        }
    }
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

    /* Mark stubs as being orphaned */
    mark_all_stubs_orphaned(ti);

    /* Delete stub hashmap */
    adt_hashmap_delete(ti->stub_map);

    free(ti);
}

PyJITAttrCacheStub*
_PyJITAttrCache_GetStub(PyTypeObject *tp, PyObject *name) {
    /* See if there is already a stub for this (tp, name) pair */
    TypeInfo *ti = _PyJITAttrCache_GetTypeInfo(tp, 1);
    PyJITAttrCacheStubKey key;
    PyJITAttrCacheStub *stub;
    key.attrname = name;
    key.attrhash = PyObject_Hash(name);
    if (adt_hashmap_get(ti->stub_map, &key, &stub)) {
        stub->refcnt++;
        return stub;
    }
    stub = _PyJITAttrCache_MakeStub(tp, key.attrname, key.attrhash);
    always_assert(adt_hashmap_insert(ti->stub_map, &key, &stub));
    return stub;
}

void _PyJITAttrCache_Notify_TypeModified(PyTypeObject *tp) {
    TypeInfo *ti = _PyJITAttrCache_GetTypeInfo(tp, 0);
    if (ti == NULL)
        return;

    /* Mark all stubs as being stale */
    //fprintf(stderr, "NOTIFY TypeModified tp=%p\n", tp);
    mark_all_stubs_stale(ti);
}

void _PyJITAttrCache_Notify_TypeDealloc(PyTypeObject *tp) {
    TypeInfo *ti = _PyJITAttrCache_GetTypeInfo(tp, 0);
    if (ti == NULL)
        return;
    //fprintf(stderr, "NOTIFY TypeDealloc tp=%p,%s\n", tp, tp->tp_name);
    _PyJITAttrCache_DeleteTypeInfo(ti);
}

void _PyJITAttrCache_Notify_SetCachedKeys(PyTypeObject *tp, PyDictKeysObject *dk) {
    TypeInfo *ti = _PyJITAttrCache_GetTypeInfo(tp, 0);
    if (ti == NULL)
        return;
    //fprintf(stderr, "NOTIFY SetCachedKeys tp=%p\n", tp);

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
    adt_hashmap_cursor_t cursor;
    adt_hashmap_iter_begin(ti->stub_map, &cursor);
    PyJITAttrCacheStubKey key;
    PyJITAttrCacheStub *stub;
    while (adt_hashmap_iter_next(&cursor, &key, &stub)) {
        if (key.attrhash == attrhash) {
            stub->mode = ATTRCACHE_STUB_STALE;
        }
    }
}

typedef struct {
    PyTypeObject *tp;
    PyJITAttrCacheStub *stub;
} PyJITAttrCacheEntry;

#define MAX_IC_SLOTS 4

typedef struct _PyJITAttrCache {
    PyJITAttrCacheEntry entries[MAX_IC_SLOTS];
    uint64_t counts[MAX_IC_SLOTS];
    PyObject *attrname; /* Borrowed reference */
} PyJITAttrCache;

PyJITAttrCache*
PyJITAttrCache_New(PyObject *attrname) {
    assert(PyUnicode_CheckExact(attrname));
    PyJITAttrCache *ret = (PyJITAttrCache*)malloc(sizeof(PyJITAttrCache));
    memset(ret, 0, sizeof(PyJITAttrCache));
    ret->attrname = attrname;
    return ret;
}

static inline size_t
_ic_lookup(PyTypeObject *tp, PyJITAttrCache *ic) {
    size_t i = 0;
    while (i < MAX_IC_SLOTS && ic->entries[i].tp != tp) i++;
    if (i == MAX_IC_SLOTS) {
        /* Find an empty slot */
        i = 0;
        while (i < MAX_IC_SLOTS && ic->entries[i].tp != NULL) i++;
        if (i == MAX_IC_SLOTS) return MAX_IC_SLOTS;
        PyJITAttrCacheStub *stub = _PyJITAttrCache_GetStub(tp, ic->attrname);
        ic->entries[i].tp = tp;
        ic->entries[i].stub = stub;
    }
    return i;
}

static inline PyObject*
_PyJITAttrCache_FetchFallback(
        PyObject *receiver,
        PyJITAttrCache *ic,
        int *method_found,
        int *side_effects) {
    assert(!_PyErr_OCCURRED());
    if (side_effects) *side_effects = 1;
    if (method_found == NULL) {
        return PyObject_GetAttr(receiver, ic->attrname);
    } else {
        PyObject *meth = NULL;
        *method_found = _PyObject_GetMethod(receiver, ic->attrname, &meth);
        return meth;
    }
}

static inline PyObject* _Py_HOT_FUNCTION
_PyJITAttrCache_Fetch(
        PyObject *receiver,
        PyJITAttrCache *ic,
        int *method_found,
        int *side_effects) {
    assert(!_PyErr_OCCURRED());
    PyTypeObject *tp = Py_TYPE(receiver);
    size_t ic_slot;
    PyJITAttrCacheStub *stub;

 restart1:
    ic_slot = _ic_lookup(tp, ic);
    stub = (ic_slot < MAX_IC_SLOTS) ? ic->entries[ic_slot].stub : NULL;

 restart2:
/*
    fprintf(stderr, "PyJITAttrCache_Fetch: ic=%p, slot=%zu, tp=%s, attr=%s, stub=%p",
            ic,
            ic_slot,
            tp->tp_name,
            PyUnicode_AsUTF8(ic->attrname),
            stub);
    if (stub != NULL) {
        fprintf(stderr, "[ refcnt=%u, mode=%u, tp_value=%p, cached_keys=%p, dk_index=%zd, attrname=%s, attrhash=%zu ]\n",
                stub->refcnt,
                stub->mode,
                stub->tp_value,
                stub->cached_keys,
                stub->dk_index,
                PyUnicode_AsUTF8(stub->attrname),
                stub->attrhash);
    } else {
        fprintf(stderr, "[]\n");
    }
*/
    if (UNLIKELY(stub == NULL)) {
        return _PyJITAttrCache_FetchFallback(receiver, ic, method_found, side_effects);
    }
    if (UNLIKELY(stub->mode != ATTRCACHE_STUB_FAST)) {
        assert(!_PyErr_OCCURRED());
        if (stub->mode == ATTRCACHE_STUB_FALLBACK) {
            return _PyJITAttrCache_FetchFallback(receiver, ic, method_found, side_effects);
        } else if (stub->mode == ATTRCACHE_STUB_STALE) {
            _update_stub(Py_TYPE(receiver), stub);
            goto restart2;
        } else if (stub->mode == ATTRCACHE_STUB_ORPHANED) {
            /* Stub is invalid. Clear it. */
            ic->entries[ic_slot].tp = NULL;
            ic->entries[ic_slot].stub = NULL;
            goto restart1;
        } else {
            char buf[256];
            sprintf(buf, "Invalid stub mode: %d", (int)stub->mode);
            Py_FatalError(buf);
        }
    }

    PyObject* descr = stub->tp_value;
    descrgetfunc f = NULL;
    int descr_is_method = 0;
    if (descr != NULL) {
        /* Need to hold strong reference to descr while calling
           tp_descr_get or doing PyDict_GetItem */
        Py_INCREF(descr);
        f = descr->ob_type->tp_descr_get;
        if (f != NULL) {
            if (method_found &&
                (PyFunction_Check(descr) ||
                 Py_TYPE(descr) == &PyMethodDescr_Type)) {
                descr_is_method = 1;
            } else if (PyDescr_IsData(descr)) {
                PyObject* res = f(descr, receiver, (PyObject*)tp);
                if (side_effects) *side_effects = 1;
                Py_DECREF(descr);
                return res;
            }
        }
    }
    /* Find the dictionary */
    PyDictObject *dict = NULL;
    Py_ssize_t dictoffset = tp->tp_dictoffset;
    if (dictoffset != 0) {
        if (dictoffset < 0) {
            Py_ssize_t tsize;
            size_t size;

            tsize = ((PyVarObject *)receiver)->ob_size;
            if (tsize < 0)
                tsize = -tsize;
            size = _PyObject_VAR_SIZE(tp, tsize);
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
            res = PyDict_GetItem((PyObject*)dict, stub->attrname);
        }
        if (res != NULL) {
            Py_INCREF(res);
            Py_XDECREF(descr);
            assert(!_PyErr_OCCURRED());
            return res;
        }
    }
    if (descr_is_method) {
        *method_found = 1;
        return descr;
    }
    if (f != NULL) {
        PyObject *res = f(descr, receiver, (PyObject*)tp);
        if (side_effects) *side_effects = 1;
        Py_DECREF(descr);
        return res;
    }
    if (UNLIKELY(descr == NULL)) {
        PyErr_Format(PyExc_AttributeError,
                     "'%.50s' object has no attribute '%U'",
                     tp->tp_name, stub->attrname);
        if (side_effects) *side_effects = 1;
        return NULL;
    }
    return descr;
}

static inline PyObject*
_PyJITAttrCache_FetchAndVerify(
        PyObject *receiver,
        PyJITAttrCache *ic,
        int *method_found,
        int *side_effects_unused) {
    int side_effects = 0;
    assert(!_PyErr_OCCURRED());
    PyObject *ret = _PyJITAttrCache_Fetch(receiver, ic, method_found, &side_effects);
    if (!side_effects) {
        assert(!_PyErr_OCCURRED());
        int tmp = 0;
        int *verify_method_found = method_found ? &tmp : NULL;
        PyObject *verify_ret = _PyJITAttrCache_FetchFallback(receiver, ic, verify_method_found, NULL);
        assert(verify_ret == ret);
        Py_XDECREF(verify_ret);
        if (method_found) {
            assert(*verify_method_found == *method_found);
        }
    }
    return ret;
}

/* This is the main entrypoint for attribute lookup */
PyObject* _Py_HOT_FUNCTION
_PyJITAttrCache_GetAttr(PyObject *receiver, PyJITAttrCache *ic) {
    return _PyJITAttrCache_Fetch(receiver, ic, NULL, NULL);
}

PyObject* _Py_HOT_FUNCTION
_PyJITAttrCache_GetAttrAndVerify(PyObject *receiver, PyJITAttrCache *ic) {
    return _PyJITAttrCache_FetchAndVerify(receiver, ic, NULL, NULL);

}

/* This is the main entrypoint for attribute lookup */
PyObject* _Py_HOT_FUNCTION
_PyJITAttrCache_GetMethod(PyObject *receiver, PyJITAttrCache *ic, int *method_found) {
    return _PyJITAttrCache_Fetch(receiver, ic, method_found, NULL);
}

PyObject* _Py_HOT_FUNCTION
_PyJITAttrCache_GetMethodAndVerify(PyObject *receiver, PyJITAttrCache *ic, int *method_found) {
    return _PyJITAttrCache_FetchAndVerify(receiver, ic, method_found, NULL);
}
