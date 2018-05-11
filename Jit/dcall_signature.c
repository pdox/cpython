#include "Jit/adt.h"
#include "Jit/dcall_signature.h"

/* CallSiteSig map */
static adt_hashmap csmap;

int csmap_key_eq(void *pa, void *pb) {
    PyJIT_DCall_Signature *a = *(PyJIT_DCall_Signature**)pa;
    PyJIT_DCall_Signature *b = *(PyJIT_DCall_Signature**)pb;
    if (a->hash != b->hash) return 0;
    if (a->argcount != b->argcount) return 0;
    if (a->kwnames && b->kwnames) {
        int k = PyObject_RichCompareBool(a->kwnames, b->kwnames, Py_EQ);
        assert(k >= 0);
        if (!k) return 0;
    } else if (a->kwnames || b->kwnames) {
        return 0;
    }
    return 1;
}

size_t csmap_key_hash(void *pa) {
    PyJIT_DCall_Signature *a = *(PyJIT_DCall_Signature**)pa;
    return (size_t)a->hash;
}

PyJIT_DCall_Signature *
PyJIT_DCall_GetSignature(int argcount, PyObject *kwnames) {
    assert(argcount >= 0);
    if (kwnames != NULL) {
        assert(PyTuple_CheckExact(kwnames));
        assert(PyTuple_GET_SIZE(kwnames) > 0);
        assert(argcount >= PyTuple_GET_SIZE(kwnames));
    }
    PyJIT_DCall_Signature key;
    key.hash = argcount + (kwnames ? PyObject_Hash(kwnames) : 0);
    key.argcount = argcount;
    key.kwnames = kwnames;

    if (csmap == NULL)
        csmap = adt_hashmap_new(sizeof(PyJIT_DCall_Signature*), sizeof(PyJIT_DCall_Signature*),
                                csmap_key_eq, csmap_key_hash);

    PyJIT_DCall_Signature *pkey = &key;
    PyJIT_DCall_Signature *ret;
    if (adt_hashmap_get(csmap, &pkey, &ret)) {
        return ret;
    }
    ret = PyMem_RawMalloc(sizeof(PyJIT_DCall_Signature));
    if (ret == NULL)
        return NULL;
    *ret = key;
    Py_XINCREF(ret->kwnames);
    adt_hashmap_insert(csmap, &ret, &ret);
    return ret;
}

