#include <stdbool.h>
#include "Python.h"


PyObject *_PyTuple_FromInline(PyTupleInline *ti) {
    Py_ssize_t i;
    PyObject *v = PyTuple_New(ti->ti_length);
    if (v == NULL) {
        return NULL;
    }
    for (i = 0; i < ti->ti_length; i++) {
        PyObject *k = ti->ti_item[i];
        Py_INCREF(k);
        PyTuple_SET_ITEM(v, i, k);
    }
    return v;
}

Py_hash_t _PyTupleInline_Hash(PyTupleInline *v) {
    Py_uhash_t x;  /* Unsigned for defined overflow behavior. */
    Py_hash_t y;
    Py_ssize_t len = v->ti_length;
    PyObject **p;
    Py_uhash_t mult = _PyHASH_MULTIPLIER;
    x = 0x345678UL;
    p = v->ti_item;
    while (--len >= 0) {
        y = PyObject_Hash(*p++);
        if (y == -1)
            return -1;
        x = (x ^ y) * mult;
        /* the cast might truncate len; that doesn't change hash stability */
        mult += (Py_hash_t)(82520UL + len + len);
    }
    x += 97531UL;
    if (x == (Py_uhash_t)-1)
        x = -2;
    return x;
}

int _PyTupleInline_RichCompareBool(PyTupleInline *v, PyTupleInline *w, int op) {
    /* This matches the behavior of tuple richcompare */
    Py_ssize_t i;
    Py_ssize_t vlen = v->ti_length;
    Py_ssize_t wlen = w->ti_length;
    for (i = 0; i < vlen && i < wlen; i++) {
        int k = PyObject_RichCompareBool(v->ti_item[i], w->ti_item[i], Py_EQ);
        if (k < 0)
            return -1;
        if (!k)
            break;
    }
    if (i >= vlen || i >= wlen) {
        /* No more items to compare -- compare sizes */
        int cmp;
        switch (op) {
        case Py_LT: cmp = vlen <  wlen; break;
        case Py_LE: cmp = vlen <= wlen; break;
        case Py_EQ: cmp = vlen == wlen; break;
        case Py_NE: cmp = vlen != wlen; break;
        case Py_GT: cmp = vlen >  wlen; break;
        case Py_GE: cmp = vlen >= wlen; break;
        default: return -1; /* cannot happen */
        }
        return cmp ? 1 : 0;
    }
    if (op == Py_EQ)
        return 0;
    if (op == Py_NE)
        return 1;
    return PyObject_RichCompareBool(v->ti_item[i], w->ti_item[i], op);
}


int PyTupleInline_InitFromTuple(PyTupleInline *ti, char *mem, PyObject *src) {
    Py_ssize_t i;
    Py_ssize_t size = PyTuple_GET_SIZE(src);
    ti->ti_item = (PyObject**)mem;
    ti->ti_length = size;
    for (i = 0; i < size; i++) {
        PyObject *k = PyTuple_GET_ITEM(src, i);
        Py_INCREF(k);
        ti->ti_item[i] = k;
    }
    return size * sizeof(PyObject*);
}

void PyTupleInline_SetFromTuple(PyTupleInline *ti, PyObject *src) {
    Py_ssize_t i;
    Py_ssize_t size = PyTuple_GET_SIZE(src);
    assert(ti->ti_length == size);
    for (i = 0; i < size; i++) {
        PyObject *k = PyTuple_GET_ITEM(src, i);
        Py_INCREF(k);
        Py_SETREF(ti->ti_item[i], k);
    }
}

//--------

PyObject* _PyBytes_FromInline(PyBytesInline *bi) {
    return PyBytes_FromStringAndSize(bi->bi_val, bi->bi_length);
}

Py_hash_t _PyBytesInline_Hash(PyBytesInline *v) {
    return _Py_HashBytes(v->bi_val, v->bi_length);
}

int PyBytesInline_InitFromBytes(PyBytesInline *bi, char *mem, PyObject *src) {
    Py_ssize_t size = PyBytes_GET_SIZE(src);
    bi->bi_val = mem;
    bi->bi_length = size;
    memcpy(bi->bi_val, PyBytes_AS_STRING(src), size);
    return size;
}

void PyBytesInline_SetFromBytes(PyBytesInline *bi, PyObject *src) {
    assert(PyBytes_GET_SIZE(src) == bi->bi_length);
    memcpy(bi->bi_val, PyBytes_AS_STRING(src), PyBytes_GET_SIZE(src));
}

int _PyBytesInline_RichCompareBool(PyBytesInline *v, PyBytesInline *w, int op) {
    /* This matches the behavior of bytes richcompare */
    Py_ssize_t vlen = v->bi_length;
    Py_ssize_t wlen = w->bi_length;
    Py_ssize_t minlen = Py_MIN(vlen, wlen);
    int c;
    if (minlen > 0) {
        c = memcmp(v->bi_val, w->bi_val, minlen);
    } else {
        c = 0;
    }
    if (c == 0)
        c = (vlen < wlen) ? -1 : (vlen > wlen) ? 1 : 0;
    switch (op) {
    case Py_LT: return c < 0;
    case Py_LE: return c <= 0;
    case Py_GT: return c > 0;
    case Py_GE: return c >= 0;
    case Py_EQ: return c == 0;
    case Py_NE: return c != 0;
    }
    PyErr_BadArgument();
    return -1;
}
