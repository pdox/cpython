typedef struct PyTupleInline {
    PyObject** ti_item;
    Py_ssize_t ti_length;
} PyTupleInline;

#define PyTupleInline_Hash(v)  (_PyTupleInline_Hash(&(v)))
#define PyTupleInline_RichCompareBool(v, w, op) (_PyTupleInline_RichCompareBool(&(v), &(w), op))
#define PyTuple_FromInline(v) (_PyTuple_FromInline(&(v)))
#define PyTupleInline_GET_SIZE(v) ((v).ti_length)
#define PyTupleInline_GET_ITEM(v, i) ((v).ti_item[i])


Py_hash_t _PyTupleInline_Hash(PyTupleInline *v);
int _PyTupleInline_RichCompareBool(PyTupleInline *v, PyTupleInline *w, int op);

int PyTupleInline_InitFromTuple(PyTupleInline *ti, char *mem, PyObject *src);
void PyTupleInline_SetFromTuple(PyTupleInline *ti, PyObject *src);

PyObject *_PyTuple_FromInline(PyTupleInline *ti);


typedef struct PyBytesInline {
    char *bi_val;
    Py_ssize_t bi_length;
} PyBytesInline;


#define PyBytes_FromInline(v) (_PyBytes_FromInline(&(v)))
#define PyBytesInline_RichCompareBool(v,w,op) (_PyBytesInline_RichCompareBool(&(v), &(w), (op)))
#define PyBytesInline_Hash(v) (_PyBytesInline_Hash(&(v)))
#define PyBytesInline_GET_SIZE(v) ((v).bi_length)
#define PyBytesInline_AS_STRING(v) ((v).bi_val)
#define PyBytesInline_AsStringAndSize(v, valp, lenp) do { \
    *(valp) = (v).bi_val; \
    *(lenp) = (v).bi_length; \
} while (0)

PyObject* _PyBytes_FromInline(PyBytesInline *bi);
int _PyBytesInline_RichCompareBool(PyBytesInline *v, PyBytesInline *w, int op);
Py_hash_t _PyBytesInline_Hash(PyBytesInline *v);

int PyBytesInline_InitFromBytes(PyBytesInline *bi, char *mem, PyObject *src);
void PyBytesInline_SetFromBytes(PyBytesInline *bi, PyObject *src);
