#ifndef Py__THREADMODULE_H
#define Py__THREADMODULE_H

PyAPI_DATA(PyTypeObject) PyThreadId_Type;

/* Extract the raw thread id (unsigned long) */
PyAPI_FUNC(unsigned long) PyThreadId_Raw(PyObject *);

/* Checks the validity of the ident object. This is stable while holding the GIL. */
PyAPI_FUNC(int) PyThreadId_IsValid(PyObject *);

/* Mark the thread_id as invalid, so that we know not to use it as an argument
 * to system calls such as pthread_kill(). This happens just before a thread terminates.
 */
PyAPI_FUNC(void) PyThreadId_Invalidate(PyObject *);

#endif
