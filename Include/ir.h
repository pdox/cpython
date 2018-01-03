#ifndef Py_IR_H
#define Py_IR_H
#ifdef __cplusplus
extern "C" {
#endif

/* This defines an intermediate representation (IR) which is used for representing a
   complete Python function (after bytecode has been lowered to machine-like instructions).

   This is roughly equivalent to LLVM bitcode, with the notable difference
   that composite Python operations without flow control (e.g. INCREF, DECREF, POP, STACKADJ)
   are represented as single instructions. This makes it easier to analyze those operations
   before converting to machine code.

   This representation starts out in non-SSA form, but is later converted to SSA form.
 */

#include <assert.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

/* TODO: Condition on Py_DEBUG */
#define IR_DEBUG   1

#define IR_PROTOTYPE(name) \
    struct name ## _t; \
    typedef struct name ## _t  name ## _t; \
    typedef struct name ## _t  *name;

/* Needed by ir_context */
IR_PROTOTYPE(ir_func)

#include "ir_macros.h"
#include "ir_context.h"
#include "ir_type_system.h"
#include "ir_func.h"
#include "ir_instructions.h"
#include "ir_libjit.h"
#include "ir_llvm.h"

#ifdef __cplusplus
}
#endif
#endif /* !Py_IR_H */
