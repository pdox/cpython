#define IR_DWARF_RAX       0
#define IR_DWARF_RDX       1
#define IR_DWARF_RCX       2
#define IR_DWARF_RBX       3
#define IR_DWARF_RSI       4
#define IR_DWARF_RDI       5
#define IR_DWARF_RBP       6
#define IR_DWARF_RSP       7
#define IR_DWARF_R8        8
#define IR_DWARF_R9        9
#define IR_DWARF_R10       10
#define IR_DWARF_R11       11
#define IR_DWARF_R12       12
#define IR_DWARF_R13       13
#define IR_DWARF_R14       14
#define IR_DWARF_R15       15

/* REGISTER: Value is in register given by dwarf_index */
/* REGISTER+OFFSET: Value is %reg + offset */
/* MEMORY: Value is in [%reg + offset] */
/* CONSTANT: Value is stored in 'offset' */

#define IR_LOCATION_REGISTER                 1
#define IR_LOCATION_REGISTER_PLUS_OFFSET     2
#define IR_LOCATION_MEMORY                   3
#define IR_LOCATION_CONSTANT                 4

IR_PROTOTYPE(ir_stackmap_value)
struct ir_stackmap_value_t {
    uint8_t location;    /* One of the IR_LOCATION_* fields */
    uint8_t dwarf_index; /* One of the IR_DWARF_* fields */
    intptr_t offset;     /* Offset or constant value */
};

IR_PROTOTYPE(ir_stackmap_info)
struct ir_stackmap_info_t {
    void *code_address;
    void *user_data;
    size_t num_values;
    ir_stackmap_value_t values[1];
};

/* Index of stackmaps sorted by code address */
IR_PROTOTYPE(ir_stackmap_index)
struct ir_stackmap_index_t {
    size_t insertion_index;
    size_t num_stackmaps;
    ir_stackmap_info stackmaps[1];
};

/* Create new index */
ir_stackmap_index ir_stackmap_index_new(size_t num_stackmaps);

/* Allocate a new stackmap info structure. A single instance of this
   corresponds to a single patchpoint or stackmap instruction. */
ir_stackmap_info ir_stackmap_info_new(size_t num_values);

/* Add stackmap to an index. The index takes ownership of 'info' */
void ir_stackmap_index_add(ir_stackmap_index index, ir_stackmap_info info);

/* Find the stackmap corresponding to the given user_data. */
ir_stackmap_info ir_stackmap_index_lookup(ir_stackmap_index index, void *user_data);

/* Free the index and all associated ir_stackmap_info. */
void ir_stackmap_index_free(ir_stackmap_index index);

/* Read the live contents of a value, using the stackmap info and register save area.
   This is only really useful inside a patchpoint handler.

   NOTE: This implementation is incomplete. It doesn't take into account the value's
         type or width. It always assumes the value being retrieved has the size of
         a pointer. It also assumes void* and uintptr_t have the same width.
  */
static inline
uintptr_t ir_stackmap_read_value(ir_stackmap_info info, size_t value_index, void **save_area) {
    assert(value_index < info->num_values);
    ir_stackmap_value v = &info->values[value_index];
    switch (v->location) {
    case IR_LOCATION_REGISTER:
        return (uintptr_t)save_area[v->dwarf_index];
    case IR_LOCATION_REGISTER_PLUS_OFFSET:
        return ((uintptr_t)save_area[v->dwarf_index]) + v->offset;
    case IR_LOCATION_MEMORY: {
        uintptr_t addr = ((uintptr_t)save_area[v->dwarf_index]) + v->offset;
        return *((uintptr_t*)addr);
    }
    case IR_LOCATION_CONSTANT:
        return (uintptr_t)(v->offset);
    }
    abort(); /* Unhandled case */
}
