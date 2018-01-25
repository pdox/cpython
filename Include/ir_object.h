IR_PROTOTYPE(ir_object)
typedef void (*compiler_free_callback_t)(ir_object);

/* An object is a handle to the compiled code of a function and all associated metadata.
   It does not depend on the ir_context or ir_func. (these can be destroyed after compilation)

   The values stored by ir_object_t are opaque, except to the compilation backends.
 */
struct ir_object_t {
    void *compiler_data; /* Compiler-specific data */
    compiler_free_callback_t compiler_free_callback; /* Callback to free compiler-specific data */
    void *entrypoint;
    ir_stackmap_index stackmap_index;
};

ir_object ir_object_new(void);
void ir_object_free(ir_object obj);
