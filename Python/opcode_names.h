static const char *opcode_names[256] = {
    "unknown_opcode_0",
    "POP_TOP",
    "ROT_TWO",
    "ROT_THREE",
    "DUP_TOP",
    "DUP_TOP_TWO",
    "unknown_opcode_6",
    "unknown_opcode_7",
    "unknown_opcode_8",
    "NOP",
    "UNARY_POSITIVE",
    "UNARY_NEGATIVE",
    "UNARY_NOT",
    "unknown_opcode_13",
    "unknown_opcode_14",
    "UNARY_INVERT",
    "BINARY_MATRIX_MULTIPLY",
    "INPLACE_MATRIX_MULTIPLY",
    "unknown_opcode_18",
    "BINARY_POWER",
    "BINARY_MULTIPLY",
    "unknown_opcode_21",
    "BINARY_MODULO",
    "BINARY_ADD",
    "BINARY_SUBTRACT",
    "BINARY_SUBSCR",
    "BINARY_FLOOR_DIVIDE",
    "BINARY_TRUE_DIVIDE",
    "INPLACE_FLOOR_DIVIDE",
    "INPLACE_TRUE_DIVIDE",
    "unknown_opcode_30",
    "unknown_opcode_31",
    "unknown_opcode_32",
    "unknown_opcode_33",
    "unknown_opcode_34",
    "unknown_opcode_35",
    "unknown_opcode_36",
    "unknown_opcode_37",
    "unknown_opcode_38",
    "unknown_opcode_39",
    "unknown_opcode_40",
    "unknown_opcode_41",
    "unknown_opcode_42",
    "unknown_opcode_43",
    "unknown_opcode_44",
    "unknown_opcode_45",
    "unknown_opcode_46",
    "unknown_opcode_47",
    "unknown_opcode_48",
    "unknown_opcode_49",
    "GET_AITER",
    "GET_ANEXT",
    "BEFORE_ASYNC_WITH",
    "unknown_opcode_53",
    "unknown_opcode_54",
    "INPLACE_ADD",
    "INPLACE_SUBTRACT",
    "INPLACE_MULTIPLY",
    "unknown_opcode_58",
    "INPLACE_MODULO",
    "STORE_SUBSCR",
    "DELETE_SUBSCR",
    "BINARY_LSHIFT",
    "BINARY_RSHIFT",
    "BINARY_AND",
    "BINARY_XOR",
    "BINARY_OR",
    "INPLACE_POWER",
    "GET_ITER",
    "GET_YIELD_FROM_ITER",
    "PRINT_EXPR",
    "LOAD_BUILD_CLASS",
    "YIELD_FROM",
    "GET_AWAITABLE",
    "unknown_opcode_74",
    "INPLACE_LSHIFT",
    "INPLACE_RSHIFT",
    "INPLACE_AND",
    "INPLACE_XOR",
    "INPLACE_OR",
    "BREAK_LOOP",
    "WITH_CLEANUP_START",
    "WITH_CLEANUP_FINISH",
    "RETURN_VALUE",
    "IMPORT_STAR",
    "SETUP_ANNOTATIONS",
    "YIELD_VALUE",
    "POP_BLOCK",
    "END_FINALLY",
    "POP_EXCEPT",
    "STORE_NAME",
    "DELETE_NAME",
    "UNPACK_SEQUENCE",
    "FOR_ITER",
    "UNPACK_EX",
    "STORE_ATTR",
    "DELETE_ATTR",
    "STORE_GLOBAL",
    "DELETE_GLOBAL",
    "unknown_opcode_99",
    "LOAD_CONST",
    "LOAD_NAME",
    "BUILD_TUPLE",
    "BUILD_LIST",
    "BUILD_SET",
    "BUILD_MAP",
    "LOAD_ATTR",
    "COMPARE_OP",
    "IMPORT_NAME",
    "IMPORT_FROM",
    "JUMP_FORWARD",
    "JUMP_IF_FALSE_OR_POP",
    "JUMP_IF_TRUE_OR_POP",
    "JUMP_ABSOLUTE",
    "POP_JUMP_IF_FALSE",
    "POP_JUMP_IF_TRUE",
    "LOAD_GLOBAL",
    "unknown_opcode_117",
    "unknown_opcode_118",
    "CONTINUE_LOOP",
    "SETUP_LOOP",
    "SETUP_EXCEPT",
    "SETUP_FINALLY",
    "unknown_opcode_123",
    "LOAD_FAST",
    "STORE_FAST",
    "DELETE_FAST",
    "STORE_ANNOTATION",
    "unknown_opcode_128",
    "unknown_opcode_129",
    "RAISE_VARARGS",
    "CALL_FUNCTION",
    "MAKE_FUNCTION",
    "BUILD_SLICE",
    "unknown_opcode_134",
    "LOAD_CLOSURE",
    "LOAD_DEREF",
    "STORE_DEREF",
    "DELETE_DEREF",
    "unknown_opcode_139",
    "unknown_opcode_140",
    "CALL_FUNCTION_KW",
    "CALL_FUNCTION_EX",
    "SETUP_WITH",
    "EXTENDED_ARG",
    "LIST_APPEND",
    "SET_ADD",
    "MAP_ADD",
    "LOAD_CLASSDEREF",
    "BUILD_LIST_UNPACK",
    "BUILD_MAP_UNPACK",
    "BUILD_MAP_UNPACK_WITH_CALL",
    "BUILD_TUPLE_UNPACK",
    "BUILD_SET_UNPACK",
    "SETUP_ASYNC_WITH",
    "FORMAT_VALUE",
    "BUILD_CONST_KEY_MAP",
    "BUILD_STRING",
    "BUILD_TUPLE_UNPACK_WITH_CALL",
    "unknown_opcode_159",
    "LOAD_METHOD",
    "CALL_METHOD",
    "unknown_opcode_162",
    "unknown_opcode_163",
    "unknown_opcode_164",
    "unknown_opcode_165",
    "unknown_opcode_166",
    "unknown_opcode_167",
    "unknown_opcode_168",
    "unknown_opcode_169",
    "unknown_opcode_170",
    "unknown_opcode_171",
    "unknown_opcode_172",
    "unknown_opcode_173",
    "unknown_opcode_174",
    "unknown_opcode_175",
    "unknown_opcode_176",
    "unknown_opcode_177",
    "unknown_opcode_178",
    "unknown_opcode_179",
    "unknown_opcode_180",
    "unknown_opcode_181",
    "unknown_opcode_182",
    "unknown_opcode_183",
    "unknown_opcode_184",
    "unknown_opcode_185",
    "unknown_opcode_186",
    "unknown_opcode_187",
    "unknown_opcode_188",
    "unknown_opcode_189",
    "unknown_opcode_190",
    "unknown_opcode_191",
    "unknown_opcode_192",
    "unknown_opcode_193",
    "unknown_opcode_194",
    "unknown_opcode_195",
    "unknown_opcode_196",
    "unknown_opcode_197",
    "unknown_opcode_198",
    "unknown_opcode_199",
    "unknown_opcode_200",
    "unknown_opcode_201",
    "unknown_opcode_202",
    "unknown_opcode_203",
    "unknown_opcode_204",
    "unknown_opcode_205",
    "unknown_opcode_206",
    "unknown_opcode_207",
    "unknown_opcode_208",
    "unknown_opcode_209",
    "unknown_opcode_210",
    "unknown_opcode_211",
    "unknown_opcode_212",
    "unknown_opcode_213",
    "unknown_opcode_214",
    "unknown_opcode_215",
    "unknown_opcode_216",
    "unknown_opcode_217",
    "unknown_opcode_218",
    "unknown_opcode_219",
    "unknown_opcode_220",
    "unknown_opcode_221",
    "unknown_opcode_222",
    "unknown_opcode_223",
    "unknown_opcode_224",
    "unknown_opcode_225",
    "unknown_opcode_226",
    "unknown_opcode_227",
    "unknown_opcode_228",
    "unknown_opcode_229",
    "unknown_opcode_230",
    "unknown_opcode_231",
    "unknown_opcode_232",
    "unknown_opcode_233",
    "unknown_opcode_234",
    "unknown_opcode_235",
    "unknown_opcode_236",
    "unknown_opcode_237",
    "unknown_opcode_238",
    "unknown_opcode_239",
    "unknown_opcode_240",
    "unknown_opcode_241",
    "unknown_opcode_242",
    "unknown_opcode_243",
    "unknown_opcode_244",
    "unknown_opcode_245",
    "unknown_opcode_246",
    "unknown_opcode_247",
    "unknown_opcode_248",
    "unknown_opcode_249",
    "unknown_opcode_250",
    "unknown_opcode_251",
    "unknown_opcode_252",
    "unknown_opcode_253",
    "unknown_opcode_254",
    "unknown_opcode_255"
};
