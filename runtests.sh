#!/bin/bash

./python Tools/scripts/run_tests.py \
    -x test_distutils -x test_ssl -x test_urllib -x test_urllib2 -x test_urllib2_localhost -x test_urllib2net \
    -x test_urllibnet -x test_normalization -x test_urllib2_localnet -x test_sys_settrace -x test_tracemalloc \
    -x test_doctest -x test_gdb -x test_pdb \
    -x test_multiprocessing_forkserver \
    -x test_multiprocessing_spawn \
    -x test_multiprocessing_fork \
    -x test_multiprocessing_main_handling \
    -x test_concurrent_futures \
    -x test_collections \
    -x test_fstring \
    -x test_import \
    -x test_json \
    -x test_unpack_ex

# test_multiprocessing_*
# test_concurrent_futures (known to pass, but takes a long time)
#   Takes too long and uses too much memory due to slow compilation

# test_collections
# test_fstring
#   Explosive memory use

# test_import
#   Gets stuck compiling longlist.py (literal list with 65000 elements)

# test_json
#   Recursion depth too high for libjit (libjit has excessively high C stack usage due to spilling)

# test_unpack_ex
#   Runs out of memory for compilation
