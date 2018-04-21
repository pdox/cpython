#!/bin/bash -eu

PYTHONJIT=${PYTHONJIT:-}

# Python's test suite has no reliable way to propagate environmental variables.
# There is an existing interpreter flag propagation mechanism, but even that
# fails to propagate to subprocesses in dozens of tests. This ugly hack ensures
# that the desired PYTHONJIT flag will be followed.
PYTHONJIT_DEFAULT_PATH="/tmp/PYTHONJIT_DEFAULT"
if [ -f "$PYTHONJIT_DEFAULT_PATH" ]; then
    echo "Error: $PYTHONJIT_DEFAULT_PATH exists."
    echo 'This may indicate that another instance of runtests.sh is active.'
    echo 'It is not currently safe to run multiple instances of runtests.sh'
    echo 'simultaneously on the same machine. If no other tests are running, '
    echo 'then the file is stale and safe to delete.'
    exit 1
fi
echo $PYTHONJIT > "$PYTHONJIT_DEFAULT_PATH"
function cleanup() {
    rm -f "$PYTHONJIT_DEFAULT_PATH"
}
trap cleanup EXIT

./python Tools/scripts/run_tests.py \
    -x test_distutils -x test_ssl -x test_urllib -x test_urllib2 -x test_urllib2_localhost -x test_urllib2net \
    -x test_urllibnet -x test_normalization -x test_urllib2_localnet -x test_sys_settrace \
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
    -x test_unpack_ex \
    -x test_lib2to3

# test_distutils
# test_ssl
# test_urllib
# test_urllib2
# test_urllib2_localhost
# test_urllib2net
# test_urllibnet
# test_normalization
# test_urllib2_localnet
#   These tests attempt to connect to hosts over the internet.

# test_sys_settrace
#   (Presumably) This is trying to enable tracing while inside a function.
#   The JIT doesn't emit any tracing guard.

# test_doctest
# test_gdb
# test_pdb
#   These tests fail because they expect the GDB backtrace to contain EvalFrameDefault.

# test_multiprocessing_*
# test_concurrent_futures
#   These tests likely pass, but take too long due to slow compilation.

# test_collections
# test_fstring
#   Explosive memory use

# test_import
#   Gets stuck compiling longlist.py (literal list with 65000 elements)

# test_json
#   Recursion depth too high for libjit (libjit has excessively high C stack usage due to spilling)

# test_unpack_ex
#   Runs out of memory for compilation

# test_lib2to3
#   Test is flakey for unknown reason, especially when running in parallel with other tests.
