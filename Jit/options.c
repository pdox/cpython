#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <Jit/macros.h>
#include <Jit/options.h>

int Py_JITFlag;
int Py_JITDebugFlag;
char *Py_JITDebugFunc;
char *Py_JITDebugFile;
int Py_JITEvalBreaks;
int Py_JITPatchpoint;
int Py_JITNoExc;
int Py_JITNoSuper;
int Py_JITAttrCache = 1;

#ifdef NDEBUG
int Py_JITAsserts = 0;
#else
int Py_JITAsserts = 1;
#endif

void PyJIT_InitOptions(const char *config) {
    /* 'config' is a comma-separated list of configuration options */
    const char *p = config;
    while (p[0]) {
       char *option;
       const char *end;
       end = strchr(p, ',');
       if (end != NULL) {
           option = strndup(p, end - p);
           p = end + 1;
       } else {
           option = strdup(p);
           p += strlen(p);
       }
#define OPTION(s) \
        (strcmp(option, s) == 0)
#define OPTION_WITH_VALUE(s) \
        (value = option + strlen(s) + 1, strncmp(option, s "=", strlen(s) + 1) == 0)

        const char *value;
        if (OPTION("on")) {
            Py_JITFlag = 1;
        } else if (OPTION("off")) {
            Py_JITFlag = 0;
        } else if (OPTION("llvm")) {
            Py_JITFlag = 2;
        } else if (OPTION("debug")) {
            Py_JITDebugFlag = 1;
        } else if (OPTION_WITH_VALUE("debug")) {
            Py_JITDebugFlag = atoi(value);
        } else if (OPTION_WITH_VALUE("func")) {
            Py_JITDebugFunc = strdup(value);
        } else if (OPTION_WITH_VALUE("file")) {
            Py_JITDebugFile = strdup(value);
        } else if (OPTION("evalbreaks")) {
            Py_JITEvalBreaks = 1;
        } else if (OPTION("patchpoint")) {
            Py_JITPatchpoint = 1;
        } else if (OPTION("noexc")) {
            Py_JITNoExc = 1;
        } else if (OPTION("nosuper")) {
            Py_JITNoSuper = 1;
        } else if (OPTION_WITH_VALUE("attrcache")) {
            if (strcmp(value, "off") == 0) {
                Py_JITAttrCache = 0;
            } else if (strcmp(value, "on") == 0) {
                Py_JITAttrCache = 1;
            } else if (strcmp(value, "verify") == 0) {
                Py_JITAttrCache = 2;
            } else {
                char err[256];
                sprintf(err, "Unrecognized PYTHONJIT attrcache option: %s", value);
                PyJIT_FatalError(err);
            }
        } else if (OPTION("asserts")) {
            Py_JITAsserts = 1;
        } else if (OPTION("noasserts")) {
            Py_JITAsserts = 0;
        } else {
            char err[256];
            sprintf(err, "Unrecognized PYTHONJIT option: %s", option);
            PyJIT_FatalError(err);
        }
#undef OPTION
#undef OPTION_WITH_VALUE
        free(option);
    }
}

static char *_default_options = NULL;
const char *
PyJIT_ReadDefaultOptions(void) {
    if (_default_options != NULL)
        return _default_options;

    FILE *fp = fopen("/tmp/PYTHONJIT_DEFAULT", "r");
    if (!fp)
        return "";
    fseek(fp, 0, SEEK_END);
    long sz = ftell(fp);
    if (sz < 0) {
        PyJIT_FatalError("Error reading /tmp/PYTHONJIT_DEFAULT");
    }
    fseek(fp, 0, SEEK_SET);
    char *buf = (char*)malloc(sz + 1);
    size_t rc = fread(buf, 1, (size_t)sz, fp);
    if (rc != (size_t)sz) {
        PyJIT_FatalError("Unexpected short count reading /tmp/PYTHONJIT_DEFAULT");
    }
    /* Strip trailing newlines/spaces */
    while (sz > 0 && isspace(buf[sz-1])) sz--;
    buf[sz] = 0;
    fclose(fp);
    _default_options = buf;
    return _default_options;
}

