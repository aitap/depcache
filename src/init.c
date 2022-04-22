#include "cacheR.h"

#include <R_ext/Rdynload.h>

static R_CallMethodDef call_methods[] = {
	{"weakref",    (DL_FUNC)&weakref,    4},
	{"weakrefkey", (DL_FUNC)&weakrefkey, 1},
	{"weakrefval", (DL_FUNC)&weakrefval, 1},
	{"env_addr",   (DL_FUNC)&env_addr,   1},
	{NULL, NULL, 0}
};

static R_NativePrimitiveArgType fnv1a64_types[] = fnv1a64_TYPES;

static R_CMethodDef c_methods[] = {
	{"fnv1a64", (DL_FUNC)&fnv1a64, 3, fnv1a64_types},
	{NULL, NULL, 0}
};

void R_init_cacheR(DllInfo *info) {
   R_registerRoutines(info, c_methods, call_methods, NULL, NULL);
}
