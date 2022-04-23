#include "cacheR.h"

#include <R_ext/Rdynload.h>

static R_CallMethodDef call_methods[] = {
	{"weakref",    (DL_FUNC)&weakref,    4},
	{"weakrefkey", (DL_FUNC)&weakrefkey, 1},
	{"weakrefval", (DL_FUNC)&weakrefval, 1},
	{"env_addr",   (DL_FUNC)&env_addr,   1},
	{"hash",       (DL_FUNC)&hash,       2},
	{NULL, NULL, 0}
};

void R_init_cacheR(DllInfo *info) {
   R_registerRoutines(info, NULL, call_methods, NULL, NULL);
}
