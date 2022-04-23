#include "cacheR.h"

#include <R_ext/Rdynload.h>

static R_CallMethodDef call_methods[] = {
	{"hash",       (DL_FUNC)&hash,       2},
	{NULL, NULL, 0}
};

void R_init_cacheR(DllInfo *info) {
   R_registerRoutines(info, NULL, call_methods, NULL, NULL);
}
