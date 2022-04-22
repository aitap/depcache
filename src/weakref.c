#include "cacheR.h"

SEXP weakref(SEXP key, SEXP val, SEXP fin, SEXP onexit) {
	int on_exit = asLogical(onexit);
	if (on_exit == NA_LOGICAL) error("'onexit' must be TRUE or FALSE");
	return R_MakeWeakRef(key, val, fin, on_exit);
}

SEXP weakrefkey(SEXP weakref) {
	return R_WeakRefKey(weakref);
}

SEXP weakrefval(SEXP weakref) {
	return R_WeakRefValue(weakref);
}
