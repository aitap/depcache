#include "cacheR.h"

SEXP env_addr(SEXP env) {
	if (!isEnvironment(env)) error("'env' must be an environment");
	SEXP raw_addr = PROTECT(allocVector(RAWSXP, sizeof(SEXP)));
	*(SEXP*)RAW(raw_addr) = env;
	UNPROTECT(1);
	return raw_addr;
}
