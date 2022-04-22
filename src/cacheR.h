#include <stdint.h>

#include <R.h>
#include <Rinternals.h>

/* env.c */
SEXP env_addr(SEXP env);

/* fnv1a64.c */
void fnv1a64(
	char const * const * text, const int * ntext, unsigned char hash[sizeof(uint64_t)]
);
#define fnv1a64_TYPES { STRSXP, INTSXP, RAWSXP }

/* weakref.c */
SEXP weakref(SEXP key, SEXP val, SEXP fin, SEXP onexit);
SEXP weakrefkey(SEXP weakref);
SEXP weakrefval(SEXP weakref);
