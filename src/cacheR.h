#include <stdint.h>

#include <R.h>
#include <Rinternals.h>

/* env.c */
SEXP env_addr(SEXP env);

/* fnv1a64.c */
SEXP hash(SEXP val, SEXP sver);

/* weakref.c */
SEXP weakref(SEXP key, SEXP val, SEXP fin, SEXP onexit);
SEXP weakrefkey(SEXP weakref);
SEXP weakrefval(SEXP weakref);
