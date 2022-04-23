#include "cacheR.h"

#include <assert.h>

/* FNV-1a hash: http://www.isthe.com/chongo/tech/comp/fnv */

static const uint64_t fnv1a64_basis = 14695981039346656037ULL;
static uint64_t do_fnv1a64(uint64_t hash, const unsigned char * buf, size_t sz) {
	for (const unsigned char * end = buf + sz; buf != end; ++buf) {
		hash ^= *buf;
		hash *= 1099511628211ULL; /* FNV_prime */
	}
	return hash;
}

static void outchar(R_outpstream_t st, int ch) {
	unsigned char c = ch;
	assert(ch >= 0);
	*(uint64_t*)st->data = do_fnv1a64(*(uint64_t*)st->data, &c, sizeof c);
}

static void outbytes(R_outpstream_t st, void * buf, int sz) {
	assert(sz >= 0);
	*(uint64_t*)st->data = do_fnv1a64(*(uint64_t*)st->data, buf, sz);
}

/* FIXME: can we just pass NULL, NULL instad of phook, pdata? */
static SEXP phook(SEXP obj, SEXP pdata) {
	return R_NilValue;
}

SEXP hash(SEXP value, SEXP sver) {
	struct R_outpstream_st stream;
	uint64_t ret = fnv1a64_basis;
	int version = asInteger(sver);
	SEXP sret = PROTECT(allocVector(RAWSXP, sizeof(uint64_t)));
	R_InitOutPStream(
		&stream, &ret,
		R_pstream_xdr_format, version,
		outchar, outbytes,
		phook, R_NilValue
	);
	R_Serialize(value, &stream);
	*(uint64_t*)RAW(sret) = ret;
	UNPROTECT(1);
	return sret;
};

void fnv1a64(
	char const * const * text, const int * ntext, unsigned char hash[sizeof(uint64_t)]
) {
	uint64_t ret = fnv1a64_basis;
	for (char const * const * end = text + *ntext; text != end; ++text)
		/* NOTE: do hash the NUL terminator */
		ret = do_fnv1a64(ret, *text, strlen(*text)+1);

	*((uint64_t*)hash) = ret;


}
