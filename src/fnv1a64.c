#include <string.h>

#include "cacheR.h"

/* FNV-1a hash: http://www.isthe.com/chongo/tech/comp/fnv */

static uint64_t do_fnv1a64(uint64_t hash, const unsigned char * buf, size_t sz) {
	for (const unsigned char * end = buf + sz; buf != end; ++buf) {
		hash ^= *buf;
		hash *= 1099511628211ULL; /* FNV_prime */
	}
	return hash;
}

void fnv1a64(
	char const * const * text, const int * ntext, unsigned char hash[sizeof(uint64_t)]
) {
	uint64_t ret = 14695981039346656037ULL; /* FNV-1 offset_basis */
	for (char const * const * end = text + *ntext; text != end; ++text)
		/* NOTE: do hash the NUL terminator */
		ret = do_fnv1a64(ret, *text, strlen(*text)+1);
	*((uint64_t*)hash) = ret;
}
