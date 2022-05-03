library(cacheR)

x <- '\xC5\xD8'
Encoding(x) <- 'latin1'
stopifnot(
	identical(x, y <- cacheR:::fixup(x)), # must be semantically equal
	!identical(charToRaw(x), charToRaw(y)) # despite representation
)

# This is a bit lazy, but does check a lot. Most of our fixes are for
# language objects (which we have plenty of here) and recursive objects
# (which functions consist of).
e <- loadNamespace('cacheR')
for (n in names(e))
	if (!isTRUE(ret <- all.equal(e[[n]], cacheR:::fixup(e[[n]]))))
		stop(
			sQuote(n), ' semantically changes after fixup, ',
			'all.equal() = ', ret
		)

# TODO: test as many kinds of *SXP as possible for all.equal
