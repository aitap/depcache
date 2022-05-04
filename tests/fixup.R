library(cacheR)

# this is supposed to handle different "unknown" (native) encodings
x <- '\xC5\xD8'
Encoding(x) <- 'latin1'
stopifnot(
	identical(x, y <- cacheR:::fixup(x)), # must be semantically equal
	!identical(charToRaw(x), charToRaw(y)) # despite representation
)

check.all.equal <- function(x) for (n in names(x)) { message(n)
	if (!isTRUE(ret <- all.equal(x[[n]], ff <- cacheR:::fixup(x[[n]]))))
		stop(
			sQuote(n), ' semantically changes after fixup\n',
			'all.equal() says: ', ret, '\n',
			'deparse(', n, '): ', paste(deparse(x[[n]]), collapse = '\n'), '\n',
			'deparse(fixup(', n, ')): ', paste(deparse(ff), collapse = '\n'), '\n'
		)
}

# This is a bit lazy, but does check a lot. Most of our fixes are for
# language objects (which we have plenty of here) and recursive objects
# (which functions consist of).
check.all.equal(loadNamespace('cacheR'))
# Try to cover all kinds of objects, except atomic vectors (we only
# touch character vectors, covered above) and stuff we're unlikely to
# ever meet in R code.
check.all.equal(list(
	NULL = NULL,
	symbol = as.symbol('hello'),
	pairlist = pairlist(a = 1),
	closure = function() NULL,
	environment = environment(),
	language = quote(a + b),
	special = substitute,
	builtin = `+`,
	expression = expression(haha),
	list = list(NULL),
	S4 = getClass('MethodDefinition')
))
