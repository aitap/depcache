# TODO: test the output of this function to make sure that all the right
# variables are taken into account, but "skip" is honoured

dependencies <- function(expr, frame, skip) {
	# any symbol in the expression can be a dependency
	symbols <- unname(unlist(walkCode(
		expr, makeCodeWalker(
			# handle generic calls and recurse
			call = function(call, w) lapply(call, walkCode, w = w),
			# handle leafs of the AST
			leaf = function(l, w) if (is.symbol(l)) as.character(l)
		)
	)))

	# except the ones we're explicitly told not to hash
	# calls unique for us too
	symbols <- setdiff(symbols, skip)

	# Reliable test for objects that don't exist. A value identical to a
	# freshly created environment won't be found in the environment of
	# a user expression that doesn't have access to here.
	notfound <- new.env(parent = emptyenv(), size = 0L)

	values <- mget(
		symbols, frame, ifnotfound = list(notfound), inherits = TRUE
	)
	# we pretend missing values don't exist (some NSE likely going on)
	# and don't waste time hashing primitives (they shouldn't change by
	# themselves, should they?)
	ret <- Filter(
		function(v) !identical(v, notfound) && !is.primitive(v),
		values
	)

	ret
}
