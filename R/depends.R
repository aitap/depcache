# TODO: test the output of this function to make sure "skip" is taken
# into account, none of the undesirables are picked up

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

	values <- mget(
		symbols, frame, ifnotfound = list(NULL), inherits = TRUE
	)
	# skip missing values, primitives, non-local functions
	ret <- Filter(
		function(n)
			!is.null(values[[n]]) && !is.primitive(values[[n]]) &&
			(!is.function(values[[n]]) || exists(n, envir = frame, inherits = FALSE)),
		symbols
	)
	values[ret]
}
