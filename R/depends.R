# TODO: test the output of this function to make sure "skip" is taken
# into account, none of the primitives are picked up

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
	symbols <- setdiff(symbols, skip)
	# primitives won't change, so ignore them; also, skip missing variables
	Filter(
		function(x) !is.null(x) && !is.primitive(x),
		mget(
			symbols, frame, ifnotfound = list(NULL), inherits = TRUE
		)
	)
}
