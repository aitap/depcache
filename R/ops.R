`%<-%` <- function(lhs, rhs) {
	symbol <- substitute(lhs)
	stopifnot(is.symbol(symbol))
	expr   <- substitute(rhs)
	frame  <- parent.frame()

	fun <- function(assignment) {
		if (!missing(assignment))
			stop(deparse(substitute(assignment)))

		do.cache(expr, frame, NULL, NULL)
	}

	makeActiveBinding(symbol, fun, frame)
	invisible(fun())
}
