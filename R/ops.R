`%<-%` <- function(lhs, rhs) {
	symbol <- substitute(lhs)
	if (!is.symbol(symbol)) stop(
		'Cache-tracking assignment only works on plain variable names'
	)
	symbol <- as.character(symbol)
	expr   <- substitute(rhs)
	frame  <- parent.frame()

	fun <- function(assignment) {
		# TODO: we should be able to cache in RAM here instead of
		# reading from disk every time
		if (missing(assignment)) {
			do.cache(expr, frame, NULL, NULL)
		} else {
			warning(
				'Overwriting a cache-tracked object (', symbol, ') ',
				'with an untracked value', call. = FALSE
			)
			rm(list = symbol, envir = frame)
			assign(symbol, assignment, envir = frame)
		}
	}

	# makeActiveBinding stops if the variable already exists
	if (exists(symbol, frame, inherits = FALSE))
		rm(list = symbol, envir = frame)
	makeActiveBinding(symbol, fun, frame)
	invisible(fun())
}

`%->%` <- function(expr, symbol)
	eval(substitute(symbol %<-% expr), parent.frame())
