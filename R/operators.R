`%<-%` <- function(symbol, expr) {
	symbol <- substitute(symbol)
	if (!is.symbol(symbol)) stop(
		"LHS of smart assignment expected to be a symbol; instead got ",
		paste(deparse(symbol), collapse = ' ')
	)
	expr  <- substitute(expr)
	frame <- parent.frame()
	assign(
		as.character(symbol),
		ret <- do.cache(expr, frame),
		envir = parent.frame()
	)
	invisible(ret)
}

`%->%` <- function(expr, symbol) eval(substitute(symbol %<-% expr), parent.frame())
