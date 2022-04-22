do.cache <- function(
	expr, frame,
	cache.dir = '.cache',
	version = 2, compress = TRUE
) {
	dir.create(cache.dir, showWarnings = FALSE)
	stopifnot(dir.exists(cache.dir))
	rds <- paste0(file.path(cache.dir, hash(deparse(expr))), '.rds')
	tryCatch(
		suppressWarnings(readRDS(rds)),
		error = function(e) {
			val <- eval(expr, frame)
			saveRDS(val, rds, version = version, compress = compress)
			val
		}
	)
}

cache <- function(expr, ...)
	do.cache(substitute(expr), parent.frame(), ...)

`%<-%` <- function(symbol, expr) {
	symbol <- substitute(symbol)
	if (!is.symbol(symbol)) stop(
		"LHS of smart assignment expected to be a symbol; instead got ",
		paste(deparse(symbol), collapse = ' ')
	)
	expr   <- substitute(expr)
	frame <- parent.frame()
	assign(
		as.character(symbol),
		ret <- do.cache(expr, frame),
		envir = parent.frame()
	)
	invisible(ret)
}

`%->%` <- function(expr, symbol) eval(substitute(symbol %<-% expr), parent.frame())
