do.cachetrack <- function(symbol, expr, frame, skip, extra, opts) {
	if (!is.symbol(symbol)) stop(
		'Cache-tracking assignment only works on plain variable names'
	)
	symbol <- as.character(symbol)

	path <- get.filename(expr, frame, skip, extra, opts)
	val <- do.cache(expr, frame, skip, extra, opts)

	fun <- function(assignment) {
		if (missing(assignment)) {
			new.path <- get.filename(expr, frame, skip, extra, opts)
			if (new.path != path) {
				val <<- do.cache(expr, frame, skip, extra, opts)
				path <<- new.path
			}
			val
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

`%<-%` <- function(symbol, expr) do.cachetrack(
	substitute(symbol), substitute(expr), parent.frame(),
	NULL, quote(NULL), cacheR.options()
)

`%->%` <- function(expr, symbol) do.cachetrack(
	substitute(symbol), substitute(expr), parent.frame(),
	NULL, quote(NULL), cacheR.options()
)

setCached <- function(symbol, expr, skip = NULL, extra = NULL, ...)
	do.cachetrack(
		substitute(symbol), substitute(expr), parent.frame(),
		skip, substitute(extra), cacheR.options(...)
	)
