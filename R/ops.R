`%<-%` <- function(symbol, expr) {
	symbol <- substitute(symbol)
	if (!is.symbol(symbol)) stop(
		'Cache-tracking assignment only works on plain variable names'
	)
	symbol <- as.character(symbol)
	expr   <- substitute(expr)
	frame  <- parent.frame()

	hash <- get.filename(
		expr, frame, NULL, NULL,
		# FIXME: duplication
		cache.dir = getOption('cacheR.dir', '.cache'),
		version = getOption('cacheR.ser.version', 2)
	)
	val <- do.cache(expr, frame, NULL, NULL)

	fun <- function(assignment) {
		if (missing(assignment)) {
			new.hash <- get.filename(
				expr, frame, NULL, NULL,
				# FIXME: duplication
				cache.dir = getOption('cacheR.dir', '.cache'),
				version = getOption('cacheR.ser.version', 2)
			)
			if (new.hash != hash) {
				val <<- do.cache(expr, frame, NULL, NULL)
				hash <<- new.hash
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

`%->%` <- function(expr, symbol)
	eval(substitute(symbol %<-% expr), parent.frame())
