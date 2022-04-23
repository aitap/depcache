.debug <- function(expr) if (getOption('cacheR.debug', FALSE)) expr

do.cache <- function(
	expr, frame, skip, extra,
	cache.dir = getOption('cacheR.dir', '.cache'),
	compress = getOption('cacheR.rds.compress', TRUE),
	version = getOption('cacheR.ser.version', 2)
) {
	dir.create(cache.dir, showWarnings = FALSE)
	stopifnot(dir.exists(cache.dir))

	deps <- depends(expr, frame, skip)

	filename <- hash(list(expr, deps, extra), version)
	rds <- paste0(file.path(cache.dir, filename), '.rds')

	.debug({
		message('Expr: ', deparse(expr))
		cat('Deps:\n'); print(sapply(deps, hash, version))
		message(rds, ' ', if (file.exists(rds)) 'found' else 'not found')
	})

	tryCatch(
		suppressWarnings(readRDS(rds)),
		error = function(e) {
			val <- eval(expr, frame)
			saveRDS(val, rds, version = version, compress = compress)
			val
		}
	)
}

cache <- function(expr, skip = NULL, extra = NULL, ...)
	do.cache(substitute(expr), parent.frame(), skip, extra, ...)
