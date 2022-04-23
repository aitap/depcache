do.cache <- function(
	expr, frame, skip, extra,
	cache.dir = getOption('cacheR.dir', '.cache'),
	compress = getOption('cacheR.rds.compress', TRUE),
	version = getOption('cacheR.ser.version', 2)
) {
	dir.create(cache.dir, showWarnings = FALSE)
	stopifnot(dir.exists(cache.dir))
	filename <- hash(
		list(expr, depends(expr, frame, skip), extra), version
	)
	rds <- paste0(file.path(cache.dir, filename), '.rds')
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
	do.cache(substitute(expr), parent.frame(), skip, ...)
