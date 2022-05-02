# TODO: test the output of this function for existence of file in
# various circumstances
get.filename <- function(expr, frame, skip, extra, cache.dir, version) {
	deps <- dependencies(expr, frame, skip)
	filename <- hash(list(fixup(expr), fixup(deps), extra), version)
	paste0(file.path(cache.dir, filename), '.rds')
}

do.cache <- function(
	expr, frame, skip, extra,
	cache.dir = getOption('cacheR.dir', '.cache'),
	compress = getOption('cacheR.rds.compress', TRUE),
	version = getOption('cacheR.ser.version', 2)
) {
	stopifnot(as.integer(version) %in% 2L:3L)

	dir.create(cache.dir, showWarnings = FALSE)
	stopifnot(dir.exists(cache.dir))

	rds <- get.filename(expr, frame, skip, extra, cache.dir, version)

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
