# TODO: test the output of this function for existence of file in
# various circumstances
get.filename <- function(expr, frame, skip, extra, opts) {
	deps <- dependencies(expr, frame, skip, opts)
	filename <- hash(
		list(fixup(expr), fixup(deps), eval(extra, frame)),
		opts$format.version
	)
	paste0(file.path(opts$dir, filename), '.rds')
}

do.cache <- function(expr, frame, skip, extra, opts) {
	dir.create(opts$dir, showWarnings = FALSE)
	stopifnot(dir.exists(opts$dir))

	rds <- get.filename(expr, frame, skip, extra, opts)

	tryCatch(
		suppressWarnings(readRDS(rds)),
		error = function(e) {
			val <- eval(expr, frame)
			saveRDS(
				val, rds,
				version = opts$format.version, compress = opts$compress
			)
			val
		}
	)
}

cache <- function(expr, skip = NULL, extra = NULL, ...) do.cache(
	substitute(expr), parent.frame(), skip, substitute(extra), cacheR.options(...)
)
