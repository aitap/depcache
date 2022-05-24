.cacheR.version <- packageVersion('cacheR')

.defaults <- list(
	'0.1' = list(
		compress = TRUE,
		local.only = FALSE,
		format.version = 2
	)
)

cacheR.options <- function(
	defaults = getOption('cacheR.version', .cacheR.version),
	dir = getOption('cacheR.dir', '.cache'),
	skip = getOption('cacheR.skip', NULL),
	compress, local.only, format.version
) {
	defaults <- .defaults[[match.arg(defaults, '0.1')]]

	if (missing(compress)) compress <- getOption(
		'cacheR.compress', defaults$compress
	)
	if (missing(local.only)) local.only <- getOption(
		'cacheR.local.only', defaults$local.only
	)
	if (missing(format.version)) format.version <- getOption(
		'cacheR.format.version',
		defaults$format.version
	)

	stopifnot(
		is.character(dir), length(dir) == 1L,
		is.character(skip) || is.null(skip),
		length(compress) == 1L && !is.na(compress),
		is.logical(local.only) && length(local.only) == 1L,
		format.version %in% c(2,3)
	)

	list(
		dir = dir, compress = compress, skip = skip,
		local.only = local.only, format.version = format.version
	)
}
