.depcache.version <- as.character(packageVersion('depcache'))

.defaults <- list(
	'0.1' = list(
		compress = TRUE,
		local.only = TRUE,
		format.version = 2
	)
)

depcache.options <- function(
	defaults = getOption('depcache.version', .depcache.version),
	dir = getOption('depcache.dir', '.cache'),
	skip = getOption('depcache.skip', NULL),
	compress, local.only, format.version
) {
	defaults <- .defaults[[match.arg(defaults, '0.1')]]

	if (missing(compress)) compress <- getOption(
		'depcache.compress', defaults$compress
	)
	if (missing(local.only)) local.only <- getOption(
		'depcache.local.only', defaults$local.only
	)
	if (missing(format.version)) format.version <- getOption(
		'depcache.format.version',
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
