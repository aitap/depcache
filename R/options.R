.cacheR.version <- packageVersion('cacheR')

cacheR.options <- function(
	dir = getOption('cacheR.dir', '.cache'),
	compress = getOption('cacheR.compress', TRUE),
	format.version = getOption('cacheR.format.version', 2),
	skip = NULL, local.only = FALSE, version = .cacheR.version
) NULL
