Rscript.old <- Sys.getenv('RSCRIPT_OLD_VERSION')
Rscript.new <- Sys.getenv('RSCRIPT_NEW_VERSION')
if (!(nzchar(Rscript.old) && nzchar(Rscript.new))) {
	message('Skipping multi-version developer test')
	q('no')
}

# R cluster protocol works on the serialization format. Old versions of
# R don't speak workspace format version 3, but new versions don't mind
# the older, non-default version.
trace(serialize, quote(version <- 2), at = 1, print = FALSE)

library(cacheR)
path <- dirname(path.package('cacheR'))
hash.all <- function(x, v) lapply(setNames(nm = names(x)), function(n)
	cacheR:::hash(cacheR:::fixup(x[[n]]), v))

library(parallel)
cl <- structure(c(
	makePSOCKcluster(1, rscript = Rscript.old),
	# NOTE: old R can't run new R as a cluster worker
	makePSOCKcluster(1, rscript = Rscript.new)
), class = c('SOCKcluster', 'cluster'))

# check that R versions differ and are in the correct order
Rversions <- clusterEvalQ(cl, getRversion())
stopifnot(Rversions[[1]] < Rversions[[2]])

clusterExport(cl, c('path', 'hash.all'))
clusterEvalQ(cl, {
	loadNamespace('codetools')
	loadNamespace('methods')
	.libPaths(path)
	# FIXME: old R can't load package installed by new R
	library(cacheR)
})

# fixed-up objects should result in the same hash
res <- clusterEvalQ(cl, hash.all(loadNamespace('cacheR'), 2))
stopifnot(all.equal(res$old, res$new))
