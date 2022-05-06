Rscript.old <- Sys.getenv('RSCRIPT_OLD_VERSION')
Rscript.new <- Sys.getenv('RSCRIPT_NEW_VERSION')
if (!(nzchar(Rscript.old) && nzchar(Rscript.new))) {
	message('Skipping multi-version test')
	q('no')
}

library(cacheR)
path <- dirname(path.package('cacheR'))

library(parallel)
clus <- list(
	old = makeCluster(1, rscript = Rscript.old, homogeneous = FALSE),
	new = makeCluster(1, rscript = Rscript.new, homogeneous = FALSE)
)
for (cl in clus) {
	clusterExport(cl, 'path')
	clusterEvalQ(cl, {
		loadNamespace('codetools')
		loadNamespace('methods')
		.libPaths(path)
		library(cacheR)
	})
}
