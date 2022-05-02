\name{cacheR-package}
\alias{cacheR-package}
\alias{cacheR}
\docType{package}
\title{\packageTitle{cacheR}}
\description{
  \packageDescription{cacheR}
}
\details{
  \packageIndices{cacheR}
  \strong{TODO}: An overview of how to use the package, including the most important functions
}
\author{\packageAuthor{cacheR}}
\keyword{package}
\seealso{\code{\link{cache}}, \code{\link{\%<-\%}}}
\examples{\dontrun{#TODO: real examples
  x <- cache(long_running_operation()) # done once
  x <- cache(long_running_operation()) # loaded from cache
  y <- cache(f(x)) # again, done once
  x <- something_else
  y <- cache(f(x)) # re-calculated

  # cache() can hash its argument and all its dependencies, but it can't
  # easily associate a value with its source code.
  x <- cache(do_x())
  y <- cache(do_y(x))
  z <- cache(do_z(y))
  x <- cache(update_x())
  # x has changed, but y hasn't. If we re-run the whole script, this
  # won't matter.
}}
