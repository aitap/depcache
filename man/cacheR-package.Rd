\name{cacheR-package}
\alias{cacheR-package}
\alias{cacheR}
\docType{package}
\title{
\packageTitle{cacheR}
}
\description{
\packageDescription{cacheR}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{cacheR}
\packageIndices{cacheR}
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
\packageAuthor{cacheR}

Maintainer: \packageMaintainer{cacheR}
}
\references{
~~ Literature or other references for background information ~~
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{\dontrun{
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

  # make active bindings, tracking the lifetimes of everything?
  x1 %<-% do_1()
  x2 %<-% do_2(x1)
  x3 %<-% do_3(x2)

  x1 %<-% do_1_differently()
  # By knowing the names of the objects we're assigning, we can track
  # the fact that the source of x1 has changed, and note this for the
  # whole dependency graph.
  x3 %<-% do_3(x2) # x2 also recalculated
  # or:
  x3 # automatically recalculate x2, x3
  # ?
}}
