\name{depcache-package}
\alias{depcache-package}
\alias{depcache}
\docType{package}
\title{\packageTitle{depcache}}
\description{
  \packageDescription{depcache}
}
\details{
  \packageIndices{depcache}
  \strong{TODO}: An overview of how to use the package, including the most important functions
}
\author{\packageAuthor{depcache}}
\keyword{package}
\references{
  FNV-1a hash: \url{http://www.isthe.com/chongo/tech/comp/fnv/}
}
\seealso{\code{\link{cache}}, \code{\link{\%<-\%}}}
\examples{
  \dontshow{
    .depcache.dir <- file.path(tempdir(), 'depcache-example')
    .op <- options(depcache.dir = .depcache.dir)
  }
  a <- 1
  # will evaluate expression
  cache({message('evaluating expression'); a + 1}) # 2
  # will reuse cached value
  x \%<-\% { message('evaluating expression'); a + 1 } # 2
  a <- 2
  # will recalculate the value
  x # 3
  \dontshow{
    unlink(.depcache.dir, recursive = TRUE)
    options(.op)
  }
}
