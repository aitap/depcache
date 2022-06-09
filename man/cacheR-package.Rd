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
\references{
  FNV-1a hash: \url{http://www.isthe.com/chongo/tech/comp/fnv}
}
\seealso{\code{\link{cache}}, \code{\link{\%<-\%}}}
\examples{
  \dontshow{
    .cacheR.dir <- file.path(tempdir(), 'cacheR-example')
    .op <- options(cacheR.dir = .cacheR.dir)
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
    unlink(.cacheR.dir, recursive = TRUE)
    options(.op)
  }
}
