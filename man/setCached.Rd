\name{\%->\%}
\alias{\%->\%}
\alias{\%<-\%}
\alias{setCached}
\title{Cache-tracking assignment}
\description{
  Cache expression values and automatically recalculate them when their
  dependencies change
}
\usage{
  symbol \%<-\% expr
  expr \%->\% symbol
  setCached(symbol, expr, skip = NULL, extra = NULL, \dots)
}
\arguments{
  \item{symbol}{
    A variable name to associate with the expression, unquoted.
  }
  \item{expr}{
    The expression to cache, taking dependencies into account.
  }
  \item{skip}{
    A character vector containing variable names to omit from the
    automatically-determinted dependencies of an expression. Use it when
    the automatic dependency process is overly eager and results in
    unexpected recalculation.
  }
  \item{extra}{
    An unquoted expression to be considered an extra part of the state,
    in addition to the automatically determined dependencies. Will be
    evaluated every time the variable is accessed to determine whether
    it should be recalculated.
  }
  \item{\dots}{
    Additional settings, see \code{\link{cacheR.options}}.
  }
}
\details{
  \strong{TODO}: mention options (link to `cache`), describe how it
  works (link to makeActiveBinding). Make sure to mention that it only
  works on variable names and what happens when you overwrite it.
}
\value{
  Returns the value of \code{expr}, invisibly. Called for the side
  effect of creating an active binding with a name specified by
  \code{symbol}.
}

\seealso{\code{\link{cache}}, \code{\link{makeActiveBinding}}}
\examples{
  \dontshow{
    .cacheR.dir <- file.path(tempdir(), 'cacheR-example')
    .op <- options(cacheR.dir = .cacheR.dir)
  }
  a <- 1
  # will evaluate the expression first
  x \%<-\% { message('side-effects'); a + 1 }
  x # 2
  # will reuse cached value
  {
    message('side-effects')
    a + 1
    # even if written a bit differently
  } \%->\% y
  y # 2
  a <- -1
  # will evaluate the expression again
  x # 0
  # will load the new cached value
  y # 0
  \dontshow{
    unlink(.cacheR.dir, recursive = TRUE)
    options(.op)
  }
}
\keyword{utilities}
