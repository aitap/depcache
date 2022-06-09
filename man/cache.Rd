\name{cache}
\alias{cache}
\title{Evaluate an expression and cache its results}
\description{
  This function extracts all dependencies of an R expression, hashes
  them together with the expression itself and either loads the
  already-existing file, or evaluates the expression and stores the
  result in that file.
}
\usage{
  cache(expr, skip = NULL, extra = NULL, \dots)
}
\arguments{
  \item{expr}{
    An expression to evaluate or load from cache, unquoted.
  }
  \item{skip}{
    A character vector of symbol names to exclude from the list of
    automatically-determined dependencies. This may be useful when a
    symbol encountered in the expression doesn't signify a variable in
    the evaluation frame (e.g. non-standard evaluation when plotting
    with \pkg{lattice}), or when the variable is being assigned to as
    part of the expression.
  }
  \item{extra}{
    Any R value that should be considered part of the state deciding
    whether the expression should be re-computed. For example, if
    \code{expr} reads a file, consider using \code{\link{file.mtime}} or
    \code{\link{md5sum}} to check for changes in it.
  }
  \item{\dots}{
    Additional options, see \code{\link{cacheR.options}}.
  }
}
\details{
  Currently, the hash is obtained by means of serialisation. In order to
  make computationally same values have same hashes on a wide range of R
  versions, the following steps were taken: \itemize{
    \item When computing the hash of the serialized data (only the XDR
    format version 2 or 3 is supported), the first 14 bytes containing
    the header (including the version of R that serialized the data) are
    ignored.

    \item To accomodate the bytecode-related differences between
    versions of R, every function is \dQuote{rebuilt} from its body
    before hashing, forcing R to discard the bytecode from the copy of
    the function before it's hashed.

    \item Strings are converted to UTF-8 before hashing.

    \item All this is done recursively.
  }

  The exact algorithm used and the way hash is obtained are
  implementation details and may eventually change, though not without a
  good reason.

  \strong{FIXME}: \itemize{
    \item Environments can't be easily fixed up, so they will probably
    have to be ignored.
  }
}
\value{
  The result of evaluating \code{expr}, either directly, or loaded from
  cache.
}
\references{
  FNV-1a hash: \url{http://www.isthe.com/chongo/tech/comp/fnv}
}

\seealso{\code{\link{\%<-\%}}}
\examples{
  \dontshow{
    .cacheR.dir <- file.path(tempdir(), 'cacheR-example')
    .op <- options(cacheR.dir = .cacheR.dir)
  }
  a <- 1
  # will evaluate the expression the first time
  cache({message('side effects'); a + 1}) # 2
  # saved value of the expression will be used
  cache({
    message('side effects')
    # even if written a bit differently
    a + 1
  }) # 2
  a <- -1
  # expression evaluated again because dependencies changed
  cache({message('side effects'); a + 1}) # 0
  \dontshow{
    unlink(.cacheR.dir, recursive = TRUE)
    options(.op)
  }
}
\keyword{utilities}
