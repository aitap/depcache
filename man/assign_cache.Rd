\name{\%->\%}
\alias{\%->\%}
\alias{\%<-\%}
\title{Cache-tracking assignment}
\description{
  Cache expression values and automatically recalculate them when their
  dependencies change
}
\usage{
  symbol \%<-\% expr
  expr \%->\% symbol
}
\arguments{
  \item{symbol}{
    A variable name to associate with the expression.
  }
  \item{expr}{
    The expression to cache and to take the dependencies into account
    of.
  }
}
\details{
  \strong{TODO}: mention options (link to `cache`), describe how it
  works (link to makeActiveBinding). Make sure to mention that it only
  works on variable names and what happens when you overwrite it.
}
\value{
  Invisibly, the value of the expression.
}

\seealso{\code{\link{cache}}, \code{\link{makeActiveBinding}}}
\examples{
  # TODO: real examples
}
\keyword{utilities}
