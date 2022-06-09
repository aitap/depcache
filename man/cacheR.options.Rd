\name{cacheR.options}
\alias{cacheR.options}
\title{Caching options}
\description{
  Control how the dependencies are gathered and hashed to locate the
  determine the file name to load the cached object from.
}
\usage{
cacheR.options(
  defaults = getOption("cacheR.version", .cacheR.version),
  dir = getOption("cacheR.dir", ".cache"),
  skip = getOption("cacheR.skip", NULL),
  compress, local.only, format.version
)
}
\arguments{
  \item{defaults}{
    A string containing the version of \pkg{cacheR} to get other
    defaults from. If not set, takes the value from the
    \verb{cacheR.version} option (see \code{\link{options}}), falling
    back to the current version of the package.

    To make the caching more reproducible against package updates, call
    \code{\link{options}(cacheR.version = "...")} once at the top of
    your scripts.
  }
  \item{dir}{
    The directory to store the cache files inside. Defaults to the
    \verb{cacheR.dir} option, or \file{.cache} if unset.
  }
  \item{skip}{
    A character vector of variables to omit from automatically-gathered
    dependencies. Variables carrying unintended or unimportant state,
    which would otherwise interfere with obtaining a reproducible hash,
    should be listed here. Potential examples include \pkg{parallel}
    cluster objects, or anything else that might differ in hash between
    executions of the script.

    Defaults to the \verb{cacheR.skip} option, or \code{NULL} if unset.
  }
  \item{compress}{
    Passed as the \code{compress} option to \code{\link{saveRDS}} when
    saving the cached objects. Defaults to the \verb{cacheR.compress}
    option, or \code{TRUE} in \pkg{cacheR} version 0.1.
  }
  \item{local.only}{
    If \code{TRUE}, only variables available in the same environment
    where the caching function has been called from are considered as
    dependencies; parent environments are ignored. Typically, this means
    taking local variables as parts of the hash that determines the file
    name, but not loaded packages or \code{\link{attach}}ed datasets.

    Defaults to the \verb{cacheR.local.only} option, or \code{TRUE} in
    \pkg{cacheR} version 0.1.
  }
  \item{format.version}{
    Passed as the \code{version} argument to \code{\link{saveRDS}} and
    also used when serialising any objects to hash them. Only versions
    \verb{2} and \verb{3} are supported. Defaults to the
    \verb{cacheR.format.version} option, or \verb{2} in \pkg{cacheR}
    version 0.1.
  }
}
\details{
  This function shouldn't be normally called by the user (except,
  perhaps, to verify the parameters about to be passed to the caching
  functions), but it is automatically invoked on every call to
  \code{\link{cache}}, \code{\link{setCached}}, or the use of
  cache-tracking assignment operators \code{\link{\%<-\%}} and
  \code{\link{\%->\%}}. Any additional options passed to the functions
  as \code{\dots} are handled here, and so are the global
  \code{\link{options}}.
}
\value{
  A list containing the settings to be used by the caching system.
  \item{dir}{The directory used for storage of the cache files.}
  \item{compress}{Passed to \code{\link{saveRDS}}.}
  \item{skip}{
    Variables to skip when hashing the dependencies of the
    expressions.
  }
  \item{local.only}{Whether to ignore non-local dependencies.}
  \item{format.version}{
    Passed to \code{\link{saveRDS}} as the \code{version} argument. Also
    determines the format version when serialising the variables to hash
    them.
  }
}

\seealso{\code{\link{cache}}, \code{\link{setCached}}}
\examples{
  # The output is affected by the user's use of options(...) and the
  # current version of the package
  print(cacheR.options())
}
\keyword{utilities}
