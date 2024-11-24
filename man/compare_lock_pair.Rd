% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/renvtools.r
\name{compare_lock_pair}
\alias{compare_lock_pair}
\title{Compare two lockfiles}
\usage{
compare_lock_pair(x)
}
\arguments{
\item{x}{A list of lists or list of 'rt_tibble' with two lockfiles. An output from \code{read_lock()}.}
}
\value{
A tibble
}
\description{
Compare two lockfiles
}
\examples{
paths <- c(
  file.path(system.file("extdata", package = "renvtools"), "renv-r4.4.1.lock"),
  file.path(system.file("extdata", package = "renvtools"), "renv-r4.3.2.lock")
)
l <- read_lock(paths, format = "list")
compare_lock_pair(l)
}
