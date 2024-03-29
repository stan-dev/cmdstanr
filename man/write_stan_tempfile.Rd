% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/example.R
\name{write_stan_tempfile}
\alias{write_stan_tempfile}
\title{Write Stan code to a temporary file}
\usage{
write_stan_tempfile(code, dir = tempdir())
}
\arguments{
\item{code}{(character vector) The Stan code to write to the file. This can
be a character vector of length one (a string) containing the entire Stan
program or a character vector with each element containing one line of the
Stan program.}

\item{dir}{(string) An optional path to the directory where the file will be
written. If omitted, a global option \code{cmdstanr_write_stan_file_dir} is
used. If the global options is not set, \link[base:tempfile]{temporary directory}
is used.}
}
\description{
This function is deprecated. Please use \code{\link[=write_stan_file]{write_stan_file()}} instead.
}
\keyword{internal}
