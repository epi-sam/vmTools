% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_path.R
\name{clean_path}
\alias{clean_path}
\title{Wrapper utility for sanitizing file.path(...) output}
\usage{
clean_path(..., normalize = TRUE, mustWork = FALSE)
}
\arguments{
\item{...}{[chr] paths passed to file.path()}

\item{normalize}{[lgl] pass path to normalizePath()?}

\item{mustWork}{[lgl] passed to normalizePath()}
}
\value{
[chr] full file paths with consistent platform-specific structure
}
\description{
Wrapper utility for sanitizing file.path(...) output
}
\examples{
vmTools:::clean_path(tempdir(), "some/other/path/")
}
