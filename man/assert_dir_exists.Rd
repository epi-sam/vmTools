% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assertions.R
\name{assert_dir_exists}
\alias{assert_dir_exists}
\title{@title Assert a directory exists on disk}
\usage{
assert_dir_exists(x)
}
\description{
@param x [path] A directory path
}
\details{
@return [none] stop if assertion fails

 @examples
 assert_dir_exists(".") # OK
 assert_dir_exists("nonexistent") # Error
}
