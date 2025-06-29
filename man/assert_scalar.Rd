% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assertions.R
\name{assert_scalar}
\alias{assert_scalar}
\title{Assert an element is atomic and length 1}
\usage{
assert_scalar(x)
}
\arguments{
\item{x}{[any] Element to check}
}
\value{
[none] stop if assertion fails
}
\description{
Assert an element is atomic and length 1
}
\examples{
vmTools:::assert_scalar("A") # OK
try(vmTools:::assert_scalar(1:2)) # Error
}
\seealso{
Other assertions: 
\code{\link{assert_dir_exists}()},
\code{\link{assert_named_list}()},
\code{\link{assert_scalar_not_empty}()},
\code{\link{assert_type}()}
}
\concept{assertions}
