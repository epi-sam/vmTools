# purpose: assertions for package internal functioning - not currently tested to
# work in a broader context
# - assertions `stop()` if conditions are violated
# - none should be exported

#' Assert an element is atomic and length 1
#'
#' @param x [any] Element to check
#'
#' @return [none] stop if assertion fails
#'
#' @family assertions
#' @keywords internal
assert_scalar <- function(x) {
   x_name <- deparse(substitute(x))
   if (!(is.atomic(x) && length(x) == 1L)) {
      stop(x_name, " must be atomic and length 1L")
   }
}

#' Assert x is a scalar, and not empty in some way
#'
#' @param x [any] some object to check
#'
#' @return [none] stop if assertion fails
#'
#' @family assertions
#' @keywords internal
assert_scalar_not_empty = function(x) {
   assert_scalar(x)
   if (!isTRUE(validate_not_empty(x))) {
      x_name <- deparse(substitute(x))
      stop(x_name, " is empty in some way.")
   }
}

#' Assert an object is a scalar of a certain type
#'
#' @param x [any] Object to check
#' @param type [chr] Type to check against
#'
#' @return [none] stop if assertion fails
#'
#' @family assertions
#' @keywords internal
assert_type = function(x, type) {
   assert_scalar(type)
   stopifnot(is.character(type))
   if (!inherits(x, type)) {
      x_name <- deparse(substitute(x))
      stop(x_name, " must be of type ", type)
   }
}


#' Assert an object is a list with named elements
#'
#' Stops if:
#' \itemize{
#'  \item{x is not a list}
#'  \item{x is a data.frame}
#'  \item{x has no names}
#'  \item{x has any NA names}
#'  \item{x has any zero-length names}
#'  \item{x has any whitespace-only names}
#' }
#'
#' @param x [list] List to check
#'
#' @return [none] stop if assertion fails
#'
#' @family assertions
#' @keywords internal
assert_named_list = function(x){
   if(!is.null(x)){
      err_msg <- "x must be a named list, not vector or data.frame (list names may not be whitespace)"
      if(!is.list(x))               stop(err_msg)
      if(is.data.frame(x))          stop(err_msg)
      if(is.null(names(x)))         stop(err_msg)
      if(any(is.na(names(x))))      stop(err_msg)
      names(x) <- trimws(names(x))
      if(any(nchar(names(x)) == 0)) stop(err_msg)
   }
}


#' Assert a directory exists on disk
#'
#' @param x [chr] A directory path
#'
#' @return [none] stop if assertion fails
#'
#' @family assertions
#' @keywords internal
assert_dir_exists = function(x){
   if(is.null(x)) stop("x is NULL")
   assert_scalar(x)
   root <- suppressWarnings(normalizePath(x))
   if(!dir.exists(root)) stop("root does not exist: ", x)
}

#' Assert all elements of x are in y
#'
#' @param x [vector] some vector
#' @param y [vector] some vector
#'
#' @return [none] stop if any elements of x are not in y
#'
#' @family assertions
#' @keywords internal
assert_x_in_y <- function(x, y){
   stopifnot(is.vector(x))
   stopifnot(is.vector(y))
   absent <- setdiff(x, y)
   if (length(absent) > 0) {
      x_name <- deparse(substitute(x))
      y_name <- deparse(substitute(y))
      stop(sprintf("required in %s but absent in %s: %s", x_name, y_name, toString(absent)))
   }
}
