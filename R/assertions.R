# purpose: assertions for package internal functioning - not currently tested to work in a broader context
# - assertions `stop()` if conditions are violated

#' Types of check functions:
#' 1. assert_x - stop if conditions are unmet
#' 2. validate_x - warn if conditions are unmet
#'               - Return TRUE/FALSE
#'
#' Assert an element is atomic and length 1
#'
#' @param x [any] Element to check
#'
#' @return [none] stop if assertion fails
#'
#' @examples
#' assert_scalar("A") # OK
#' assert_scalar(1:2) # Error
assert_scalar = function(x){
   x_name <- deparse(substitute(x))
   if(!(is.atomic(x) && length(x) == 1L)){
      stop(x_name, " must be atomic and length 1L")
   }
}

#' Assert x is a scalar, and not empty in some way
#'
#' @param x [any] some object to check
#'
#' @return [none] stop if assertion fails
#'
#' @examples
#' assert_scalar_not_empty("A") # OK
#' assert_scalar_not_empty(Inf) # Error - Inf considered non-meaningful - see validate_not_empty
assert_scalar_not_empty = function(x){
   assert_scalar(x)
   if(!isTRUE(validate_not_empty(x))){
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
#' @examples
#' assert_type("A", "character") # OK
#' assert_type(1, "integer") # Error - need 1L
assert_type = function(x, type){
   assert_scalar(type)
   stopifnot(is.character(type))
   if(!inherits(x, type)){
      x_name <- deparse(substitute(x))
      stop(x_name, " must be of type ", type)
   }
}

#'  Assert an object is a list with named elements
#'
#'  Stops if:
#'   - x is not a list
#'   - x is a data.table
#'   - x has no names
#'   - x has any NA names
#'   - x has any zero-length names
#'   - x has any whitespace-only names
#'
#'  @param x [list] List to check
#'
#'  @return [none] stop if assertion fails
#'
#'  @examples
#'  assert_named_list(list(a = 1, b = 2)) # OK
#'  assert_named_list(data.table(a = 1, b = 2)) # Error
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

#'  @title Assert a directory exists on disk
#'
#'  @param x [path] A directory path
#'
#'  @return [none] stop if assertion fails
#'
#'  @examples
#'  assert_dir_exists(".") # OK
#'  assert_dir_exists("nonexistent") # Error
assert_dir_exists = function(x){
   assert_scalar(x)
   root <- suppressWarnings(normalizePath(x))
   if(!dir.exists(root)) stop("root does not exist: ", x)
}

