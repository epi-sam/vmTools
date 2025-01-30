# purpose: validations for package internal functioning - not currently tested to work in a broader context
# - validations return TRUE/FALSE
# - none should be exported

#' Validate an object is not length 0, empty, blank etc.
#'
#' Designed to also catch missing args when called inside a function.
#'
#' @param x [any] some argument to check
#'
#' @return [lgl] FALSE if empty in some way, TRUE otherwise
#'
#' @examples
#' validate_not_empty(1) # TRUE
#' validate_not_empty(NULL) # FALSE
validate_not_empty = function(x) {
   # Check for missing arguments
   if (missing(x)) return(FALSE)
   # Check for zero-length vectors or empty lists
   if (length(x) == 0) return(FALSE)
   # Check for NULL, NA
   if (is.null(x) || is.na(x)) return(FALSE)
   # Check if the argument is only whitespace
   if (is.character(x) && trimws(x) == "") return(FALSE)
   # Check if the argument is a numeric value that is not finite (NaN, Inf, -Inf)
   if (is.numeric(x) && !is.finite(x)) return(FALSE)
   # Check for empty data frames
   if (is.data.frame(x) && nrow(x) == 0 && ncol(x) == 0) return(FALSE)
   return(TRUE)
}

#'  Validate whether a directory exists
#'
#'  @param x [path] A directory path
#'  @param verbose [lgl] message to std_out?
#'
#'  @return [lgl] TRUE if directory exists, FALSE otherwise
#'
#'  @examples
#'  validate_dir_exists(".") # TRUE
#'  validate_dir_exists("nonexistent") # FALSE
validate_dir_exists = function(x, verbose = TRUE){
   assert_scalar(x)
   root <- suppressWarnings(normalizePath(x))
   if(!dir.exists(root)) {
      if(verbose) message("root does not exist: ", x)
      return(invisible(FALSE))
   } else {
      return(invisible(TRUE))
   }
}

#'  Determine if an object is an error
#'
#'  @param x [obj] some R object
#'
#'  @return [lgl] TRUE / FALSE
#'
#'  @examples
#'  is_an_error(simpleError("error")) # TRUE
#'  is_an_error(message("error)) # FALSE
is_an_error = function(x) {
   return(inherits(x, "simpleError") || inherits(x, "try-error"))
}
