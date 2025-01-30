#' lapply at some list dept
#'
#' Very simple replacement for purrr::map_depth to remove package dependency,
#' but not very robust.  Internal package use only in select cases.
#'
#' @param .x [list] List to apply function to
#' @param .depth [integer] Depth to apply function at
#' @param .f [function] Function to apply
#' @param ... [any] Additional arguments to pass to .f
#'
#' @return [list] List with function applied at target depth
#'
#' @examples
#' # intended use case - "pluck" a named list element out of each list
#' lapply_depth(list(A = list(one = 1, two = 2), B = list(one = 3, two = 4)), 1, `[[`, "one")
lapply_depth <- function(.x, .depth, .f, ...) {
   assert_named_list(.x)
   # browser()
   if (.depth == 0) {
      return(.f(.x, ...))  # Apply function directly if at target depth
   }
   if (!is.list(.x)) {
      return(.x)  # If .x is not a list, return as-is (handles mixed structures)
   }
   lapply(.x, function(elem) lapply_depth(elem, .depth - 1, .f, ...))
}
