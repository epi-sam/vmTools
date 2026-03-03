# started: 2025 Aug 29 14:00:06
# purpose: switch function that strictly enforces choices and fails helpfully

#' Wrapper for `switch` that errors helpfully if no match is found
#'
#' Options for handling empty string ("") and no match cases
#' - no case matching is not allowed by default (hence 'strict')
#'
#' @param EXPR [string] expression to evaluate
#' @param ... named options to match against EXPR
#' @param .empty [any: default NULL] value to return if EXPR is an empty string ("")
#' @param .default [any: default NULL] value to return if no match is found - NULL allows no defaults (strict)
#'
#' @returns [any] the value of the matched option
#' @keywords internal
#'
switch_strict <- function(EXPR, ..., .empty = NULL, .default = NULL) {
   # Handle empty string case first
   if (EXPR == "" && !is.null(.empty)) {
      return(.empty)
   }

   # find option names for the switch for messaging
   opts <- substitute(list(...))
   if (length(opts) > 1) {
      opts <- opts[-1]  # Remove the 'list' part
      opt_names <- names(opts)
   } else {
      opts <- list()
      opt_names <- character(0)
   }
   # Debug the options
   # message("Available options: ", toString(opt_names), "\n")
   # message("EXPR matches 'prop': ", EXPR == "prop", "\n")

   res <- switch(EXPR, ...)

   if (is.null(res)) {

      if (!is.null(.default)) {
         return(.default)
      }
      # pass name of function calling the switch to error handler
      call       <- sys.call(-1)
      caller     <- if (!is.null(call)) as.character(call[[1]]) else NULL
      # caller_msg <- if (!is.null(caller)) sprintf(" in '%s()'", caller) else ""
      stop(
         sprintf(
            # "'%s' is not a valid option%s. Valid options are: %s",
            "\nInvalid option: %s\nValid options:  %s",
            EXPR,
            # caller_msg,
            toString(rev(opt_names)) # in the context switch_strict is called, reversing the options seems more informative
         )
         , call. = FALSE # suppresses switch_strict in error message - not sure if I want this
      )
   }
   res
}
