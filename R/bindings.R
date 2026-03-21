#' Lock select bindings in the global environment
#'
#' Safely tries to lock the specified object bindings in the given environment.
#'
#' @param objs (chr) a vector of object names to lock
#' @param env (environment) the environment to lock the bindings in
#'
#' @returns (chr) invisible vector of input objects, to allow easier un-locking
#' @keywords internal
#'
#' @family bindings
#' @keywords internal
#'
lock_some_bindings <- function(objs, env){
   checkmate::assert_character(objs)
   for(x in objs){
      try({if(exists(x, envir = env) && !bindingIsLocked(x, env)) lockBinding(x, env)})
   }
   return(invisible(objs))
}