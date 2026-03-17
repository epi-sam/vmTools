# ---- Init -----------------------------------------------------------------

# required for tests/vignettes
# Known issue: https://github.com/rstudio/rmarkdown/issues/187
.datatable.aware = TRUE

# Create a package-level environment
.symlink_registry_env <- new.env(parent = emptyenv())

# ---- private accessors -------------------------------------------------------
#' @keywords internal
arrow_regex <- function() "->" # used by `ls -l` to show symlinks


# function factories to create the various functions needed for each symlink type

#' @keywords internal
make_symlink_suffix_fn <- function(is_singleton) {
    checkmate::assert_flag(is_singleton)
    if (is_singleton) {
        return(function(version_name) {""})
    } else {
        return(function(version_name) {sprintf("_%s", version_name)})
    }
}

#' @keywords internal
make_ls_regex_fn <- function(is_singleton, symlink_type) {
    checkmate::assert_flag(is_singleton)
    checkmate::assert_string(symlink_type)
    if (is_singleton) {
        return(function(version_name) {sprintf("^%s %s .*%s$", symlink_type, arrow_regex(), version_name)})
    } else {
        return(function(version_name) {sprintf("^%s_%s %s .*%s$", symlink_type, version_name, arrow_regex(), version_name)})
    }
}

#' @keywords internal
make_query_regex <- function(is_singleton, symlink_type) {
    checkmate::assert_flag(is_singleton)
    checkmate::assert_string(symlink_type)
    if (is_singleton) {
        return(sprintf("^%s$", symlink_type))
    } else {
        return(sprintf("^%s_", symlink_type))
    }
}

# ---- Package-level symlink registry environment ----------------------------
# Created when the package loads, persists for the session.
# CRAN-compliant: no global assignment, scoped to package namespace.
# Built-in bindings are locked so they cannot be overwritten by users.
.onLoad <- function(libname, pkgname){

    symlink_registry_builtins <- list(
        # Singleton-version marks
        # - These marks follow the Higlander rule: there can be only one per root directory
        best     = list(symlink_name = "best",   is_singleton = TRUE)
        , latest = list(symlink_name = "latest", is_singleton = TRUE)

        # Multiple-version marks
        # - These marks can be applied to multiple versions within the same root directory
        , keep   = list(symlink_name = "keep",   is_singleton = FALSE)
        , remove = list(symlink_name = "remove", is_singleton = FALSE)
    )

    for(idx in seq_along(symlink_registry_builtins)) {

        entry <- symlink_registry_builtins[[idx]]
        nm    <- names(symlink_registry_builtins)[idx]

        entry$symlink_suffix_fn <- make_symlink_suffix_fn(entry$is_singleton)
        entry$ls_regex_fn       <- make_ls_regex_fn(entry$is_singleton, nm)
        entry$extract_regex     <- make_query_regex(entry$is_singleton, nm)

        symlink_registry_builtins[[idx]] <- entry
    }

    for(nm in names(symlink_registry_builtins)){
        assign(nm, symlink_registry_builtins[[nm]], envir = .symlink_registry_env)
    }

    # Prevent user from overwriting built-in entries
    lock_some_bindings(
        objs  = names(symlink_registry_builtins)
        , env = .symlink_registry_env
    )

}


# ---- Public accessors -------------------------------------------------------

#' Get the full symlink registry
#'
#' Returns the named list of symlink types and their associated properties/functions.
#' Expected entries are:
#' - `is_singleton`: (logical) whether this symlink type is a singleton (only one per root)
#' - `symlink_suffix_fn`: (function) function to create symlink name based on version name (for non-singleton symlink types)
#' - `ls_regex_fn`: (function) function to find a symlink (by version_name for non-singletons) when running `ls` system calls
#' - `extract_regex`: (function) function to get the regex pattern to find the symlink type
#'
#' @returns (list) the full symlink registry as a named list
#' @export
#'
#' @examples
#' get_symlink_registry()
get_symlink_registry <- function() {
    return(as.list(.symlink_registry_env))
}

#' Get one entry from the symlink registry by name
#'
#' @param symlink_type (chr) name of the symlink type to retrieve, e.g. "best",
#'   "keep", "remove", "latest"
#'
#' @returns (named list) the registry entry for the specified symlink type. See
#'   get_symlink_registry() for expected entry structure.
#' @export
#'
#' @examples
#' get_symlink_registry_entry("best")
get_symlink_registry_entry <- function(symlink_type) {
    checkmate::assert_string(symlink_type)
    if (!exists(symlink_type, envir = .symlink_registry_env, inherits = FALSE)) {
        stop("Symlink type '", symlink_type, "' is not registered.")
    }
    return(get_symlink_registry()[[symlink_type]])
}


#' Register a new symlink type
#'
#' @param symlink_type (chr) name of the new symlink type to register. Must be
#'   unique (not already in registry).
#' @param is_singleton (lgl) whether this symlink type is a singleton (only one
#'   per root) or can be applied to multiple versions within the same root
#' @param verbose (lgl: default `TRUE`) control message verbosity when
#'   registering the new symlink type
#'
#' @returns (invisible NULL) called for side-effect of registering the new
#'   symlink type in the package-level registry environment
#' @export
#'
#' @examples
#' new_symlink_type("stalled", is_singleton = FALSE)
new_symlink_type <- function(
    symlink_type
    , is_singleton
    , verbose = TRUE
) {
    # assertions
    checkmate::assert_flag(is_singleton)
    checkmate::assert_string(symlink_type)
   if (tolower(symlink_type) %in% c("best", "latest", "keep", "remove")){
        stop("Cannot overwrite built-in symlink type (case-insensitive): '", tolower(symlink_type), "'")
    }
    # TODO - 2026 Mar 17 - probably want to allow user to re-write their own entries
    # if (exists(symlink_type, envir = .symlink_registry_env, inherits = FALSE)){
    #     stop("'", symlink_type, "' is already registered. Cannot overwrite.")
    # }

    # build symlink_registry entry
    new_entry <- list(
        is_singleton        = is_singleton
        , symlink_name      = symlink_type
        , symlink_suffix_fn = make_symlink_suffix_fn(is_singleton)
        , ls_regex_fn       = make_ls_regex_fn(is_singleton, symlink_type)
        , extract_regex     = make_query_regex(is_singleton, symlink_type)
    )

    assign(symlink_type, new_entry, envir = .symlink_registry_env)

    if (verbose) message("Registered new symlink type: '", symlink_type, "' (is_singleton = ", is_singleton, ")")
    invisible(NULL)
}
