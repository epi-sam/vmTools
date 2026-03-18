# ---- Init -----------------------------------------------------------------

# required for tests/vignettes
# Known issue: https://github.com/rstudio/rmarkdown/issues/187
.datatable.aware = TRUE

# Create a package-level environment
.symlink_registry <- new.env(parent = emptyenv())

# ---- private accessors -------------------------------------------------------
#' @keywords internal
arrow_regex <- function() "->" # used by `ls -l` to show symlinks


# function factories to create the various functions needed for each symlink type

#' Make a function to create the symlink name based on the version name
#'
#' The symlink name depends on whether the symlink type is a singleton or not.
#' For example, for a singleton "best" symlink type, the symlink name would be
#' "best". For a non-singleton "keep" symlink type, the symlink name would be
#' "keep_" + version_name (e.g. "keep_2025_01_01.01").
#'
#' @param is_singleton (logical) whether this symlink type is a singleton (only
#'   one per root)
#'
#' @keywords internal
make_symlink_suffix_fn <- function(is_singleton) {
    checkmate::assert_flag(is_singleton)
    if (is_singleton) {
        return(function(version_name) {""})
    } else {
        return(function(version_name) {sprintf("_%s", version_name)})
    }
}

#' Make a function to create a regex pattern to find the symlink type when
#' running `ls` system calls
#'
#' The pattern depends on whether the symlink type is a singleton or not. For
#' example, for a singleton "best" symlink type, the pattern would be "^best$".
#' For a non-singleton "keep" symlink type, the pattern would be "^keep_.* ->
#' .*version_name$".
#'
#' @param is_singleton (logical) whether this symlink type is a singleton (only
#'   one per root)
#' @param symlink_type (chr) the name of the symlink type, e.g. "best", "keep",
#'   "remove", "latest". Used to build the regex pattern for `ls` system calls.
#'
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

#' Make a regex pattern to find the symlink type when running `ls` system calls
#'
#' The pattern depends on whether the symlink type is a singleton or not. For
#' example, for a singleton "best" symlink type, the pattern would be "^best$".
#' For a non-singleton "keep" symlink type, the pattern would be "^keep_".
#'
#' @param is_singleton (logical) whether this symlink type is a singleton (only
#'   one per root)
#' @param symlink_type (chr) the name of the symlink type, e.g. "best", "keep",
#'   "remove", "latest".
#'
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
#' @keywords internal
get_symlink_registry <- function() {
    return(as.list(.symlink_registry))
}

#' Get one entry from the symlink registry by name
#'
#' @param symlink_type (chr) name of the symlink type to retrieve, e.g. "best",
#'   "keep", "remove", "latest"
#'
#' @returns (named list) the registry entry for the specified symlink type. See
#'   get_symlink_registry() for expected entry structure.
#' @keywords internal
get_symlink_registry_entry <- function(symlink_type) {
    checkmate::assert_string(symlink_type)
    if (!exists(symlink_type, envir = .symlink_registry, inherits = FALSE)) {
        stop("Symlink type '", symlink_type, "' is not registered.")
    }
    return(get_symlink_registry()[[symlink_type]])
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
        assign(nm, symlink_registry_builtins[[nm]], envir = .symlink_registry)
    }

    # Prevent user from overwriting built-in entries
    lock_some_bindings(
        objs  = names(symlink_registry_builtins)
        , env = .symlink_registry
    )

}


# ---- Public accessors -------------------------------------------------------


#' Register a new symlink type
#'
#' @param symlink_type (chr) name of the new symlink type to register. Must not
#'   be a canonical built-in symlink type `c("best", "keep", "remove",
#'   "latest")` Built-in symlink types are case-insensitive (i.e. the user may
#'   not register a "BEST" symlink type). User-defined symlink types may be
#'   overwritten.
#' @param is_singleton (lgl) Can there be only one of this symlink type per root
#'   directory? If `TRUE`, the symlink name will be exactly `symlink_type` (e.g.
#'   "best"). If `FALSE`, the symlink name will be `symlink_type` + "_" +
#'   version_name (e.g. "keep_2025_01_01.01").
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
    checkmate::assert_flag(is_singleton)
    checkmate::assert_string(symlink_type)
   if (tolower(symlink_type) %in% c("best", "latest", "keep", "remove")){
        stop("Cannot overwrite built-in symlink type (case-insensitive): '", tolower(symlink_type), "'")
    }
    # TODO - 2026 Mar 17 - probably want to allow user to re-write their own entries
    # if (exists(symlink_type, envir = .symlink_registry, inherits = FALSE)){
    #     stop("'", symlink_type, "' is already registered. Cannot overwrite.")
    # }

    new_entry <- list(
        is_singleton        = is_singleton
        , symlink_name      = symlink_type
        , symlink_suffix_fn = make_symlink_suffix_fn(is_singleton)
        , ls_regex_fn       = make_ls_regex_fn(is_singleton, symlink_type)
        , extract_regex     = make_query_regex(is_singleton, symlink_type)
    )

    assign(symlink_type, new_entry, envir = .symlink_registry)

    if (verbose) message("Registered new symlink type: '", symlink_type, "' (is_singleton = ", is_singleton, ")")
    invisible(NULL)
}
