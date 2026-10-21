# ---- Init -----------------------------------------------------------------

# required for tests/vignettes
# Known issue: https://github.com/rstudio/rmarkdown/issues/187
.datatable.aware = TRUE

# Create a package-level environment
.symlink_registry <- new.env(parent = emptyenv())

# ---- Utils -------------------------------------------------------

# 2026 Mar 17 - These replace legacy hard-coded DICT entries within the symlink
# tool



# ---- Function Factories -----------------------------------------------------------------

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
#' @return (function) a function that takes a version_name argument and returns
#'   the appropriate symlink name based on the version name and whether the
#'   symlink type is a singleton or not
#'
#'
#' @keywords internal
make_symlink_suffix_fn <- function(is_singleton) {
    checkmate::assert_flag(is_singleton)
    if (is_singleton) {
        function(version_name) {""}
    } else {
        function(version_name) {sprintf("_%s", version_name)}
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
#' @return (chr) the regex pattern to find the symlink type when running `ls`
#'
#' @keywords internal
make_extract_regex <- function(is_singleton, symlink_type) {
    checkmate::assert_flag(is_singleton)
    checkmate::assert_string(symlink_type)
    if (is_singleton) {
        sprintf("^%s$", symlink_type)
    } else {
        sprintf("^%s_", symlink_type)
    }
}


# ---- Reserved version names --------------------------------------------------

#' Reserved folder names, derived from the symlink registry
#'
#' Singleton symlink types live as bare names directly in a root (`best`,
#' `latest`), so a real folder with that name collides with the tool's own
#' symlink. Non-singleton types claim a prefix (`keep_<version_name>`,
#' `remove_<version_name>`). Registering a new type with `new_symlink_type()`
#' reserves its name automatically.
#'
#' @returns (list) `words` (chr) exact names that are reserved; `prefixes`
#'   (chr) leading strings that are reserved, each ending in `_`
#' @keywords internal
get_reserved_version_names <- function() {
    reg          <- get_symlink_registry()
    is_singleton <- vapply(reg, function(entry) isTRUE(entry[["is_singleton"]]), logical(1))
    list(
        words      = names(reg)[is_singleton]
        , prefixes = sprintf("%s_", names(reg)[!is_singleton])
    )
}

#' Find the reserved folder name a version_name collides with, if any
#'
#' @param version_name (chr) a candidate folder name
#'
#' @returns (chr) the reserved word or prefix it collides with, or `NULL` if it is free
#' @keywords internal
find_reserved_name_conflict <- function(version_name) {
    checkmate::assert_string(version_name)
    reserved <- get_reserved_version_names()
    # compare ignoring case: Windows and macOS treat `Best` and `best` as the
    # same folder, so a version folder `Best` would occupy the `best` link's slot
    name_lc     <- tolower(version_name)
    words_lc    <- tolower(reserved$words)
    prefixes_lc <- tolower(reserved$prefixes)

    if (name_lc %in% words_lc) return(reserved$words[match(name_lc, words_lc)])

    conflicting_prefix <- reserved$prefixes[startsWith(name_lc, prefixes_lc)]
    if (length(conflicting_prefix)) return(conflicting_prefix[1])

    return(NULL)
}

#' Stop if a version_name is a reserved folder name
#'
#' @param version_name (chr) a candidate folder name
#'
#' @returns (invisible TRUE) or stops with a plain-language message
#' @keywords internal
assert_version_name_not_reserved <- function(version_name) {
    conflict <- find_reserved_name_conflict(version_name)
    if (is.null(conflict)) return(invisible(TRUE))
    reserved <- get_reserved_version_names()
    what <- if (tolower(conflict) == tolower(version_name)) {
        sprintf("the tool uses this name (ignoring case) for its '%s' symlink (shortcut).", conflict)
    } else {
        sprintf("the tool uses names starting with '%s' for its '%s' symlinks (shortcuts).", conflict, sub("_$", "", conflict))
    }
    stop(
        sprintf("'%s' cannot be used as a version_name - %s", version_name, what)
        , "\n    - Reserved names:    ", toString(reserved$words)
        , "\n    - Reserved prefixes: ", toString(reserved$prefixes)
        , sprintf("\n    - Names are compared ignoring case: Windows and macOS treat '%s' and '%s' as the same folder,", version_name, tolower(version_name))
        , sprintf("\n      so a '%s' symlink (shortcut) would collide with an existing folder name.", sub("_$", "", conflict))
        , "\n    - Please choose a different folder name."
    )
}


# ---- Private Accessors -------------------------------------------------------

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
        stop(
            "Symlink type '", symlink_type, "' is not registered. "
            , "\n    - Available types: ", toString(names(get_symlink_registry()))
            , "\n    - Do you need to run `new_symlink_type()`?"
        )
    }
    get_symlink_registry()[[symlink_type]]
}


#' Get the names of all registered symlink types
#'
#' @returns (chr) vector of names of all registered symlink types, e.g.
#'   c("best", "keep", "remove", "latest", etc.)
#' @keywords internal
get_symlink_types <- function() {
    names(get_symlink_registry())
}


#' Get the regex patterns for extracting symlink types from `ls` system calls
#'
#' @returns (list) named list of regex patterns for extracting symlink types
#'   from `ls` system calls, keyed by symlink type.
#' @keywords internal
get_symlink_regex_extract <- function() {
    registry <- get_symlink_registry()
    lapply(registry, `[[`, "extract_regex")
}


# TODO - 2026 Mar 17 - Remove? - not very useful and now prone to unpredictable behavior

#' Get the valid actions for marking folders
#'
#' Used mainly for the discrepancy report
#'
#' @returns (chr) vector of valid actions for marking folders, including
#'   "create", "delete_remove_folder", and the promote/demote actions for each
#'   registered symlink type (e.g. "promote_best", "demote_best",
#'   "promote_keep", "demote_keep", etc.)
#' @keywords internal
get_valid_actions <- function(){
    symlink_types <- names(get_symlink_registry())
    c(
        c("create", "delete_remove_folder")
        , sprintf("promote_%s", symlink_types)
        , sprintf("demote_%s", symlink_types)
    )
}

# ---- Package-level symlink registry environment ----------------------------
# Created when the package loads, persists for the session.
# CRAN-compliant: no global assignment, scoped to package namespace.
# Built-in bindings are locked so they cannot be overwritten by users.
.onLoad <- function(libname, pkgname){

    # Symlink-based Finite State Machine
    #
    # State Transitions:
    # [unmarked] <-> [latest] <-> [best]
    #     ^            ^           ^
    #     v            v           v
    #   [keep]       [keep]      [keep]
    #     ^            ^           ^
    #     v            v           v
    #  [remove]     [remove]    [remove]
    #
    # Rules:
    # 1. Only ONE symlink per folder at any time
    # 2. Only ONE 'best' folder per project root (Highlander rule)
    # 3. Only ONE 'latest' folder per project root (Highlander rule)
    # 4. Multiple 'keep' folders allowed
    # 5. Multiple 'remove' folders allowed
    # 6. 'best' <-> 'latest' transitions auto-demote the conflicting state
    #
    # Symlink Naming Convention:
    # - 'best':   singleton symlink named "best" (no version in name)
    # - 'latest': singleton symlink named "latest" (no version in name)
    # - 'keep':   multiple symlinks named "keep_<version_name>"
    # - 'remove': multiple symlinks named "remove_<version_name>"

    symlink_registry_builtins <- list(
        # Singleton-version marks
        # - These marks follow the Higlander rule: there can be only one per root directory
        best     = list(is_singleton = TRUE)
        , latest = list(is_singleton = TRUE)

        # Multiple-version marks
        # - These marks can be applied to multiple versions within the same root directory
        , keep   = list(is_singleton = FALSE)
        , remove = list(is_singleton = FALSE)
    )

    for(idx in seq_along(symlink_registry_builtins)) {

        entry        <- symlink_registry_builtins[[idx]]
        symlink_name <- names(symlink_registry_builtins)[idx]

        entry$symlink_suffix_fn <- make_symlink_suffix_fn(entry$is_singleton)
        entry$extract_regex     <- make_extract_regex(entry$is_singleton, symlink_name)

        symlink_registry_builtins[[idx]] <- entry
    }

    for(symlink_name in names(symlink_registry_builtins)){
        assign(
            symlink_name
            , symlink_registry_builtins[[symlink_name]]
            , envir = .symlink_registry
        )
    }

    # Decide the link mechanism for this OS once per session (symlink vs junction)
    # - see R/links.R; this is the only OS check in the package
    link_set_backend()

    # Prevent user from overwriting built-in entries
    lock_some_bindings(
        objs  = names(symlink_registry_builtins)
        , env = .symlink_registry
    )

}


# ---- Public accessors -------------------------------------------------------

#' Get the full symlink registry
#'
#' Returns the named list of symlink types and their associated properties/functions.
#' Expected entries are:
#' - `is_singleton`: (logical) whether this symlink type is a singleton (only one per root)
#' - `symlink_suffix_fn`: (function) function to create symlink name based on version name (for non-singleton symlink types)
#' - `extract_regex`: (function) function to get the regex pattern to find the symlink type
#'
#' @returns (list) the full symlink registry as a named list
#' @export
#'
#' @examples
#' get_symlink_registry()
get_symlink_registry <- function() {
   as.list(.symlink_registry)
}

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
#'   version_name (e.g. "keep_2025_01_01.01"). Either way, registering a type
#'   reserves that name (or that prefix) so it cannot be used as a
#'   version_name folder.
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
    assert_scalar_not_empty(symlink_type)
    checkmate::assert_string(symlink_type, min.chars = 1)
    if (tolower(symlink_type) %in% c("best", "latest", "keep", "remove")){
        stop(
            sprintf("Cannot overwrite built-in symlink type '%s' (you passed '%s').", tolower(symlink_type), symlink_type)
            , "\n    - Built-in types: best, latest, keep, remove."
            , "\n    - Type names are compared ignoring case: Windows and macOS treat 'BEST' and 'best' as the same folder name."
            , "\n    - Choose a different name for your new type."
        )
    }
    # a name that differs from a registered type only by case would share that
    # type's folder names on Windows and macOS
    registered <- get_symlink_types()
    clash      <- registered[tolower(registered) == tolower(symlink_type) & registered != symlink_type]
    if (length(clash)) {
        stop(
            sprintf("'%s' differs only by case from the registered symlink type '%s'.", symlink_type, clash[1])
            , "\n    - Type names are compared ignoring case: Windows and macOS treat these as the same folder name."
            , "\n    - Use the existing type '", clash[1], "', or choose a clearly different name."
        )
    }
    # TODO - 2026 Mar 17 - probably want to allow user to re-write their own entries
    # if (exists(symlink_type, envir = .symlink_registry, inherits = FALSE)){
    #   stop("'", symlink_type, "' is already registered. Cannot overwrite.")
    # }

    new_entry <- list(
        is_singleton        = is_singleton
        , symlink_suffix_fn = make_symlink_suffix_fn(is_singleton)
        , extract_regex     = make_extract_regex(is_singleton, symlink_type)
    )

    assign(symlink_type, new_entry, envir = .symlink_registry)

    if (verbose) message("Registered new symlink type: '", symlink_type, "' (is_singleton = ", is_singleton, ")")
    invisible(NULL)
}
