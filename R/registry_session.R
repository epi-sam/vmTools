# Session symlink registry -----------------------------------------------------
# The package-level registry of symlink (shortcut) types: built-ins (locked in
# .onLoad(), R/zzz.R) plus any type registered for this session with
# new_symlink_type(). Per-root persistence lives in R/registry_disk.R.

# required for tests/vignettes
# Known issue: https://github.com/rstudio/rmarkdown/issues/187
.datatable.aware = TRUE

.symlink_registry <- new.env(parent = emptyenv())


#' The built-in symlink types and their fixed shapes
#'
#' The one definition: `.onLoad()` registers and locks these, and they are
#' listed first in every registry and every root's disk registry file.
#'
#' @returns (named lgl) `is_singleton` per built-in type, names alphabetical
#' @keywords internal
get_builtin_symlink_types <- function() {
   return(c(
      best     = TRUE
      , keep   = FALSE
      , latest = TRUE
      , remove = FALSE
   ))
}


# Function factories -----------------------------------------------------------
# 2026 Mar 17 - These replace legacy hard-coded DICT entries within the symlink
# tool

#' Make a function to create the symlink name based on the version name
#'
#' The symlink name depends on whether the symlink type is a singleton or not.
#' For example, for a singleton "best" symlink type, the symlink name would be
#' "best". For a non-singleton "keep" symlink type, the symlink name would be
#' "keep_" + version_name (e.g. "keep_2025_01_01.01").
#'
#' @param is_singleton (lgl) whether this symlink type is a singleton (only
#'   one per root)
#'
#' @return (function) a function that takes a version_name argument and returns
#'   the appropriate symlink name based on the version name and whether the
#'   symlink type is a singleton or not
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
#' @param is_singleton (lgl) whether this symlink type is a singleton (only
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


#' Build one registry entry for a symlink type
#'
#' The one place an entry's shape is defined; used for the built-ins in
#' `.onLoad()`, for `new_symlink_type()`, and for types read from a root's
#' disk registry.
#'
#' @param symlink_type (chr) the type name, e.g. "best", "keep"
#' @param is_singleton (lgl) singleton (one per root) or multiple-version
#' @param is_builtin (lgl) one of the four package built-ins?
#'
#' @returns (named list) `is_singleton` (lgl), `is_builtin` (lgl),
#'   `symlink_suffix_fn` (function), `extract_regex` (chr)
#' @keywords internal
make_symlink_registry_entry <- function(symlink_type, is_singleton, is_builtin = FALSE) {
   checkmate::assert_string(symlink_type)
   checkmate::assert_flag(is_singleton)
   checkmate::assert_flag(is_builtin)
   return(list(
      is_singleton        = is_singleton
      , is_builtin        = is_builtin
      , symlink_suffix_fn = make_symlink_suffix_fn(is_singleton)
      , extract_regex     = make_extract_regex(is_singleton, symlink_type)
   ))
}


# Reserved version names -------------------------------------------------------

#' Reserved folder names, derived from the symlink registry
#'
#' Singleton symlink types live as bare names directly in a root (`best`,
#' `latest`), so a real folder with that name collides with the tool's own
#' symlink. Non-singleton types claim a prefix (`keep_<version_name>`,
#' `remove_<version_name>`). Registering a new type with `new_symlink_type()`
#' reserves its name automatically.
#'
#' @param registry (named list) a symlink registry; default the session
#'   registry. A tool passes its own (built-ins plus its roots' disk registries)
#'
#' @returns (list) `words` (chr) exact names that are reserved; `prefixes`
#'   (chr) leading strings that are reserved, each ending in `_`
#' @keywords internal
get_reserved_version_names <- function(registry = get_symlink_registry()) {
   is_singleton <- vapply(
      registry
      , function(entry) isTRUE(entry[["is_singleton"]])
      , logical(1)
   )
   return(list(
      words      = names(registry)[is_singleton]
      , prefixes = sprintf("%s_", names(registry)[!is_singleton])
   ))
}


#' Find the reserved folder name a version_name collides with, if any
#'
#' @param version_name (chr) a candidate folder name
#' @param registry (named list) a symlink registry; default the session
#'   registry
#'
#' @returns (chr) the reserved word or prefix it collides with, or `NULL` if it
#'   is free
#' @keywords internal
find_reserved_name_conflict <- function(version_name, registry = get_symlink_registry()) {
   checkmate::assert_string(version_name)
   reserved <- get_reserved_version_names(registry)
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
#' @param registry (named list) a symlink registry; default the session
#'   registry
#'
#' @returns (invisible TRUE) or stops with a plain-language message
#' @keywords internal
assert_version_name_not_reserved <- function(version_name, registry = get_symlink_registry()) {
   conflict <- find_reserved_name_conflict(version_name, registry)
   if (is.null(conflict)) return(invisible(TRUE))
   reserved <- get_reserved_version_names(registry)
   type     <- sub("_$", "", conflict)
   what <- if (tolower(conflict) == tolower(version_name)) {
      sprintf(
         "the tool uses this name (ignoring case) for its '%s' symlink (shortcut)."
         , conflict
      )
   } else {
      sprintf(
         "the tool uses names starting with '%s' for its '%s' symlinks (shortcuts)."
         , conflict
         , type
      )
   }
   stop(
      sprintf("'%s' cannot be used as a version_name - %s", version_name, what)
      , "\n    - Reserved names:    ", toString(reserved$words)
      , "\n    - Reserved prefixes: ", toString(reserved$prefixes)
      , "\n    - Names are compared ignoring case: Windows and macOS treat '"
      , version_name, "' and '", tolower(version_name), "' as the same folder,"
      , sprintf(
         "\n      so a '%s' symlink (shortcut) would collide with an existing folder name."
         , type
      )
      , "\n    - Please choose a different folder name."
   )
}


# Private accessors ------------------------------------------------------------

#' Get one entry from the symlink registry by name
#'
#' @param symlink_type (chr) name of the symlink type to retrieve, e.g. "best",
#'   "keep", "remove", "latest"
#' @param registry (named list) a symlink registry; default the session
#'   registry
#'
#' @returns (named list) the registry entry for the specified symlink type. See
#'   get_symlink_registry() for expected entry structure.
#' @keywords internal
get_symlink_registry_entry <- function(symlink_type, registry = get_symlink_registry()) {
   checkmate::assert_string(symlink_type)
   if (!symlink_type %in% names(registry)) {
      stop(
         "Symlink type '", symlink_type, "' is not registered. "
         , "\n    - Available types: ", toString(names(registry))
         , "\n    - Do you need to run `new_symlink_type()`?"
      )
   }
   return(registry[[symlink_type]])
}


#' Get the names of all registered symlink types
#'
#' @param registry (named list) a symlink registry; default the session
#'   registry
#'
#' @returns (chr) vector of names of all registered symlink types, e.g.
#'   c("best", "keep", "remove", "latest", etc.)
#' @keywords internal
get_symlink_types <- function(registry = get_symlink_registry()) {
   return(names(registry))
}


#' Get the regex patterns for extracting symlink types from `ls` system calls
#'
#' @param registry (named list) a symlink registry; default the session
#'   registry
#'
#' @returns (list) named list of regex patterns for extracting symlink types
#'   from `ls` system calls, keyed by symlink type.
#' @keywords internal
get_symlink_regex_extract <- function(registry = get_symlink_registry()) {
   return(lapply(registry, `[[`, "extract_regex"))
}


#' Which registered symlink type does a link name belong to?
#'
#' `best` -> `"best"`, `keep_1990_01_01` -> `"keep"`, `1990_01_01` -> `NA`.
#'
#' @param name (chr) one leaf name as it appears in a root folder
#' @param registry (named list) a symlink registry; default the session
#'   registry
#'
#' @returns (chr) the symlink type, or `NA` if the name is not a tool link name
#' @keywords internal
get_symlink_type_from_name <- function(name, registry = get_symlink_registry()) {
   checkmate::assert_string(name)
   regexes <- get_symlink_regex_extract(registry)
   hits    <- vapply(regexes, function(rx) grepl(rx, name), logical(1))
   if (!any(hits)) return(NA_character_)
   return(names(regexes)[hits][1])
}


# TODO - 2026 Mar 17 - Remove? - not very useful and now prone to unpredictable
# behavior

#' Get the valid actions for marking folders
#'
#' Used mainly for the discrepancy report
#'
#' @param registry (named list) a symlink registry; default the session
#'   registry
#'
#' @returns (chr) vector of valid actions for marking folders, including
#'   "create", "delete_remove_folder", and the promote/demote actions for each
#'   registered symlink type (e.g. "promote_best", "demote_best",
#'   "promote_keep", "demote_keep", etc.)
#' @keywords internal
get_valid_actions <- function(registry = get_symlink_registry()){
   symlink_types <- names(registry)
   return(c(
      c("create", "delete_remove_folder")
      , sprintf("promote_%s", symlink_types)
      , sprintf("demote_%s", symlink_types)
      # the tool removed a link whose target folder another person or process
      # had deleted, e.g. a dangling "best"
      , sprintf("remove_dangling_%s", symlink_types)
   ))
}


# Public accessors -------------------------------------------------------------

#' Get the full symlink registry
#'
#' Returns the named list of symlink types and their associated properties and
#' functions. Expected entries are:
#' - `is_singleton`: (lgl) whether this symlink type is a singleton (only one
#'   per root)
#' - `is_builtin`: (lgl) one of the four package built-ins (best, keep, latest,
#'   remove)
#' - `symlink_suffix_fn`: (function) creates the symlink name from a version
#'   name (for non-singleton symlink types)
#' - `extract_regex`: (chr) the regex pattern that finds the symlink type
#'
#' @returns (list) the full symlink registry as a named list, the four
#'   built-ins first and then the other types, each group alphabetical
#' @export
#'
#' @examples
#' get_symlink_registry()
get_symlink_registry <- function() {
   return(sort_symlink_registry(as.list(.symlink_registry)))
}


#' Order a registry: built-ins first, then the rest, each group alphabetical
#'
#' @param registry (named list) a symlink registry
#'
#' @returns (named list) the same entries, reordered
#' @keywords internal
sort_symlink_registry <- function(registry) {
   builtins <- sort(intersect(names(registry), names(get_builtin_symlink_types())))
   others   <- sort(setdiff(names(registry), builtins))
   return(registry[c(builtins, others)])
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
         sprintf(
            "Cannot overwrite built-in symlink type '%s' (you passed '%s')."
            , tolower(symlink_type)
            , symlink_type
         )
         , "\n    - Built-in types: best, latest, keep, remove."
         , "\n    - Type names are compared ignoring case: Windows and macOS "
         , "treat 'BEST' and 'best' as the same folder name."
         , "\n    - Choose a different name for your new type."
      )
   }
   # a name that differs from a registered type only by case would share that
   # type's folder names on Windows and macOS
   registered <- get_symlink_types()
   same_lc    <- tolower(registered) == tolower(symlink_type)
   clash      <- registered[same_lc & registered != symlink_type]
   if (length(clash)) {
      stop(
         sprintf(
            "'%s' differs only by case from the registered symlink type '%s'."
            , symlink_type
            , clash[1]
         )
         , "\n    - Type names are compared ignoring case: Windows and macOS "
         , "treat these as the same folder name."
         , "\n    - Use the existing type '", clash[1]
         , "', or choose a clearly different name."
      )
   }
   # TODO - 2026 Mar 17 - probably want to allow user to re-write their own
   # entries
   # if (exists(symlink_type, envir = .symlink_registry, inherits = FALSE)){
   #   stop("'", symlink_type, "' is already registered. Cannot overwrite.")
   # }

   assign(
      symlink_type
      , make_symlink_registry_entry(symlink_type, is_singleton)
      , envir = .symlink_registry
   )

   if (verbose) {
      message(
         "Registered new symlink type: '", symlink_type
         , "' (is_singleton = ", is_singleton, ")"
      )
   }
   invisible(NULL)
}
