# Symlink registry: built-in types and registry helpers -------------------------
# The package-level registry holds the four built-in symlink (shortcut) types,
# locked in .onLoad() (R/zzz.R), and nothing else. User-defined types live in
# each root's registry file (R/registry_disk.R); a tool builds its own registry
# from the built-ins plus its roots' files and passes it to the helpers here.

# required for tests/vignettes
# Known issue: https://github.com/rstudio/rmarkdown/issues/187
.datatable.aware = TRUE

.symlink_registry <- new.env(parent = emptyenv())


#' The built-in symlink types and their fixed `is_singleton`
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
#' The one place an entry's fields are defined; used for the built-ins in
#' `.onLoad()` and for types read from a root's disk registry.
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
#' `remove_<version_name>`). A type added to a root's registry file reserves
#' its name the same way.
#'
#' @param registry (named list) a symlink registry; default the built-in
#'   types alone. A tool passes its own (built-ins plus its roots' disk registries)
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
#' @param registry (named list) a symlink registry; default the built-in
#'   types alone
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
#' @param registry (named list) a symlink registry; default the built-in
#'   types alone
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
#' @param registry (named list) a symlink registry; default the built-in
#'   types alone
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
         , "\n    - Add it to a root with `update_symlink_disk_registry()`, "
         , "or to a tool's roots with `slt$update_symlink_registry()`."
      )
   }
   return(registry[[symlink_type]])
}


#' Get the names of all registered symlink types
#'
#' @param registry (named list) a symlink registry; default the built-in
#'   types alone
#'
#' @returns (chr) vector of names of all registered symlink types, e.g.
#'   c("best", "keep", "remove", "latest", etc.)
#' @keywords internal
get_symlink_types <- function(registry = get_symlink_registry()) {
   return(names(registry))
}


#' Get the regex patterns for extracting symlink types from `ls` system calls
#'
#' @param registry (named list) a symlink registry; default the built-in
#'   types alone
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
#' @param registry (named list) a symlink registry; default the built-in
#'   types alone
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


#' Is a log action one the tool writes?
#'
#' Used by the discrepancy report. An action is judged by its form, not
#' against the current symlink types: a root's registry file can lose a type
#' (`update_symlink_disk_registry(operation = "drop")`) while old log rows for
#' it remain, and those rows are still the tool's own.
#'
#' @param action (chr) log actions, e.g. `"promote_best"`
#'
#' @returns (lgl) one value per action: TRUE for `create`,
#'   `delete_remove_folder`, and anything of the form `promote_<type>`,
#'   `demote_<type>` or `remove_dangling_<type>`
#' @keywords internal
is_valid_action <- function(action) {
   actions_fixed  <- c("create", "delete_remove_folder")
   regex_per_type <- "^(promote|demote|remove_dangling)_[^_]"
   return(action %in% actions_fixed | grepl(regex_per_type, action))
}


# Public accessors -------------------------------------------------------------

#' Get the built-in symlink registry
#'
#' Returns the four built-in symlink types (best, keep, latest, remove) as a
#' named list, each with its properties and functions. A tool's own registry,
#' the built-ins plus the types on its roots' registry files, comes from
#' `slt$get_symlink_registry()` and has the same form. Entries are:
#' - `is_singleton`: (lgl) whether this symlink type is a singleton (only one
#'   per root)
#' - `is_builtin`: (lgl) one of the four package built-ins (best, keep, latest,
#'   remove)
#' - `symlink_suffix_fn`: (function) creates the symlink name from a version
#'   name (for non-singleton symlink types)
#' - `extract_regex`: (chr) the regex pattern that finds the symlink type
#'
#' @returns (list) the built-in symlink registry as a named list, in
#'   alphabetical order
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
