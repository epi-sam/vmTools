# started: 2026 Nov 10 19:47:56
# Disk registry ----------------------------------------------------------------
# The session registry (R/registry_session.R) forgets every user-defined type
# when R restarts. Each root carries a small file recording the types ever used
# there, so a new tool on that root starts with the same types (see
# `SLT$new()`), and typed roundups work without re-registering every session.
#
# - one row per symlink type: current state, not history
# - the four built-in types are written too, so a person reading the file sees
#   every type the root uses; their `is_singleton` is fixed by the package,
#   never by the file
# - a file edit never changes the live session registry; `new_symlink_type()`
#   does
# - a tool's registry = built-ins + the union of its roots' files (+ session
#   defaults while `new_symlink_type()` lives); see `merge_symlink_registries()`


# Dictionary and small helpers -------------------------------------------------

#' Fixed names and formats of the disk registry file
#'
#' - `fname`: a dotfile, so `list.files()` without `all.files` skips it. It
#'   must stay a FILE: `link_list()` lists every folder in a root, so a dot
#'   folder would be reported as a version folder
#' - `schema`: column name and type; `timestamp` and `user` match the log schema
#' - `timestamp_format`: same format as the tool's `datestamp_format`; kept here
#'   because the disk registry is written outside the SLT class
#' - `help_pointer`: the closing line of errors that need the docs, naming
#'   levers the user can run
#'
#' @returns (list) `fname` (chr), `schema` (named list of chr),
#'   `timestamp_format` (chr), `help_pointer` (chr)
#' @keywords internal
get_disk_registry_dictionary <- function() {
   return(list(
      fname = ".symlink_registry.csv"
      , schema = list(
         symlink_type      = "character"
         , is_singleton    = "logical"
         , is_builtin      = "logical"
         , timestamp       = "character"
         , user            = "character"
         , vmtools_version = "character"
      )
      , timestamp_format = "%Y_%m_%d_%H%M%S"
      , help_pointer = paste0(
         "See vignette(\"user_defined_symlink_types\", package = \"vmTools\") "
         , "or ?update_symlink_disk_registry"
      )
   ))
}


#' Path of a root's disk registry file
#'
#' @param root (chr) one root folder
#'
#' @returns (chr) the file path, whether or not the file exists
#' @keywords internal
get_disk_registry_path <- function(root) {
   assert_scalar_not_empty(root)
   fname <- get_disk_registry_dictionary()$fname
   return(clean_path(root, fname, normalize = FALSE))
}


#' Is this one of the built-in symlink types?
#'
#' Compared ignoring case, as `new_symlink_type()` does.
#'
#' @param symlink_type (chr) a type name
#'
#' @returns (lgl) TRUE for best, latest, keep, remove in any case
#' @keywords internal
is_builtin_symlink_type <- function(symlink_type) {
   return(tolower(symlink_type) %in% names(get_builtin_symlink_types()))
}


# Rows and tables --------------------------------------------------------------

#' An empty disk registry table with the right columns and types
#'
#' @returns (data.table) zero rows, columns per the `schema` of
#'   `get_disk_registry_dictionary()`
#' @keywords internal
make_empty_disk_registry <- function() {
   schema <- get_disk_registry_dictionary()$schema
   cols   <- lapply(schema, function(type) vector(type, 0L))
   return(data.table::as.data.table(cols))
}


#' One disk registry row for a symlink type
#'
#' @param symlink_type (chr) the type
#' @param is_singleton (lgl) singleton or multiple-version
#' @param is_builtin (lgl) one of the four package built-ins?
#'
#' @returns (data.table) one row, columns per the `schema` of
#'   `get_disk_registry_dictionary()`
#' @keywords internal
make_disk_registry_row <- function(symlink_type, is_singleton, is_builtin = FALSE) {
   timestamp_format <- get_disk_registry_dictionary()$timestamp_format
   return(data.table::data.table(
      symlink_type      = symlink_type
      , is_singleton    = is_singleton
      , is_builtin      = is_builtin
      , timestamp       = format(Sys.time(), timestamp_format)
      , user            = Sys.info()[["user"]]
      , vmtools_version = as.character(utils::packageVersion("vmTools"))
   ))
}


#' Disk registry rows for the built-in types
#'
#' @returns (data.table) four rows, columns per the `schema` of
#'   `get_disk_registry_dictionary()`
#' @keywords internal
make_builtin_disk_registry_rows <- function() {
   builtin_types <- get_builtin_symlink_types()
   rows <- Map(
      make_disk_registry_row
      , symlink_type = names(builtin_types)
      , is_singleton = unname(builtin_types)
      , is_builtin   = TRUE
   )
   return(data.table::rbindlist(rows))
}


# One root: read and write -----------------------------------------------------

#' Read one root's disk registry
#'
#' A missing file reads as an empty table. A file whose columns do not match
#' the schema, that lists one type with two `is_singleton` values, or that
#' gives a built-in type the wrong `is_singleton`, stops: all mean the file was edited
#' by hand.
#'
#' @param root (chr) one root folder
#'
#' @returns (data.table) one row per symlink type, columns per the `schema` of
#'   `get_disk_registry_dictionary()`, built-ins first, each group alphabetical
#' @keywords internal
read_disk_registry <- function(root) {
   fpath <- get_disk_registry_path(root)
   if (!file.exists(fpath)) return(make_empty_disk_registry())

   schema        <- get_disk_registry_dictionary()$schema
   builtin_types <- get_builtin_symlink_types()

   dt <- data.table::setDT(utils::read.csv(fpath, colClasses = unlist(schema)))

   nms_dt       <- names(dt)
   nms_reg      <- names(schema)
   nms_dt_sort  <- sort(nms_dt)
   nms_reg_sort <- sort(nms_reg)

   if (!identical(nms_dt_sort, nms_reg_sort)) {
      stop(
         "The symlink (shortcut) type registry file has unexpected columns: "
         , fpath
         , "\n    - Expected: ", toString(nms_reg)
         , "\n    - Found:    ", toString(nms_dt)
         , "\n    - Was it edited by hand? Fix or delete the file. "
         , "Nothing has been changed."
      )
   }

   data.table::setcolorder(dt, nms_reg)
   dt  <- unique(dt, by = c("symlink_type", "is_singleton"))
   dup <- dt[duplicated(symlink_type), symlink_type]
   if (length(dup)) {
      stop(
         "The symlink (shortcut) type registry file lists a type as both "
         , "singleton and multiple-version: ", fpath
         , "\n    - Type(s): ", toString(dup)
         , "\n    - Was it edited by hand? Fix or delete the file. "
         , "Nothing has been changed."
      )
   }

   builtin_rows <- dt[symlink_type %in% names(builtin_types)]
   bad_builtin  <- builtin_rows[is_singleton != builtin_types[symlink_type], symlink_type]
   if (length(bad_builtin)) {
      stop(
         "The symlink (shortcut) type registry file gives a built-in type the "
         , "wrong is_singleton: ", fpath
         , "\n    - Type(s): ", toString(bad_builtin)
         , "\n    - Built-ins are fixed: best and latest are is_singleton = TRUE; "
         , "keep and remove are is_singleton = FALSE."
         , "\n    - Was it edited by hand? Fix or delete the file. "
         , "Nothing has been changed."
      )
   }

   is_builtin_expected <- dt$symlink_type %in% names(builtin_types)
   bad_flag            <- dt[is_builtin != is_builtin_expected, symlink_type]
   if (length(bad_flag)) {
      stop(
         "The symlink (shortcut) type registry file marks a type's is_builtin "
         , "column wrongly: ", fpath
         , "\n    - Type(s): ", toString(bad_flag)
         , "\n    - Only best, keep, latest and remove are built-in."
         , "\n    - Was it edited by hand? Fix or delete the file. "
         , "Nothing has been changed."
      )
   }

   data.table::setorder(dt, -is_builtin, symlink_type)
   return(dt)
}


#' Write one root's disk registry
#'
#' Atomic: temp file in the same folder, then rename, so a parallel reader sees
#' the old file or the new one, never a partial one (same rule as the tool's
#' log writer). Existing permissions are kept. Any built-in type missing from
#' `dt` is added, so every file lists all four.
#'
#' @param root (chr) one root folder
#' @param dt (data.table) the table to write, columns per the `schema` of
#'   `get_disk_registry_dictionary()`
#'
#' @returns (chr) the file path, invisibly
#' @keywords internal
write_disk_registry <- function(root, dt) {
   dictionary <- get_disk_registry_dictionary()
   fpath      <- get_disk_registry_path(root)
   fpath_tmp  <- tempfile(
      pattern   = paste0(dictionary$fname, ".")
      , tmpdir  = dirname(fpath)
      , fileext = ".tmp"
   )
   on.exit(if (file.exists(fpath_tmp)) unlink(fpath_tmp), add = TRUE)

   dt <- data.table::copy(dt)
   missing_builtins <- setdiff(names(get_builtin_symlink_types()), dt$symlink_type)
   if (length(missing_builtins)) {
      builtin_rows <- make_builtin_disk_registry_rows()
      dt <- rbind(dt, builtin_rows[symlink_type %in% missing_builtins])
   }
   data.table::setcolorder(dt, names(dictionary$schema))
   data.table::setorder(dt, -is_builtin, symlink_type)
   utils::write.csv(dt, fpath_tmp, row.names = FALSE)

   if (file.exists(fpath)) {
      Sys.chmod(fpath_tmp, mode = file.mode(fpath), use_umask = FALSE)
   }
   if (!isTRUE(file.rename(fpath_tmp, fpath))) {
      stop(
         "Could not write the symlink (shortcut) type registry file: ", fpath
         , "\n    - The original file (if any) was left untouched."
         , "\n    - Common causes: no write permission on the folder, "
         , "the file is locked, or the disk is full."
      )
   }
   return(invisible(fpath))
}


#' Add a symlink type to a root's disk registry if it is not there yet
#'
#' The automatic path: called when a type is first promoted in a root. A
#' built-in type, or a type already on file with the same `is_singleton`, is a
#' no-op. A type on file with the other `is_singleton` stops: the session and
#' the root disagree about its `is_singleton`, and only
#' `update_symlink_disk_registry()` may change that.
#'
#' @param root (chr) one root folder
#' @param symlink_type (chr) the type being promoted
#' @param is_singleton (lgl) its value in the tool's registry
#'
#' @returns (lgl) TRUE invisibly if a row was written, FALSE invisibly otherwise
#' @keywords internal
record_symlink_type_on_disk <- function(root, symlink_type, is_singleton) {
   checkmate::assert_flag(is_singleton)
   if (is_builtin_symlink_type(symlink_type)) return(invisible(FALSE))
   dt <- read_disk_registry(root)
   # a scalar named like the column would resolve to the column inside `[`
   .symlink_type <- symlink_type
   on_file       <- dt[symlink_type == .symlink_type]
   if (nrow(on_file) == 1L) {
      if (identical(on_file$is_singleton, is_singleton)) return(invisible(FALSE))
      stop(
         "Symlink (shortcut) type '", symlink_type, "' has is_singleton = "
         , is_singleton
         , " in the live session registry but is_singleton = "
         , on_file$is_singleton
         , " in ", get_disk_registry_path(root)
         , "\nChange one side: update_symlink_disk_registry() for the root, "
         , "new_symlink_type() for the session. Nothing has been changed."
      )
   }
   new_row <- make_disk_registry_row(symlink_type, is_singleton)
   write_disk_registry(root, rbind(dt, new_row))
   return(invisible(TRUE))
}


# Several roots: read, merge, backfill -----------------------------------------

#' Read the disk registries of several roots as one table
#'
#' Stops if two roots give one type different `is_singleton` values: the roots
#' must agree
#' before a tool can span them. Absence is not a disagreement; see
#' `backfill_disk_registries()`.
#'
#' @param root_list (chr or list) root folders
#'
#' @returns (data.table) one row per root and type, columns per the `schema` of
#'   `get_disk_registry_dictionary()` plus `root` (chr); zero rows if no root has a file
#' @keywords internal
read_disk_registries <- function(root_list) {
   roots <- unlist(root_list, use.names = FALSE)
   checkmate::assert_character(roots, min.len = 1, any.missing = FALSE)
   dt <- data.table::rbindlist(
      lapply(roots, function(x) read_disk_registry(x)[, root := x])
   )

   counts    <- dt[, .(n_values = data.table::uniqueN(is_singleton)), by = symlink_type]
   conflicts <- counts[n_values > 1L, symlink_type]
   if (length(conflicts)) {
      detail <- dt[symlink_type %in% conflicts][order(symlink_type, -is_singleton, root)]
      lines  <- detail[, sprintf("  %s: is_singleton = %s in %s", symlink_type, is_singleton, root)]
      stop(
         "Root folders disagree about a symlink (shortcut) type's is_singleton:\n"
         , paste(lines, collapse = "\n")
         , "\nPick the roots to change and run, for each:\n"
         , "update_symlink_disk_registry(<root>, \"<type>\", "
         , "is_singleton = <TRUE or FALSE>, operation = \"update\")"
         , "\nNothing has been changed.\n"
         , get_disk_registry_dictionary()$help_pointer
      )
   }
   return(dt)
}


#' Merge the roots' disk registries with the session registry
#'
#' Precedence: a root's file, then the session registry (built-ins locked in
#' `.onLoad()` plus any `new_symlink_type()` default), so a session default is
#' used only for a type on no root's file. Built-ins always come from the
#' package. Order per `sort_symlink_registry()`.
#'
#' @param disk_dt (data.table) from `read_disk_registries()`
#' @param session_registry (named list) default `get_symlink_registry()`
#'
#' @returns (named list) a symlink registry, entries per
#'   `make_symlink_registry_entry()`
#' @keywords internal
merge_symlink_registries <- function(disk_dt, session_registry = get_symlink_registry()) {
   on_disk      <- unique(disk_dt[is_builtin == FALSE], by = "symlink_type")
   disk_entries <- Map(
      make_symlink_registry_entry
      , symlink_type = on_disk$symlink_type
      , is_singleton = on_disk$is_singleton
   )
   # built-ins are never among the disk entries, so they always come from the session
   types_session_only <- setdiff(names(session_registry), names(disk_entries))
   merged             <- c(session_registry[types_session_only], disk_entries)
   return(sort_symlink_registry(merged))
}


#' Session defaults that a root's file overrides
#'
#' @param disk_dt (data.table) from `read_disk_registries()`
#' @param session_registry (named list) default `get_symlink_registry()`
#'
#' @returns (data.table) `symlink_type` (chr), `session_is_singleton` (lgl),
#'   `file_is_singleton` (lgl); zero rows if none
#' @keywords internal
find_session_overrides <- function(disk_dt, session_registry = get_symlink_registry()) {
   on_disk <- unique(
      disk_dt[is_builtin == FALSE, .(symlink_type, file_is_singleton = is_singleton)]
      , by = "symlink_type"
   )
   in_session <- data.table::data.table(
      symlink_type           = names(session_registry)
      , session_is_singleton = vapply(session_registry, `[[`, logical(1), "is_singleton")
   )
   in_both <- in_session[on_disk, on = "symlink_type", nomatch = NULL]
   return(in_both[session_is_singleton != file_is_singleton])
}


#' Rows a root's disk registry lacks and a sibling root's has
#'
#' Reads nothing and writes nothing: works from the table alone. A root with
#' no file has no rows in `disk_dt`, so it lacks every type.
#'
#' @param roots (chr) root folders
#' @param disk_dt (data.table) from `read_disk_registries()` over the same
#'   roots
#'
#' @returns (data.table) one row per root and missing type: `root` (chr) plus
#'   the sibling's row as-is, columns per the `schema` of
#'   `get_disk_registry_dictionary()`; zero rows if nothing is missing
#' @keywords internal
find_disk_registry_gaps <- function(roots, disk_dt) {
   rows_known <- unique(disk_dt[is_builtin == FALSE, !"root"], by = "symlink_type")
   wanted     <- data.table::CJ(
      root           = roots
      , symlink_type = rows_known$symlink_type
      , sorted       = FALSE
   )
   missing <- wanted[!disk_dt, on = c("root", "symlink_type")]
   return(rows_known[missing, on = "symlink_type"])
}


#' Give every root every row a sibling root has
#'
#' The registry state machine's repair step: after `read_disk_registries()`
#' has shown the roots agree, any type on one root's file and absent from
#' another's is copied over as-is (original timestamp and user), so all roots
#' of a tool carry the same rows. A root with no file gets one.
#'
#' @param root_list (chr or list) root folders
#' @param disk_dt (data.table) from `read_disk_registries()` over the same
#'   roots
#'
#' @returns (data.table) the rows written: `root` (chr), `symlink_type` (chr),
#'   `is_singleton` (lgl); zero rows if nothing was missing
#' @keywords internal
backfill_disk_registries <- function(root_list, disk_dt) {
   roots    <- unlist(root_list, use.names = FALSE)
   gaps     <- find_disk_registry_gaps(roots, disk_dt)
   has_file <- file.exists(vapply(roots, get_disk_registry_path, character(1)))

   roots_to_write <- roots[roots %in% gaps$root | !has_file]
   for (root_to_write in roots_to_write) {
      rows_on_file <- read_disk_registry(root_to_write)
      rows_missing <- gaps[root == root_to_write, !"root"]
      write_disk_registry(root_to_write, rbind(rows_on_file, rows_missing))
   }
   return(gaps[, .(root, symlink_type, is_singleton)])
}


# Links that block a change ----------------------------------------------------

#' Links in root folders whose name follows a symlink type, by `is_singleton`
#'
#' A singleton type's link is named `<type>`; a multiple-version type's links
#' are named `<type>_<version_name>`.
#'
#' @param roots (chr) one or more root folders
#' @param symlink_type (chr) the type
#' @param is_singleton (lgl) one or more values: which link names to look for;
#'   `logical(0)` finds nothing
#'
#' @returns (data.table) `link_list()` rows plus `version_name` (chr), the
#'   target's leaf or the link's leaf when the target is unknown, and `root`
#'   (chr); zero rows if there are none
#' @keywords internal
find_links_by_singleton_status <- function(roots, symlink_type, is_singleton) {
   grid  <- data.table::CJ(root = roots, is_singleton = is_singleton)
   links <- Map(
      function(root, is_singleton) {
         regex <- make_extract_regex(is_singleton, symlink_type)
         found <- link_list(root)[is_link == TRUE][grepl(regex, leaf)]
         found[, version_name := leaf]
         found[!is.na(target) & nzchar(target), version_name := basename(target)]
         found[, root := root]
         return(found)
      }
      , grid$root
      , grid$is_singleton
   )
   return(data.table::rbindlist(links))
}


# Messages ---------------------------------------------------------------------

#' Format symlink types and their `is_singleton` as a two-column table for messages
#'
#' @param dt (data.table) with `symlink_type` and `is_singleton`
#'
#' @returns (chr) one string, one line per type
#' @keywords internal
format_disk_registry_table <- function(dt) {
   width <- max(nchar(c("symlink_type", dt$symlink_type)))
   lines <- c(
      sprintf("  %-*s  is_singleton", width, "symlink_type")
      , sprintf("  %-*s  %s", width, dt$symlink_type, dt$is_singleton)
   )
   return(paste(lines, collapse = "\n"))
}


#' The unmark / re-mark recipe printed when a registry change is refused
#'
#' Short by design: an error longer than `getOption("warning.length")` (1000
#' characters by default) is truncated, so live links are addressed through a
#' roundup rather than listed, and only dangling links, which a roundup does
#' not return, are named.
#'
#' @param blocking (data.table) from `find_links_by_singleton_status()`
#' @param symlink_type (chr) the type
#' @param is_singleton (lgl or NULL) the requested value (NULL for drop)
#' @param operation (chr) `"update"` or `"drop"`
#'
#' @returns (chr) one string of R code lines, flush left, pastable
#' @keywords internal
format_unmark_recipe <- function(blocking, symlink_type, is_singleton, operation) {
   reason <- if (operation == "drop") "dropped" else "is_singleton changed"
   entry  <- sprintf("list(comment = \"%s %s\")", symlink_type, reason)
   lines  <- character(0)
   if (any(!blocking$is_dangling)) {
      lines <- c(lines, paste0(
         sprintf("for (v in slt$roundup(\"%s\", return_as = \"version_name\")) ", symlink_type)
         , sprintf("slt$unmark(v, user_entry = %s)", entry)
      ))
   }
   if (any(blocking$is_dangling)) {
      leaves <- unique(blocking[is_dangling == TRUE, leaf])
      lines  <- c(lines, sprintf(
         "for (l in c(%s)) slt$unmark(l, user_entry = %s)"
         , paste(sprintf("\"%s\"", leaves), collapse = ", ")
         , entry
      ))
   }
   if (operation == "update") {
      placeholder <- if (isTRUE(is_singleton)) {
         "<the one version to mark>"
      } else {
         "<each version to mark>"
      }
      lines <- c(
         lines
         , "# repeat this call, then re-mark:"
         , sprintf("slt$mark(\"%s\", \"%s\", user_entry = %s)", symlink_type, placeholder, entry)
      )
   }
   return(paste(lines, collapse = "\n"))
}


# Edit a root's registry -------------------------------------------------------

#' A symlink type's row in one root's registry file, against `is_singleton`
#'
#' The `state` column of `get_disk_registry_outcomes()`.
#'
#' @param root (chr) one root folder
#' @param symlink_type (chr) the type
#' @param is_singleton (lgl or NULL) the requested value; NULL (a drop) never
#'   matches, so a type on file reads as `"differs"`
#'
#' @returns (chr) `"absent"` (the file has no row for the symlink type),
#'   `"matches"` (a row exists and its `is_singleton` equals the argument) or
#'   `"differs"` (a row exists and its `is_singleton` is the other value)
#' @keywords internal
get_disk_registry_state <- function(root, symlink_type, is_singleton) {
   # a scalar named like the column would resolve to the column inside `[`
   .symlink_type <- symlink_type
   on_file       <- read_disk_registry(root)[symlink_type == .symlink_type]
   if (nrow(on_file) == 0L) return("absent")
   if (identical(on_file$is_singleton, is_singleton)) return("matches")
   return("differs")
}


#' What `update_symlink_disk_registry()` does to a root's symlink registry file
#'
#' The function's whole decision table: one row per case, for one symlink type
#' in one root's `.symlink_registry.csv`.
#'
#' - `operation`: the `operation` argument of `update_symlink_disk_registry()`.
#' - `state`: the symlink type's row in that root's file, compared with the
#'   `is_singleton` argument (worked out by `get_disk_registry_state()`):
#'   - `absent`: the file has no row for the symlink type
#'   - `matches`: a row for the symlink type exists and its `is_singleton`
#'     equals the argument
#'   - `differs`: a row for the symlink type exists and its `is_singleton` is
#'     the other value; always the case for a `drop` called without
#'     `is_singleton`
#' - `action`: what happens to that root's file. `write` changes it, unless
#'   an `update` or `drop` is blocked by links of the symlink type (see
#'   `find_links_by_singleton_status()`); `skip` leaves it; `stop` refuses the
#'   whole call before any root's file is written.
#' - `note`: the text reported for that root: inside the error for `stop`,
#'   otherwise in a message when `verbose = TRUE`.
#'
#' | operation | state   | action | note                                                                  |
#' |-----------|---------|--------|-----------------------------------------------------------------------|
#' | add       | absent  | write  | add done                                                              |
#' | add       | matches | skip   | already on file with the same is_singleton; nothing changed           |
#' | add       | differs | stop   | already on file with the other is_singleton; use operation = 'update' |
#' | update    | absent  | stop   | not on file; use operation = 'add'                                    |
#' | update    | matches | skip   | already on file with the same is_singleton; nothing changed           |
#' | update    | differs | write  | update done                                                           |
#' | drop      | absent  | skip   | not on file; nothing to drop                                          |
#' | drop      | matches | write  | drop done                                                             |
#' | drop      | differs | write  | drop done                                                             |
#'
#' @returns (data.table) nine rows: `operation` (chr), `state` (chr) from
#'   `get_disk_registry_state()`, `action` (chr), `note` (chr)
#' @keywords internal
get_disk_registry_outcomes <- function() {
   no_change <- "already on file with the same is_singleton; nothing changed"
   differs   <- "already on file with the other is_singleton; use operation = 'update'"
   rows <- list(
      list("add",      "absent",  "write", "add done")
      , list("add",    "matches", "skip",  no_change)
      , list("add",    "differs", "stop",  differs)
      , list("update", "absent",  "stop",  "not on file; use operation = 'add'")
      , list("update", "matches", "skip",  no_change)
      , list("update", "differs", "write", "update done")
      , list("drop",   "absent",  "skip",  "not on file; nothing to drop")
      , list("drop",   "matches", "write", "drop done")
      , list("drop",   "differs", "write", "drop done")
   )
   outcomes <- data.table::rbindlist(rows)
   data.table::setnames(outcomes, c("operation", "state", "action", "note"))
   return(outcomes)
}


#' Add, update or drop a symlink type in root folders' disk registries
#'
#' Every root carries a file, `.symlink_registry.csv`, listing the symlink
#' (shortcut) types ever used there and whether each is a singleton (one per
#' root, named `<type>`) or multiple-version (named `<type>_<version_name>`).
#' `SLT$new()` reads it and registers those types for the session, so a new
#' tool on an old root can round up every type the root has seen. The tool
#' writes the file itself the first time a type is promoted in a root; this
#' function is the explicit way to edit it.
#'
#' The live session registry is never changed by this function. To use a type
#' in the current session, call `new_symlink_type()` or construct a new tool.
#'
#' Changing a type's `is_singleton`, or dropping it, is refused while links of
#' that type exist in a root: afterwards those links would belong to no registered
#' type, so roundups would skip them and `delete_version_folders()` would
#' delete them without a log. The error prints the `unmark()` calls needed
#' first, with the affected version names.
#'
#' @param root (chr) one or more root folders; each gets the same change
#' @param symlink_type (chr) the type; built-in types (best, latest, keep,
#'   remove) are refused
#' @param is_singleton (lgl) required for `add` and `update`; ignored by
#'   `drop`, which goes by name alone
#' @param operation (chr) `"add"` a type not yet on file; `"update"` the
#'   `is_singleton` of a type on file; `"drop"` a type from the file
#' @param verbose (lgl: default `TRUE`) message what changed in each root
#'
#' @returns (invisible NULL) called for the side effect of writing each root's
#'   registry file
#' @export
#'
#' @examples
#' \dontrun{
#' update_symlink_disk_registry(
#'    "/path/to/root", "stalled", is_singleton = FALSE, operation = "add"
#' )
#' update_symlink_disk_registry("/path/to/root", "stalled", operation = "drop")
#' }
update_symlink_disk_registry <- function(
      root
      , symlink_type
      , is_singleton = NULL
      , operation    = c("add", "update", "drop")
      , verbose      = TRUE
) {
   operation <- match.arg(operation)
   checkmate::assert_character(root, min.len = 1, any.missing = FALSE)
   lapply(root, assert_dir_exists)
   assert_scalar_not_empty(symlink_type)
   checkmate::assert_string(symlink_type, min.chars = 1)
   checkmate::assert_flag(verbose)
   needs_is_singleton <- operation != "drop"
   if (needs_is_singleton && is.null(is_singleton)) {
      stop(
         "operation = '", operation, "' needs is_singleton "
         , "(TRUE for singleton, FALSE for multiple-version)."
      )
   }
   if (needs_is_singleton) checkmate::assert_flag(is_singleton)
   if (is_builtin_symlink_type(symlink_type)) {
      stop(
         "Built-in symlink (shortcut) types are fixed by the package and "
         , "cannot be added, updated or dropped (you passed '", symlink_type, "')."
         , "\n    - Built-in types: best, latest, keep, remove."
         , "\n    - Nothing has been changed."
      )
   }
   roots <- clean_path(root)

   # every root is judged before any root is written
   states <- vapply(
      roots
      , get_disk_registry_state
      , character(1)
      , symlink_type = symlink_type
      , is_singleton = is_singleton
   )
   plans <- data.table::data.table(root = roots, operation = operation, state = states)
   plans <- get_disk_registry_outcomes()[plans, on = c("operation", "state")]

   stops <- plans[action == "stop", sprintf("%s: %s", root, note)]
   if (length(stops)) {
      stop(
         "Cannot ", operation, " symlink (shortcut) type '", symlink_type, "':\n  "
         , paste(stops, collapse = "\n  ")
         , "\nNothing has been changed."
      )
   }

   # links named for the is_singleton on file would belong to no registered
   # type afterwards
   roots_to_write        <- plans[action == "write", root]
   is_singleton_blocking <- switch_strict(
      operation
      , add    = logical(0)
      , update = !is_singleton
      , drop   = c(TRUE, FALSE)
   )
   blocking <- find_links_by_singleton_status(
      roots_to_write
      , symlink_type
      , is_singleton_blocking
   )
   if (nrow(blocking)) {
      n_live     <- sum(!blocking$is_dangling)
      n_dangling <- sum(blocking$is_dangling)
      n_roots    <- data.table::uniqueN(blocking$root)
      stop(
         "Cannot ", operation, " symlink (shortcut) type '", symlink_type, "': "
         , n_live, " live and ", n_dangling, " dangling link(s) of this type "
         , "exist in ", n_roots, " root(s)."
         , "\nUnmark them first (slt = a tool on these roots), then repeat "
         , "this call. Nothing has been changed.\n\n"
         , format_unmark_recipe(blocking, symlink_type, is_singleton, operation)
         , "\n\n"
         , get_disk_registry_dictionary()$help_pointer
      )
   }

   # a scalar named like the column would resolve to the column inside `[`
   .symlink_type <- symlink_type
   rows_new      <- make_empty_disk_registry()
   if (needs_is_singleton) rows_new <- make_disk_registry_row(symlink_type, is_singleton)
   for (root_to_write in roots_to_write) {
      rows_kept <- read_disk_registry(root_to_write)[symlink_type != .symlink_type]
      write_disk_registry(root_to_write, rbind(rows_kept, rows_new))
   }

   if (verbose) {
      lines <- plans[, sprintf("%s: '%s' %s", root, symlink_type, note)]
      for (line in lines) message(line)
   }
   if (verbose && operation != "drop") {
      message(
         "The live session registry is unchanged.\nTo use '", symlink_type
         , "' in this session: new_symlink_type('", symlink_type
         , "', is_singleton = ", is_singleton, ")"
      )
   }
   return(invisible(NULL))
}
