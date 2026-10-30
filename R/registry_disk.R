# ---- Disk registry ----------------------------------------------------------
# The session registry (R/registry_session.R) forgets every user-defined type when R restarts.
# Each root carries a small file recording the types ever used there, so a new
# tool on that root starts with the same types (see `SLT$new()`), and typed
# roundups work without re-registering every session.
#
# - one row per symlink type: current state, not history
# - built-in types are never written: they can never be absent from a session
# - a file edit never changes the live session registry; `new_symlink_type()` does

# file name at the top of each root; a dotfile so `list.files()` without
# `all.files` skips it. It must stay a FILE: `link_list()` lists every folder
# in a root, so a dot folder would be reported as a version folder
.disk_registry_fname <- ".symlink_registry.csv"

# column names and types; `timestamp` and `user` match the log schema
.disk_registry_schema <- list(
    symlink_type      = "character"
    , is_singleton    = "logical"
    , timestamp       = "character"
    , user            = "character"
    , vmtools_version = "character"
)

# same shape as the tool's `datestamp_format`; kept here because the disk
# registry is written outside the SLT class
.disk_registry_timestamp_format <- "%Y_%m_%d_%H%M%S"


#' Path of a root's disk registry file
#'
#' @param root (chr) one root folder
#'
#' @returns (chr) the file path, whether or not the file exists
#' @keywords internal
get_disk_registry_path <- function(root) {
    assert_scalar_not_empty(root)
    return(clean_path(root, .disk_registry_fname, normalize = FALSE))
}


#' An empty disk registry table with the right columns and types
#'
#' @returns (data.table) zero rows, columns per `.disk_registry_schema`
#' @keywords internal
make_empty_disk_registry <- function() {
    cols <- lapply(.disk_registry_schema, function(type) vector(type, 0L))
    return(data.table::as.data.table(cols))
}


#' Read one root's disk registry
#'
#' A missing file reads as an empty table. A file whose columns do not match
#' the schema, or that lists one type with two `is_singleton` values, stops:
#' both mean the file was edited by hand.
#'
#' @param root (chr) one root folder
#'
#' @returns (data.table) one row per symlink type, columns per
#'   `.disk_registry_schema`, sorted by `symlink_type`
#' @keywords internal
read_disk_registry <- function(root) {
    fpath <- get_disk_registry_path(root)
    if (!file.exists(fpath)) return(make_empty_disk_registry())

    dt <- data.table::setDT(utils::read.csv(
        fpath
        , colClasses       = unlist(.disk_registry_schema)
        , stringsAsFactors = FALSE
    ))
    if (!identical(sort(names(dt)), sort(names(.disk_registry_schema)))) {
        stop(
            "The symlink (shortcut) type registry file has unexpected columns: ", fpath
            , "\n    - Expected: ", toString(names(.disk_registry_schema))
            , "\n    - Found:    ", toString(names(dt))
            , "\n    - Was it edited by hand? Fix or delete the file. Nothing has been changed."
        )
    }
    data.table::setcolorder(dt, names(.disk_registry_schema))
    dt <- unique(dt, by = c("symlink_type", "is_singleton"))
    dup <- dt[duplicated(symlink_type), symlink_type]
    if (length(dup)) {
        stop(
            "The symlink (shortcut) type registry file lists a type as both singleton and multiple-version: ", fpath
            , "\n    - Type(s): ", toString(dup)
            , "\n    - Was it edited by hand? Fix or delete the file. Nothing has been changed."
        )
    }
    data.table::setorder(dt, symlink_type)
    return(dt)
}


#' Write one root's disk registry
#'
#' Atomic: temp file in the same folder, then rename, so a parallel reader sees
#' the old file or the new one, never a partial one (same rule as the tool's
#' log writer). Existing permissions are kept.
#'
#' @param root (chr) one root folder
#' @param dt (data.table) the table to write, columns per `.disk_registry_schema`
#'
#' @returns (chr) the file path, invisibly
#' @keywords internal
write_disk_registry <- function(root, dt) {
    fpath     <- get_disk_registry_path(root)
    fpath_tmp <- tempfile(pattern = paste0(.disk_registry_fname, "."), tmpdir = dirname(fpath), fileext = ".tmp")
    on.exit(if (file.exists(fpath_tmp)) unlink(fpath_tmp), add = TRUE)

    dt <- data.table::copy(dt)
    data.table::setcolorder(dt, names(.disk_registry_schema))
    data.table::setorder(dt, symlink_type)
    utils::write.csv(dt, fpath_tmp, row.names = FALSE)

    if (file.exists(fpath)) {
        Sys.chmod(fpath_tmp, mode = file.mode(fpath), use_umask = FALSE)
    }
    if (!isTRUE(file.rename(fpath_tmp, fpath))) {
        stop(
            "Could not write the symlink (shortcut) type registry file: ", fpath
            , "\n    - The original file (if any) was left untouched."
            , "\n    - Common causes: no write permission on the folder, the file is locked, or the disk is full."
        )
    }
    return(invisible(fpath))
}


#' One disk registry row for a symlink type
#'
#' @param symlink_type (chr) the type
#' @param is_singleton (lgl) singleton or multiple-version
#'
#' @returns (data.table) one row, columns per `.disk_registry_schema`
#' @keywords internal
make_disk_registry_row <- function(symlink_type, is_singleton) {
    return(data.table::data.table(
        symlink_type      = symlink_type
        , is_singleton    = is_singleton
        , timestamp       = format(Sys.time(), .disk_registry_timestamp_format)
        , user            = Sys.info()[["user"]]
        , vmtools_version = as.character(utils::packageVersion("vmTools"))
    ))
}


#' Add a symlink type to a root's disk registry if it is not there yet
#'
#' The automatic path: called when a type is first promoted in a root. A
#' built-in type, or a type already on file with the same `is_singleton`, is a
#' no-op. A type on file with the other `is_singleton` stops: the session and
#' the root disagree about the link shape, and only
#' `update_symlink_disk_registry()` may change that.
#'
#' @param root (chr) one root folder
#' @param symlink_type (chr) the type being promoted
#'
#' @returns (lgl) TRUE invisibly if a row was written, FALSE invisibly otherwise
#' @keywords internal
record_symlink_type_on_disk <- function(root, symlink_type) {
    if (is_builtin_symlink_type(symlink_type)) return(invisible(FALSE))
    is_singleton <- isTRUE(get_symlink_registry_entry(symlink_type)[["is_singleton"]])
    dt           <- read_disk_registry(root)
    # a scalar named like the column would resolve to the column inside `[`
    type_wanted  <- symlink_type
    on_file      <- dt[symlink_type == type_wanted]
    if (nrow(on_file) == 1L) {
        if (identical(on_file$is_singleton, is_singleton)) return(invisible(FALSE))
        stop(
            "The live session registry has the symlink (shortcut) type '", symlink_type, "' as "
            , describe_singleton(is_singleton), ", but the root's registry file says "
            , describe_singleton(on_file$is_singleton), ": ", get_disk_registry_path(root)
            , "\n    - Links of this type in that root are named the other way."
            , "\n    - To change the root's registration: update_symlink_disk_registry(root, '", symlink_type
            , "', is_singleton = ", is_singleton, ", operation = 'update')"
            , "\n    - To follow the root instead: new_symlink_type('", symlink_type, "', is_singleton = ", on_file$is_singleton, ")"
            , "\n    - Nothing has been changed."
        )
    }
    write_disk_registry(root, rbind(dt, make_disk_registry_row(symlink_type, is_singleton)))
    return(invisible(TRUE))
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
    return(tolower(symlink_type) %in% c("best", "latest", "keep", "remove"))
}


#' Words for `is_singleton` in messages
#'
#' @param is_singleton (lgl)
#'
#' @returns (chr) "singleton" or "multiple-version"
#' @keywords internal
describe_singleton <- function(is_singleton) {
    if (isTRUE(is_singleton)) return("singleton")
    return("multiple-version")
}


#' Read the disk registries of several roots and register what the session lacks
#'
#' Used by `SLT$new()`. Every type found on file is registered for the session
#' with `new_symlink_type()`. Stops if two roots, or a root and the session,
#' disagree about a type's `is_singleton`.
#'
#' @param root_list (chr or list) root folders
#' @param verbose (lgl) message the newly registered types as a small table
#'
#' @returns (data.table) the rows that were newly registered, invisibly
#'   (zero rows when the session already had them all)
#' @keywords internal
register_symlink_types_from_disk <- function(root_list, verbose = TRUE) {
    checkmate::assert_flag(verbose)
    roots <- unlist(root_list, use.names = FALSE)
    dt    <- data.table::rbindlist(lapply(roots, function(root) {
        out <- read_disk_registry(root)
        if (nrow(out)) out[, root := root]
        return(out)
    }), fill = TRUE)
    if (nrow(dt) == 0L) return(invisible(make_empty_disk_registry()))

    # two roots disagree
    conflicts <- dt[, .(n_shapes = data.table::uniqueN(is_singleton)), by = symlink_type][n_shapes > 1L, symlink_type]
    if (length(conflicts)) {
        detail <- dt[symlink_type %in% conflicts, sprintf("%s: %s in %s", symlink_type, vapply(is_singleton, describe_singleton, character(1)), root)]
        stop(
            "Root folders disagree about a symlink (shortcut) type's shape:"
            , "\n    - ", paste(detail, collapse = "\n    - ")
            , "\n    - Fix one side with update_symlink_disk_registry(). Nothing has been changed."
        )
    }
    dt <- unique(dt, by = "symlink_type")

    # a root disagrees with the session
    registered <- get_symlink_registry()
    for (idx in seq_len(nrow(dt))) {
        type <- dt$symlink_type[idx]
        if (!type %in% names(registered)) next
        session_shape <- isTRUE(registered[[type]][["is_singleton"]])
        if (!identical(session_shape, dt$is_singleton[idx])) {
            stop(
                "The live session registry has the symlink (shortcut) type '", type, "' as ", describe_singleton(session_shape)
                , ", but a root's registry file says ", describe_singleton(dt$is_singleton[idx]), ": ", get_disk_registry_path(dt$root[idx])
                , "\n    - Fix one side: new_symlink_type() for the session, update_symlink_disk_registry() for the root."
                , "\n    - Nothing has been changed."
            )
        }
    }

    new_dt <- dt[!symlink_type %in% names(registered)]
    for (idx in seq_len(nrow(new_dt))) {
        new_symlink_type(new_dt$symlink_type[idx], is_singleton = new_dt$is_singleton[idx], verbose = FALSE)
    }
    if (verbose && nrow(new_dt)) {
        message(
            "Registered symlink (shortcut) types found in the root folders:\n"
            , format_disk_registry_table(new_dt)
        )
    }
    new_dt[, root := NULL]
    return(invisible(new_dt))
}


#' Format symlink types and their shape as a small two-column table for messages
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


#' Links in a root whose name follows a given type and shape
#'
#' @param root (chr) one root folder
#' @param symlink_type (chr) the type
#' @param is_singleton (lgl) which shape to look for
#'
#' @returns (data.table) `link_list()` rows plus `version_name` (chr) the
#'   target's leaf, or the link's leaf when the target is unknown
#' @keywords internal
find_links_of_shape <- function(root, symlink_type, is_singleton) {
    links <- link_list(root)[is_link == TRUE]
    links <- links[grepl(make_extract_regex(is_singleton, symlink_type), leaf)]
    links[, version_name := leaf]
    links[!is.na(target) & nzchar(target), version_name := basename(target)]
    return(links)
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
#' The live session registry is never changed by this function. To use a type in
#' the current session, call `new_symlink_type()` or construct a new tool.
#'
#' Changing a type's shape, or dropping it, is refused while links of the old
#' shape exist in a root: afterwards those links would belong to no registered
#' type, so roundups would skip them and `delete_version_folders()` would
#' delete them without a log. The error prints the `unmark()` calls needed
#' first, with the affected version names.
#'
#' @param root (chr) one or more root folders; each gets the same change
#' @param symlink_type (chr) the type; built-in types (best, latest, keep,
#'   remove) are refused
#' @param is_singleton (lgl) required for `add` and `update`; ignored by
#'   `drop`, which goes by name alone
#' @param operation (chr) `"add"` a type not yet on file; `"update"` the shape
#'   of a type on file; `"drop"` a type from the file
#' @param verbose (lgl: default `TRUE`) message what changed in each root
#'
#' @returns (invisible NULL) called for the side effect of writing each root's
#'   registry file
#' @export
#'
#' @examples
#' \dontrun{
#' update_symlink_disk_registry("/path/to/root", "stalled", is_singleton = FALSE, operation = "add")
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
    if (operation != "drop") {
        if (is.null(is_singleton)) stop("operation = '", operation, "' needs is_singleton (TRUE for singleton, FALSE for multiple-version).")
        checkmate::assert_flag(is_singleton)
    }
    if (is_builtin_symlink_type(symlink_type)) {
        stop(
            "Built-in symlink (shortcut) types are never on file (you passed '", symlink_type, "')."
            , "\n    - Built-in types: best, latest, keep, remove."
            , "\n    - Nothing has been changed."
        )
    }
    roots <- clean_path(root)
    # a scalar named like the column would resolve to the column inside `[`
    type_wanted <- symlink_type

    # pass 1: decide per root; stop before writing to any root if one would stop
    plans <- lapply(roots, function(one_root) {
        dt      <- read_disk_registry(one_root)
        on_file <- dt[symlink_type == type_wanted]
        plan    <- list(root = one_root, dt = dt, write = FALSE, stop = NULL, note = NULL, blocking = NULL)

        if (operation == "add") {
            if (nrow(on_file) == 0L) {
                plan$write <- TRUE
            } else if (identical(on_file$is_singleton, is_singleton)) {
                plan$note <- "already on file with that shape; nothing changed"
            } else {
                plan$stop <- sprintf(
                    "'%s' is already on file as %s in %s; use operation = 'update' to change its shape"
                    , symlink_type, describe_singleton(on_file$is_singleton), one_root
                )
            }
        } else if (operation == "update") {
            if (nrow(on_file) == 0L) {
                plan$stop <- sprintf("'%s' is not on file in %s; use operation = 'add'", symlink_type, one_root)
            } else if (identical(on_file$is_singleton, is_singleton)) {
                plan$note <- "already on file with that shape; nothing changed"
            } else {
                plan$blocking <- find_links_of_shape(one_root, symlink_type, on_file$is_singleton)
                plan$write    <- TRUE
            }
        } else {
            if (nrow(on_file) == 0L) {
                plan$note <- "not on file; nothing to drop"
            } else {
                plan$blocking <- rbind(
                    find_links_of_shape(one_root, symlink_type, TRUE)
                    , find_links_of_shape(one_root, symlink_type, FALSE)
                )
                plan$write <- TRUE
            }
        }
        return(plan)
    })

    stops <- unlist(lapply(plans, `[[`, "stop"))
    if (length(stops)) {
        stop(
            "Cannot ", operation, " the symlink (shortcut) type:"
            , "\n    - ", paste(stops, collapse = "\n    - ")
            , "\n    - Nothing has been changed."
        )
    }

    blocking <- data.table::rbindlist(lapply(plans, function(plan) {
        if (is.null(plan$blocking) || nrow(plan$blocking) == 0L) return(NULL)
        return(plan$blocking[, root := plan$root])
    }))
    if (nrow(blocking)) {
        old_shape <- if (operation == "update") describe_singleton(!is_singleton) else "either shape"
        stop(
            "Cannot ", operation, " the symlink (shortcut) type '", symlink_type, "': links of the current shape still exist."
            , "\n    - Afterwards these links would belong to no registered type: roundups would skip them and"
            , " delete_version_folders() would delete them without a log."
            , "\n    - Links (", old_shape, "):"
            , "\n        ", paste(sprintf("%s -> %s", blocking$path, blocking$target), collapse = "\n        ")
            , "\n    - Run this first, where `slt` is a tool built on these roots, then repeat this call:"
            , "\n"
            , format_unmark_recipe(unique(blocking$version_name), roots, symlink_type, is_singleton, operation)
            , "\n    - Nothing has been changed."
        )
    }

    # pass 2: write
    for (plan in plans) {
        if (plan$write) {
            dt <- plan$dt[symlink_type != type_wanted]
            if (operation != "drop") dt <- rbind(dt, make_disk_registry_row(symlink_type, is_singleton))
            write_disk_registry(plan$root, dt)
            plan$note <- sprintf("%s done", operation)
        }
        if (verbose) message(sprintf("%s: '%s' %s", plan$root, symlink_type, plan$note))
    }
    if (verbose && operation != "drop") {
        message(
            "The live session registry is unchanged. To use '", symlink_type, "' in this session: new_symlink_type('"
            , symlink_type, "', is_singleton = ", is_singleton, ")"
        )
    }
    return(invisible(NULL))
}


#' The unmark / re-register / re-mark recipe printed when a registry change is refused
#'
#' @param version_names (chr) versions carrying links of the old shape
#' @param roots (chr) the roots passed by the caller
#' @param symlink_type (chr) the type
#' @param is_singleton (lgl or NULL) the requested shape (NULL for drop)
#' @param operation (chr) `"update"` or `"drop"`
#'
#' @returns (chr) one string of R code lines, indented for a message
#' @keywords internal
format_unmark_recipe <- function(version_names, roots, symlink_type, is_singleton, operation) {
    quote_vec <- function(x) {
        if (length(x) == 1L) return(sprintf('"%s"', x))
        return(sprintf("c(%s)", paste(sprintf('"%s"', x), collapse = ", ")))
    }
    comment <- sprintf('list(comment = "%s %s")', symlink_type, if (operation == "drop") "retired" else "shape changed")
    unmark  <- if (length(version_names) == 1L) {
        sprintf('slt$unmark(%s, user_entry = %s)', quote_vec(version_names), comment)
    } else {
        sprintf('for (v in %s) slt$unmark(v, user_entry = %s)', quote_vec(version_names), comment)
    }
    this_call <- if (operation == "drop") {
        sprintf('update_symlink_disk_registry(%s, "%s", operation = "drop")', quote_vec(roots), symlink_type)
    } else {
        sprintf('update_symlink_disk_registry(%s, "%s", is_singleton = %s, operation = "update")', quote_vec(roots), symlink_type, is_singleton)
    }
    lines <- c(unmark, this_call)
    if (operation == "update") {
        lines <- c(
            lines
            , sprintf('new_symlink_type("%s", is_singleton = %s)', symlink_type, is_singleton)
            , if (isTRUE(is_singleton)) {
                sprintf('slt$mark("%s", "<the one version to keep>", user_entry = %s)', symlink_type, comment)
            } else {
                sprintf('for (v in %s) slt$mark("%s", v, user_entry = %s)', quote_vec(version_names), symlink_type, comment)
            }
        )
    }
    return(paste0("        ", lines, collapse = "\n"))
}
