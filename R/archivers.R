
#' Archive a directory to a time-stamped location
#'
#' Moves or copies \code{current_dir} as a whole into a time-stamped
#' subdirectory inside \code{archive_dir}. The archived directory is named
#' after the original basename with a timestamp appended,
#' e.g. \code{.../archive/results_20260409_143022/}.
#' When \code{copy = FALSE} (default), the original \code{current_dir} is
#' removed. When \code{copy = TRUE}, the original is preserved.
#'
#' \strong{Requires:} \code{zip} utility available (e.g. \code{/usr/bin/zip}) when
#' \code{method = "zip"}.
#' \strong{Note:} \code{zip} does not preserve file permissions -- permissions are
#' set to \code{"775"}.
#'
#' @param current_dir (path) Directory to archive.
#' @param archive_dir (chr) Directory to archive into. Defaults to an
#'   \code{"archive"} folder next to \code{current_dir}.
#' @param method (chr) Archive method: \code{"subfolder"} (default) or \code{"zip"}.
#' @param copy (lgl) If \code{TRUE}, the original directory is preserved. If
#'   \code{FALSE} (default), it is removed.
#' @param verbose (lgl) If \code{TRUE}, progress messages are printed. Default \code{FALSE}.
#'
#' @return (chr) Path to the archived directory or zip, invisibly.
#' @family archivers
#' @keywords internal
archive_directory <- function(current_dir, archive_dir = file.path(dirname(current_dir), "archive"), method = "subfolder", copy = FALSE, verbose = FALSE) {

  if(!dir.exists(current_dir)) { message(current_dir, " does not exist, exiting."); return(invisible(NULL)) }
  checkmate::assert_choice(method, c("subfolder", "zip"))

  dir_basename <- basename(current_dir)
  archived_dir <- file.path(archive_dir, paste0(dir_basename, format(Sys.time(), "_%Y%m%d_%H%M%S")))
  dir.create(archive_dir, showWarnings = FALSE, recursive = TRUE)

  if (method == "subfolder") {

    if (verbose) message("Archiving directory:\n     from: ", current_dir, "\n     to:   ", archived_dir)
    if (copy == FALSE) {
      success <- file.rename(current_dir, archived_dir)
    } else {
      file.copy(current_dir, archive_dir, recursive = TRUE)
      success <- file.rename(file.path(archive_dir, dir_basename), archived_dir)
    }
    if (!success) stop("Failed to archive directory - see:\n     ", current_dir, "\n     ", archived_dir)

  } else if (method == "zip") {

    archived_dir <- paste0(archived_dir, ".zip")
    if (verbose) message("Archiving directory:\n     from: ", current_dir, "\n     to:   ", archived_dir)
    which_path_status <- system("which zip", intern = TRUE)
    if (!is.null(attr(which_path_status, "status"))) stop("The 'zip' utility is not installed on this system.")
    if (copy == FALSE) { flags <- "-rm" } else { flags <- "-r" }
    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(dirname(current_dir))
    status_code <- zip(zipfile = archived_dir, files = dir_basename, flags = flags)
    if (verbose) message("Setting archive permissions to 775.")
    Sys.chmod(archived_dir, "775", use_umask = FALSE)
    if (!status_code %in% c(0)) stop("Failed to archive directory - see:\n     ", current_dir, "\n     ", archived_dir)

  }

  return(invisible(archived_dir))
}


#' Unarchive a directory from an archive to its original location
#'
#' Moves the entire archived directory out of \code{archive/} and into
#' \code{restore_dir}, stripping the timestamp suffix to recover the original
#' directory name, e.g. \code{model_subdir_20260409_143022} becomes
#' \code{model_subdir}. Accepts either a timestamped subfolder or a
#' \code{.zip} file produced by \code{archive_directory()}.
#'
#' The default \code{restore_dir} (one level above the archive directory) only
#' resolves correctly if the directory was archived using \code{archive_directory()}
#' with its default \code{archive_dir}.
#'
#' \strong{Requires:} \code{unzip} utility available on the system when
#' \code{archived_dir} is a \code{.zip} file.
#'
#' @param archived_dir (chr) Path to the archive (subfolder or \code{.zip}).
#'   Must contain \code{"/archive/"} in its path.
#' @param restore_dir (chr) The directory to unarchive into. Defaults to
#'   the parent of the archive directory.
#' @param verbose (lgl) If \code{TRUE}, progress messages are printed. Default \code{FALSE}.
#'
#' @return (chr) Path to the restored directory, invisibly.
#' @family archivers
#' @keywords internal
unarchive_directory <- function(archived_dir, restore_dir = dirname(dirname(archived_dir)), verbose = FALSE) {

  if(!grepl("/archive/", archived_dir)) stop("This function is only for paths containing '/archive/' in the path")
  if(!file.exists(archived_dir) && !dir.exists(archived_dir)) { if (verbose) message(archived_dir, " does not exist, exiting."); return(invisible(NULL)) }
  if(!dir.exists(restore_dir)) { if (verbose) message(restore_dir, " does not exist, exiting."); return(invisible(NULL)) }

  is_zip <- grepl("\\.zip$", archived_dir, ignore.case = TRUE)

  if (!is_zip) {

    dir_name      <- basename(archived_dir)
    original_name <- sub("_\\d{8}_\\d{6}$", "", dir_name)
    dest_dir      <- file.path(restore_dir, original_name)
    if (dir.exists(dest_dir)) stop("Destination already exists, aborting to prevent data loss:\n     ", dest_dir)
    if (verbose) message("Unarchiving directory:\n     from: ", archived_dir, "\n     to:   ", dest_dir)
    success <- file.rename(archived_dir, dest_dir)
    if (!success) stop("Failed to unarchive directory:\n     from: ", archived_dir, "\n     to:   ", dest_dir)

  } else {

    which_path_status <- system("which unzip", intern = TRUE)
    if (!is.null(attr(which_path_status, "status"))) stop("The 'unzip' utility is not installed on this system.")
    zip_name      <- sub("\\.zip$", "", basename(archived_dir), ignore.case = TRUE)
    original_name <- sub("_\\d{8}_\\d{6}$", "", zip_name)
    dest_dir      <- file.path(restore_dir, original_name)
    if (dir.exists(dest_dir)) stop("Destination already exists, aborting to prevent data loss:\n     ", dest_dir)
    if (verbose) message("Unarchiving directory:\n     from: ", archived_dir, "\n     to:   ", dest_dir)
    utils::unzip(zipfile = archived_dir, exdir = restore_dir)

  }

  return(invisible(dest_dir))
}


#' Archive a single file to an archive directory
#'
#' The archived file name is the original file name with a timestamp appended,
#' e.g. \code{.../archive/file_20260407_143022.ext} or
#' \code{.../archive/file_20260407_143022.zip} when \code{method = "zip"}.
#'
#' \strong{Requires:} \code{zip} utility available (e.g. \code{/usr/bin/zip}) when
#' \code{method = "zip"}.
#' \strong{Note:} \code{zip} does not preserve file permissions -- permissions are
#' set to \code{"775"}.
#'
#' @param current_path (chr) Full path (with filename and extension) of the file
#'   to archive.
#' @param archive_dir (chr) Directory to archive into. Defaults to an
#'   \code{"archive"} subfolder next to \code{current_path}.
#' @param copy (lgl) If \code{TRUE}, the original is copied (kept). If
#'   \code{FALSE} (default), the original is moved (removed).
#' @param method (chr) Archive method: \code{"file"} (default) or \code{"zip"}.
#' @param verbose (lgl) If \code{TRUE}, progress messages are printed. Default \code{FALSE}.
#'
#' @return (chr) Full path to the archived file (or zip), invisibly.
#' @family archivers
#' @keywords internal
archive_file <- function(current_path, archive_dir = file.path(dirname(current_path), "archive"), copy = FALSE, method = "file", verbose = FALSE) {

  if(!file.exists(current_path)) { message(current_path, " does not exist, exiting."); return(invisible(NULL)) }
  checkmate::assert_choice(method, c("file", "zip"))

  dir.create(archive_dir, showWarnings = FALSE, recursive = TRUE)

  file_name     <- basename(current_path)
  ext           <- paste0(".", tools::file_ext(file_name))
  file_basename <- tools::file_path_sans_ext(file_name)

  if (method == "file") {

    path_archive <- file.path(archive_dir, paste0(file_basename, format(Sys.time(), "_%Y%m%d_%H%M%S"), ext))
    if (verbose) message("Archiving file:\n     from: ", current_path, "\n     to:   ", path_archive)
    if (copy == TRUE) {
      file.copy(current_path, path_archive)
    } else {
      file.rename(current_path, path_archive)
    }

  } else if (method == "zip") {

    which_path_status <- system("which zip", intern = TRUE)
    if (!is.null(attr(which_path_status, "status"))) stop("The 'zip' utility is not installed on this system.")
    path_archive <- file.path(archive_dir, paste0(file_basename, format(Sys.time(), "_%Y%m%d_%H%M%S"), ".zip"))
    if (verbose) message("Archiving file:\n     from: ", current_path, "\n     to:   ", path_archive)
    if (copy == FALSE) { flags <- "-jm" } else { flags <- "-j" }
    status_code <- zip(zipfile = path_archive, files = current_path, flags = flags)
    if (verbose) message("Setting archive file permissions to 775.")
    Sys.chmod(path_archive, "775", use_umask = FALSE)
    if (!status_code %in% c(0)) stop("Failed to archive file - see:\n     ", current_path, "\n     ", path_archive)

  }

  return(invisible(path_archive))
}


#' Unarchive a single file from an archive directory
#'
#' Strips the timestamp suffix from the archived filename to recover the
#' original name, then moves the file to \code{restore_dir}. If the archived
#' file is a \code{.zip}, it is extracted using \code{utils::unzip()}.
#'
#' The default \code{restore_dir} (one level above the archive directory) only
#' resolves correctly if the file was archived using \code{archive_file()} with
#' its default \code{archive_dir}.
#'
#' \strong{Requires:} \code{unzip} utility available on the system when
#' the archived file is a \code{.zip}.
#'
#' @param archived_path (chr) Full path to the archived file (or \code{.zip}).
#'   Must contain \code{"/archive/"} in the path.
#' @param restore_dir (chr) Directory to unarchive the file into. Defaults to
#'   the parent of the archive directory.
#' @param verbose (lgl) If \code{TRUE}, progress messages are printed. Default \code{FALSE}.
#'
#' @return (chr) Full path to the restored file or directory, invisibly.
#' @family archivers
#' @keywords internal
unarchive_file <- function(archived_path, restore_dir = dirname(dirname(archived_path)), verbose = FALSE) {

  if(!file.exists(archived_path)) stop("The file to unarchive does not exist: ", archived_path)
  if(!grepl("/archive/", archived_path)) stop("This function is only for files within an 'archive' directory")
  if(!dir.exists(restore_dir)) { if (verbose) message(restore_dir, " does not exist, exiting."); return(invisible(NULL)) }

  is_zip <- grepl("\\.zip$", archived_path, ignore.case = TRUE)

  if (!is_zip) {

    file_name     <- basename(archived_path)
    ext           <- paste0(".", tools::file_ext(file_name))
    file_basename <- tools::file_path_sans_ext(file_name)
    original_base <- sub("_\\d{8}_\\d{6}$", "", file_basename)
    original_name <- paste0(original_base, ext)
    path_restore  <- file.path(restore_dir, original_name)
    if (file.exists(path_restore)) stop("Destination already exists, aborting to prevent data loss:\n     ", path_restore)
    if (verbose) message("Unarchiving file:\n     from: ", archived_path, "\n     to:   ", path_restore)
    success       <- file.rename(from = archived_path, to = path_restore)
    if (!success) stop("Failed to unarchive file:\n     from: ", archived_path, "\n     to:   ", path_restore)

  } else {

    which_path_status <- system("which unzip", intern = TRUE)
    if (!is.null(attr(which_path_status, "status"))) stop("The 'unzip' utility is not installed on this system.")
    zip_contents <- utils::unzip(archived_path, list = TRUE)$Name
    conflicts    <- zip_contents[file.exists(file.path(restore_dir, zip_contents))]
    if (length(conflicts) > 0) stop("Destination already exists, aborting to prevent data loss:\n     ", paste(file.path(restore_dir, conflicts), collapse = "\n     "))
    if (verbose) message("Unarchiving file:\n     from: ", archived_path, "\n     to:   ", restore_dir)
    utils::unzip(zipfile = archived_path, exdir = restore_dir)
    path_restore <- restore_dir

  }

  return(invisible(path_restore))
}


#' Archive a file or directory to a time-stamped location
#'
#' Dispatches to \code{archive_file()} or \code{archive_directory()} based on
#' whether \code{current_path} is a file or a directory.
#'
#' @param current_path (chr) Path to the file or directory to archive.
#' @param archive_dir (chr) Directory to archive into. Defaults to an
#'   \code{"archive"} folder next to \code{current_path}.
#' @param method (chr) Archive method. For files: \code{"file"} (default) or
#'   \code{"zip"}. For directories: \code{"subfolder"} (default) or \code{"zip"}.
#' @param copy (lgl) If \code{TRUE}, the original is preserved. If \code{FALSE}
#'   (default), it is removed.
#' @param verbose (lgl) If \code{TRUE}, progress messages are printed. Default \code{FALSE}.
#'
#' @return (chr) Path to the archived file or directory, invisibly.
#' @family archivers
#' @export
archive <- function(current_path, archive_dir = file.path(dirname(current_path), "archive"), method = NULL, copy = FALSE, verbose = FALSE) {

  checkmate::assert_choice(method, c("file", "zip", "subfolder"), null.ok = TRUE)

  if (file.exists(current_path) && !dir.exists(current_path)) {
    method <- if (is.null(method)) "file" else method
    archive_file(current_path = current_path, archive_dir = archive_dir, copy = copy, method = method, verbose = verbose)
  } else if (dir.exists(current_path)) {
    method <- if (is.null(method)) "subfolder" else method
    archive_directory(current_dir = current_path, archive_dir = archive_dir, method = method, copy = copy, verbose = verbose)
  } else {
    stop("Path does not exist: ", current_path)
  }
}


#' Unarchive a file or directory from an archive location
#'
#' Dispatches to \code{unarchive_file()} or \code{unarchive_directory()} based
#' on whether \code{archived_path} is a file or a directory (or \code{.zip}).
#' The path must contain \code{"/archive/"}.
#'
#' @param archived_path (chr) Path to the archived file or directory (or \code{.zip}).
#'   Must contain \code{"/archive/"} in the path.
#' @param restore_dir (chr) Directory to restore into. Defaults to the parent of
#'   the archive directory.
#' @param verbose (lgl) If \code{TRUE}, progress messages are printed. Default \code{FALSE}.
#'
#' @return (chr) Path to the restored file or directory, invisibly.
#' @family archivers
#' @export
unarchive <- function(archived_path, restore_dir = dirname(dirname(archived_path)), verbose = FALSE) {

  if (dir.exists(archived_path)) {
    unarchive_directory(archived_dir = archived_path, restore_dir = restore_dir, verbose = verbose)
  } else if (file.exists(archived_path)) {
    is_zip     <- grepl("\\.zip$", archived_path, ignore.case = TRUE)
    looks_dir  <- grepl("_\\d{8}_\\d{6}\\.zip$", archived_path)
    if (is_zip && looks_dir) {
      unarchive_directory(archived_dir = archived_path, restore_dir = restore_dir, verbose = verbose)
    } else {
      unarchive_file(archived_path = archived_path, restore_dir = restore_dir, verbose = verbose)
    }
  } else {
    stop("Path does not exist: ", archived_path)
  }
}
