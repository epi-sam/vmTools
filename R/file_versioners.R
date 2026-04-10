#' If a file exists, get a new path with `v1`, `v2`, etc. appended
#'
#' For example `my_file.txt` becomes `my_file_v1.txt` if `my_file.txt` already
#' exists.  If `my_file_v1.txt` also exists, it becomes `my_file_v2.txt`, and so
#' on until a non-existing file path is found.
#'
#' If file does not exist, return the original path.
#'
#' @param outpath (chr) full path to file
#' @param increment_after_gap (lgl: default FALSE) If FALSE, will stop with an
#'   error if any gaps are detected (e.g. if v2 is missing but v1 and v3 exist,
#'   it will stop with an error about the gap in versioning and missing
#'   versions).  Default is FALSE to prevent unintentional infill of missing
#'   versions when gaps are present (from e.g. archiving). If TRUE, will
#'   continue numbering from highest version after any gaps in versioning (e.g.
#'   if v2 is missing but v1 and v3 exist, it will return v4).
#'
#' @return (chr) new path with version number appended (if necessary)
#' @export
#'
#' @examples
#' dir.create(tempdir(), recursive = TRUE, showWarnings = FALSE)
#' fname_old <- "test_file.csv"
#' file.create(file.path(tempdir(), fname_old))
#' list.files(tempdir()) # [1] "test_file.csv"
#' fname_new <- increment_file_version(file.path(tempdir(), "test_file.csv"))
#' file.create(fname_new)
#' list.files(tempdir()) # 1] "test_file_v1.csv" "test_file.csv"
#' file.remove(c(file.path(tempdir(), fname_old), fname_new))
increment_file_version <- function(outpath, increment_after_gap = FALSE){

  checkmate::assert_character(outpath, len = 1)
  checkmate::assert_flag(increment_after_gap)

  fname_og       <- basename(outpath)
  dirname_og     <- dirname(outpath)
  fname_base     <- tools::file_path_sans_ext(fname_og)
  fname_base_esc <- escape_filename_rgx(fname_base)
  fname_ext      <- tools::file_ext(fname_og)
  sep_ext        <- ifelse(nchar(fname_ext) > 0, ".", "")
  outpath_new    <- file.path(dirname_og, sprintf("%s_v1%s%s", fname_base, sep_ext, fname_ext))
  idx            <- 2

  # check for gaps in versioning (in case of deletion/archiving)
  existing_files <- list.files(dirname_og, pattern = sprintf("^%s_v[0-9]+\\%s%s$", fname_base_esc, sep_ext, fname_ext))
  existing_vers  <- sort(as.integer(gsub(sprintf("^%s_v([0-9]+)\\%s%s$", fname_base_esc, sep_ext, fname_ext), "\\1", existing_files, perl = TRUE)))
  idx_max        <- max(existing_vers, 0)
  missing_vers   <- setdiff(seq_len(idx_max), existing_vers)

  # Handle case where original file does not exist but versioned files do exist
  if(!file.exists(outpath)) {
    if(length(existing_vers) == 0){
      # "Incrementing" a brand new file should do nothing
      return(outpath)
    } else {
      stop(
        "Your original file ", fname_og, " does not exist, but there are already versioned files in the same directory: ",
        "\n   - ", toString(existing_files),
        "\n  Before incrementing a new file with the same name either:",
        "\n   - restore the original file",
        "\n   - remove the versioned files"
      )
    }
  }

  if(length(missing_vers) > 0){

    if(increment_after_gap == FALSE) {
      stop(
        "Gap in existing versions detected for file: ", fname_og,
        "\n  Missing versions: ", toString(missing_vers), " out of ", idx_max, " total.",
        "\n  Set increment_after_gap = TRUE to continue numbering from highest version after the gap."
      )
    }

    idx         <- idx_max + 1
    outpath_new <- file.path(dirname_og, sprintf("%s_v%s%s%s", fname_base, idx, sep_ext, fname_ext))

  }

  while(file.exists(outpath_new)){
    fname_new   <- sprintf("%s_v%s%s%s", fname_base, idx, sep_ext, fname_ext)
    outpath_new <- file.path(dirname_og, fname_new)
    idx         <- idx + 1
  }

  return(outpath_new)
}

