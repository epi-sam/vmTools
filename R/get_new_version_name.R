
#' get the latest index for given an output dir and a date
#'
#' directories are assumed to be named in YYYY_MM_DD.VV format with sane
#' year/month/date/version values.
#'
#' @param dir [chr] path to directory with versioned dirs
#' @param date [chr] character in YYYY_MM_DD format
#'
#' @return [int] largest version in directory tree or 0 if there are no version OR
#' the directory tree does not exist
#'
#' @examples
#' \dontrun{
#' # this function is not exported
#' vmTools:::get_latest_output_date_index("tests/testthat/fixtures/versioned-dirs/nested/1999_09_09"
#'                                        , date = "1999_09_09")
#'}
get_latest_output_date_index <- function(dir, date) {
   currentfolders <- list.files(dir)

   # subset to date
   pat <- sprintf("^%s[.]\\d{2}$", date)
   date_dirs <- grep(pat, currentfolders, value = TRUE)

   if (length(date_dirs) == 0) {
      return(0)
   }

   # get the index after day
   date_list <- strsplit(date_dirs, "[.]")

   inds <- unlist(lapply(date_list, function(x) x[2]))
   if (is.na(max(inds, na.rm = T))) inds <- 0

   return(max(as.integer(inds)))
}

#' Find the latest output directory with format YYYY_MM_DD.VV
#'
#' Used only for signaling/messaging
#'
#' @param root [chr] path to root of output results
#'
#' @return [chr] path to latest output directory
#'
#' @examples
#' vmTools:::find_latest_output_dir(tempdir())
#' \dontrun{
#' # causes an error in R CMD check
#' # this function is not exported
#' vmTools:::find_latest_output_dir("tests/testthat/fixtures/versioned-dirs/nested/1999_09_09")
#' }
find_latest_output_dir <- function(root) {
   if (!dir.exists(root)) {
      stop(sprintf("root %s does not exist", root))
   }
   raw <- list.dirs(root, full.names = FALSE, recursive = FALSE)
   valid.idx <- grep("^\\d{4}_\\d{2}_\\d{2}[.]\\d{2}$", raw)
   if (length(valid.idx) == 0) {
      # message(sprintf("No YYYY_MM_DD.VV directories in %s", root))
      return(NULL)
   } else {
      return(file.path(root, max(raw[valid.idx])))
   }
}


#' Increment a new output folder version as "YYYY_MM_DD.VV"
#'
#' Return on the date-version, not the full path.  Does not create a folder.
#'
#' @param root [chr] path to root of output results
#' @param date [chr] character date in form of "YYYY_MM_DD" or "today". "today" will be interpreted as today's date.
#'
#' @return [chr] new output version of the form "YYYY_MM_DD.VV"
#' @export
#'
#' @examples
#' get_new_version_name(root = tempdir(), date = "today") # expect "YYYY_MM_DD.01"
get_new_version_name <- function(root, date = "today"){
   if (date == "today") {
      date <- format(Sys.Date(), "%Y_%m_%d")
   } else {
      if(!grepl("^\\d{4}_\\d{2}_\\d{2}$", date)){
         stop("Invalid user_date. Must be formatted as YYYY_MM_DD.\n  ",
              "Example: 2020_01_01 \n  ",
              "Received: ", date)
      }
   }
   cur.version <- get_latest_output_date_index(root, date = date)
   dir.name <- sprintf("%s.%02i", date, cur.version + 1)
   return(dir.name)
}


