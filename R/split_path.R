# purpose: Functions borrowed from DescTools to remove dependency
# - https://cran.r-project.org/web/packages/DescTools/index.html
# - none should be exported

#' Replace NAs by a user-defined string
#'
#' replaces - DescTools::BlankIfNA()
#'
#' Replace NAs in a numeric vector x with 0. This function has the same
#' logic as the zeroifnull function in SQL. NAIfZero() does replace zeros
#' with NA. BlankIfNA() and NAIfBlank() do the same, but for character
#' vectors.
#'
#' @param x [vector] whose NAs should be overwritten with 0s.
#' @param blank [chr] the value to replace NAs with.
#'
#' @return [vector] edited vector x
#'
#' @examples
#' BlankIfNA(c(1, 2, NA, 4)) # "1" "2" "" "4"
BlankIfNA = function (x, blank = "") {
   replace(x, is.na(x), blank)
}

#' Replace NAs by 0
#'
#' replaces - DescTools::ZeroIfNA()
#'
#' Replace NAs in a numeric vector x with 0. This function has the same
#' logic as the zeroifnull function in SQL. NAIfZero() does replace zeros
#' with NA. BlankIfNA() and NAIfBlank() do the same, but for character
#' vectors.
#'
#' @param x [vector] whose NAs should be overwritten with 0s.
#'
#' @return [vector] edited vector x
#'
#' @examples
#' ZeroIfNA(c(1, 2, NA, 4)) # 1 2 0 4
ZeroIfNA = function (x) {
   replace(x, is.na(x), 0L)
}

#' Extract Part of a String
#'
#' replaces - DescTools::StrExtract()
#'
#' Extract a part of a string, defined as regular expression.
#' StrExtractBetween() is a convenience function used to extract parts
#' between a left and right delimiter.
#'
#' @param x [chr] vector where matches are sought, or an object which can
#'   be coerced by as.character to a character vector.
#' @param pattern [chr]  string containing a regular expression (or
#'   character string for fixed = TRUE) to be matched in the given
#'   character vector. Coerced by as.character to a character string if
#'   possible. If a character vector of length 2 or more is supplied, the
#'   first element is used with a warning. Missing values are not allowed.
#' @param ... the dots are passed to the the internally used function
#'   regexpr(), which allows to use e.g. Perl-like regular expressions.
#'
#' @return [chr] A character vector.
#'
#' @examples
#' StrExtract("abc123def", "[0-9]+") # "123"
StrExtract = function (x, pattern, ...) {
   m                    <- regexpr(pattern, x, ...)
   regmatches(x, m)
   res                  <- rep(NA_character_, length(m))
   res[ZeroIfNA(m) > 0] <- regmatches(x, m)
   return(res)
}

#' Split Path In Drive, Path, Filename
#'
#' replaces - DescTools::SplitPath()
#'
#' Split a full path in its components. This is specifically an issue in
#' Windows and not really interesting for other OSs.
#'
#' @param path [chr] a path
#' @param last.is.file [lgl] logical, determining if the basename should be
#' interpreted as filename or as last directory. If set to NULL
#' (default), the last entry will be interpreted if the last character is
#' either \ or / and as filename else.
#'
#' @return [list] with components:
#'  - normpath: normalized path
#'  - drive: drive letter
#'  - dirname: directory name
#'  - fullfilename: full filename
#'  - fullpath: full path
#'  - filename: filename
#'  - extension: extension
#'
#'  If last.is.file is FALSE, dirname, filename, extension and fullfilename
#'  will be NA.
#'
#'  @examples
#'  split_path("C:/Users/JohnDoe/Documents/MyFile.txt")
split_path = function(path, last.is.file = NULL) {
   if (is.null(last.is.file)) {
      last.is.file <- (length(grep(pattern = "[/\\]$", path)) == 0)
   }

   path         <- normalizePath(path, mustWork = FALSE)
   lst          <- list()
   lst$normpath <- path

   if (.Platform$OS.type == "windows") {
      lst$drive   <- regmatches(path, regexpr("^([[:alpha:]]:)|(\\\\[[:alnum:]]+)",path))
      lst$dirname <- gsub(pattern = lst$drive, x = dirname(path), replacement = "")
   } else {
      lst$drive   <- NA
      lst$dirname <- dirname(path)
   }

   lst$dirname      <- paste(lst$dirname, "/", sep = "")
   lst$fullfilename <- basename(path)
   lst$fullpath     <- paste0(BlankIfNA(lst$drive), lst$dirname)
   lst$filename     <- gsub(pattern = "(.*)\\.(.*)$", "\\1", lst$fullfilename)
   lst$extension    <- StrExtract(pattern = "(?<=\\.)[^\\.]+$", lst$fullfilename, perl = TRUE)

   if (!last.is.file) {
      lst$dirname <- paste(lst$dirname, lst$fullfilename, "/", sep = "")
      lst$extension <- lst$filename <- lst$fullfilename <- NA
   }

   return(lst)
}
