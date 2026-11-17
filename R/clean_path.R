#' Wrapper utility for sanitizing file.path(...) output
#'
#' @param ... (chr) paths passed to file.path()
#' @param normalize (lgl) pass path to normalizePath()?
#' @param mustWork (lgl) passed to normalizePath()
#'
#' @return (chr) full file paths with consistent platform-specific structure:
#'   forward slashes, no doubled slashes, no trailing slash (a bare root such
#'   as `"/"` or `"C:/"` is kept)
#'
#' @export
#'
#' @examples
#' clean_path(tempdir(), "/some/other/path/") # build a single path like file.path
#' clean_path(c(".", tempdir(), "/some/other/path/")) # vectorized
clean_path <- function(..., normalize = TRUE, mustWork = FALSE){
   pths <- file.path(...)
   if (normalize == TRUE){
      pths <- normalizePath(pths, mustWork = mustWork)
   }

   if (length(pths) > 0) { # NULL if pths is empty - keep chr return type stable
      pths <- unlist(
         lapply(
            pths, function(pth) {
               pth <- gsub("\\\\", "/", pth)
               while (grepl("//", pth) == TRUE) pth <- gsub("//", "/", pth)
               # drop a trailing slash: "best/" means "the folder behind best"
               # to the OS, so link checks would silently look through the
               # link. Keep a bare root ("/" or Windows "C:/") intact.
               if (!grepl("^(/|[A-Za-z]:/)$", pth)) pth <- sub("/$", "", pth)
               pth
            }
         )
      )
   }
   pths
}
