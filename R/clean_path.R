#' Wrapper utility for sanitizing file.path(...) output
#'
#' @param ... [chr] paths passed to file.path()
#' @param normalize [lgl] pass path to normalizePath()?
#' @param mustWork [lgl] passed to normalizePath()
#'
#' @return [chr] full file paths with consistent platform-specific structure
#'
#' @examples
#' vmTools:::clean_path(tempdir(), "some/other/path/")
clean_path <- function(..., normalize = TRUE, mustWork = FALSE){
   pths <- file.path(...)
   if (normalize == TRUE){
      pths <- normalizePath(pths, mustWork = mustWork)
   }
   pths <- unlist(
      lapply(
         pths, function(pth) {
            pth <- gsub("\\\\", "/", pth)
            while (grepl("//", pth) == TRUE) pth <- gsub("//", "/", pth)
            pth <- sub("/$", "/", pth)
            pth
         }
      )
   )
   pths
}
