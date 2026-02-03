# started: 2025 Jan 31 13:34:16
# purpose: assist vignette displays

#' Split a character vector by line breaks
#'
#' @param string [chr] A character vector.
#'
#' @return All elements of the character vector are split by `\\n` into new elements.
#' @keywords internal
split_line_breaks = function(string) {
   if (length(grep('\n', string)) == 0L) return(string)
   string = gsub('\n$', '\n\n', string)
   string[string == ''] = '\n'
   unlist(strsplit(string, '\r?\n'))
}
