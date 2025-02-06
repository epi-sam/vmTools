# started: 2025 Jan 31 13:34:16
# purpose: remove xfun dependency with small util
# - used in vignettes only

#' Split a character vector by line breaks
#'
#' @param x [chr] A character vector.
#'
#' @return All elements of the character vector are split by `\\n` into lines.
#'
#' @examples
#' vmTools:::split_lines(c('a', 'b\nc'))
split_lines = function(x) {
   if (length(grep('\n', x)) == 0L) return(x)
   x = gsub('\n$', '\n\n', x)
   x[x == ''] = '\n'
   unlist(strsplit(x, '\r?\n'))
}
