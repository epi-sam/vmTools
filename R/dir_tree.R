#' Print a directory tree to stdout
#'
#' @param path [chr] The path to the directory to print
#' @param level [int] The maximum depth to print
#' @param prefix [chr] The prefix to add to each line
#'
#' @return NULL
dir_tree <- function(path = ".", level = Inf, prefix = "") {

   if(!file.exists(path)) {
      stop("Failed to search directory ", path, ": no such file or directory")
   }
   # ANSI escape codes for colors and formatting
   green        <- "\033[32m"  # Green for files inside subdirectories
   bright_white <- "\033[97m"  # Bright white for files in the base directory
   bold_purple  <- "\033[1;35m"  # Bold purple for folders
   bold_blue    <- "\033[1;34m"  # Bold blue for top-level folders
   reset        <- "\033[0m"  # Reset to default color

   is_markdown <- knitr::is_html_output() || knitr::is_latex_output()
   if (is_markdown) {
      # Remove ANSI escape codes for markdown output - they don't render correctly
      green        <- ""
      bright_white <- ""
      bold_purple  <- ""
      bold_blue    <- ""
      reset        <- ""
   }

   files <- list.files(path, full.names = TRUE)
   N <- length(files)

   for (i in seq_along(files)) {
      is_last <- i == N
      connector <- if (is_last) "`-- " else "|-- "

      # Determine if it's a folder or file
      is_folder <- file.info(files[i])$isdir
      is_base_folder <- identical(path, ".")  # Check if it's the base folder

      # Apply color based on whether it's a folder or file
      if (is_folder) {
         # Bold blue for top-level folders, bold purple for nested folders
         colored_name <- if (is_base_folder) {
            paste0(bold_blue, basename(files[i]), reset)
         } else {
            paste0(bold_purple, basename(files[i]), reset)
         }
      } else {
         # Bright white for files in the base directory, green for files in subdirectories
         colored_name <- if (is_base_folder) {
            paste0(bright_white, basename(files[i]), reset)
         } else {
            paste0(green, basename(files[i]), reset)
         }
      }

      # Print the file or folder name with the correct color
      cat(prefix, connector, colored_name, "\n", sep = "")

      # Recurse into folders
      if (is_folder && level > 1) {
         new_prefix <- if (is_last) paste0(prefix, "   ") else paste0(prefix, "|  ")
         dir_tree(files[i], level - 1, new_prefix)
      }
   }
   return(invisible(files))
}



