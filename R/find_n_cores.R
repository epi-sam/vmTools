#' Cross platform helper to find number of cores
#'
#' @returns [int]
#' @keywords internal
find_n_cores <- function(){
   if (.Platform$OS.type == "windows") {
      as.integer(Sys.getenv("NUMBER_OF_PROCESSORS"))
   } else if (Sys.info()[["sysname"]] == "Darwin") {  # macOS
      as.integer(system("sysctl -n hw.logicalcpu", intern = TRUE))
   } else {  # Assume Linux
      as.integer(system("nproc", intern = TRUE))
   }
}
