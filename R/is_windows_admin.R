#' Is the current OS windows
#'
#' @returns [lgl]
#' @keywords internal
is_windows <- function(){
   .Platform$OS.type == "windows"
}

#' If running on windows, check if the user has admin privileges
#'
#' @return [lgl] TRUE if the user in on a windows OS and has admin privileges, FALSE otherwise
#' @keywords internal
is_windows_admin <- function() {
   if (!is_windows()) {
      return(FALSE)  # Not relevant for non-Windows systems
   }

   # Use PowerShell to check for admin privileges
   result <- tryCatch({
      output <- system2("powershell",
                        args = "-Command \"([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)\"",
                        stdout = TRUE, stderr = FALSE)
      tolower(output) == "true"
   }, error = function(e) {
      FALSE
   })

   return(result)
}
