library(withr)
library(devtools)
with_libpaths(

   # NOTE: installs 'main' branch - does not seem to have another refs option
   new = "/mnt/share/code/ssbyrne/r_pkgs/"
   , code =  devtools::install_local(path = "~/rstudio/projects/vmTools"

   )
)
