library(withr)
library(devtools)
with_libpaths(

   new = "/mnt/share/code/ssbyrne/r_pkgs/"
   , code =  devtools::install_local(path = "~/rstudio/projects/vmTools"))

   )
)
