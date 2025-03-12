message("Loading .Rprofile packages")
.pkgs <- c(
   "devtools"
)
for(.pkg in .pkgs){
   message(" -- ", .pkg)
   library(.pkg, character.only = TRUE)
}

devtools::load_all()

rm(.pkgs, .pkg)
