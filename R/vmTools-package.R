# Package-level declarations ---------------------------------------------------
# This file holds what belongs to the package as a whole, not to one function.
# It was created by usethis::use_package_doc().
#
# - "_PACKAGE" tells roxygen2 to write man/vmTools-package.Rd, the help page
#   shown by ?vmTools, from the Title, Description, Authors and URL fields of
#   DESCRIPTION.  `@keywords internal` keeps that page out of the help index;
#   ?vmTools still finds it.
# - The block between the two "usethis namespace" marker lines holds the
#   `@importFrom` tags that roxygen2 turns into importFrom() lines in
#   NAMESPACE, so e.g. head() can be called without utils::.
#   usethis::use_import_from() adds tags between the markers; do not move or
#   reword the marker lines.
# - The NULL after the block is required: roxygen2 only reads a comment block
#   that sits on top of an R object, and NULL is that object.
# - Global variables for `R CMD check` (see the header further down).

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
#' @importFrom utils head tail capture.output menu read.csv read.csv2 write.csv write.csv2
## usethis namespace: end
NULL

# Global variables -------------------------------------------------------------
# data.table column names used unquoted in R/registry_disk.R, e.g.
# disk_dt[is_builtin == FALSE].  `R CMD check` reads the source without running
# it and cannot tell a column from an undefined variable, leading to a NOTE, so
# each name is declared here.  `.` is data.table's alias for list() inside dt[,
# .(...)].
#
# Add a name only when it is a column of a table in R/registry_disk.R.
#
# Failure modes.  The list applies to every function in the package, not only
# to R/registry_disk.R:
#
# 1. A function uses a listed name (e.g. `root`) as an ordinary variable without
#    defining it or taking it as an argument.  `R CMD check` no longer flags
#    it.  At run time it stops with "object 'root' not found", unless an
#    object of that name exists in an enclosing environment, in which case
#    that object is used silently (leaky function - avoid).
# 2. A table lacks a listed column, e.g. dt[is_builtin == FALSE] where dt has
#    no `is_builtin` column. data.table then looks for `is_builtin` outside the
#    table, with the same two outcomes as in 1.
#
# Still flagged by `R CMD check`: a misspelling of a listed name (e.g.
# `is_bultin`), because the misspelling is not on the list.
utils::globalVariables(
   c(
      "."
      , "action"
      , "is_builtin"
      , "is_dangling"
      , "is_link"
      , "is_singleton"
      , "leaf"
      , "n_values"
      , "note"
      , "root"
      , "symlink_type"
      , "target"
      , "version_name"
   )
)
