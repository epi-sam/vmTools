## Resubmission

This is a resubmission. In this version I have:

* Removed /dontrun{} examples and converted to tests.
* Removed `:::` instances (only those required for tests & vignettes remain).
* Added verbosity control to R/symlink_tool constructor.
  * All messaging comes through std_err stream.
  * Messages for irregularities always trigger.
* R/dir_tree() returns as well as prints.
* Reset all options in vignettes.
* Removed default paths from vignette print functions.
* There are no methods references to cite in the DESCRIPTION.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
