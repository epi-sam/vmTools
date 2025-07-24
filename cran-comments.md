## Resubmission

This is a resubmission. In this version I have:

* Removed /dontrun{} examples and converted to tests.
* Removed `:::` instances (only those required for tests & vignettes remain).
* Added verbosity control to R/symlink_tool constructor.
  * All messaging comes through std_err stream.
  * Only print exceptions if verbose=FALSE.
* R/dir_tree() has a return value.
* All options in vignettes now reset.
* There are no methods references to cite in the DESCRIPTION.
* All filesystem operations use tempdir().
  * Functions to not save to user filespace.
  * Removed default paths from vignette print functions.
  * If warnings arise, I am eager to address if provided files/line numbers.


## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
