# vmTools 1.1.0

## Breaking:

`roundup_x()` family 

- now returns single table of paths, rather than list of tables by root_name
- tables have root_name column for clarity
- table are sorted by root_name and version_name

# vmTools 1.0.1 (2025-07-24)

Cran resubmission:

- Removed `/dontrun{}` examples and converted to tests.
- Removed `:::` instances (only those required for tests & vignettes remain).
- Added verbosity control to R/symlink_tool constructor.
  - All messaging comes through std_err stream.
  - Only print exceptions if verbose=FALSE.
- R/dir_tree() now has a return value.
- All options in vignettes now reset.
- There are no methods references to cite in the DESCRIPTION.
- All filesystem operations use tempdir().
  - Functions to not save to user filespace.
  - Removed default paths from vignette print functions.

# vmTools 1.0.0 (2025-06-20)

Updates:

- symlink_tool logs now save to 'logs' subdirectory
  - running reports moves existing legacy logs safely to subdirectories
- SLT made some processes lazier
  - init: if only one user_root is defined, central_log_root defaults to this folder
  - make_new_version_folder: finds a new version_name by default
- vignettes display code but do not evaluate on Windows without admin privileges
- all github CRAN checks passing



# vmTools 0.8.0 (2025-02-20)

Updates:

- Removed all lingering borrowed utilities
- Renamed symlink_tool public and private methods for consistency




# vmTools 0.7.0 (2025-01-30)

Updates:

- Removed purrr package dependency with simple utility function
- Renamed split_path internal function
- Added alternative csv reader options (utils::read.csv and utils::read.csv2)



# vmTools 0.6.0 (2025-01-14)

Update:

- Removed lubridate package dependency



# vmTools 0.5.0 (2025-01-14)

Update:

- Added basic tests



# vmTools 0.4.0 (2025-01-14)

Update:

- Removed DescTools package dependency



# vmTools 0.3.7 (2024-11-19)

Fixes:

- Vignette header fixed to build with package



# vmTools 0.3.6 (2024-11-19)

Update:

- Attempting folder creation writes a version log, even if the folder already exists
   - assumption: using the tool to attempt to create a folder assumes the user wants a log
- README updates



# vmTools 0.3.5 (2024-11-15)

Update:

- Logs and reports sort columns according to internal log schema before writing to disk.



# vmTools 0.3.4 (2024-10-16)

Bugfixes:

- deletions now write to central log



# vmTools 0.3.3 (2024-10-16)

Bugfixes:

- central log write bug



# vmTools 0.3.2 (2024-10-16)

Bugfixes:

- documented



# vmTools 0.3.1 (2024-10-16)

Bugfixes:

- extensive repairs to logs and reports
- allowances for mismatched logs on disk
   - user may choose for strict or flexible control when writing logs
   - reporting is flexible by default
- option for csv reader to suppress warnings (rely on tool's messaging)



# vmTools 0.3.0 (2024-10-15)

Several improvements for consistency and sensible defaults based on trial deployment.

## Logs

- Central log now gets all demotions (previously only promotions were recorded, but this can create confusion).
- Marked folders are no longer demoted and re-promoted if they are marked as the same folder type
   - Prevents log bloat.
   - Prevents confusion about when date-versions were elevated.

## Reports

- Main 'important versions' report has a sensible name.
- Report names are consolidated to private dictionaries.
- Reports are now sorted based on the `timestamp` field, which should keep all versions chronological based on when they were marked.

## Methods

- added `roundup_unmark()` method
   - Useful for finding folders that are not being tracked for easier bulk-removal.
- added `roundup_best()` method
   - Back-filling a conspicuously absent option - allows the tool to be used for systematic operations with the `best` version, instead of hand-building file paths.

## NEWS

- Added this file.



# vmTools 0.2.0

## Vignettes

Two written:

- one fully technical
- one scoped for new users



# vmTools 0.1.0

- Symlink tool is fully functional with basic tasks.
