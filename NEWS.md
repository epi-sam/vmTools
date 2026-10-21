# vmTools 2.0.3

## QOL Updates

- `make_new_version_folder()` returns the `version_name`

## Bug Fixes

- `clean_path()` now drops a trailing slash (bare roots such as `/` and `C:/` are kept). Previously a trailing slash survived, which made symlink checks look through the link instead of at it.



# vmTools 2.0.2

## Bug Fixes

- Safe for many processes to instantiate `SLT` at once. The constructor no
  longer writes the central log; it is created on the first mark.
- All log and report writes are atomic (temp file + rename), so concurrent
  readers never see a partially written file.
- A log that is non-empty but unreadable now errors instead of being silently
  rebuilt (and its history lost). Zero-byte logs are still rebuilt.
- Passing a symlink name (`"latest"`, `"keep_<version>"`) as a `version_name`
  now resolves to the real folder it points to instead of silently doing nothing
  or logging under the wrong name.
- Symlink names are reserved: `make_new_version_folder()` refuses them, and a
  real folder sitting on one blocks marking and appears in the discrepancy
  report.
- Symlink lookup no longer matches other versions by suffix or treats `.` as a
  wildcard, so marking one version can no longer delete another's symlink.



# vmTools 2.0.1

Introduces a plugin architecture for symlink types: register a new type once with
`new_symlink_type()` and it works immediately across all Symlink Tool and 
Symlink Composer instances via the type-generic `mark()` and `roundup()` methods.

## Breaking from 1.0.0 to 2.0.0

Symlink Tool constructor args were clarified

- `SLT$new()` 
  - `user_root_list` is now `root_list`
  - `user_central_log_root` is now `central_log_root`

Symlink Registry related changes

- `SLT$return_dictionaries()` no longer `returns DICT$symlink_types`, 
  `DICT$symlink_regex_extract`, or `DICT$symlink_registry`. The symlink type 
  registry (the package's central record of all known types and their behaviour) 
  is now managed at the package level 
  - extend it with `new_symlink_type()`.
  - read it with `get_symlink_registry()`

## New Features

**Improved Vignettes**

See new, simplified vignette documents.


**Extensible symlink type registry**

```r
new_symlink_type("production", is_singleton = TRUE)
# One per root — promoting a new version demotes the previous one
new_symlink_type("flagged", is_singleton = FALSE)
# Many per root — flag several versions for QA review at once.
```

- Type definitions now live in a package-level environment shared across all
  SLT/SLC instances in your R session. Adding a type with
  `new_symlink_type()` takes effect immediately — no restart or object
  re-creation needed.
- `new_symlink_type(symlink_type, is_singleton)` — the `is_singleton` flag
  controls the type's behaviour:
   - `TRUE` — one folder per root ("Highlander rule"). Promoting a new folder
     automatically demotes the previous holder. For superlatives like `"best"`
     or `"latest"`.
   - `FALSE` — many folders per root. For accumulating marks like `"keep"` or
     `"remove"`.
   - All derived patterns are generated automatically. Built-in types are locked
     ('best', 'latest', 'keep', 'remove').

**Type-generic `mark()` and `roundup()` on SLT**

- `slt$mark(symlink_type, version_name, user_entry)` — marks a version with any
  built-in or custom type registered via `new_symlink_type()`.
- `slt$roundup(symlink_type, type)` — lists all symlinks of a given type across
  roots. Accepts any registered type.
- Per-type methods (`mark_best()`, `roundup_keep()`, etc.) still work and are
  maintained indefinitely (see Superseded below).

**Type-generic `roundup_all()` on SLC**

- `slc$roundup_all(symlink_type)` — combines results across all Symlink 
  Composer tools for any registered type. Replaces `roundup_all_symlinks()`.

**`return_as` pass-through on SLC aggregates**

- `slc$roundup_all_unmarked()` and `slc$roundup_all_by_date()` now accept
  `return_as = c("table", "version_name")`, matching the SLT equivalents.

## Superseded

The per-type methods below remain fully functional and are maintained
indefinitely. The type-generic equivalents are preferred for new code,
especially with custom types from `new_symlink_type()`.

| Superseded | Replacement |
|---|---|
| `slt$mark_best()` | `slt$mark("best", ...)` |
| `slt$mark_latest()` | `slt$mark("latest", ...)` |
| `slt$mark_keep()` | `slt$mark("keep", ...)` |
| `slt$mark_remove()` | `slt$mark("remove", ...)` |
| `slt$roundup_best()` | `slt$roundup("best", type)` |
| `slt$roundup_latest()` | `slt$roundup("latest", type)` |
| `slt$roundup_keep()` | `slt$roundup("keep", type)` |
| `slt$roundup_remove()` | `slt$roundup("remove", type)` |


# vmTools 1.3.1

## Features:

`roundup_x()` functions

- New argument - `return_as = 'version_name'` returns a character vector of version names 
  instead of a full table of paths for easier automation. Full table remains the
  default return data structure.
  

# vmTools 1.3.0

## Features:

'Latest' symlink

- New methods to mark and roundup a 'latest' symlink that 
- Mechanically similar to 'best', just a different label
  - Singleton rule - only one 'latest' version can be marked at a time
  - A folder cannot simulataneously be 'latest' and any other symlink type

# vmTools 1.2.0

## Features:

Symlink Composer (SLC)

- New class that takes a list of Symlink Tools as its main argument.
- Use SLC to compose sets of e.g. "best" symlinks across multiple roots/projects.
  - Useful if you have various datasets that are all "best", produced at 
  different times, but all still "go together" in a meaningful way.
  - Currently supports all `roundup_x()` functions.

## Breaking:

Symlink Tool constructor args have changed

- SLT$new() 
  - user_root_list is now root_list
  - user_central_log_root is now central_log_root

# vmTools 1.1.0

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
