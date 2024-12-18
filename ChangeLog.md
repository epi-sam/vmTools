# ChangeLog for vmTools Package

--------------------------------------------------------------------------------

## v.0.3.7

2024-11-19

Fixes:

- Vignette header fixed to build with package



## v.0.3.6

2024-11-19

Update:

- Attempting folder creation writes a version log, even if the folder already exists
   - assumption: using the tool to attempt to create a folder assumes the user wants a log
- README updates



## v.0.3.5

2024-11-15

Update:

- Logs and reports sort columns according to internal log schema before writing to disk.



## v.0.3.4

2024-10-16

Bugfixes:

- deletions now write to central log



## v.0.3.3

2024-10-16

Bugfixes:

- central log write bug



## v.0.3.2

2024-10-16

Bugfixes:

- documented



## v.0.3.1

2024-10-16

Bugfixes:

- extensive repairs to logs and reports 
- allowances for mismatched logs on disk 
   - user may choose for strict or flexible control when writing logs 
   - reporting is flexible by default
- option for csv reader to suppress warnings (rely on tool's messaging)



## v.0.3.0

2024-10-15 

Several improvements for consistency and sensible defaults based on trial deployment.

### Logs

- Central log now gets all demotions (previously only promotions were recorded, but this can create confusion).
- Marked folders are no longer demoted and re-promoted if they are marked as the same folder type
   - Prevents log bloat.
   - Prevents confusion about when date-versions were elevated.

### Reports

- Main 'important versions' report has a sensible name.
- Report names are consolidated to private dictionaries.
- Reports are now sorted based on the `timestamp` field, which should keep all versions chronological based on when they were marked.

### Methods

- added `roundup_unmark()` method 
   - Useful for finding folders that are not being tracked for easier bulk-removal.
- added `roundup_best()` method
   - Back-filling a conspicuously absent option - allows the tool to be used for systematic operations with the `best` version, instead of hand-building file paths.

### ChangeLog

- Added this file.



## v.0.2.0

### Vignettes

Two written:

- one fully technical
- one scoped for new users



## v.0.1.0

- Symlink tool is fully functional with basic tasks.
