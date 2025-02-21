# vmTools

## What is it?

- Version Management Tools For Data Science Projects using R6 classes.
   - Lightweight data versioning using the file system
      - No database required
   - User-control of important versions with automated logs and reports
      - best   - a single best folder of data/outputs
      - keep   - an arbitrary number of important folders to keep
      - remove - an arbitrary number of folders staged for removal (with option to delete)



## Can you show me how it works?

- See the vignettes folder for examples of how to use the package.



## When was it last updated, and how?

- See the NEWS file



## Who wrote it?  Who maintains it?

- See the DESCRIPTION file.



## How do I install it?

```r
# Setup
library(callr) # may be necessary for some users
# Install to your standard package library
devtools::install_github("epi-sam/vmTools")

# Install to a team package library
r_team_lib <- "/mnt/share/code/vaccines/R_library"
withr::with_libpaths(new = r_team_lib, devtools::install_github("epi-sam/vmTools"))

# Install to a team library and build vignettes (look in the 'doc' subfolder after installation)
withr::with_libpaths(
   new = r_team_lib
   , devtools::install_github(
      repo              = "epi-sam/vmTools"
      , depencies       = TRUE # required for vignettes
      , build_vignettes = TRUE
   )
)

```



### The newest version doesn't work for me, how do I install an older release?

```r
devtools::install_github("epi-sam/vmTools@v0.3.0")
```



## Which Remote is actively maintained?

- Use the [Github](https://github.com/epi-sam/vmTools) repository for the most up-to-date version of the code.
