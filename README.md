# vmTools

## What is it?

- Version Management Tools For Data Science Projects
   - Lightweight data versioning with the file system
      - No database required
   - User-control of important versions with notes
      - best   - a single best folder of data/outputs
      - keep   - an arbitrary number of important folders to keep
      - remove - an arbitrary number of folders staged for removal (with option to delete)
   - Automated logs and reports



## Can you show me how it works?

- See the vignettes folder for examples of how to use the package.



## Who wrote it?  Who maintains it?

- See the DESCRIPTION package file.



## When was it last updated, and how?

- See the ChangeLog.md file



## How do I install it?

```r
# Install to your standard package library
devtools::install_github("epi-sam/vmTools")

# Install to a team package library
devtools::install_github("epi-sam/vmTools", lib = "path/to/team/package/library")
```



### The newest version doesn't work for me, how do I install an older release?

```r
# Install to your standard package library
devtools::install_github("epi-sam/vmTools@v0.3.0")
```



## Which Remote is actively maintained?

- The [Bitbucket](https://stash.ihme.washington.edu/users/ssbyrne/repos/vmtools/browse) repository is _**INACTIVE**_.
- Use the [Github](https://github.com/epi-sam/vmTools) repository for the most up-to-date version of the code.
