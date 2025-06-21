---
output: github_document
title: "vmTools"
---

## Installation

```
install.packages("vmTools")
```



## What is it?

- Version Management Tools For Data Science Projects using R6 classes.
   - Lightweight data versioning using the file system and symbolic links
      - No database required
   - User-control of important versions with automated logs and reports
      - best   - a single best folder of data/outputs
      - keep   - an arbitrary number of important folders to keep
      - remove - an arbitrary number of folders staged for removal (with method to delete)



## How does it work?

- See the vignettes folder for examples of how to use the package.



## When was the package updated?

- See the NEWS file



## Who wrote it?  Who maintains it?

- See the DESCRIPTION file.




### The newest version doesn't work for me, how do I install an older release?

```r
# fill in your favorite version number
devtools::install_github("epi-sam/vmTools@v0.0.0")
```

<!-- badges: start -->
[![R-CMD-check](https://github.com/epi-sam/vmTools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/epi-sam/vmTools/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->
  
