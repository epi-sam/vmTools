# vmTools Style Guide

Norms for writing code in the vmTools project.

Lean into the [Tidyverse Style Guide](https://style.tidyverse.org) when possible, but don't adhere to it slavishly when common sense dictates otherwise, or where noted below.



## Names

"There are only two hard things in Computer Science: cache invalidation and naming things." - Phil Karlton



## General

- **Be descriptive**: Names should be descriptive and meaningful. They should describe what the variable, function, or class does.
- **Be consistent**: Names should be consistent throughout the project. If you use `get` for a function that retrieves data, don't use `fetch` in another part of the project.
- **Be concise**: Names should be concise and to the point. Avoid using long names that are hard to read.
- **No abbreviations**: Do not abbreviate function argument calls, `TRUE/FALSE`, or other common terms. Avoid abbreviations when naming variables/functions.
- **Use snake_case**: Use snake_case for files, variable and function names. This means that words are separated by underscores.
  - **Use noun_adjective order**: This keeps like objects named similarly, and makes it easier to find/group/understand objects by name.



## Files

- **One riot, one ranger**: One file should contain one function, one class, or one object. 
  - **Name files by function**: The file name should be identical to the function/class name (case-sensitive). 
  - **Exception**: A file may contains multiple functions, but they should be _closely_ inter-related.
- **No numbers**: The order of files can change at any time for any reason. Don't use numbers in file names to indicate order.
- **Line length**: Keep lines under 80 characters, when practical.
- **Section Headers**: Use section headers to separate different parts of the file. Use `# ... ----` for the main header and `## ... ----` for subheaders.  All headers should be 80 characters long.

```r
# Load data --------------------------------------------------------------------

## Load reference files --------------------------------------------------------
```




## Tests

- **Testability**: Fuctions should be testable, and tests should pass before PRs are submitted.
- **Conventions**: Aim for "R-like" behavior e.g. functions return objects whenever possible.
  - **Resources**: https://contributor.r-project.org/cran-cookbook/code_issues.html
- **Test driven development**: Test core functionality and likely edge cases; expand tests as issues arise.
  - **Coverage** There is no need for 100% test coverage.



## Style

- **Comments**: Reserve comments for code intent, not what the code does. Comments should explain why the code is written the way it is, not what the code does. If the code is not self-explanatory, rewrite the code to be more clear.
- **Comma-separate lists/vectors on new lines with leading commas**: This makes lists easier to read and edit (SQL standard).
   - **Newline after `(` and `[`**: Except in `data.table` calls, by convention.  Avoid hanging indentation.
   - **Align list elements**: Align lists by assignment operator, and names when possible. The `remedy` package creates an Rstudio plugin to assign a hotkey (I like `ctrl + alt + =` and `ctrl + alt + -` for `=` and `<-`, respectively.)

```r

# Not good
my_list <- list(name1 = "value1", 
                name2 = "value2", 
                name3 = "value3")
# Good
my_list <- list(
  name1   = "value1"
  , name2 = "value2"
  , name3 = "value3"
)

# Best
my_list <- list(
    name1 = "value1"
  , name2 = "value2"
  , name3 = "value3"
)
```

- **Avoid index-based call**: Call variables/list elements etc. by name, not by index.
- **Avoid decimal.names**: These imply `object.method()` syntax - only use when relevant (usually S3).



## Variables

- **Use nouns**: Variables should be named after the data they hold. Use nouns to name variables.


## Functions

- **Use verbs**: Functions should be named after the action they perform.
- **Returns**: Functions return _once_ at the end of the function; avoid early returns if at all possible.


### Function Arguments

- **Order**: Arguments should be ordered from most general to most specific.
  - **First argument**: When a function primarily operates on some object `x`, the first argument is _always_ `x`.
  - **Second argument**: For I/O functions, if the first argument is an object, the second argument is _always_ a `/file/path`.
- **Default arguments**: Avoid default arguments when possible. If you must use them, use `NULL` as the default value, and handle `NULL` behavior explicitly in the function.
- **Argument data types**: Avoid types in argument names - use `data` instead of `data_df` or `data_tbl`.  Types in argument names makes function maintenance more difficult.  If a type needs to change, then the function argument also needs to change, which breaks backward compatibility.  Handle type-checking explicitly within the function instead.

END
