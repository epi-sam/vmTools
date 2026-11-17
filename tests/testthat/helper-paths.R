# Test path helpers
# - testthat sources helper-*.R before every test file. also loaded with devtools::load_all()
# - setup-*.R files are another testthat convention for side effects like making temp_dirs (pair with teardown())

make_directory <- function(path) dir.create(path, recursive = TRUE, showWarnings = FALSE)

# Build a fixture root under tempdir() that is CREATED before it is normalized.
# - normalizePath() leaves a path it cannot find unchanged, so a root built
#   before dir.create() never resolves on macOS (/var -> /private/var) or on
#   Windows (8.3 short name RUNNER~1 -> long name).
# - the tool normalizes every path it is given, so test fixtures must match or
#   string comparisons against tool output fail.
make_test_root <- function(...) {
   path <- file.path(tempdir(), ...)
   make_directory(path)
   clean_path(path)
}
