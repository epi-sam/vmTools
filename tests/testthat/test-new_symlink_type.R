# Setup ------------------------------------------------------------------------

# use clean_path() liberally for github/cran checks

make_directory <- function(path) dir.create(path, recursive = TRUE, showWarnings = FALSE)
root_base <- clean_path(tempdir(), "new_types")
root_list <- list(
   root_a   = clean_path(root_base, "root_a")
   , root_b = clean_path(root_base, "root_b")
)
central_log <- clean_path(root_base, "central_logs")

dv <- list(
   v1   = "2024_06_01.1"
   , v2 = "2024_06_01.2"
   , v3 = "2024_06_01.3"
)


# Windows Guard ----------------------------------------------------------------

if(tolower(.Platform$OS.type) == "windows" & vmTools:::is_windows_admin() == FALSE){

   test_that(
      "Windows non-admin startup produces correct error",
      {
         expect_error(
            SLT$new(), regexp = "Symbolic links are not supported on Windows without admin privileges."
         )
      }
   )

   testthat::skip("Symbolic links are not supported on Windows without admin privileges")

}


# Unit: Registry Accessors -----------------------------------------------------

test_that("Symlink registiry is well-formed", {
   registry <- get_symlink_registry()
   assert_named_list(registry)
   expect_true(all(c("best", "latest", "remove", "keep") %in% names(registry)))
   for (entry in registry) {
      expect_type(entry$is_singleton, "logical")
      expect_true(is.function(entry$symlink_suffix_fn))
      expect_true(is.function(entry$ls_regex_fn))
   }
})

test_that("Registration succeeds and types appear in registry", {
   new_symlink_type("in_progress", is_singleton = TRUE)
   new_symlink_type("failed", is_singleton = FALSE)
   expect_true("in_progress" %in% vmTools:::get_symlink_types())
   expect_true("failed" %in% vmTools:::get_symlink_types())
})

test_that("Registry entries have correct structure", {
   entry_ip <- vmTools:::get_symlink_registry_entry("in_progress")
   expect_true(entry_ip$is_singleton)
   expect_equal(entry_ip$symlink_suffix_fn("2024_01_01"), "")

   entry_f <- vmTools:::get_symlink_registry_entry("failed")
   expect_false(entry_f$is_singleton)
   expect_equal(entry_f$symlink_suffix_fn("2024_01_01"), "_2024_01_01")
})

test_that("Overwriting a built-in type errors (case-insensitive)", {
   expect_error(new_symlink_type("BEST", is_singleton = TRUE), regexp = "Cannot overwrite")
   expect_error(new_symlink_type("keep", is_singleton = FALSE), regexp = "Cannot overwrite")
})

test_that("Querying an unregistered type errors", {
   expect_error(vmTools:::get_symlink_registry_entry("nonexistent"), regexp = "not registered")
})


# Integration: SLT with Two Roots ---------------------------------------------

lapply(c(unlist(root_list), central_log), make_directory)

suppressMessages(
   suppressWarnings({
      slt <- SLT$new(
         root_list        = root_list
         , central_log_root = central_log
         , verbose          = FALSE
         , verbose_startup  = FALSE
      )
   })
)

lapply(dv, function(v) slt$make_new_version_folder(v))

test_that("mark() creates a singleton symlink for 'in_progress'", {
   slt$mark("in_progress", dv$v1, list(comment = "wip"))
   expect_true(all(file.exists(clean_path(root_list, "in_progress"))))
   resolved <- sapply(root_list, function(root) clean_path(root, "in_progress"))
   expect_true(all(grepl(dv$v1, resolved)))
})

test_that("mark() creates a non-singleton symlink for 'failed'", {
   slt$mark("failed", dv$v2, list(comment = "bad run"))
   slt$mark("failed", dv$v3, list(comment = "bad run again"))
   expect_true(all(file.exists(clean_path(root_list, "failed_2024_06_01.2"))))
   expect_true(all(file.exists(clean_path(root_list, "failed_2024_06_01.3"))))
})

test_that("roundup() returns correct results for custom types", {
   dt_ip <- slt$roundup("in_progress")
   expect_s3_class(dt_ip, "data.table")
   expect_true(dv$v1 %in% dt_ip$version_name)

   dt_f <- slt$roundup("failed")
   expect_s3_class(dt_f, "data.table")
   expect_true(all(c(dv$v2, dv$v3) %in% dt_f$version_name))
})

test_that("unmark() removes custom symlinks", {
   slt$unmark(dv$v1, list(comment = "done"))
   expect_false(any(file.exists(clean_path(root_list, "in_progress"))))

   slt$unmark(dv$v2, list(comment = "done"))
   expect_false(any(file.exists(clean_path(root_list, "failed_2024_06_01.2"))))
})


# Integration: SLC Roundup ----------------------------------------------------

# Re-mark so there's something to roundup
slt$mark("in_progress", dv$v1, list(comment = "wip again"))
slt$mark("failed", dv$v2, list(comment = "bad again"))
slt$mark("failed", dv$v3, list(comment = "bad again"))

suppressMessages({
   slc <- SLC$new(
      tool_list = list(my_project = slt)
      , verbose = FALSE
   )
})

test_that("get_common_symlink_types() includes user-defined types", {
   types <- slc$get_common_symlink_types()
   expect_true(all(c("in_progress", "failed") %in% types))
})

test_that("roundup_all() works for user-defined types", {
   dt_ip <- slc$roundup_all("in_progress")
   expect_true("my_project" %in% dt_ip$tool_name)
   expect_true(dv$v1 %in% dt_ip$version_name)

   dt_f <- slc$roundup_all("failed")
   expect_true("my_project" %in% dt_f$tool_name)
   # expect_true(dv$v2 %in% dt_f$version_name)
   expect_true(all(c(dv$v2, dv$v3) %in% dt_f$version_name))
})


# Cleanup ----------------------------------------------------------------------

test_that("Cleanup is complete", {
   system(paste("rm -rf", root_base))
   expect_error(dir_tree(root_base), regexp = "no such file")
})


# Reserved names follow the registry -------------------------------------------

test_that("Registering a symlink type reserves its name for folders", {
   expect_identical(vmTools:::find_reserved_name_conflict("in_progress"), "in_progress")  # singleton word
   expect_identical(vmTools:::find_reserved_name_conflict("failed_x"),    "failed_")      # non-singleton prefix
   expect_null(vmTools:::find_reserved_name_conflict("2024_01_01.01"))
   expect_error(vmTools:::assert_version_name_not_reserved("in_progress"), regexp = "symlink \\(shortcut\\)")
})


# ls regex is anchored and escaped ---------------------------------------------

test_that("ls regex matches only its own version: no suffix match, no wildcard dot", {
   ls_line <- function(name, target) sprintf("lrwxrwxrwx 1 user group 46 Oct  1 20:20 %s -> /root/%s", name, target)
   keep_regex <- vmTools:::make_ls_regex_fn(is_singleton = FALSE, "keep")("1990_01_01")
   best_regex <- vmTools:::make_ls_regex_fn(is_singleton = TRUE,  "best")("2024_01_01.01")

   expect_true( grepl(keep_regex, ls_line("keep_1990_01_01",  "1990_01_01")))
   expect_false(grepl(keep_regex, ls_line("keep_x1990_01_01", "x1990_01_01")))   # suffix collision (E)
   expect_true( grepl(best_regex, ls_line("best", "2024_01_01.01")))
   expect_false(grepl(best_regex, ls_line("best", "2024_01_01x01")))             # wildcard dot (F)
})
