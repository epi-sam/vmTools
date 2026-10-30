# Setup ------------------------------------------------------------------------

# use clean_path() liberally for github/cran checks

root_base <- make_test_root("new_types")
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


# Link capability guard -------------------------------------------------------

testthat::skip_if_not(vmTools:::dir_supports_links(tempdir()), "this machine cannot create symlinks (shortcuts) in tempdir()")


# Unit: Registry Accessors -----------------------------------------------------

test_that("Symlink registiry is well-formed", {
   registry <- get_symlink_registry()
   assert_named_list(registry)
   expect_true(all(c("best", "latest", "remove", "keep") %in% names(registry)))
   for (entry in registry) {
      expect_type(entry$is_singleton, "logical")
      expect_true(is.function(entry$symlink_suffix_fn))
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

test_that("A type differing from a registered type only by case is refused", {
   expect_error(new_symlink_type("In_Progress", is_singleton = TRUE), regexp = "differs only by case.*'in_progress'")
   expect_error(new_symlink_type("FAILED", is_singleton = FALSE), regexp = "differs only by case.*'failed'")
   expect_false("In_Progress" %in% vmTools:::get_symlink_types())
})

test_that("An empty, missing, blank or non-scalar type name is refused", {
   expect_error(new_symlink_type("",            is_singleton = TRUE), regexp = "empty in some way")
   expect_error(new_symlink_type(NA_character_, is_singleton = TRUE), regexp = "empty in some way")
   expect_error(new_symlink_type(" ",           is_singleton = TRUE), regexp = "empty in some way")
   expect_error(new_symlink_type(c("a", "b"),   is_singleton = TRUE), regexp = "length 1")
   expect_error(new_symlink_type(1,             is_singleton = TRUE), regexp = "type 'string'")
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


# Disk registry ----------------------------------------------------------------
# - each root carries .symlink_registry.csv so a new session's tool starts with
#   the types the root has seen; these tests drive the file directly

reg_roots <- c(
   reg_a   = clean_path(root_base, "reg_a")
   , reg_b = clean_path(root_base, "reg_b")
)
lapply(reg_roots, make_directory)
reg_roots <- unname(clean_path(reg_roots))

test_that("update_symlink_disk_registry() adds, updates and drops a type in every root's file", {
   expect_message(update_symlink_disk_registry(reg_roots, "stalled", is_singleton = FALSE, operation = "add"), regexp = "add done")
   expect_true(all(file.exists(file.path(reg_roots, ".symlink_registry.csv"))))
   on_file <- vmTools:::read_disk_registry(reg_roots[1])
   expect_identical(on_file$symlink_type, "stalled")
   expect_false(on_file$is_singleton)
   expect_identical(names(on_file), c("symlink_type", "is_singleton", "timestamp", "user", "vmtools_version"))

   # explicit operations: the mismatched one stops, the identical one is a no-op
   expect_message(update_symlink_disk_registry(reg_roots, "stalled", is_singleton = FALSE, operation = "add"), regexp = "nothing changed")
   expect_error(update_symlink_disk_registry(reg_roots, "stalled", is_singleton = TRUE, operation = "add"), regexp = "operation = 'update'")
   expect_error(update_symlink_disk_registry(reg_roots, "hold", is_singleton = TRUE, operation = "update"), regexp = "operation = 'add'")
   expect_error(update_symlink_disk_registry(reg_roots, "best", is_singleton = TRUE, operation = "add"), regexp = "Built-in")
   expect_error(update_symlink_disk_registry(reg_roots, "stalled", operation = "add"), regexp = "needs is_singleton")

   # no links on disk, so the shape may change and the row may go
   expect_message(update_symlink_disk_registry(reg_roots, "stalled", is_singleton = TRUE, operation = "update"), regexp = "update done")
   expect_true(vmTools:::read_disk_registry(reg_roots[2])$is_singleton)
   expect_message(update_symlink_disk_registry(reg_roots, "stalled", is_singleton = TRUE, operation = "drop"), regexp = "drop done")   # is_singleton ignored
   expect_identical(nrow(vmTools:::read_disk_registry(reg_roots[1])), 0L)
   expect_message(update_symlink_disk_registry(reg_roots, "stalled", operation = "drop"), regexp = "nothing to drop")
})

test_that("A shape change or drop is refused while links of the old shape exist, and prints the unmark recipe", {
   # root_a still carries failed_<v2> and failed_<v3> made by slt above
   root_a <- unname(root_list$root_a)
   update_symlink_disk_registry(root_a, "failed", is_singleton = FALSE, operation = "add", verbose = FALSE)

   err <- expect_error(update_symlink_disk_registry(root_a, "failed", is_singleton = TRUE, operation = "update"), regexp = "links of the current shape still exist")
   expect_match(conditionMessage(err), "slt\\$unmark\\(v", fixed = FALSE)
   expect_match(conditionMessage(err), dv$v2, fixed = TRUE)
   expect_match(conditionMessage(err), dv$v3, fixed = TRUE)
   expect_match(conditionMessage(err), "<the one version to keep>", fixed = TRUE)
   expect_error(update_symlink_disk_registry(root_a, "failed", operation = "drop"), regexp = "links of the current shape still exist")
   expect_false(vmTools:::read_disk_registry(root_a)$is_singleton)   # untouched

   # the recipe's first step, then the drop goes through
   suppressMessages(for (v in c(dv$v2, dv$v3)) slt$unmark(v, user_entry = list(comment = "failed retired")))
   expect_message(update_symlink_disk_registry(root_a, "failed", operation = "drop"), regexp = "drop done")
   expect_identical(nrow(vmTools:::read_disk_registry(root_a)), 0L)
})

test_that("Types on file are registered for the session, and disagreements stop", {
   update_symlink_disk_registry(reg_roots, "stalled", is_singleton = FALSE, operation = "add", verbose = FALSE)
   expect_false("stalled" %in% vmTools:::get_symlink_types())

   expect_message(new_dt <- vmTools:::register_symlink_types_from_disk(reg_roots), regexp = "stalled +FALSE")
   expect_identical(new_dt$symlink_type, "stalled")
   expect_false(vmTools:::get_symlink_registry_entry("stalled")$is_singleton)
   # second read: nothing new, no message
   expect_silent(again <- vmTools:::register_symlink_types_from_disk(reg_roots))
   expect_identical(nrow(again), 0L)

   # two roots disagree
   update_symlink_disk_registry(reg_roots[2], "stalled", is_singleton = TRUE, operation = "update", verbose = FALSE)
   expect_error(vmTools:::register_symlink_types_from_disk(reg_roots), regexp = "Root folders disagree")
   # one root disagrees with the session ("stalled" is multiple-version here)
   expect_error(vmTools:::register_symlink_types_from_disk(reg_roots[2]), regexp = "live session registry has")
   update_symlink_disk_registry(reg_roots[2], "stalled", is_singleton = FALSE, operation = "update", verbose = FALSE)
})

test_that("The first promote of a type records it on file; built-ins and repeats do not", {
   root_fresh <- clean_path(root_base, "reg_fresh")
   make_directory(root_fresh)
   root_fresh <- clean_path(root_fresh)

   expect_true(vmTools:::record_symlink_type_on_disk(root_fresh, "failed"))
   expect_false(vmTools:::record_symlink_type_on_disk(root_fresh, "failed"))
   expect_false(vmTools:::record_symlink_type_on_disk(root_fresh, "best"))
   on_file <- vmTools:::read_disk_registry(root_fresh)
   expect_identical(on_file$symlink_type, "failed")
   expect_false(on_file$is_singleton)

   # the root says singleton, the session says multiple-version
   update_symlink_disk_registry(root_fresh, "failed", is_singleton = TRUE, operation = "update", verbose = FALSE)
   expect_error(vmTools:::record_symlink_type_on_disk(root_fresh, "failed"), regexp = "registry file says singleton")
})


# Cleanup ----------------------------------------------------------------------

test_that("Cleanup is complete", {
   unlink(root_base, recursive = TRUE)
   expect_error(dir_tree(root_base), regexp = "no such file")
})


# Reserved names follow the registry -------------------------------------------

test_that("Registering a symlink type reserves its name for folders", {
   expect_identical(vmTools:::find_reserved_name_conflict("in_progress"), "in_progress")  # singleton word
   expect_identical(vmTools:::find_reserved_name_conflict("failed_x"),    "failed_")      # non-singleton prefix
   expect_null(vmTools:::find_reserved_name_conflict("2024_01_01.01"))
   expect_error(vmTools:::assert_version_name_not_reserved("in_progress"), regexp = "symlink \\(shortcut\\)")
})


