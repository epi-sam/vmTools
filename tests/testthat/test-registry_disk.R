# R/registry_disk.R - each root's .symlink_registry.csv, and how a tool uses it

# Setup ------------------------------------------------------------------------

# use clean_path() liberally for github/cran checks

root_base <- make_test_root("registry_disk")

dv <- list(
   v1   = "2024_06_01.1"
   , v2 = "2024_06_01.2"
   , v3 = "2024_06_01.3"
)

# the exported call, with messages off
update_quiet <- function(root, symlink_type, is_singleton = NULL, operation) {
   return(update_symlink_disk_registry(
      root
      , symlink_type
      , is_singleton = is_singleton
      , operation    = operation
      , verbose      = FALSE
   ))
}

make_tool_on_roots <- function(root_list, central_log_root) {
   return(suppressMessages(suppressWarnings(SLT$new(
      root_list          = root_list
      , central_log_root = central_log_root
      , verbose          = FALSE
      , verbose_startup  = FALSE
   ))))
}


# Link capability guard -------------------------------------------------------

testthat::skip_if_not(
   vmTools:::dir_supports_links(tempdir())
   , "this machine cannot create symlinks (shortcuts) in tempdir()"
)


# The registry file, edited directly --------------------------------------------

reg_roots <- c(
   make_test_root("registry_disk", "reg_a")
   , make_test_root("registry_disk", "reg_b")
)

test_that("update_symlink_disk_registry() adds, updates and drops a type in every root's file", {
   update_quiet(reg_roots, "stalled", is_singleton = FALSE, operation = "add")
   on_file <- vmTools:::read_disk_registry(reg_roots[1])
   # the file format people will see: built-ins first, then the rest
   expect_identical(
      names(on_file)
      , c("symlink_type", "is_singleton", "is_builtin", "timestamp", "user", "vmtools_version")
   )
   expect_identical(
      on_file[, .(symlink_type, is_singleton, is_builtin)]
      , data.table::data.table(
         symlink_type   = c("best", "keep", "latest", "remove", "stalled")
         , is_singleton = c(TRUE, FALSE, TRUE, FALSE, FALSE)
         , is_builtin   = c(TRUE, TRUE, TRUE, TRUE, FALSE)
      )
   )

   # a second add never overwrites; built-in rows are not the user's to change;
   # names are lowercase, so two can never differ only by case
   expect_error(
      update_quiet(reg_roots, "stalled", is_singleton = TRUE, operation = "add")
      , regexp = "operation = 'update'"
   )
   expect_error(
      update_quiet(reg_roots, "best", is_singleton = TRUE, operation = "add")
      , regexp = "Built-in"
   )
   expect_error(
      update_quiet(reg_roots, "Stalled", is_singleton = FALSE, operation = "add")
      , regexp = "must be lowercase"
   )
   # a repeat is a no-op with a message, never an error
   expect_message(
      update_symlink_disk_registry(reg_roots, "stalled", is_singleton = FALSE, operation = "add")
      , regexp = "nothing changed"
   )

   # no links on disk, so is_singleton may change and the row may go
   update_quiet(reg_roots, "stalled", is_singleton = TRUE, operation = "update")
   expect_true(vmTools:::read_disk_registry(reg_roots[2])[symlink_type == "stalled", is_singleton])
   update_quiet(reg_roots, "stalled", operation = "drop")
   expect_false("stalled" %in% vmTools:::read_disk_registry(reg_roots[1])$symlink_type)
   expect_message(
      update_symlink_disk_registry(reg_roots, "stalled", operation = "drop")
      , regexp = "nothing to drop"
   )
})

test_that("A registry file edited by hand is refused on read", {
   root_edited <- make_test_root("registry_disk", "reg_edited")
   update_quiet(root_edited, "stalled", is_singleton = FALSE, operation = "add")
   fpath   <- vmTools:::get_disk_registry_path(root_edited)
   on_file <- utils::read.csv(fpath)
   on_file$is_builtin[on_file$symlink_type == "stalled"] <- TRUE
   utils::write.csv(on_file, fpath, row.names = FALSE)

   expect_error(vmTools:::read_disk_registry(root_edited), regexp = "is_builtin column wrongly")
})


# A tool and its roots' registry files -------------------------------------------

tool_roots <- list(
   root_x   = make_test_root("registry_disk", "tool_x")
   , root_y = make_test_root("registry_disk", "tool_y")
)
tool_central <- make_test_root("registry_disk", "tool_central")
update_quiet(tool_roots$root_x, "stalled", is_singleton = FALSE, operation = "add")

test_that("Building a tool writes no registry file; its first mark repairs every root's file", {
   fpaths  <- vapply(tool_roots, vmTools:::get_disk_registry_path, character(1))
   slt_reg <- make_tool_on_roots(tool_roots, tool_central)
   expect_identical(unname(file.exists(fpaths)), c(TRUE, FALSE))

   slt_reg$make_new_version_folder(dv$v1)
   slt_reg$mark_best(dv$v1, list(comment = "first mark"))
   expect_identical(
      vmTools:::read_disk_registry(tool_roots$root_y)[is_builtin == FALSE, symlink_type]
      , "stalled"
   )
})

test_that("slt$update_symlink_registry() edits every root's file; the tool has the type at once", {
   slt_reg <- make_tool_on_roots(tool_roots, tool_central)
   slt_reg$update_symlink_registry("in_progress", is_singleton = TRUE, operation = "add")
   slt_reg$update_symlink_registry("failed", is_singleton = FALSE, operation = "add")
   expect_identical(vmTools:::read_disk_registries(tool_roots)[symlink_type == "failed", .N], 2L)
   expect_true("failed" %in% names(slt_reg$get_symlink_registry()))
})

test_that("is_singleton on file decides one symlink per root or one per version", {
   slt_reg <- make_tool_on_roots(tool_roots, tool_central)
   slt_reg$make_new_version_folder(dv$v2)
   slt_reg$make_new_version_folder(dv$v3)

   slt_reg$mark("in_progress", dv$v1, list(comment = "wip"))
   slt_reg$mark("in_progress", dv$v2, list(comment = "wip moved"))
   expect_identical(slt_reg$roundup("in_progress", return_as = "version_name"), dv$v2)

   slt_reg$mark("failed", dv$v1, list(comment = "bad run"))
   slt_reg$mark("failed", dv$v3, list(comment = "bad run again"))
   expect_identical(slt_reg$roundup("failed", return_as = "version_name"), c(dv$v1, dv$v3))

   slt_reg$unmark(dv$v2, list(comment = "done"))
   expect_identical(slt_reg$roundup("in_progress", return_as = "version_name"), character(0))
})

test_that("A type on file reserves its name for folders: whole word or prefix", {
   slt_reg <- make_tool_on_roots(tool_roots, tool_central)
   expect_error(
      slt_reg$make_new_version_folder("in_progress")
      , regexp = "for its 'in_progress' symlink"
   )
   expect_error(
      slt_reg$make_new_version_folder("failed_x")
      , regexp = "names starting with 'failed_'"
   )
})

test_that("An is_singleton change or a drop is refused while links of the type exist", {
   # both roots still carry failed_<v1> and failed_<v3> from the test above
   slt_reg <- make_tool_on_roots(tool_roots, tool_central)

   err <- expect_error(
      slt_reg$update_symlink_registry("failed", is_singleton = TRUE, operation = "update")
      , regexp = "4 live and 0 dangling"
   )
   # never truncated
   expect_lt(nchar(conditionMessage(err)), getOption("warning.length"))
   expect_error(slt_reg$update_symlink_registry("failed", operation = "drop"), regexp = "live and")
   # untouched
   expect_false(slt_reg$get_symlink_registry()$failed$is_singleton)

   # the recipe's first step, then the drop goes through
   for (v in slt_reg$roundup("failed", return_as = "version_name")) {
      slt_reg$unmark(v, user_entry = list(comment = "failed dropped"))
   }
   slt_reg$update_symlink_registry("failed", operation = "drop")
   expect_false("failed" %in% names(slt_reg$get_symlink_registry()))
})

test_that("Building a verbose tool lists the user-defined types on its roots", {
   expect_message(
      suppressWarnings(SLT$new(
         root_list          = tool_roots
         , central_log_root = tool_central
         , verbose          = TRUE
      ))
      , regexp = "in_progress +TRUE"
   )
})

test_that("A tool is refused on roots whose registry files disagree", {
   update_quiet(tool_roots$root_y, "stalled", is_singleton = TRUE, operation = "update")
   expect_error(make_tool_on_roots(tool_roots, tool_central), regexp = "disagree")
})


# Cleanup ----------------------------------------------------------------------

unlink(root_base, recursive = TRUE)
