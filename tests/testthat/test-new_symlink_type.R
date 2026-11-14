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

testthat::skip_if_not(
   vmTools:::dir_supports_links(tempdir())
   , "this machine cannot create symlinks (shortcuts) in tempdir()"
)


# Session types on a tool with two roots ---------------------------------------

lapply(c(unlist(root_list), central_log), make_directory)

new_symlink_type("in_progress", is_singleton = TRUE, verbose = FALSE)
new_symlink_type("failed", is_singleton = FALSE, verbose = FALSE)

suppressMessages(
   suppressWarnings({
      slt <- SLT$new(
         root_list          = root_list
         , central_log_root = central_log
         , verbose          = FALSE
         , verbose_startup  = FALSE
      )
   })
)

lapply(dv, function(v) slt$make_new_version_folder(v))

test_that("is_singleton given at registration decides one link per root or one per version", {
   slt$mark("in_progress", dv$v1, list(comment = "wip"))
   slt$mark("in_progress", dv$v2, list(comment = "wip moved"))
   expect_identical(slt$roundup("in_progress", return_as = "version_name"), dv$v2)

   slt$mark("failed", dv$v1, list(comment = "bad run"))
   slt$mark("failed", dv$v3, list(comment = "bad run again"))
   expect_identical(slt$roundup("failed", return_as = "version_name"), c(dv$v1, dv$v3))

   slt$unmark(dv$v2, list(comment = "done"))
   expect_identical(slt$roundup("in_progress", return_as = "version_name"), character(0))
})

test_that("A registered type's name is reserved for folders: whole word or prefix", {
   expect_error(
      slt$make_new_version_folder("in_progress")
      , regexp = "for its 'in_progress' symlink"
   )
   expect_error(
      slt$make_new_version_folder("failed_x")
      , regexp = "names starting with 'failed_'"
   )
})

test_that("A built-in name, a case-only clash or an empty name is refused at registration", {
   expect_error(new_symlink_type("BEST", is_singleton = TRUE), regexp = "Cannot overwrite built-in")
   expect_error(new_symlink_type("", is_singleton = TRUE), regexp = "empty in some way")
   expect_error(
      new_symlink_type("In_Progress", is_singleton = TRUE)
      , regexp = "differs only by case.*'in_progress'"
   )
})

test_that("The composer offers user types and rounds them up by tool", {
   slc <- suppressMessages(SLC$new(tool_list = list(my_project = slt), verbose = FALSE))
   expect_true(all(c("in_progress", "failed") %in% slc$get_symlink_types()))
   expect_identical(unique(slc$roundup_all("failed")$tool_name), "my_project")
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

# the exported call, with messages off; every test below uses it
update_quiet <- function(root, symlink_type, is_singleton = NULL, operation) {
   return(update_symlink_disk_registry(
      root
      , symlink_type
      , is_singleton = is_singleton
      , operation    = operation
      , verbose      = FALSE
   ))
}

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

   # a second add never overwrites; built-in rows are not the user's to change
   expect_error(
      update_quiet(reg_roots, "stalled", is_singleton = TRUE, operation = "add")
      , regexp = "operation = 'update'"
   )
   expect_error(
      update_quiet(reg_roots, "best", is_singleton = TRUE, operation = "add")
      , regexp = "Built-in"
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
   root_edited <- make_test_root("new_types", "reg_edited")
   update_quiet(root_edited, "stalled", is_singleton = FALSE, operation = "add")
   fpath   <- vmTools:::get_disk_registry_path(root_edited)
   on_file <- utils::read.csv(fpath)
   on_file$is_builtin[on_file$symlink_type == "stalled"] <- TRUE
   utils::write.csv(on_file, fpath, row.names = FALSE)

   expect_error(vmTools:::read_disk_registry(root_edited), regexp = "is_builtin column wrongly")
})

test_that("An is_singleton change or a drop is refused while links of the type exist", {
   # root_a still carries failed_<v1> and failed_<v3> made by slt above
   root_a <- unname(root_list$root_a)
   update_quiet(root_a, "failed", is_singleton = FALSE, operation = "add")

   err <- expect_error(
      update_quiet(root_a, "failed", is_singleton = TRUE, operation = "update")
      , regexp = "2 live and 0 dangling"
   )
   # never truncated
   expect_lt(nchar(conditionMessage(err)), getOption("warning.length"))
   expect_error(update_quiet(root_a, "failed", operation = "drop"), regexp = "live and")
   # untouched
   expect_false(vmTools:::read_disk_registry(root_a)[symlink_type == "failed", is_singleton])

   # the recipe's first step, then the drop goes through
   suppressMessages(
      for (v in slt$roundup("failed", return_as = "version_name")) {
         slt$unmark(v, user_entry = list(comment = "failed dropped"))
      }
   )
   update_quiet(root_a, "failed", operation = "drop")
   expect_false("failed" %in% vmTools:::read_disk_registry(root_a)$symlink_type)
})

test_that("Roots' files merge over the session; disagreements stop; absences are backfilled", {
   update_quiet(reg_roots, "stalled", is_singleton = FALSE, operation = "add")
   # the live session registry is untouched
   expect_false("stalled" %in% vmTools:::get_symlink_types())

   disk_dt <- vmTools:::read_disk_registries(reg_roots)
   reg     <- vmTools:::merge_symlink_registries(disk_dt)
   expect_identical(names(reg)[1:4], c("best", "keep", "latest", "remove"))
   expect_false(reg$stalled$is_singleton)
   expect_false(reg$stalled$is_builtin)

   # a session default for a type on file: the file wins and is reported
   new_symlink_type("stalled", is_singleton = TRUE, verbose = FALSE)
   expect_false(vmTools:::merge_symlink_registries(disk_dt)$stalled$is_singleton)
   expect_identical(vmTools:::find_session_overrides(disk_dt)$symlink_type, "stalled")

   # two roots disagree
   update_quiet(reg_roots[2], "stalled", is_singleton = TRUE, operation = "update")
   expect_error(vmTools:::read_disk_registries(reg_roots), regexp = "disagree")
   update_quiet(reg_roots[2], "stalled", is_singleton = FALSE, operation = "update")

   # a root lacking a row a sibling has is backfilled; a root with no file gets one
   update_quiet(reg_roots[2], "stalled", operation = "drop")
   root_c <- clean_path(root_base, "reg_c")
   make_directory(root_c)
   root_c <- clean_path(root_c)
   roots3 <- c(reg_roots, root_c)
   written <- vmTools:::backfill_disk_registries(roots3, vmTools:::read_disk_registries(roots3))
   expect_identical(written$root, c(reg_roots[2], root_c))
   disk3 <- vmTools:::read_disk_registries(roots3)
   expect_true(all(disk3[symlink_type == "stalled", !is_singleton]))
   expect_identical(nrow(vmTools:::backfill_disk_registries(roots3, disk3)), 0L)
})

test_that("The first promote of a type records it on file; built-ins and repeats do not", {
   root_fresh <- clean_path(root_base, "reg_fresh")
   make_directory(root_fresh)
   root_fresh <- clean_path(root_fresh)
   record <- vmTools:::record_symlink_type_on_disk

   expect_true(record(root_fresh, "failed", is_singleton = FALSE))
   expect_false(record(root_fresh, "failed", is_singleton = FALSE))
   expect_false(record(root_fresh, "best", is_singleton = TRUE))
   expect_false(vmTools:::read_disk_registry(root_fresh)[symlink_type == "failed", is_singleton])

   # the root says singleton, the session says multiple-version
   update_quiet(root_fresh, "failed", is_singleton = TRUE, operation = "update")
   expect_error(
      record(root_fresh, "failed", is_singleton = FALSE)
      , regexp = "is_singleton = TRUE in"
   )
})


# The tool and its roots' registry files -----------------------------------------

tool_roots <- list(
   root_x   = make_test_root("new_types", "tool_x")
   , root_y = make_test_root("new_types", "tool_y")
)
tool_central <- make_test_root("new_types", "tool_central")
make_tool_on_roots <- function(root_list, central_log_root) {
   return(suppressMessages(suppressWarnings(SLT$new(
      root_list          = root_list
      , central_log_root = central_log_root
      , verbose          = FALSE
      , verbose_startup  = FALSE
   ))))
}
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

test_that("A symlink type on a root's file works in a tool, as a singleton if is_singleton", {
   update_quiet(unname(unlist(tool_roots)), "review", is_singleton = TRUE, operation = "add")
   slt_reg <- make_tool_on_roots(tool_roots, tool_central)
   slt_reg$make_new_version_folder(dv$v2)
   slt_reg$mark("review", dv$v1, list(comment = "first"))
   slt_reg$mark("review", dv$v2, list(comment = "moved"))
   expect_identical(slt_reg$roundup("review", return_as = "version_name"), dv$v2)
})

test_that("slt$update_symlink_registry() edits every root's file; the tool has the type at once", {
   slt_reg <- make_tool_on_roots(tool_roots, tool_central)
   slt_reg$update_symlink_registry("hold", is_singleton = FALSE, operation = "add")
   expect_identical(vmTools:::read_disk_registries(tool_roots)[symlink_type == "hold", .N], 2L)
   expect_true("hold" %in% names(slt_reg$get_symlink_registry()))
})

test_that("Building a verbose tool lists the user-defined types on its roots", {
   expect_message(
      suppressWarnings(SLT$new(
         root_list          = tool_roots
         , central_log_root = tool_central
         , verbose          = TRUE
      ))
      , regexp = "review +TRUE"
   )
})

test_that("A tool is refused on roots whose registry files disagree", {
   update_quiet(tool_roots$root_y, "stalled", is_singleton = TRUE, operation = "update")
   expect_error(make_tool_on_roots(tool_roots, tool_central), regexp = "disagree")
})


# Cleanup ----------------------------------------------------------------------

unlink(root_base, recursive = TRUE)
