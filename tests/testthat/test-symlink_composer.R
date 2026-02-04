# Setup ------------------------------------------------------------------------

# use clean_path() liberally for github/cran checks

.user <- Sys.info()[['user']]
make_directory <- function(path) dir.create(path, recursive = TRUE, showWarnings = FALSE)

# Base directory for all SLC tests
root_base <- clean_path(tempdir(), "slc")

# Project A - two roots
project_a_roots <- list(
   input    = clean_path(root_base, "project_a/input")
   , output = clean_path(root_base, "project_a/output")
)
central_log_a <- clean_path(root_base, "project_a/central_logs")

# Project B - two roots
project_b_roots <- list(
   results = clean_path(root_base, "project_b/results")
   , plots = clean_path(root_base, "project_b/plots")
)
central_log_b <- clean_path(root_base, "project_b/central_logs")

# Project C - single root (tests lazy central log)
project_c_roots <- list(
   main = clean_path(root_base, "project_c/main")
)

# Version names for testing
dv_list <- list(
   "2024_03_01.1"   = "2024_03_01.1"
   , "2024_03_01.2" = "2024_03_01.2"
   , "2024_03_02.1" = "2024_03_02.1"
)

# User entries for marking operations
ue_list <- list(
   best     = list(comment = "Testing mark best")
   , keep   = list(comment = "Testing mark keep")
   , remove = list(comment = "Testing mark remove")
   , unmark = list(comment = "Testing unmark")
)


# Integration Tests ------------------------------------------------------------

# All integration tests will fail on Windows unless the R process runs with
# administrative privileges, which is not possible on CRAN servers.
# See description section of ?base::file.symlink for details.

if(tolower(.Platform$OS.type) == "windows" & vmTools:::is_windows_admin() == FALSE){

   testthat::skip("Symbolic links are not supported on Windows without admin privileges")

} else {


   # Setup directories -----------------------------------------------------------

   # Create all project directories
   lapply(c(unlist(project_a_roots), unlist(project_b_roots), unlist(project_c_roots)), make_directory)
   lapply(c(central_log_a, central_log_b), make_directory)


   # Create SLC ------------------------------------------------------------------

   test_that("SLC rejects non-SLT objects in tool_list", {
      expect_error(
         SLC$new(
            tool_list = list(
               bad_tool = mtcars
            )
         )
         , regexp = "is not an SLT instance"
      )
   })

   test_that("SLC requires a named list", {
      expect_error(
         SLC$new(
            tool_list = "not a list"
         )
         , regexp = "must be a named list"
      )
   })

   # Create individual SLT instances
   suppressMessages(
      suppressWarnings({
         slt_a <- SLT$new(
            root_list          = project_a_roots
            , central_log_root = central_log_a
            , verbose               = FALSE
            , verbose_startup       = FALSE
         )

         slt_b <- SLT$new(
            root_list          = project_b_roots
            , central_log_root = central_log_b
            , verbose               = FALSE
            , verbose_startup       = FALSE
         )

         # Project C uses lazy central log (single root = central log root)
         slt_c <- SLT$new(
            root_list = project_c_roots
            , verbose      = FALSE
         )
      })
   )

   # Create SLC with pre-instantiated tools
   suppressMessages({
      slc <- SLC$new(
         tool_list = list(
            project_a   = slt_a
            , project_b = slt_b
            , project_c = slt_c
         )
         , verbose = FALSE
      )
   })

   test_that("SLC initializes correctly with SLT instances", {
      expect_s3_class(slc, "Symlink_Composer")
      expect_equal(length(slc$list_tools()), 3)
      expect_equal(slc$list_tools(), c("project_a", "project_b", "project_c"))
   })


   # Empty directories -----------------------------------------------------------

   test_that("SLC roundups run cleanly on empty directories", {
      expect_no_error({
         slc$roundup_all_best()
         slc$roundup_all_keep()
         slc$roundup_all_remove()
         slc$roundup_all_unmarked()
         slc$roundup_all_by_date(Sys.Date(), "lt")
      })
   })

   test_that("SLC roundups return empty data.tables with correct structure", {
      best_dt <- slc$roundup_all_best()
      expect_s3_class(best_dt, "data.table")
      expect_equal(nrow(best_dt), 0)
   })


   # Access tools ----------------------------------------------------------------

   test_that("get_tool returns correct SLT instance", {
      tool_a <- slc$get_tool("project_a")
      expect_s3_class(tool_a, "Symlink_Tool")
   })

   test_that("get_tool errors on non-existent tool", {
      expect_error(
         slc$get_tool("nonexistent")
         , regexp = "Tool 'nonexistent' not found"
      )
   })


   # Create version folders via SLC tools ----------------------------------------

   test_that("SLC tools can create version folders", {
      tool_a <- slc$get_tool("project_a")
      tool_b <- slc$get_tool("project_b")
      tool_c <- slc$get_tool("project_c")

      expect_no_error({
         tool_a$make_new_version_folder(version_name = dv_list[["2024_03_01.1"]])
         tool_a$make_new_version_folder(version_name = dv_list[["2024_03_01.2"]])
         tool_b$make_new_version_folder(version_name = dv_list[["2024_03_01.1"]])
         tool_b$make_new_version_folder(version_name = dv_list[["2024_03_02.1"]])
         tool_c$make_new_version_folder(version_name = dv_list[["2024_03_01.1"]])
      })

      # Verify folders exist
      expect_true(file.exists(clean_path(project_a_roots$input, "2024_03_01.1")))
      expect_true(file.exists(clean_path(project_a_roots$output, "2024_03_01.2")))
      expect_true(file.exists(clean_path(project_b_roots$results, "2024_03_02.1")))
      expect_true(file.exists(clean_path(project_c_roots$main, "2024_03_01.1")))
   })


   # Mark operations via SLC tools -----------------------------------------------

   test_that("SLC tools can mark folders and update central logs", {
      tool_a <- slc$get_tool("project_a")
      tool_b <- slc$get_tool("project_b")
      tool_c <- slc$get_tool("project_c")

      expect_no_error({
         tool_a$mark_best(version_name = "2024_03_01.2", user_entry = ue_list[["best"]])
         tool_b$mark_best(version_name = "2024_03_02.1", user_entry = ue_list[["best"]])
         tool_c$mark_keep(version_name = "2024_03_01.1", user_entry = ue_list[["keep"]])
         tool_a$mark_remove(version_name = "2024_03_01.1", user_entry = ue_list[["remove"]])
      })

      # Verify symlinks exist
      expect_true(file.exists(clean_path(project_a_roots$input, "best")))
      expect_true(file.exists(clean_path(project_b_roots$results, "best")))
      expect_true(file.exists(clean_path(project_c_roots$main, "keep_2024_03_01.1")))
      expect_true(file.exists(clean_path(project_a_roots$input, "remove_2024_03_01.1")))
   })


   # Aggregated roundups ---------------------------------------------------------

   test_that("roundup_all_best aggregates across all tools", {
      best_dt <- slc$roundup_all_best()
      expect_s3_class(best_dt, "data.table")
      expect_true("tool_name" %in% names(best_dt))
      expect_true(nrow(best_dt) > 0)

      # Should have results from project_a and project_b
      expect_true("project_a" %in% best_dt$tool_name)
      expect_true("project_b" %in% best_dt$tool_name)
      # project_c has no best, only keep
      expect_false("project_c" %in% best_dt$tool_name)
   })

   test_that("roundup_all_keep aggregates across all tools", {
      keep_dt <- slc$roundup_all_keep()
      expect_s3_class(keep_dt, "data.table")
      expect_true("tool_name" %in% names(keep_dt))

      # Should have results from project_c
      expect_true("project_c" %in% keep_dt$tool_name)
   })

   test_that("roundup_all_remove aggregates across all tools", {
      remove_dt <- slc$roundup_all_remove()
      expect_s3_class(remove_dt, "data.table")
      expect_true("tool_name" %in% names(remove_dt))

      # Should have results from project_a
      expect_true("project_a" %in% remove_dt$tool_name)
   })

   test_that("roundup_all_unmarked aggregates across all tools", {
      unmarked_dt <- slc$roundup_all_unmarked()
      expect_s3_class(unmarked_dt, "data.table")
      expect_true("tool_name" %in% names(unmarked_dt))
   })

   test_that("roundup_all_by_date aggregates across all tools", {
      date_dt <- slc$roundup_all_by_date("2024-03-01", "gte")
      expect_s3_class(date_dt, "data.table")
      expect_true("tool_name" %in% names(date_dt))
      expect_true(nrow(date_dt) > 0)

      # Should have results from all three projects
      expect_true("project_a" %in% date_dt$tool_name)
      expect_true("project_b" %in% date_dt$tool_name)
      expect_true("project_c" %in% date_dt$tool_name)
   })



   # tool_name column ordering ---------------------------------------------------

   test_that("tool_name is the first column in aggregated results", {
      best_dt <- slc$roundup_all_best()
      if(nrow(best_dt) > 0) {
         expect_equal(names(best_dt)[1], "tool_name")
      }

      keep_dt <- slc$roundup_all_keep()
      if(nrow(keep_dt) > 0) {
         expect_equal(names(keep_dt)[1], "tool_name")
      }

      remove_dt <- slc$roundup_all_remove()
      if(nrow(remove_dt) > 0) {
         expect_equal(names(remove_dt)[1], "tool_name")
      }
   })


   # Unmark via SLC tools --------------------------------------------------------

   test_that("SLC tools can unmark folders", {
      tool_a <- slc$get_tool("project_a")

      expect_no_error({
         tool_a$unmark("2024_03_01.2", ue_list[["unmark"]])
      })

      # best symlink should be gone
      expect_false(file.exists(clean_path(project_a_roots$input, "best")))
   })

   test_that("roundup_all_unmarked reflects unmarked folders", {
      unmarked_dt <- slc$roundup_all_unmarked()

      # project_a's 2024_03_01.2 should now be unmarked
      project_a_unmarked <- unmarked_dt[tool_name == "project_a"]
      expect_true("2024_03_01.2" %in% project_a_unmarked$version_name)
   })


   # Central log verification ----------------------------------------------------

   test_that("Central logs are updated by SLC tool operations", {
      # Check that project_a's central log exists and has entries
      central_log_path_a <- slt_a$return_dictionaries()$LOG_CENTRAL$path
      expect_true(file.exists(central_log_path_a))

      central_log <- data.table::fread(central_log_path_a)
      expect_true(nrow(central_log) > 0)

      # Should have mark operations recorded
      expect_true("promote_best" %in% central_log$action)
   })


   # Clean up --------------------------------------------------------------------

   test_that("Cleanup is complete", {
      system(paste("rm -rf", root_base))
      expect_error(dir_tree(root_base), regexp = "Failed to search directory.*no such file or directory")
   })

}
