
# Setup ------------------------------------------------------------------------

library(data.table)

make_directory <- function(path) dir.create(path, recursive = TRUE, showWarnings = FALSE)
root_base      <- file.path(tempdir(), "slt")
root_list      <- list(
   root_input  = file.path(root_base, "dir_1")
   , root_output = file.path(root_base, "dir_2")
)
make_directory(root_list[["root_input"]])
make_directory(root_list[["root_output"]])
# dir_tree(root_base)
dv_list <- list(
   dv1 = "1990_01_01"
   , dv2 = "1990_01_02"
   , dv3 = "1990_01_03"
)
path_list <- lapply(root_list, function(x) file.path(x, dv_list))

dv_list_fullpath <- lapply(root_list, function(root) file.path(root, dv_list))

ue_list <- list(
   best = list(comment = "Testing mark best")
   , keep = list(comment = "Testing mark keep")
   , remove = list(comment = "Testing mark remove")
   , unmark = list(comment = "Testing mark unmark")
)


# Create tool ------------------------------------------------------------------

test_that(
   "Naive startup produces messages and error",
   {
      expect_message(
         expect_message(
            expect_error(
               SLT$new(), regexp = "You must provide both user_root_list and user_central_log_root"
            ) , regexp = "This tool expects `user_central_log_root` to be a single directory for the central log"
         ) , regexp = "This tool expects \\`user_root_list\\` to be a named list of root directories for pipeline outputs"
      )

   }
)

# Create a real tool with two roots
# - doing integration tests with multiple roots to really torture test things
suppressMessages(
   suppressWarnings({ # idiosyncratic and benign cluster message

      slt <- SLT$new(
         user_root_list = list(
            root_input  = root_list[["root_input"]],
            root_output = root_list[["root_output"]]
         )
         , user_central_log_root = root_base
      )
   })
)



# Test private methods ---------------------------------------------------------

# 2024 Oct 16 - put this off for now and focus on some integration tests first


# Integration Tests ------------------------------------------------------------

# Integration - Make new folders -----------------------------------------------


test_that("SLT creates new folders",
          {
             lapply(dv_list, function(dv){
                slt$create_date_version_folders_with_logs(date_version = dv)
             })
             dirlist <- unlist(lapply(root_list, function(root) file.path(root, dv_list)))
             expect_true(
                all(file.exists(dirlist))
             )
          }
)



# Integration - Marking operations  --------------------------------------------

test_that("Mark best works",
          {
             slt$mark_best(date_version = dv_list[["dv1"]], user_entry = ue_list[["best"]])
             expect_true(
                all(file.exists(file.path(root_list, "best")))
             )
          })


test_that("Mark keep works",
          {
             slt$mark_keep(date_version = dv_list[["dv1"]], user_entry = ue_list[["keep"]])
             expect_true(
                all(file.exists(file.path(root_list, "keep_1990_01_01")))
             )
          })

test_that("Mark remove works",
          {
             slt$mark_remove(date_version = dv_list[["dv1"]], user_entry = ue_list[["remove"]])
             expect_true(
                all(file.exists(file.path(root_list, "remove_1990_01_01")))
             )
          })

test_that("Mark unmark works",
          {
             slt$unmark(date_version = dv_list[["dv1"]] , user_entry = ue_list[["unmark"]])
             expect_true(
                all(!file.exists(file.path(root_list, "remove_1990_01_01")))
             )
          })

test_that("Expected folders exist after marking and unmarking",
          {
             expect_equal(
                list.files(root_list[[1]])
                , c("1990_01_01", "1990_01_02", "1990_01_03", "report_key_versions.csv")
             )
          })

test_that("Only logs exist so far",
          {
             dv_content_list <- lapply(dv_list_fullpath, list.files, full.names = TRUE)

             dv_log_list <- lapply(dv_list_fullpath, function(dv_content){
                fnames_logs <- list.files(dv_content, pattern = "log", full.names = TRUE)
             })
             expect_equal(dv_content_list, dv_log_list)
          })

test_that("Marked logs have correct structure",
          {
             dv_log_list <- lapply(dv_list_fullpath, function(dv_content){
                fnames_logs <- list.files(dv_content, pattern = "log", full.names = TRUE)
             })
             expect_no_error(
                log_list <- lapply(dv_log_list, function(logs) lapply(logs, fread))
             )
             # First log should go through best, keep, remove and finally unmark steps correctly
             for(root in names(root_list)){
                expect_equal(
                   log_list[[root]][[1]][, .(log_id, user, date_version, version_path, action, comment)],
                   data.table(
                      log_id = 0:6
                      , user = rep(Sys.info()[["user"]], 7)
                      , date_version = rep(dv_list[[1]], 7)
                      , version_path = rep(file.path(root_list[[root]], dv_list[[1]]), 7)
                      , action = c("create", "promote_best", "demote_best", "promote_keep", "demote_keep", "promote_remove", "demote_remove")
                      , comment = c("log created", "Testing mark best", "Testing mark keep", "Testing mark keep", "Testing mark remove", "Testing mark remove", "Testing mark unmark")
                   )
                )
             }
          })

#>  TODO SB - 2024 Oct 16
#> - test that:
#> - [ ] the log files are created and have the expected content
#> - [ ] the report file has expected content
#> - [ ] there's no discrepancy report after some manual edits
#>    - don't worry about minor issues, focus on log order issues, things related to report building
#> - [ ] there is an appropriate discrepancy report after some key edits
#> - [ ] roundups work



# Integration - Roundups -------------------------------------------------------

test_that("Roundup by date throws expected errors",
          {
             expect_error(
                slt$roundup_by_date(user_date = "01-01-2023", date_selector = "gte")
                , regexp = "Invalid user_date. Must be formatted as YYYY MM DD, with one of these delimiters \\[-/_\\] between."
             )
             expect_error(
                slt$roundup_by_date(user_date = "2023-01-01", date_selector = "bte")
                , regexp = "Invalid date_selector. Must be one of \\(case-insensitive\\):.*\n  gt, gte, lt, lte, e"
             )
          })

test_that("Roundup by date works",
          {
             expect_equal(
                slt$roundup_by_date(user_date = "2023-01-01", date_selector = "gte")$root_input
                , data.table(
                   dir_date_version = unlist(dv_list)
                   , dir_name = path_list$root_input
                   , dir_name_resolved = path_list$root_input
                )
             )
          })


# Integration - CSV readers ----------------------------------------------------

suppressMessages(
   suppressWarnings({ # idiosyncratic and benign cluster message

      slt_readcsv <- SLT$new(
         user_root_list = list(
            root_input  = root_list[["root_input"]],
            root_output = root_list[["root_output"]]
         )
         , user_central_log_root = root_base
         , csv_reader = "read.csv"
      )
   })
)

test_that("bad csv_reader option produces expected error",
          {
             expect_error(
                SLT$new(
                   user_root_list = list(
                      root_input  = root_list[["root_input"]],
                      root_output = root_list[["root_output"]]
                   )
                   , user_central_log_root = root_base
                   , csv_reader = "readr"
                )
                , regexp = "csv_reader must be one of: fread, fread_quiet, read.csv, read.csv2"
             )
          })

test_that("read.csv alternate CSV reader works",
          {
             expect_equal(
                slt$roundup_by_date(user_date = "2023-01-01", date_selector = "gte")$root_input
                , slt_readcsv$roundup_by_date(user_date = "2023-01-01", date_selector = "gte")$root_input
             )
          })



# Clean up ---------------------------------------------------------------------

# need files to persist over the tests, so cannot use `withr::local_file()`

test_that(
   "Cleanup is complete",
   {
      unlink(root_base, recursive = TRUE)
      expect_error(dir_tree(root_base), regexp = "Failed to search directory.*no such file or directory")
   }
)

