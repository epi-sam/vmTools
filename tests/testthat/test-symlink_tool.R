
# Setup ------------------------------------------------------------------------

# library(data.table)

# system(paste("rm -rf", tempdir()))
make_directory <- function(path) dir.create(path, recursive = TRUE, showWarnings = FALSE)
root_base      <- clean_path(tempdir(), "slt")
root_list      <- list(
   root_input  = clean_path(root_base, "dir_1")
   , root_output = clean_path(root_base, "dir_2")
)
make_directory(root_list[["root_input"]])
make_directory(root_list[["root_output"]])
# dir_tree(root_base)
dv_list <- list(
   dv1 = "1990_01_01"
   , dv2 = "1990_01_02"
   , dv3 = "1990_01_03"
)
path_list <- lapply(root_list, function(x) clean_path(x, dv_list))

dv_list_fullpath <- lapply(root_list, function(root) clean_path(root, dv_list))

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

if(tolower(.Platform$OS.type) == "windows" & vmTools:::is_windows_admin() == FALSE){
   testthat::skip("Symbolic links are not supported on Windows without admin privileges")
} else {

   # All integration tests will fail on Windows unless the R process runs with
   # administrative privileges, which is not possible on CRAN servers.
   # - Right click on Rstudio > "Run as administrator" > Yes
   #
   # See description section of ?base::file.symlink.
   #
   # Symbolic links on Windows
   # Symbolic links in the sense of POSIX file systems do not exist on Windows:
   # however, NTFS file systems support two similar concepts.
   #
   # ...
   #
   # A version of symbolic linking to files/directories was implemented more
   # recently, and file.symlink makes use of that interface. However, it has
   # restrictions which are crippling. First, the user needs permission to make
   # symbolic links, and that permission is not normally granted except to
   # Administrator accounts (note: not users with Administrator rights): further
   # many users report that whereas the Policy Editor appears to be able to grant
   # such rights, the API still reports insufficient permissions. Second, the
   # interface needs to know if from is a file or a directory (and it need not yet
   # exist): we have implemented this to allow linking from a directory only if it
   # currently exists.

   # Integration - Make new folders -----------------------------------------------


   test_that("SLT creates new folders",
             {
                lapply(dv_list, function(dv){
                   slt$create_date_version_folders_with_logs(version_name = dv)
                })
                dirlist <- unlist(lapply(root_list, function(root) clean_path(root, dv_list)))
                expect_true(
                   all(file.exists(dirlist))
                )
             }
   )



   # Integration - Marking operations  --------------------------------------------

   test_that("Mark best works",
             {
                slt$mark_best(version_name = dv_list[["dv1"]], user_entry = ue_list[["best"]])
                expect_true(
                   all(file.exists(clean_path(root_list, "best")))
                )
             })


   test_that("Mark keep works",
             {
                slt$mark_keep(version_name = dv_list[["dv1"]], user_entry = ue_list[["keep"]])
                expect_true(
                   all(file.exists(clean_path(root_list, "keep_1990_01_01")))
                )
             })

   test_that("Mark remove works",
             {
                slt$mark_remove(version_name = dv_list[["dv1"]], user_entry = ue_list[["remove"]])
                expect_true(
                   all(file.exists(clean_path(root_list, "remove_1990_01_01")))
                )
             })

   test_that("Key version report has expected content",
             {
                fname_report_key_versions <- slt$return_dictionaries()$report_fnames$all_logs_tool_symlink
                report_key_versions <- data.table::fread(file.path(root_list[[1]], fname_report_key_versions))
                # timestamp won't test well - keep testable columns
                report_key_versions <- report_key_versions[, .(log_id, user, version_name, version_path, action, comment)]
                report_key_versions_test <- structure(
                   list(
                      log_id = 5L,
                      user = Sys.getenv("USER"),
                      version_name = "1990_01_01",
                      version_path = file.path(root_base, "dir_1/1990_01_01"),
                      action = "promote_remove",
                      comment = "Testing mark remove"
                   ),
                   row.names = c(NA,
                                 -1L),
                   class = c("data.table", "data.frame")
                )

                expect_identical(report_key_versions, report_key_versions_test)

             })


   test_that("There is no discrepancy report after SLT marking",
             {
                slt$reports()
                fname_discrepnacy_report <- slt$return_dictionaries()$report_fnames$discrepancies
                expect_false(
                   file.exists(file.path(root_list[[1]], fname_discrepnacy_report))
                )
             })

   test_that("Mark unmark works",
             {
                slt$unmark(version_name = dv_list[["dv1"]] , user_entry = ue_list[["unmark"]])
                expect_true(
                   all(!file.exists(clean_path(root_list, "remove_1990_01_01")))
                )
             })

   test_that("Key version report is empty after unmarking",
             {
                fname_report_key_versions <- slt$return_dictionaries()$report_fnames$all_logs_tool_symlink
                report_key_versions <- data.table::fread(file.path(root_list[[1]], fname_report_key_versions))
                # timestamp won't test well - keep testable columns
                report_key_versions <- report_key_versions[, .(log_id, user, version_name, version_path, action, comment)]
                expect_equal(nrow(report_key_versions), 0)

             })

   test_that("Expected folders exist after marking, unmarking, and running reports",
             {
                expect_equal(
                   list.files(root_list[[1]])
                   , c(
                      "1990_01_01"
                      , "1990_01_02"
                      , "1990_01_03"
                      , "report_all_logs.csv"
                      , "report_all_logs_non_symlink.csv"
                      , "report_all_logs_symlink.csv"
                      , "report_key_versions.csv"
                   )
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
                      log_list[[root]][[1]][, .(log_id, user, version_name, version_path, action, comment)],
                      data.table(
                         log_id = 0:6
                         , user = rep(Sys.info()[["user"]], 7)
                         , version_name = rep(dv_list[[1]], 7)
                         , version_path = rep(clean_path(root_list[[root]], dv_list[[1]]), 7)
                         , action = c("create", "promote_best", "demote_best", "promote_keep", "demote_keep", "promote_remove", "demote_remove")
                         , comment = c("log created", "Testing mark best", "Testing mark keep", "Testing mark keep", "Testing mark remove", "Testing mark remove", "Testing mark unmark")
                      )
                   )
                }
             })

test_that("Central log has expected content",
          {
             fpath_central_log <- slt$return_dictionaries()$LOG_CENTRAL$path
             log_central <- data.table::fread(fpath_central_log)
             # Timstamps won't test well, trim to testable columns
             log_central <- log_central[, .(log_id, user, version_name, version_path, action, comment)]
             log_central_test <- structure(
                list(
                   log_id = 0:12,
                   user = rep(Sys.getenv("USER"), 13),
                   version_name = c(
                      "CENTRAL_LOG",
                      "1990_01_01",
                      "1990_01_01",
                      "1990_01_01",
                      "1990_01_01",
                      "1990_01_01",
                      "1990_01_01",
                      "1990_01_01",
                      "1990_01_01",
                      "1990_01_01",
                      "1990_01_01",
                      "1990_01_01",
                      "1990_01_01"
                   ),
                   version_path = file.path(
                      root_base,
                      c(
                         "log_symlinks_central.csv",
                         "dir_1/1990_01_01",
                         "dir_2/1990_01_01",
                         "dir_1/1990_01_01",
                         "dir_1/1990_01_01",
                         "dir_2/1990_01_01",
                         "dir_2/1990_01_01",
                         "dir_1/1990_01_01",
                         "dir_1/1990_01_01",
                         "dir_2/1990_01_01",
                         "dir_2/1990_01_01",
                         "dir_1/1990_01_01",
                         "dir_2/1990_01_01"
                      )
                   ),
                   action = c(
                      "create",
                      "promote_best",
                      "promote_best",
                      "demote_best",
                      "promote_keep",
                      "demote_best",
                      "promote_keep",
                      "demote_keep",
                      "promote_remove",
                      "demote_keep",
                      "promote_remove",
                      "demote_remove",
                      "demote_remove"
                   ),
                   comment = c(
                      "log created",
                      "Testing mark best",
                      "Testing mark best",
                      "Testing mark keep",
                      "Testing mark keep",
                      "Testing mark keep",
                      "Testing mark keep",
                      "Testing mark remove",
                      "Testing mark remove",
                      "Testing mark remove",
                      "Testing mark remove",
                      "Testing mark unmark",
                      "Testing mark unmark"
                   )
                ),
                row.names = c(NA,
                              -13L),
                class = c("data.table", "data.frame")
             )

             expect_identical(log_central, log_central_test)
          })


   #>  TODO SB - 2024 Oct 16
   #> - test that:
   #> - [x] the log files are created and have the expected content
   #> - [x] central log has expected content
   #> - [x] the key-version report file has expected content
   #> - [x] there's no discrepancy report after normal runs
   #> - [ ] there is an appropriate discrepancy report after some key edits
   #> - [ ] roundups work
   #>   - [ ] dates
   #>   - [ ] best
   #>   - [ ] keep
   #>   - [ ] remove
   #>   - [ ] unmarked
   #> - [ ] deletion works
   #>   - [ ] fail if not marked 'remove'
   #>   - [ ] succeed if marked 'remove'
   #> - add:
   #> - [ ] get_new_dv from SamsElves and test
   #>



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
                      version_name = unlist(dv_list)
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

   # need files to persist during tests, so cannot use `withr::local_file()`
   # - instead, clean up after tests run, or tests will fail on subsequent runs
   test_that(
      "Cleanup is complete",
      {
         # unlink(root_base, recursive = TRUE)
         system(paste("rm -rf", tempdir()))
         expect_error(dir_tree(root_base), regexp = "Failed to search directory.*no such file or directory")
      }
   )

}
