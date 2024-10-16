
# Setup ------------------------------------------------------------------------

make_directory <- function(path) dir.create(path, recursive = TRUE, showWarnings = FALSE)
print_tree     <- function(root) {fs::dir_tree(root, recurse = TRUE)}
root_base      <- file.path(tempdir(), "slt")
root_list      <- list(
   root_input  = file.path(root_base, "dir_1")
   , root_output = file.path(root_base, "dir_2")
)
make_directory(root_list[["root_input"]])
make_directory(root_list[["root_output"]])
print_tree(root_base)
dv_list <- list(
   dv1 = "1990_01_01"
   , dv2 = "1990_01_02"
   , dv3 = "1990_01_03"
)

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

# 2024 Oct 16 - let's put this off for now and focus on some integration tests first


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

# Clean up ---------------------------------------------------------------------
test_that(
   "Cleanup is complete",
   {
      unlink(root_base, recursive = TRUE)
      expect_error(print_tree(root_base), regexp = "\\[ENOENT\\] Failed to search directory.*no such file or directory")
   }
)

