dir_parent  <- tempdir()
dir_child   <- 'temp_directory_1'
dir_full    <- file.path(dir_parent, dir_child)

test_that("get_latest_output_date_index returns 0 if no dirs exist", {
   # neither of these directories exist
   expect_equal(0, get_latest_output_date_index("/does/not/exist", date = "2001_01_01"))
   expect_equal(0, get_latest_output_date_index("fixtures/versioned-dirs/2000_01_01", date = "2001_01_01"))
})

test_that("get_latest_output_date_index returns correct value", {
   expect_equal(2, get_latest_output_date_index("fixtures/versioned-dirs/nested/1999_09_09", date = "1999_09_09"))
})

test_that("get_new_version_name functionality works", {

   testthat::skip_if_not_installed("withr")

   # create random root directory with self-teardown (`teardown()` is deprecated)
   withr::local_file(dir_full)
   dir.create(dir_full, recursive = TRUE)

   # expect bootstrap to work
   expect_equal("1999_09_09.01", get_new_version_name(root = dir_full, date = "1999_09_09"))
   expect_false(dir.exists(file.path(dir_full, "1999_09_09.01")))
})


# Last test
test_that("test cleanup works - tempdir (dir_parent) exists and dir_full does not",
          {
             expect_true(dir.exists(dir_parent))
             expect_false(dir.exists(dir_full))
          })
