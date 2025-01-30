# assert_scalar ----------------------------------------------------------------
test_that("assert_scalar works", {
   expect_error(assert_scalar(1:2), regexp = "1:2 must be atomic and length 1L")
})

# assert_scalar_not_empty ------------------------------------------------------
test_that("assert_scalar_not_empty works", {
   expect_error(assert_scalar_not_empty(1:2), regexp = "x must be atomic and length 1L")
   expect_error(assert_scalar_not_empty(Inf), regexp = "Inf is empty in some way.")
})

# assert_type ------------------------------------------------------------------
test_that("assert_type works", {
   expect_error(assert_type(1, "integer"), regexp = "1 must be of type integer")
})

# assert_named_list ------------------------------------------------------------------
test_that("assert_named_list works", {
   expect_no_error(assert_named_list(list(a = 1)))
   expect_error(assert_named_list(1), regexp = "x must be a named list, not vector or data.frame \\(list names may not be whitespace\\)")
   expect_error(assert_named_list(data.frame()), regexp = "x must be a named list, not vector or data.frame \\(list names may not be whitespace\\)")
   expect_error(assert_named_list(list(1)), regexp = "x must be a named list, not vector or data.frame \\(list names may not be whitespace\\)")
})

# assert_dir_exists ------------------------------------------------------------
test_that("assert_dir_exists works", {
   expect_error(assert_dir_exists("nonexistent"), regexp = "root does not exist:")
   expect_no_error(assert_dir_exists("."))
})
