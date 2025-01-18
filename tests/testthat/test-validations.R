# validate_not_empty -----------------------------------------------------------
test_that("validate_not_empty works", {
   expect_true(validate_not_empty(1))
   expect_false(validate_not_empty(NULL))
   expect_false(validate_not_empty(NA))
   expect_false(validate_not_empty(""))
   expect_false(validate_not_empty(NaN))
   expect_false(validate_not_empty(Inf))
   expect_false(validate_not_empty(data.frame()))
})

# validate_dir_exists -----------------------------------------------------------
test_that("validate_dir_exists works", {
   expect_true(validate_dir_exists("."))
   expect_message(expect_false(validate_dir_exists("nonexistent")))
})

# is_an_error ------------------------------------------------------------------
test_that("is_an_error works", {
   expect_true(is_an_error(simpleError("error")))
   expect_true(is_an_error(tryCatch(stop("error"), error = function(e) e)))
   expect_message(expect_false(is_an_error(message("error"))))
})
