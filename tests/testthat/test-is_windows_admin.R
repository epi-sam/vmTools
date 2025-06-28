if(is_windows() == TRUE) {
   test_that("is_windows_admin works on windows", {
      if (is_windows_admin() == TRUE) {
         expect_true(is_windows_admin())
      } else {
         expect_false(is_windows_admin())
      }
   })
} else {
   test_that("is_windows_admin works on non-windows", {
      expect_false(is_windows_admin())
   })
}
