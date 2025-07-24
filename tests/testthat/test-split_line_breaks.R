test_that("split_line_breaks works", {
  expect_equal(split_line_breaks(c('a', 'b\nc')), c("a", "b", "c"))
})
