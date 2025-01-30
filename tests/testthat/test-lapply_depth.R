test_that("lapply_depth plucking works", {
  expect_equal(
     lapply_depth(list(A = list(one = 1, two = 2), B = list(one = 3, two = 4)), 1, `[[`, "one")
     , list(A = 1, B = 3)
  )
})
