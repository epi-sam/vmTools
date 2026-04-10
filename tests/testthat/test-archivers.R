test_that("archive() moves a file and unarchive() restores it symmetrically", {
  tmp      <- withr::local_tempdir()
  f        <- file.path(tmp, "results.csv")
  writeLines("a,b,c", f)

  archived <- archive(f)
  expect_false(file.exists(f))
  expect_true(file.exists(archived))
  expect_true(grepl("/archive/", archived))

  restored <- unarchive(archived)
  expect_true(file.exists(f))
  expect_false(file.exists(archived))
  expect_equal(basename(restored), "results.csv")
})

test_that("archive() moves a directory and unarchive() restores it symmetrically", {
  tmp <- withr::local_tempdir()
  d   <- file.path(tmp, "model_subdir")
  dir.create(d)
  writeLines("x", file.path(d, "output.csv"))

  archived <- archive(d)
  expect_false(dir.exists(d))
  expect_true(dir.exists(archived))
  expect_true(grepl("/archive/", archived))

  restored <- unarchive(archived)
  expect_true(dir.exists(d))
  expect_false(dir.exists(archived))
  expect_true(file.exists(file.path(d, "output.csv")))
})

test_that("archive(copy = TRUE) preserves the original file", {
  tmp      <- withr::local_tempdir()
  f        <- file.path(tmp, "report.csv")
  writeLines("1,2,3", f)

  archived <- archive(f, copy = TRUE)
  expect_true(file.exists(f))
  expect_true(file.exists(archived))
  expect_error(unarchive(archived), "Destination already exists")
})

test_that("archive(copy = TRUE) preserves the original directory", {
  tmp <- withr::local_tempdir()
  d   <- file.path(tmp, "run_outputs")
  dir.create(d)
  writeLines("result", file.path(d, "data.csv"))

  archived <- archive(d, copy = TRUE)
  expect_true(dir.exists(d))
  expect_true(dir.exists(archived))
  expect_true(file.exists(file.path(d, "data.csv")))
  expect_error(unarchive(archived), "Destination already exists")
})
