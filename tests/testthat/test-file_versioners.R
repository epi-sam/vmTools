test_that("increment_file_version works without an extension",
          {
            d <- file.path(tempdir(), "incr")
            t <- "sample text"
            f <- file.path(d, "file_no_ext")
            withr::local_file(d)
            make_directory(d)
            writeLines(t, f)
            for(i in 1:2){
              fn <- increment_file_version(f)
              writeLines(t, fn)
            }
            expect_equal(
              sort(list.files(d))
              , sort(c(
                "file_no_ext"
                , "file_no_ext_v1"
                , "file_no_ext_v2"
              ))
            )
          })

test_that("increment_file_version detects gaps with regex metacharacters in filename",
          {
            d <- file.path(tempdir(), "incr")
            t <- "sample text"
            f <- file.path(d, "file(1).txt")
            withr::local_file(d)
            make_directory(d)
            writeLines(t, f)
            for(i in 1:3){
              fn <- increment_file_version(f)
              writeLines(t, fn)
            }
            # remove v2 to create a gap
            file.remove(file.path(d, "file(1)_v2.txt"))
            # old code: bad regex treats (1) as capture group, existing_files
            # is empty, gap is invisible -> no error (test would fail)
            # new code: glob2rx escapes parens, gap correctly detected -> error
            expect_error(
              increment_file_version(f)
              , regexp = "Gap in existing versions detected for file:"
            )
          })

test_that("increment_file_version detects orphan versions with regex metacharacters in filename",
          {
            d <- file.path(tempdir(), "incr")
            t <- "sample text"
            f <- file.path(d, "file(1).txt")
            withr::local_file(d)
            make_directory(d)
            writeLines(t, f)
            for(i in 1:3){
              fn <- increment_file_version(f)
              writeLines(t, fn)
            }
            # remove original, leaving orphan versions
            file.remove(f)
            # old code: bad regex -> existing_files empty -> silently returns
            # path (test would fail)
            # new code: glob2rx escapes parens -> versions found -> orphan error
            expect_error(
              increment_file_version(f)
              , regexp = "Your original file .* does not exist, but there are already versioned files"
            )
          })

test_that("increment_file_version creates versioned files",
          {
            d <- file.path(tempdir(), "incr")
            t <- "sample text"
            f <- file.path(d, "file.txt")
            withr::local_file(d)
            make_directory(d)
            writeLines(t, f)
            for(i in 1:5){
              fn <- increment_file_version(f)
              writeLines(t, fn)
            }
            expect_equal(
              sort(list.files(d))
              , sort(c(
                "file.txt"
                , "file_v1.txt"
                , "file_v2.txt"
                , "file_v3.txt"
                , "file_v4.txt"
                , "file_v5.txt"
              ))
            )
          })

test_that("increment_file_version detects removed files",
          {
            d <- file.path(tempdir(), "incr")
            t <- "sample text"
            f <- file.path(d, "file.txt")
            withr::local_file(d)
            make_directory(d)
            writeLines(t, f)
            for(i in 1:5){
              fn <- increment_file_version(f)
              writeLines(t, fn)
            }
            fls <- list.files(d, full.names = TRUE)
            file.remove(fls[grepl("v1\\.|v2\\.|v4\\.", fls)])
            expect_equal(
              sort(list.files(d))
              , sort(c("file.txt", "file_v3.txt", "file_v5.txt"))
            )
          })

test_that("increment_file_version errors on version gap",
          {
            d <- file.path(tempdir(), "incr")
            t <- "sample text"
            f <- file.path(d, "file.txt")
            withr::local_file(d)
            make_directory(d)
            writeLines(t, f)
            for(i in 1:5){
              fn <- increment_file_version(f)
              writeLines(t, fn)
            }
            fls <- list.files(d, full.names = TRUE)
            file.remove(fls[grepl("v1\\.|v2\\.|v4\\.", fls)])
            expect_error(
              increment_file_version(f)
              , regexp = "Gap in existing versions detected for file:"
            )
          })

test_that("increment_file_version creates files after gap with increment_after_gap",
          {
            d <- file.path(tempdir(), "incr")
            t <- "sample text"
            f <- file.path(d, "file.txt")
            withr::local_file(d)
            make_directory(d)
            writeLines(t, f)
            for(i in 1:5){
              fn <- increment_file_version(f)
              writeLines(t, fn)
            }
            fls <- list.files(d, full.names = TRUE)
            file.remove(fls[grepl("v1\\.|v2\\.|v4\\.", fls)])
            for(i in 1:3){
              fn <- increment_file_version(f, increment_after_gap = TRUE)
              writeLines(t, fn)
            }
            expect_equal(
              sort(list.files(d))
              , sort(c(
                "file.txt"
                , "file_v3.txt"
                , "file_v5.txt"
                , "file_v6.txt"
                , "file_v7.txt"
                , "file_v8.txt"
              ))
            )
          })

test_that("increment_file_version errors when original file is missing but versions exist",
          {
            d <- file.path(tempdir(), "incr")
            t <- "sample text"
            f <- file.path(d, "file.txt")
            withr::local_file(d)
            make_directory(d)
            writeLines(t, f)
            for(i in 1:5){
              fn <- increment_file_version(f)
              writeLines(t, fn)
            }
            fls <- list.files(d, full.names = TRUE)
            file.remove(fls[grepl("v1\\.|v2\\.|v4\\.", fls)])
            for(i in 1:3){
              fn <- increment_file_version(f, increment_after_gap = TRUE)
              writeLines(t, fn)
            }
            file.remove(f)
            expect_error(
              increment_file_version(f)
              , regexp = "Your original file .* does not exist, but there are already versioned files"
            )
          })

