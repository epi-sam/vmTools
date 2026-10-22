
test_that("dir_tree produces correct top-level repo file structure and formatting", {
   expect_equal(
      sort(capture.output(dir_tree(path = "fixtures")))
      , sort(c("`-- \033[1;35mversioned-dirs\033[0m",
               "   `-- \033[1;35mnested\033[0m",
               "      `-- \033[1;35m1999_09_09\033[0m",
               "         |-- \033[1;35m1999_09_09.01\033[0m",
               "         |  `-- \033[32mdata.txt\033[0m",
               "         |-- \033[1;35m1999_09_09.02\033[0m",
               "         |  `-- \033[32mdata.txt\033[0m",
               "         |-- \033[1;35m1999_09_09.15char\033[0m",
               "         |  `-- \033[32mdata.txt\033[0m",
               "         `-- \033[1;35m1999_09_09.NA\033[0m",
               "            `-- \033[32mQC_1999_09_09.01\033[0m"))
   )
})

test_that("dir_tree prints a link with its target and does not expand it", {
   testthat::skip_if_not(vmTools::dir_supports_links(tempdir()), "this machine cannot create symlinks (shortcuts) in tempdir()")
   root <- make_test_root("dir_tree_links")
   on.exit(unlink(root, recursive = TRUE), add = TRUE)
   target <- clean_path(root, "1999_09_09.01")
   make_directory(target)
   file.create(clean_path(target, "data.txt"))
   vmTools:::link_create(target, clean_path(root, "best"))

   out <- gsub("\033\\[[0-9;]*m", "", capture.output(dir_tree(root)))   # strip colour codes

   expect_true(any(endsWith(out, paste0("best -> ", target))))   # the link, with an arrow
   expect_identical(sum(grepl("data.txt", out, fixed = TRUE)), 1L)   # printed once, under the real folder only
})

test_that("dir_tree flags a dangling link", {
   testthat::skip_on_os("windows")   # a dangling junction is invisible to the tool
   testthat::skip_if_not(vmTools::dir_supports_links(tempdir()), "this machine cannot create symlinks (shortcuts) in tempdir()")
   root <- make_test_root("dir_tree_dangling")
   on.exit(unlink(root, recursive = TRUE), add = TRUE)
   target <- clean_path(root, "gone")
   make_directory(target)
   vmTools:::link_create(target, clean_path(root, "best"))
   unlink(target, recursive = TRUE)

   out <- gsub("\033\\[[0-9;]*m", "", capture.output(dir_tree(root)))
   expect_true(any(grepl("best -> .*gone \\(target missing\\)$", out)))
})

