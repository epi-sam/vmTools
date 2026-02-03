
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
               "         `-- \033[1;35m1999_09_09.NA\033[0m",
               "            `-- \033[32mQC_1999_09_09.01\033[0m"))
   )
})

