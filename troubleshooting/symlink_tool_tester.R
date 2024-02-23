# Testing functionality --------------------------------------------------------
# 2024 Feb 01 ------------------------------------------------------------------


# works as expected.
# slt$test_scalar("a")
# slt$test_scalar(c("a", "b"))
# slt$test_print_private("integer")
# slt$test_print_private("numeric")
# slt$test_assert_data_type(1:2)
# slt$test_assert_data_type("x")
# slt$test_assert_data_type("character")
# slt$test_assert_data_type("numeric")

# 2024 Feb 05 ------------------------------------------------------------------

# check that I set paths to the scratch folder
slt <- SLT$new(gbd_round = 'gbd2021')
slt$print_dictionaries()
slt$print_dynamic_fields()
# slt$print_dictionaries(item_name = "ROOTS")
# slt$print_dictionaries(item_name = "sam") # expect error
# slt$print_dictionaries(item_name = c("ROOTS", "gbd_round"))


# try setting a best symlink back and forth

# slt <- SLT$new(gbd_round = 'gbd2021')
# slt$mark_best(date_version = "12345", user_entry = list(comment = "junk"))
# slt$mark_best(date_version = "67890", user_entry = list(comment = "junk"))
# slt$unmark(date_version = "12345", user_entry = list(comment = "testing unmark"))
# slt$unmark(date_version = "67890", user_entry = list(comment = "testing unmark"))

# slt <- SLT$new(gbd_round = 'gbd2021')
# slt$test_fun(date_version = "12345")
#
# system("ln -s /mnt/share/homes/ssbyrne/scratch2/vc/slt/to_model/gbd2021/12345 /mnt/share/homes/ssbyrne/scratch2/vc/slt/to_model/gbd2021/best")
# system("ln -nsf /mnt/share/homes/ssbyrne/scratch2/vc/slt/to_model/gbd2021/54321 /mnt/share/homes/ssbyrne/scratch2/vc/slt/to_model/gbd2021/best")
#
# log_dt <- fread("/mnt/share/homes/ssbyrne/scratch2/vc/slt/to_model/gbd2021/log_version_history.csv")
# glimpse(log_dt)
# glimpse(dt_log)

# 2024 Feb 06 ------------------------------------------------------------------


# str_test <- "lrwxrwxrwx 1 ssbyrne Domain Users  63 Feb  6 17:04 best -> /mnt/share/homes/ssbyrne/scratch2/vc/slt/to_model/gbd2021/67890"
# match_idx <- regexpr(" -> ", str_test)
# # grab the substring before the match
# str_pre_arrow <- substr(str_test, 1, match_idx - 1)
# # grab the last string of non-whitespace characters
# str_symlink <- tail(strsplit(str_pre_arrow, " ")[[1]], 1)
# Not what I wanted
# start_idx <- as.integer(match_idx) + attr(match_idx, "match.length")
# str_len <- nchar(str_test)
# substr(str_test, start_idx, str_len)

# Make some symlinks to remove

# system("ln -nfs /mnt/share/homes/ssbyrne/scratch2/vc/slt/to_model/gbd2021/12345 /mnt/share/homes/ssbyrne/scratch2/vc/slt/to_model/gbd2021/best")
# system("ln -nfs /mnt/share/homes/ssbyrne/scratch2/vc/slt/to_model/gbd2021/12345 /mnt/share/homes/ssbyrne/scratch2/vc/slt/to_model/gbd2021/keep_12345")
# system("ln -nfs /mnt/share/homes/ssbyrne/scratch2/vc/slt/to_model/gbd2021/12345 /mnt/share/homes/ssbyrne/scratch2/vc/slt/to_model/gbd2021/remove_12345")
# system("ln -nfs /mnt/share/homes/ssbyrne/scratch2/vc/slt/to_model/gbd2021/67890 /mnt/share/homes/ssbyrne/scratch2/vc/slt/to_model/gbd2021/best")
# system("ln -nfs /mnt/share/homes/ssbyrne/scratch2/vc/slt/to_model/gbd2021/67890 /mnt/share/homes/ssbyrne/scratch2/vc/slt/to_model/gbd2021/keep_67890")
# system("ln -nfs /mnt/share/homes/ssbyrne/scratch2/vc/slt/to_model/gbd2021/67890 /mnt/share/homes/ssbyrne/scratch2/vc/slt/to_model/gbd2021/remove_67890")
#
# # source("/mnt/share/code/vaccines/ssbyrne/vaccines/version_tools/symlink_tool.R")
# slt <- SLT$new(gbd_round = 'gbd2021')
# slt$unmark(date_version = "12345", user_entry = list(comment = "unmarking before testing marks"))
# slt$unmark(date_version = "67890", user_entry = list(comment = "unmarking before testing marks"))
# slt$mark_keep(date_version = "12345", user_entry = list(comment = "testing mark_keep"))
# slt$mark_best(date_version = "12345", user_entry = list(comment = "testing mark_best"))
# slt$mark_remove(date_version = "12345", user_entry = list(comment = "testing mark_remove"))
# # slt$test_fun(date_version = "12345", list(comment = "testing symlink remover"))
# slt$mark_best(date_version = "67890", user_entry = list(comment = "testing mark_best 67890 after 12345"))
# slt$mark_best(date_version = "12345", user_entry = list(comment = "testing mark_best 12345 after 67890"))
# slt$mark_best(date_version = "54321", user_entry = list(comment = "testing mark_best 54321 after 12345"))
#
# slt$print_dictionaries()
# slt$print_dynamic_fields()
#
# DescTools::SplitPath("/mnt/share/homes/ssbyrne/scratch2/vc/slt")

# 2024 Feb 07 ------------------------------------------------------------------

# testing report functions
# slt <- SLT$new(gbd_round = 'gbd2021')
# slt$reports()

# 2024 Feb 08 ------------------------------------------------------------------
# 2024 Feb 12 ------------------------------------------------------------------
# still refining

# refining reports

# make some BS symlinks that shouldn't exist
# system("ln -nfs /mnt/share/homes/ssbyrne/scratch2/vc/slt/to_model/gbd2021/12345 /mnt/share/homes/ssbyrne/scratch2/vc/slt/to_model/gbd2021/rubbish_test")
# system("ln -nfs /mnt/share/homes/ssbyrne/scratch2/vc/slt/to_model/gbd2021/12345 /mnt/share/homes/ssbyrne/scratch2/vc/slt/to_model/gbd2021/should_not_exist")



# 2024 Feb 15 ------------------------------------------------------------------

# roundup functions

# remove symlinks
# dv <- "2024_02_15_new_output"
# slt <- SLT$new(gbd_round = 'gbd2021')
# slt$make_new_log(dv)
#
# # reading a fully empty file with fread
# sam <- fread("/mnt/share/homes/ssbyrne/scratch2/vc/slt/modeled/gbd2021/2024_02_15_new_output/log_version_history.csv")
# tryCatch(
#    {
#       data.table::fread("/mnt/share/homes/ssbyrne/scratch2/vc/slt/modeled/gbd2021/2024_02_15_new_output/log_version_history.csv")
#    }, warning = function(w) message(
#       "If NULL log is found, new 'create' row will be written. \n ||----- ", w)
# )

# 2024 Feb 21 ------------------------------------------------------------------

slt <- SLT$new(gbd_round = 'gbd2021')
# slt$roundup_remove() # Great, this works as intended
# test out datestamp
# slt$mark_remove(date_version = "2024_02_13_cruddy_model", list(comment = "testing new stamp"))
slt <- SLT$new(gbd_round = 'gbd2021')
# slt$make_new_log(date_version = "2024_02_21_roundup_dates")
slt$roundup_by_date("24/2/24", "gt")
