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
# slt <- SLT$new(gbd_round = 'gbd2021')
# slt$print_dictionaries()
# slt$print_dynamic_fields()
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

# slt <- SLT$new(gbd_round = 'gbd2021')
# slt$roundup_remove() # Great, this works as intended
# test out datestamp
# slt$mark_remove(date_version = "2024_02_13_cruddy_model", list(comment = "testing new stamp"))
# slt$make_new_log(date_version = "2024_02_21_roundup_dates")
# slt <- SLT$new(gbd_round = 'gbd2021')
# slt$roundup_by_date(user_date = "2024/02/13", date_selector = "lte")

# 2024 Feb 23 ------------------------------------------------------------------

# slt <- SLT$new(gbd_round = 'gbd2021') # OK, new log instantiation is good...
# Try out central log
# Does it update?
# slt$mark_best(date_version = "67890", user_entry = list(comment = "testing mark_best in central log"))
# slt$mark_keep(date_version = "67890", user_entry = list(comment = "testing mark_keep in central log"))
# slt$mark_remove(date_version = "67890", user_entry = list(comment = "testing mark_remove in central log"))
# slt <- SLT$new(gbd_round = 'gbd2021') # OK, new log instantiation is good...
# slt$reports()
# slt$unmark(date_version = "67890", user_entry = list(comment = "testing unmark in central log"))
# slt$roundup_by_date(user_date = "2024/02/13", date_selector = "lte") # works as expected
# slt$roundup_by_date(user_date = "2024/02/13", date_selector = "le") # gives desired error message

# 2024 Feb 26 ------------------------------------------------------------------

# debugging extra rows added to central log during `unmark`
# slt <- SLT$new(gbd_round = 'gbd2021')
# slt$unmark(date_version = "67890", user_entry = 'debugging extra demote_best rows added to central log')

# slt <- SLT$new(gbd_round = 'gbd2021')
# slt$mark_best(date_version = "67890", user_entry = list(comment ='debugging extra demote_best rows added to central log'))
# slt$mark_keep(date_version = "67890", user_entry = list(comment ='debugging extra demote_best rows added to central log'))
# slt$mark_remove(date_version = "67890", user_entry = list(comment ='debugging extra demote_best rows added to central log'))
# slt$unmark(date_version = "67890", user_entry = list(comment = 'debugging extra demote_best rows added to central log'))

# 2024 Feb 28 ------------------------------------------------------------------

# testing folder removal
# slt <- SLT$new(gbd_round = 'gbd2021')
# slt$mark_remove(date_version = "20240228_delete_test", user_entry = list(comment = "testing folder removal"))
# slt$delete_date_version_folders(date_version = "20240228_delete_test", user_entry = list(comment = "testing folder removal"))

# testing folder creation - whole folder life-cycle works as expected
slt <- SLT$new(gbd_round = 'gbd2021')
slt$create_date_version_folders_with_logs(date_version = "20240228_test_creation")
slt$make_new_log(date_version = "20240228_test_creation") # should already exist
slt$mark_best(date_version = "20240228_test_creation", user_entry = list(comment = "testing mark on new folder"))
slt$mark_best(date_version = "20240228_test_creation", user_entry = list(comment = "testing mark on new folder", junk_field = "should not exist")) # expect error
slt$mark_keep(date_version = "20240228_test_creation", user_entry = list(comment = "testing mark on new folder"))
slt$unmark(date_version = "20240228_test_creation", user_entry = list(comment = "testing mark on new folder"))
slt$delete_date_version_folders(date_version = "20240228_test_creation", user_entry = list(comment = "testing folder removal on new folder")) # expect a "no remove" message - yes
slt$mark_remove(date_version = "20240228_test_creation", user_entry = list(comment = "testing mark on new folder"))
slt$delete_date_version_folders(date_version = "20240228_test_creation", user_entry = list(comment = "testing folder removal on new folder")) # expect deletion




# 2024 Feb 29 ------------------------------------------------------------------

# debugging why only 'remove_' demotions appear in the central log
# - as last of the 'symlink types', it wasn't getting cleared from log actions in `remove_one_symlink`
# slt <- SLT$new(gbd_round = 'gbd2021')
# slt$mark_remove(date_version = "20240228_test_creation", user_entry = list(comment = "testing mark on new folder"))
# # slt$mark_best(date_version = "20240228_test_creation", user_entry = list(comment = "testing mark on new folder"))
# slt$unmark(date_version = "20240228_test_creation", user_entry = list(comment = "testing mark on new folder"))


# Testing multiple folder management at once

# slt <- SLT$new(gbd_round = 'gbd2021')
# slt$create_date_version_folders_with_logs(date_version = "20240229_1")
# slt$create_date_version_folders_with_logs(date_version = "20240229_2")
# slt$create_date_version_folders_with_logs(date_version = "20240229_3")
# # Now mark each one best in turn
# slt$mark_best(date_version = "20240229_1", user_entry = list(comment = "testing mark on new folder"))
# slt$mark_best(date_version = "20240229_2", user_entry = list(comment = "testing mark on new folder"))
# slt$mark_best(date_version = "20240229_3", user_entry = list(comment = "testing mark on new folder"))
# # Now unmark the last one
# slt$unmark(date_version = "20240229_3", user_entry = list(comment = "testing mark on new folder"))
# # Mark all as keep
# slt$mark_keep(date_version = "20240229_1", user_entry = list(comment = "testing mark on new folder"))
# slt$mark_keep(date_version = "20240229_2", user_entry = list(comment = "testing mark on new folder"))
# slt$mark_keep(date_version = "20240229_3", user_entry = list(comment = "testing mark on new folder"))
# # Now randomly mark and unmark different folders in various ways
# slt$mark_keep(date_version = "20240229_1", user_entry = list(comment = "testing mark on new folder"))
# slt$mark_remove(date_version = "20240229_2", user_entry = list(comment = "testing mark on new folder"))
# slt$mark_best(date_version = "20240229_1", user_entry = list(comment = "testing mark on new folder"))
# slt$unmark(date_version = "20240229_1", user_entry = list(comment = "testing mark on new folder"))
# slt$mark_best(date_version = "20240229_3", user_entry = list(comment = "testing mark on new folder"))
# slt$mark_remove(date_version = "20240229_1", user_entry = list(comment = "testing mark on new folder"))
# slt$mark_keep(date_version = "20240229_2", user_entry = list(comment = "testing mark on new folder"))
# slt$unmark(date_version = "20240229_2", user_entry = list(comment = "testing mark on new folder"))
# slt$mark_keep(date_version = "20240229_1", user_entry = list(comment = "testing mark on new folder"))
# # Now delete all the folders, first marking to remove
# slt$mark_remove(date_version = "20240229_3", user_entry = list(comment = "testing mark on new folder"))
# slt$mark_remove(date_version = "20240229_2", user_entry = list(comment = "testing mark on new folder"))
# slt$mark_remove(date_version = "20240229_1", user_entry = list(comment = "testing mark on new folder"))
# slt$delete_date_version_folders(date_version = "20240229_1", user_entry = list(comment = "testing folder removal on new folder"))
# slt$delete_date_version_folders(date_version = "20240229_2", user_entry = list(comment = "testing folder removal on new folder"))
# slt$delete_date_version_folders(date_version = "20240229_3", user_entry = list(comment = "testing folder removal on new folder"))
#
# slt$reports()
# slt$roundup_remove()
# slt$roundup_by_date("2024-02-28", "gte")
# slt$print_dictionaries()
# slt$print_dynamic_fields()

# my_roots <- list(
#    to_model = "/mnt/share/homes/ssbyrne/scratch2/vc/slt/to_model/gbd2021",
#    modeled  = "/mnt/share/homes/ssbyrne/scratch2/vc/slt/modeled/gbd2021"
# )
# my_roots <- lapply(my_roots, function(root) file.path(root, "gbd2021"))
# central_log_root <- "/mnt/share/homes/ssbyrne/scratch2/vc/slt"
# expect an error
# my_roots <- file.path(my_roots, "gbd2021")

# Try new custom root setting
# slt <- SLT$new(
#    user_root_list = list(
#       to_model = "/mnt/share/homes/ssbyrne/scratch2/vc/slt/to_model/gbd2021",
#       modeled  = "/mnt/share/homes/ssbyrne/scratch2/vc/slt/modeled/gbd2021"
#    )
#    , user_central_log_root = "/mnt/share/homes/ssbyrne/scratch2/vc/slt"
# )
root_base  <- file.path("/mnt/share/homes/ssbyrne/scratch2/vc/slt_debug")
slt <- SLT$new(
   user_root_list = list(
      root_input = "/mnt/share/homes/ssbyrne/scratch2/vc/slt/to_model/gbd2021",
      root_ouput = "/mnt/share/homes/ssbyrne/scratch2/vc/slt/modeled/gbd2021",
      root_third = "/mnt/share/homes/ssbyrne/scratch2/vc/slt/third_root/gbd2021"
   )
   , user_central_log_root = "/mnt/share/homes/ssbyrne/scratch2/vc/slt"
)

# Go through all this again
slt$create_date_version_folders_with_logs(date_version = "20240229_1")
slt$create_date_version_folders_with_logs(date_version = "20240229_2")
slt$create_date_version_folders_with_logs(date_version = "20240229_3")
# Now mark each one best in turn
slt$mark_best(date_version = "20240229_1", user_entry = list(comment = "testing mark on new folder"))
slt$mark_best(date_version = "20240229_2", user_entry = list(comment = "testing mark on new folder"))
slt$mark_best(date_version = "20240229_3", user_entry = list(comment = "testing mark on new folder"))
slt$mark_best(date_version = "20240229_4", user_entry = list(comment = "testing mark on new folder")) # expect nothing to happen
# Now unmark the last one
slt$unmark(date_version = "20240229_3", user_entry = list(comment = "testing mark on new folder"))
# Mark all as keep
slt$mark_keep(date_version = "20240229_1", user_entry = list(comment = "testing mark on new folder"))
slt$mark_keep(date_version = "20240229_2", user_entry = list(comment = "testing mark on new folder"))
slt$mark_keep(date_version = "20240229_3", user_entry = list(comment = "testing mark on new folder"))
# Now randomly mark and unmark different folders in various ways
slt$mark_keep(date_version = "20240229_1", user_entry = list(comment = "testing mark on new folder"))
slt$mark_remove(date_version = "20240229_2", user_entry = list(comment = "testing mark on new folder"))
slt$mark_best(date_version = "20240229_1", user_entry = list(comment = "testing mark on new folder"))
slt$unmark(date_version = "20240229_1", user_entry = list(comment = "testing mark on new folder"))
slt$mark_best(date_version = "20240229_3", user_entry = list(comment = "testing mark on new folder"))
slt$mark_remove(date_version = "20240229_1", user_entry = list(comment = "testing mark on new folder"))
slt$mark_keep(date_version = "20240229_2", user_entry = list(comment = "testing mark on new folder"))
slt$unmark(date_version = "20240229_2", user_entry = list(comment = "testing mark on new folder"))
slt$mark_keep(date_version = "20240229_1", user_entry = list(comment = "testing mark on new folder"))
# Now delete all the folders, first marking to remove
slt$mark_remove(date_version = "20240229_3", user_entry = list(comment = "testing mark on new folder"))
slt$mark_remove(date_version = "20240229_2", user_entry = list(comment = "testing mark on new folder"))
slt$mark_remove(date_version = "20240229_1", user_entry = list(comment = "testing mark on new folder"))
slt$delete_date_version_folders(date_version = "20240229_1", user_entry = list(comment = "testing folder removal on new folder"))
slt$delete_date_version_folders(date_version = "20240229_2", user_entry = list(comment = "testing folder removal on new folder"))
slt$delete_date_version_folders(date_version = "20240229_3", user_entry = list(comment = "testing folder removal on new folder"))

slt$reports()
slt$roundup_remove()
slt$roundup_by_date("2024-02-28", "lte")
slt$roundup_by_date("2024-02-28", "gte")
slt$print_dictionaries()
slt$print_dynamic_fields()

# 2024 Mar 06 ------------------------------------------------------------------
# test initialize messages
# slt <- SLT$new()

# Deal with hard-baked list name scheme
slt <- SLT$new(
   user_root_list = list(
      root_input = "/mnt/share/homes/ssbyrne/scratch2/vc/slt/to_model/gbd2021",
      root_ouput = "/mnt/share/homes/ssbyrne/scratch2/vc/slt/modeled/gbd2021",
      root_third = "/mnt/share/homes/ssbyrne/scratch2/vc/slt/third_root/gbd2021"
   )
   , user_central_log_root = "/mnt/share/homes/ssbyrne/scratch2/vc/slt"
)

# 2024 Mar 07 ------------------------------------------------------------------

# Vignette is failing for unknown reasons
# - is this just a markdown bug?
# - due to the 'filter_null_logs_safely' function needing some tweaking, now resolved 2024 Mar 09

# Clean-up
# unlink(root_base, recursive = TRUE, force = TRUE)
#
# source("~/rstudio/projects/vmTools/R/symlink_tool.R")
# root_base  <- file.path("/mnt/share/homes/ssbyrne/scratch2/vc/slt_debug")
# root_input <- file.path(root_base, "to_model")
# root_ouput <- file.path(root_base, "modeled")
# dir.create(root_input, recursive = TRUE, showWarnings = FALSE)
# dir.create(root_ouput, recursive = TRUE, showWarnings = FALSE)
# print_tree <- function() {fs::dir_tree(root_base, recurse = TRUE)}
# print_tree()
# slt <- SLT$new(
#    user_root_list = list(
#       root_input = root_input,
#       root_ouput = root_ouput
#    )
#    , user_central_log_root = root_base
# )
# print_tree()
# # First we'll create two `date_version` folders to play with in each root
# dir.create(file.path(root_input, "2024_02_02"), recursive = TRUE, showWarnings = FALSE)
# dir.create(file.path(root_ouput, "2024_02_02"), recursive = TRUE, showWarnings = FALSE)
# dir.create(file.path(root_input, "2024_04_10"), recursive = TRUE, showWarnings = FALSE)
# dir.create(file.path(root_ouput, "2024_04_10"), recursive = TRUE, showWarnings = FALSE)
# # Define some paths for later use
# PATHS <- list(
#    log_cent = file.path(root_base, "log_symlinks_central.csv"),
#    log_2024_02_02 = file.path(root_input, "2024_02_02", "log_version_history.csv"),
#    log_2024_04_10 = file.path(root_input, "2024_04_10", "log_version_history.csv")
# )
# # Then we'll mark one as best
# slt$mark_best(date_version = "2024_02_02", user_entry = list(comment = "testing mark_best"))
# slt$mark_best(date_version = "2024_04_10", user_entry = list(comment = "testing mark_best"))
#
# # look at folders
# print_tree()
# system(paste("ls -alt", root_input))
# system(paste("ls -alt", root_ouput))
#
# # And we'll look at logs
#
# fread(PATHS$log_cent)
# fread(PATHS$log_2024_02_02)
# fread(PATHS$log_2024_04_10)
#
# undebug(slt$mark_best)
# slt$mark_best(date_version = "2024_02_02", user_entry = list(comment = "testing mark_best"))
