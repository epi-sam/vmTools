devtools::load_all()

# if desired, start fresh
test_root <- clean_path(tempdir(), "vmTools/slt")
if (FALSE) {
   t_root <- gsub("\\\\", "/", test_root)
}
# unlink(test_root, recursive = TRUE)
system(paste("rm -rf", test_root))

root_list <- list(
   first_root = clean_path(test_root, "first_root"),
   second_root = clean_path(test_root, "second_root"),
   third_root = clean_path(test_root, "third_root")
)
lapply(root_list, dir.create, recursive = TRUE, showWarnings = FALSE)
# SLT$new()
slt <- SLT$new(
   user_root_list          = root_list
   , user_central_log_root = test_root
   # , csv_reader = "read.csv"
)

# should run cleanly with empty directories
slt$roundup_best()
slt$roundup_keep()
slt$roundup_remove()
slt$roundup_unmarked()
slt$roundup_by_date(Sys.Date(), "lt")

# Go through all this again
slt$make_new_version_folder(version_name = "2024_02_29.1")
slt$make_new_version_folder(version_name = "2024_02_29.2")
slt$make_new_version_folder(version_name = "2024_02_29.3")

# Get an automated new version_name compatible with all roots
slt$get_common_new_version_name()

# Make a non-SLT folder, try to re-create it, ensure a log writes
dir.create(clean_path(root_list$first_root, "20240229_handmade"), recursive = TRUE, showWarnings = FALSE)
slt$make_new_version_folder(version_name = "20240229_handmade")
# now delete the non-SLT folder log, mark that folder 'keep' and also expect a log to be created
file.remove(clean_path(root_list$first_root, "20240229_handmade", "log_version_history.csv"))
slt$mark_keep(version_name = "20240229_handmade", user_entry = list(comment = "testing mark on new folder"))
# Ensure validation performs correctly
# slt$mark_best(version_name = "2024_02_29.1", user_entry = list(comment = ""))
# Now mark each one best in turn
slt$mark_best(version_name = "2024_02_29.1", user_entry = list(comment = "testing mark on new folder"))
slt$mark_best(version_name = "2024_02_29.2", user_entry = list(comment = "testing mark on new folder"))
slt$mark_best(version_name = "2024_02_29.3", user_entry = list(comment = "testing mark on new folder"))
slt$mark_best(version_name = "20240229_4", user_entry = list(comment = "testing mark on new folder")) # expect nothing to happen
slt$roundup_by_date("2025-01-27", "gt")
# Now unmark the last one
slt$unmark(version_name = "2024_02_29.3", user_entry = list(comment = "testing unmark on new folder"))
# Mark all as keep
slt$mark_keep(version_name = "2024_02_29.1", user_entry = list(comment = "testing mark on new folder"))
slt$mark_keep(version_name = "2024_02_29.2", user_entry = list(comment = "testing mark on new folder"))
slt$mark_keep(version_name = "2024_02_29.3", user_entry = list(comment = "testing mark on new folder"))
# Now randomly mark and unmark different folders in various ways
slt$mark_keep(version_name = "2024_02_29.1", user_entry = list(comment = "testing mark on new folder"))
slt$mark_remove(version_name = "2024_02_29.2", user_entry = list(comment = "testing mark on new folder"))
slt$mark_best(version_name = "2024_02_29.1", user_entry = list(comment = "testing mark on new folder"))
slt$unmark(version_name = "2024_02_29.1", user_entry = list(comment = "testing mark on new folder"))
slt$mark_best(version_name = "2024_02_29.3", user_entry = list(comment = "testing mark on new folder"))
slt$mark_remove(version_name = "2024_02_29.1", user_entry = list(comment = "testing mark on new folder"))
slt$mark_keep(version_name = "2024_02_29.2", user_entry = list(comment = "testing mark on new folder"))
slt$unmark(version_name = "2024_02_29.2", user_entry = list(comment = "testing mark on new folder"))
slt$mark_keep(version_name = "2024_02_29.1", user_entry = list(comment = "testing mark on new folder"))
# Now delete all the folders, first marking to remove
slt$mark_remove(version_name = "2024_02_29.3", user_entry = list(comment = "testing mark on new folder"))
slt$mark_remove(version_name = "2024_02_29.2", user_entry = list(comment = "testing mark on new folder"))
slt$mark_remove(version_name = "2024_02_29.1", user_entry = list(comment = "testing mark on new folder"))

slt$delete_version_folders(version_name = "2024_02_29.1", user_entry = list(comment = "testing folder deletion"), require_user_input = FALSE)

slt$make_reports()
slt$roundup_best()
slt$roundup_remove()
slt$roundup_keep()
slt$unmark("2024_02_29.2", list(comment = "testing roundup_unmarked"))
slt$roundup_unmarked()
slt$roundup_by_date("2024-02-28", "lte")
slt$roundup_by_date("2024-02-28", "gte")
slt$return_dictionaries()
slt$return_dynamic_fields()


# Test roundup_keep
slt$roundup_keep()


# Clean up
system(paste("rm -rf", test_root))
