devtools::load_all()

# if desired, start fresh
test_root <- clean_path(tempdir(), "vmTools/slc")
if (FALSE) {
   t_root <- gsub("\\\\", "/", test_root)
}
# unlink(test_root, recursive = TRUE)
system(paste("rm -rf", test_root))

# Create root_list structure for multiple projects
root_list <- list(
   project_a = list(
      input  = clean_path(test_root, "project_a/input")
      , output = clean_path(test_root, "project_a/output")
   ),
   project_b = list(
      results = clean_path(test_root, "project_b/results")
      , plots   = clean_path(test_root, "project_b/plots")
   ),
   project_c = list(
      main = clean_path(test_root, "project_c/main")
   )
)

# Create all directories
all_paths <- unlist(root_list, use.names = FALSE)
lapply(all_paths, dir.create, recursive = TRUE, showWarnings = FALSE)

# Instantiate SLC
# SLC$new()  # call with no args to see instructions
slc <- SLC$new(
   root_list       = root_list
   , verbose         = FALSE
   , verbose_startup = FALSE
)

# Test basic info methods
slc$list_tools()

# Should run cleanly with empty directories
slc$roundup_all_best()
slc$roundup_all_keep()
slc$roundup_all_remove()
slc$roundup_all_unmarked()
slc$roundup_all_by_date(Sys.Date(), "lt")

# Create version folders using individual tools
project_a_tool <- slc$get_tool("project_a")
project_b_tool <- slc$get_tool("project_b")
project_c_tool <- slc$get_tool("project_c")

# Create some version folders
project_a_tool$make_new_version_folder(version_name = "2024_03_01.1")
project_a_tool$make_new_version_folder(version_name = "2024_03_01.2")
project_b_tool$make_new_version_folder(version_name = "2024_03_01.1")
project_b_tool$make_new_version_folder(version_name = "2024_03_02.1")
project_c_tool$make_new_version_folder(version_name = "2024_03_01.1")

# Mark some folders
project_a_tool$mark_best(version_name = "2024_03_01.2", user_entry = list(comment = "testing best"))
project_b_tool$mark_best(version_name = "2024_03_02.1", user_entry = list(comment = "testing best"))
project_c_tool$mark_keep(version_name = "2024_03_01.1", user_entry = list(comment = "testing keep"))
project_a_tool$mark_remove(version_name = "2024_03_01.1", user_entry = list(comment = "testing remove"))

# Test roundup methods across all tools
slc$roundup_all_best()
slc$roundup_all_keep()
slc$roundup_all_remove()
slc$roundup_all_unmarked()

# Test date-based roundup
slc$roundup_all_by_date("2024-03-01", "gte")
slc$roundup_all_by_date("2024-03-02", "lt")

# Unmark some folders and test roundup_all_unmarked
project_a_tool$unmark("2024_03_01.2", list(comment = "testing unmarked roundup"))
slc$roundup_all_unmarked()

# Test that tool names appear correctly in results
best_dt <- slc$roundup_all_best()
if(nrow(best_dt) > 0) {
   print("Tool names in best roundup:")
   print(unique(best_dt$tool_name))
}

# Test accessing non-existent tool
try(slc$get_tool("nonexistent"), silent = FALSE)

# Clean up
system(paste("rm -rf", test_root))
