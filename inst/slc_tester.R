devtools::load_all()

# if desired, start fresh
test_root <- clean_path(tempdir(), "vmTools/slc")
if (FALSE) {
   t_root <- gsub("\\\\", "/", test_root)
}
# unlink(test_root, recursive = TRUE)
system(paste("rm -rf", test_root))

# Create directory structure for multiple projects
project_a_roots <- list(
   input  = clean_path(test_root, "project_a/input")
   , output = clean_path(test_root, "project_a/output")
)
project_b_roots <- list(
   results = clean_path(test_root, "project_b/results")
   , plots   = clean_path(test_root, "project_b/plots")
)
project_c_roots <- list(
   main = clean_path(test_root, "project_c/main")
)

# Create all directories
all_paths <- c(unlist(project_a_roots), unlist(project_b_roots), unlist(project_c_roots))
lapply(all_paths, dir.create, recursive = TRUE, showWarnings = FALSE)

# Create central log directories
central_log_a <- clean_path(test_root, "project_a/central_logs")
central_log_b <- clean_path(test_root, "project_b/central_logs")
# central_log_c <- clean_path(test_root, "project_c/central_logs")
lapply(c(central_log_a, central_log_b, central_log_c), dir.create, recursive = TRUE, showWarnings = FALSE)



# ---- Instantiate SLC -----------------------------------------------------------------
# use pre-instantiated Symlink tools

slc <- SLC$new(
   tool_list = list(
      project_a = SLT$new(
         root_list          = project_a_roots
         , central_log_root = central_log_a
         , verbose               = TRUE # let one be verbose for messaging
      )
      , project_b = SLT$new(
         root_list          = project_b_roots
         , central_log_root = central_log_b
      )
      # ensure the lazy-central log definition is preserved
      , project_c = SLT$new(root_list = project_c_roots)
      # , project_nonexistent = mtcars # expect error
   )
)

# Test basic info methods
slc$list_tools()

# Should run cleanly with empty directories
slc$roundup_all_best()
slc$roundup_all_keep()
slc$roundup_all_remove()
slc$roundup_all_unmarked()
slc$roundup_all_by_date(Sys.Date(), "lt")

# Access individual tools - these are full SLTs with central logs
project_a_tool <- slc$get_tool("project_a")
project_b_tool <- slc$get_tool("project_b")
project_c_tool <- slc$get_tool("project_c")

# Create some version folders
project_a_tool$make_new_version_folder(version_name = "2024_03_01.1")
project_a_tool$make_new_version_folder(version_name = "2024_03_01.2")
project_b_tool$make_new_version_folder(version_name = "2024_03_01.1")
project_b_tool$make_new_version_folder(version_name = "2024_03_02.1")
project_c_tool$make_new_version_folder(version_name = "2024_03_01.1")

# Mark some folders (these will update central logs!)
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

# ---- Clean up -----------------------------------------------------------------
system(paste("rm -rf", test_root))
