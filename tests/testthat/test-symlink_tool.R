
# Setup ------------------------------------------------------------------------

make_directory <- function(path) dir.create(path, recursive = TRUE, showWarnings = FALSE)
print_tree     <- function(root) {fs::dir_tree(root, recurse = TRUE)}
root_base   <- file.path(tempdir(), "slt")
root_input  <- file.path(root_base, "to_model")
root_output <- file.path(root_base, "modeled")
make_directory(root_input)
make_directory(root_output)
print_tree(root_base)



# Create tool ------------------------------------------------------------------

test_that(
   "Naive startup produces messages and error",
   {
      expect_message(
         expect_message(
            expect_error(
               SLT$new(), regexp = "You must provide both user_root_list and user_central_log_root"
            ) , regexp = "This tool expects `user_central_log_root` to be a single directory for the central log"
         ) , regexp = "This tool expects \\`user_root_list\\` to be a named list of root directories for pipeline outputs"
      )

   }
)

suppressWarnings({ # idiosyncratic and benign cluster message

   slt <- SLT$new(
      user_root_list = list(
         root_input  = root_input,
         root_output = root_output
      )
      , user_central_log_root = root_base
   )

})



# Test private methods ---------------------------------------------------------




# Clean up ---------------------------------------------------------------------
test_that(
   "Cleanup is complete",
   {
      unlink(root_base, recursive = TRUE)
      expect_error(print_tree(root_base), regexp = "\\[ENOENT\\] Failed to search directory.*no such file or directory")
   }
)

