#' @title SymlinkComposer R6 class
#' @description Manages multiple pre-instantiated SymlinkTool instances for
#'   aggregated query operations across multiple projects.
#' @import data.table
#' @importFrom R6 R6Class
#' @export
SLC <- R6::R6Class(
  "Symlink_Composer"

  , private = list(

    # Store the named list of SLT instances
    TOOLS = NULL

    , verbose = NULL

  )

  , public = list(

    #' @description Initialize the SymlinkComposer
    #'
    #' @param tool_list (named list) Named list of pre-instantiated SLT objects.
    #'   Names become tool identifiers for combined queries. Example:
    #'   list(
    #'     project_a = my_slt_a,
    #'     project_b = my_slt_b
    #'   )
    #' @param verbose (lgl) Control message verbosity
    #'
    #' @return (SymlinkComposer) A composer managing multiple SLT instances
    initialize = function(
    tool_list
    , verbose = FALSE
    ) {

      # Validate inputs
      assert_named_list(tool_list)
      assert_scalar(verbose)
      assert_type(verbose, "logical")

      # Validate each element is an SLT instance
      lapply(names(tool_list), function(nm) {
        if(!inherits(tool_list[[nm]], "Symlink_Tool")) {
          stop("Element '", nm, "' is not an SLT instance. ",
               "Please provide pre-instantiated SLT objects.")
        }
      })

      private$verbose <- verbose
      private$TOOLS   <- tool_list

      if(verbose) {
        message("\nSymlinkComposer initialized with ", length(private$TOOLS), " tool(s):")
        message("  ", paste(names(private$TOOLS), collapse = ", "))
      }
    }

    #' @description Access individual SLT instances
    #'
    #' @param tool_name (chr) Name of the tool to retrieve
    #'
    #' @return (SLT) The requested SymlinkTool instance
    , get_tool = function(tool_name) {
      if(!tool_name %in% names(private$TOOLS)) {
        stop("Tool '", tool_name, "' not found. Available: ",
             paste(names(private$TOOLS), collapse = ", "))
      }
      return(private$TOOLS[[tool_name]])
    }

    #' @description List all tool names
    #'
    #' @return (chr) Vector of tool names
    , list_tools = function() {
      return(names(private$TOOLS))
    }

    #' @description Roundup symlinks of a given type across all tools
    #'
    #' Valid types are determined by the intersection of `symlink_types` from
    #' all SLT instances held by this composer. Use `$get_common_symlink_types()`
    #' to inspect the valid set.
    #'
    #' @param symlink_type (chr) One of the common symlink types across all tools,
    #'   e.g. "best", "keep", "remove", "latest"
    #'
    #' @return (data.table) Combined results with 'tool_name' column
    , roundup_all_symlinks = function(symlink_type) {
      valid_types <- self$get_common_symlink_types()
      if (!symlink_type %in% valid_types) {
        stop(sprintf(
          "'symlink_type' must be one of: %s\n  Got: '%s'",
          toString(valid_types),
          symlink_type
        ))
      }
      method_name <- sprintf("roundup_%s", symlink_type)
      results <- lapply(names(private$TOOLS), function(tn) {
        dt <- private$TOOLS[[tn]][[method_name]]()
        if (nrow(dt) > 0) dt[, tool_name := tn]
        return(dt)
      })
      combined <- data.table::rbindlist(results, fill = TRUE)
      if (nrow(combined) > 0) {
        data.table::setcolorder(combined, c("tool_name", setdiff(names(combined), "tool_name")))
      }
      return(combined)
    }

    #' @description Return the symlink types common to all held SLT instances
    #'
    #' These are the valid values for the `symlink_type` argument of `roundup_all()`.
    #'
    #' @return (chr) Vector of common symlink type strings
    , get_common_symlink_types = function() {
      type_lists <- lapply(names(private$TOOLS), function(tn) {
        private$TOOLS[[tn]]$return_dictionaries("symlink_types")[["symlink_types"]]
      })
      return(Reduce(intersect, type_lists))
    }

    #' @description Roundup all unmarked folders across all tools
    #'
    #' @return (data.table) Combined results with 'tool_name' column
    , roundup_all_unmarked = function() {
      results <- lapply(names(private$TOOLS), function(tn) {
        dt <- private$TOOLS[[tn]]$roundup_unmarked()
        if(nrow(dt) > 0) dt[, tool_name := tn]
        return(dt)
      })
      combined <- data.table::rbindlist(results, fill = TRUE)
      if(nrow(combined) > 0) {
        data.table::setcolorder(combined, c("tool_name", setdiff(names(combined), "tool_name")))
      }
      return(combined)
    }

    #' @description Roundup folders by date across all tools
    #'
    #' @param user_date (chr/Date/POSIXct) Date to query
    #' @param date_selector (chr) One of: 'gt', 'gte', 'lt', 'lte', 'e'
    #'
    #' @return (data.table) Combined results with 'tool_name' column
    , roundup_all_by_date = function(user_date, date_selector) {
      results <- lapply(names(private$TOOLS), function(tn) {
        dt <- private$TOOLS[[tn]]$roundup_by_date(user_date, date_selector)
        if(nrow(dt) > 0) dt[, tool_name := tn]
        return(dt)
      })
      combined <- data.table::rbindlist(results, fill = TRUE)
      if(nrow(combined) > 0) {
        data.table::setcolorder(combined, c("tool_name", setdiff(names(combined), "tool_name")))
      }
      return(combined)
    }
  )
)

# Add class for custom print
class(SLC) <- c("Symlink_Composer", class(SLC))

#' Symlink Composer custom print method
#'
#' @param x (Symlink_Composer) The SLC class
#' @param ... (any) Additional arguments to print()
#'
#' @return (stdout)
#' @exportS3Method print Symlink_Composer
#'
#' @examples SLC
print.Symlink_Composer <- function(x, ...) {
  NextMethod("print")
  cat("\n\n    Call SLC$new(root_list = list(...)) to create a Symlink Composer!\n")
  cat("    Manages multiple SLT instances in read-only mode.\n\n")
}
