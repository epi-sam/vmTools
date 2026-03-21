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
      lapply(names(tool_list), function(tool_name) {
        if(!inherits(tool_list[[tool_name]], "Symlink_Tool")) {
          stop("Element '", tool_name, "' is not a Symlink Tool (SLT) instance. ",
               "Please provide pre-instantiated SLT objects.")
        }
      })

      private$verbose <- verbose
      private$TOOLS   <- tool_list

      if(verbose) {
        message("\nSymlink Composer initialized with ", length(private$TOOLS), " tool(s):")
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
    #' @param type (chr) `"table"` (default) or `"version_name"`
    #' @return (data.table/chr)
    , roundup_all = function(symlink_type, type = c("table", "version_name")) {
       type <- match.arg(type)
       checkmate::assert_choice(symlink_type, names(get_symlink_registry()))

       results <- lapply(names(private$TOOLS), function(tool_name) {
          dt <- private$TOOLS[[tool_name]]$roundup(symlink_type = symlink_type, type = "table")
          if (nrow(dt) > 0) dt[, tool_name := tool_name]
          return(dt)
       })

       combined <- data.table::rbindlist(results, fill = TRUE)

       if (nrow(combined) > 0) {
          data.table::setcolorder(combined, c("tool_name", setdiff(names(combined), "tool_name")))
       }

       if (type == "version_name") return(unique(combined$version_name))
       return(combined)
    }

    #' @description Return the symlink types common to all held SLT instances
    #'
    #' These are the valid values for the `symlink_type` argument of `roundup_all()`.
    #'
    #' @return (chr) Vector of common symlink type strings
    , get_common_symlink_types = function() {
       get_symlink_types()
    }

    #' @description Roundup all unmarked folders across all tools
    #'
    #' @param type (chr default: `"table"`) or `"version_name"`
    #'
    #' @return (data.table) Combined results with 'tool_name' column
    , roundup_all_unmarked = function(type = c("table", "version_name")) {
       type <- match.arg(type)
       results <- lapply(names(private$TOOLS), function(tool_name) {
          dt <- private$TOOLS[[tool_name]]$roundup_unmarked(type = "table")
          if (nrow(dt) > 0) dt[, tool_name := tool_name]
          return(dt)
       })
       combined <- data.table::rbindlist(results, fill = TRUE)
       if (nrow(combined) > 0) {
          data.table::setcolorder(combined, c("tool_name", setdiff(names(combined), "tool_name")))
       }
       if (type == "version_name") return(unique(combined$version_name))
       return(combined)
    }

    #' @description Roundup folders by date across all tools
    #'
    #' @param user_date (chr/Date/POSIXct) Date to query
    #' @param date_selector (chr) One of: 'gt', 'gte', 'lt', 'lte', 'e'
    #' @param type (chr default: `"table"`) or `"version_name"`
    #'
    #' @return (data.table) Combined results with 'tool_name' column
    , roundup_all_by_date = function(user_date, date_selector, type = c("table", "version_name")) {
       type <- match.arg(type)
       results <- lapply(names(private$TOOLS), function(tool_name) {
          dt <- private$TOOLS[[tool_name]]$roundup_by_date(user_date, date_selector, type = "table")
          if (nrow(dt) > 0) dt[, tool_name := tool_name]
          return(dt)
       })
       combined <- data.table::rbindlist(results, fill = TRUE)
       if (nrow(combined) > 0) {
          data.table::setcolorder(combined, c("tool_name", setdiff(names(combined), "tool_name")))
       }
       if (type == "version_name") return(unique(combined$version_name))
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
