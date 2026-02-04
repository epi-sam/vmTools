#' @title SymlinkComposer R6 class
#' @description Manages multiple SymlinkTool instances for read-only query
#'   operations across multiple projects. Each SLT runs in 'independent mode'
#'   without central logs.
#' @import data.table
#' @importFrom R6 R6Class
#' @export
SLC <- R6::R6Class(
  "Symlink_Composer",

  private = list(

    # Store the named list of SLT instances
    TOOLS = NULL,

    verbose = NULL
  ),

  public = list(

    #' @description Initialize the SymlinkComposer
    #'
    #' @param root_list [named list] Named list where each element is a list of
    #'   root paths. Names become tool identifiers. Example:
    #'   list(
    #'     project_a = list(root1 = "/path/a1", root2 = "/path/a2"),
    #'     project_b = list(root1 = "/path/b1")
    #'   )
    #' @param verbose [lgl] Control message verbosity
    #' @param verbose_startup [lgl] Show startup messages from each SLT
    #' @param csv_reader [chr] CSV reader for all SLT instances (default "fread_quiet")
    #' @param timezone [chr] Timezone for all SLT instances
    #' @param schema_repair [lgl] Schema repair for all SLT instances
    #'
    #' @return [SymlinkComposer] A composer managing multiple SLT instances
    initialize = function(
    root_list
    , verbose         = TRUE
    , verbose_startup = FALSE
    , csv_reader      = "fread_quiet"
    , timezone        = Sys.timezone()
    , schema_repair   = TRUE
    ) {

      # Validate inputs
      assert_named_list(root_list)
      lapply(root_list, assert_named_list)
      assert_scalar(verbose)
      assert_type(verbose, "logical")
      assert_scalar(verbose_startup)
      assert_type(verbose_startup, "logical")
      assert_scalar(csv_reader)
      assert_type(csv_reader, "character")
      assert_scalar(timezone)
      assert_type(timezone, "character")
      assert_scalar(schema_repair)
      assert_type(schema_repair, "logical")

      private$verbose <- verbose

      # Create SLT instances in independent mode (no central logs)
      private$TOOLS <- lapply(names(root_list), function(tool_name) {
        roots <- root_list[[tool_name]]

        if(verbose) message("Creating SLT for: ", tool_name)

        SLT$new(
          user_root_list        = roots
          , user_central_log_root = NULL  # Not used in independent mode
          , schema_repair         = schema_repair
          , verbose               = verbose
          , verbose_startup       = verbose_startup
          , csv_reader            = csv_reader
          , timezone              = timezone
          , .internal_mode        = "independent"
        )
      })

      names(private$TOOLS) <- names(root_list)

      if(verbose) {
        message("\nSymlinkComposer initialized with ", length(private$TOOLS), " tool(s):")
        message("  ", paste(names(private$TOOLS), collapse = ", "))
        message("\nRead-only mode: use roundup_*() methods for queries across tools.")
      }
    },

    #' @description Access individual SLT instances
    #'
    #' @param tool_name [chr] Name of the tool to retrieve
    #'
    #' @return [SLT] The requested SymlinkTool instance
    get_tool = function(tool_name) {
      if(!tool_name %in% names(private$TOOLS)) {
        stop("Tool '", tool_name, "' not found. Available: ",
             paste(names(private$TOOLS), collapse = ", "))
      }
      return(private$TOOLS[[tool_name]])
    },

    #' @description List all tool names
    #'
    #' @return [chr] Vector of tool names
    list_tools = function() {
      return(names(private$TOOLS))
    },

    #' @description Roundup all 'best' symlinks across all tools
    #'
    #' @return [data.table] Combined results with 'tool_name' column
    roundup_all_best = function() {
      results <- lapply(names(private$TOOLS), function(tn) {
        dt <- private$TOOLS[[tn]]$roundup_best()
        if(nrow(dt) > 0) dt[, tool_name := tn]
        return(dt)
      })
      combined <- data.table::rbindlist(results, fill = TRUE)
      if(nrow(combined) > 0) {
        data.table::setcolorder(combined, c("tool_name", setdiff(names(combined), "tool_name")))
      }
      return(combined)
    },

    #' @description Roundup all 'keep' symlinks across all tools
    #'
    #' @return [data.table] Combined results with 'tool_name' column
    roundup_all_keep = function() {
      results <- lapply(names(private$TOOLS), function(tn) {
        dt <- private$TOOLS[[tn]]$roundup_keep()
        if(nrow(dt) > 0) dt[, tool_name := tn]
        return(dt)
      })
      combined <- data.table::rbindlist(results, fill = TRUE)
      if(nrow(combined) > 0) {
        data.table::setcolorder(combined, c("tool_name", setdiff(names(combined), "tool_name")))
      }
      return(combined)
    },

    #' @description Roundup all 'remove' symlinks across all tools
    #'
    #' @return [data.table] Combined results with 'tool_name' column
    roundup_all_remove = function() {
      results <- lapply(names(private$TOOLS), function(tn) {
        dt <- private$TOOLS[[tn]]$roundup_remove()
        if(nrow(dt) > 0) dt[, tool_name := tn]
        return(dt)
      })
      combined <- data.table::rbindlist(results, fill = TRUE)
      if(nrow(combined) > 0) {
        data.table::setcolorder(combined, c("tool_name", setdiff(names(combined), "tool_name")))
      }
      return(combined)
    },

    #' @description Roundup all unmarked folders across all tools
    #'
    #' @return [data.table] Combined results with 'tool_name' column
    roundup_all_unmarked = function() {
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
    },

    #' @description Roundup folders by date across all tools
    #'
    #' @param user_date [chr/Date/POSIXct] Date to query
    #' @param date_selector [chr] One of: 'gt', 'gte', 'lt', 'lte', 'e'
    #'
    #' @return [data.table] Combined results with 'tool_name' column
    roundup_all_by_date = function(user_date, date_selector) {
      # Show timezone message once (SLC controls messaging for aggregated queries)
      if(private$verbose && length(private$TOOLS) > 0) {
        message("Querying across all tools - dates will be formatted according to each tool's configured timezone")
      }

      results <- lapply(names(private$TOOLS), function(tn) {
        dt <- private$TOOLS[[tn]]$roundup_by_date(user_date, date_selector, print_tz_msg = FALSE)
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
#' @param x [Symlink_Composer] The SLC class
#' @param ... [any] Additional arguments to print()
#'
#' @return [stdout]
#' @exportS3Method print Symlink_Composer
#'
#' @examples SLC
print.Symlink_Composer <- function(x, ...) {
  NextMethod("print")
  cat("\n\n    Call SLC$new(root_list = list(...)) to create a Symlink Composer!\n")
  cat("    Manages multiple SLT instances in read-only mode.\n\n")
}
