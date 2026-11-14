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
    , verbose_startup = NULL

    # Symlink types held by each tool, read from the tools at call time
    #
    # @return (named list of chr) one vector of symlink types per tool
    , get_symlink_types_by_tool = function() {
      return(lapply(private$TOOLS, function(tool) names(tool$get_symlink_registry())))
    }

    # Say which symlink types only some tools hold, if any
    #
    # Silent when every tool holds the same types, or when the composer was
    # built with `verbose = FALSE`.
    #
    # @return (lgl) TRUE invisibly if a message was sent
    , message_if_symlink_types_differ = function() {
      if (!private$verbose) return(invisible(FALSE))
      types_by_tool <- private$get_symlink_types_by_tool()
      types_all     <- unique(unlist(types_by_tool, use.names = FALSE))
      types_shared  <- Reduce(intersect, types_by_tool)
      types_partial <- setdiff(types_all, types_shared)
      if (length(types_partial) == 0L) return(invisible(FALSE))

      lines <- vapply(types_partial, function(symlink_type) {
        holds_type <- vapply(types_by_tool, function(types) symlink_type %in% types, logical(1))
        tools_with <- toString(names(types_by_tool)[holds_type])
        return(sprintf("  %s: %s", symlink_type, tools_with))
      }, character(1))
      message(
        "This composer's symlink (shortcut) types are a superset of some tools' types.\n"
        , "Types held by only some tools, and the tools that hold them:\n"
        , paste(lines, collapse = "\n")
        , "\nroundup_all() returns no rows from a tool that lacks the type."
      )
      return(invisible(TRUE))
    }

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
    #' @param verbose (lgl: default TRUE) Send the message that lists symlink
    #'   types held by only some of the tools? It is sent only when the tools'
    #'   types differ, at construction and from `$get_symlink_types()`. It
    #'   controls nothing else, and does not change the tools' own `verbose`.
    #' @param verbose_startup (lgl: default FALSE) Send the start-up summary
    #'   that counts and names the tools held? It controls nothing else.
    #'
    #' @return (SymlinkComposer) A composer managing multiple SLT instances
    initialize = function(
    tool_list
    , verbose         = TRUE
    , verbose_startup = FALSE
    ) {

      # Validate inputs
      assert_named_list(tool_list)
      assert_scalar(verbose)
      assert_type(verbose, "logical")
      assert_scalar(verbose_startup)
      assert_type(verbose_startup, "logical")

      # Validate each element is an SLT instance
      lapply(names(tool_list), function(tool_name) {
        if(!inherits(tool_list[[tool_name]], "Symlink_Tool")) {
          stop("Element '", tool_name, "' is not a Symlink Tool (SLT) instance. ",
               "Please provide pre-instantiated SLT objects.")
        }
      })

      private$verbose         <- verbose
      private$verbose_startup <- verbose_startup
      private$TOOLS           <- tool_list

      if(verbose_startup) {
        message("\nSymlink Composer initialized with ", length(private$TOOLS), " tool(s):")
        message("  ", paste(names(private$TOOLS), collapse = ", "))
      }

      private$message_if_symlink_types_differ()
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
    #' Valid types are the union of the symlink types of all SLT instances held
    #' by this composer: any type that at least one tool holds. A tool that
    #' lacks the type contributes no rows. Use `$get_symlink_types()` to
    #' inspect the valid set.
    #'
    #' @param symlink_type (chr) A symlink type held by at least one tool,
    #'   e.g. "best", "keep", "remove", "latest"
    #' @param return_as (chr) `"table"` (default) or `"version_name"`
    #' @return (data.table/chr)
    , roundup_all = function(symlink_type, return_as = c("table", "version_name")) {
       return_as <- match.arg(return_as)
       types_by_tool <- private$get_symlink_types_by_tool()
       checkmate::assert_choice(symlink_type, unique(unlist(types_by_tool, use.names = FALSE)))

       holds_type  <- vapply(types_by_tool, function(types) symlink_type %in% types, logical(1))
       tools_asked <- names(types_by_tool)[holds_type]
       results <- lapply(tools_asked, function(tool_name) {
          dt <- private$TOOLS[[tool_name]]$roundup(symlink_type = symlink_type, return_as = "table")
          if (nrow(dt) > 0) dt[, tool_name := tool_name]
          return(dt)
       })

       combined <- data.table::rbindlist(results, fill = TRUE)

       if (nrow(combined) > 0) {
          data.table::setcolorder(combined, c("tool_name", setdiff(names(combined), "tool_name")))
       }

       if (return_as == "version_name") return(unique(combined$version_name))
       return(combined)
    }

    #' @description Return the symlink types held by any SLT instance of this
    #'   composer
    #'
    #' The union of the tools' types, read from the tools each time: the
    #' built-in types first, then the rest, each group alphabetical. These are
    #' the valid values for the `symlink_type` argument of `roundup_all()`.
    #' Sends a message if some types are held by only some tools and the
    #' composer was built with `verbose = TRUE`.
    #'
    #' @return (chr) Vector of symlink type strings
    , get_symlink_types = function() {
       private$message_if_symlink_types_differ()
       types_all <- unique(unlist(private$get_symlink_types_by_tool(), use.names = FALSE))
       builtins  <- intersect(names(get_builtin_symlink_types()), types_all)
       others    <- sort(setdiff(types_all, builtins))
       return(c(builtins, others))
    }

    #' @description Roundup all unmarked folders across all tools
    #'
    #' @param return_as (chr default: `"table"`) or `"version_name"`
    #'
    #' @return (data.table) Combined results with 'tool_name' column
    , roundup_all_unmarked = function(return_as = c("table", "version_name")) {
       return_as <- match.arg(return_as)
       results <- lapply(names(private$TOOLS), function(tool_name) {
          dt <- private$TOOLS[[tool_name]]$roundup_unmarked(return_as = "table")
          if (nrow(dt) > 0) dt[, tool_name := tool_name]
          return(dt)
       })
       combined <- data.table::rbindlist(results, fill = TRUE)
       if (nrow(combined) > 0) {
          data.table::setcolorder(combined, c("tool_name", setdiff(names(combined), "tool_name")))
       }
       if (return_as == "version_name") return(unique(combined$version_name))
       return(combined)
    }

    #' @description Roundup folders by date across all tools
    #'
    #' @param user_date (chr/Date/POSIXct) Date to query
    #' @param date_selector (chr) One of: 'gt', 'gte', 'lt', 'lte', 'e'
    #' @param return_as (chr default: `"table"`) or `"version_name"`
    #'
    #' @return (data.table) Combined results with 'tool_name' column
    , roundup_all_by_date = function(user_date, date_selector, return_as = c("table", "version_name")) {
       return_as <- match.arg(return_as)
       results <- lapply(names(private$TOOLS), function(tool_name) {
          dt <- private$TOOLS[[tool_name]]$roundup_by_date(user_date, date_selector, return_as = "table")
          if (nrow(dt) > 0) dt[, tool_name := tool_name]
          return(dt)
       })
       combined <- data.table::rbindlist(results, fill = TRUE)
       if (nrow(combined) > 0) {
          data.table::setcolorder(combined, c("tool_name", setdiff(names(combined), "tool_name")))
       }
       if (return_as == "version_name") return(unique(combined$version_name))
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
