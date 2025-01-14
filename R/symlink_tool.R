#> started: 2024 Jan 18 12:11:00
#> purpose: tool to manage symlinks for outputs in a standardized way
#> - using as an intermediate option for version management since log_tool is more complex than anticipated
#>

# TODS for v 1.0
# ~TODO~ SB - 2024 Jan 18 - ensure each date_version can only have one symlink (ls -l | grep ); DONE 2024 Feb 05
# ~TODO~ SB - 2024 Feb 05 - User function get declarative interior
# ~TODO~ SB - 2024 Feb 06 - Fix phantom symlink showing up on log creation
# ~TODO~ SB - 2024 Feb 06 - Get simultaneous promotion/demotion working across symlink types
# - probably want a strategy change
#   - first assert only one extant symlink _per version_ (ensure no handmade links exist)
#   - find and demote existing symlinks by _existing type_
#   - then move onto promotion by user's _chosen type_
# TODO SB - 2024 Feb 07 - write a report function
# - [x] Function to read all logs in a `root` - `try(read_log())` for every folder
#     - [x] separate into active symlinks and other folders
#     - [x] any other symlinks (probably won't get logs for these - how to handle?)
#     - [x] validate active symlinks have `promote` in last row
#     - [x] validate others have `demote` in last row
#         - [x] discrepancies imply tool bug or user manipulation
#     - [x] Wrap report updater into `handler_post_mark`
# TODO SB - 2024 Feb 07 - write a tool for deletion and roundup of st-gpr models
#  - [x] pull assert_data_schema (maybe abbreviated form) into the tool
# TODO SB - 2024 Feb 06 - write a tool for deletion and roundup of st-gpr models
# - require user-input to confirm deletion
# - [x] Function to roundup all  active 'remove' symlinks
#     - [x] this should wrap the `find_active_symlinks` function and filter
# TODO SB - 2024 Feb 23 - Other Roundups
# - [x] by date
# TODO SB - 2024 Feb 23 - Central log
# - [x] central log is updated after all marking operations
# TODO SB - 2024 Feb 26 - THINK ABOUT
# - [x] central updates ONLY with the user-intended action, and does not show demotions, as the date_version logs do
#       - this wasn't _intended_ behavior, but may or may not be desirable
#       - could be nice to have "high level" view in central log, the fine-grained chain-of-custody in the date_version logs
#       - this would show reveal if a symlink were deleted by hand, rather than with the tool, but leave the central log a bit more readable
#       - this would also make the central log a bit more "high level" and less "fine-grained"
#       - THIS IS FINE for v 1.0
# TODO SB - 2024 Feb 28 -
# - [x] option to delete `remove_` folders and symlink and append action to central log
# - [x] option to create new folders with new log
# TODO SB - 2024 Feb 15 -
# - [x] public function for empty log (for first pipeline outputs)
# - [x] allow user to set root(s) at initialization
# TODO SB - 2024 Jun 07
# - [x] fully document the package
# - [ ] consistent strategy to pass around private$DICT values
#     PICK ONE:
#     - [ ] use private$DICT directly
#     - [ ] submit private$DICT through all function args


# LATER stuff - v2.0
# TODO SB - 2024 Feb 06
# - [x] don't demote if the symlink is already the desired type
#       - clutters up the logs, but not a deal-breaker

#> NOTE:
#> These are required, but all namespaced within:
#>
#> library(R6)
#> library(DescTools)
#> library(data.table)
#> library(lubridate)
#>
#> WARNING: DO NOT touch '.__enclos_env__' unless you want the tool to break
#>
#> NOTES:
#>   DEBUGGING:
#>     - The 'red dot' stop point will not work with R6 objects
#>     - Use `debug(my_fun)/undebug(my_fun)` or place a `browser()` in the code
#>     - See https://r6.r-lib.org/articles/Debugging.html
#>
#>   LIBRARIES
#>     - Relies on libraries declared above, but calls on all functions by package namespace for clarity and robustness
#>     - data.tables themselves cannot be namespaced
#>
#>   ROXYGEN
#>     - Roxygen docstrings cannot be added to private methods, because reasons
#>
#> Required for vignette to work in Rmarkdown
#> Known issue: https://github.com/rstudio/rmarkdown/issues/187
.datatable.aware = TRUE

SLT <- R6::R6Class(

   "Symlink_Tool",

   private = list(

      # Dictionaries -----------------------------------------------------------

      #> All private$___ items are functions, except for:
      #> - constants stored in the DICT sub-list
      #> - dynamic values stored in the DYNAMIC sub-list

      DICT = list(

         #> The tool expects that each 'root' has a bunch of 'date_version' folders of pipeline output
         #> - each 'date_version' represents a fresh run of a data pipeline
         #> - each 'date_version' must be a folder one level under each 'root'
         #> - the tool expects that each 'date_version' exists in each 'root',
         #>   - i.e. you could split pipeline outputs from the same `date_version` into different 'root' folders
         #> - the tool can work with heterogeneous 'root' folders
         #>   - i.e. you don't need exactly the same 'date_versions' in each, but the tool will try to manage `date_versions` across `roots` in parallel
         #>
         #> Every time the tool runs a 'mark' operation, it will update the report on tool-generated active symlinks


         # Control-flow flags
         FLAGS = list(
            # TRUE if logs formats may differ over time
            # FALSE if user requires logs formats to be consistent
            allow_schema_repair = NULL
         ),

         ## Initialize: user-defined

         ROOTS = NULL,


         ## Logs

         LOG_CENTRAL = list(
            root  = NULL,
            fname = "log_symlinks_central.csv",
            path  = NULL
         ),

         log_name = "log_version_history.csv",

         log_schema = list(
            log_id         = "integer"
            , timestamp    = "character"
            , user         = "character"
            , date_version = "character"
            , version_path = "character"
            , action       = "character"
            , comment      = "character"
         ),

         log_sort_varname = "timestamp",

         valid_log_field_types = c(
            "integer"
            , "character"
         ),

         log_fields_auto = c(
            "log_id"
            , "timestamp"
            , "user"
            , "date_version"
            , "version_path"
            , "action"
         ),


         ## Initialize: internally-defined
         # In `initialize()`, defined as `setdiff(names(private$DICT$log_schema), private$DICT$log_fields_auto)`
         # e.g. c("comment") at time of writing
         log_fields_user = NULL,


         # Reports - summary of last log entries across date-versioned folders
         # - Since these are the most visible/useful user summary, these could be set by `initialize()` in the future.

         report_fnames = list(
            all_logs_tool_symlink = "report_key_versions.csv"
            , all_logs              = "report_all_logs.csv"
            , all_logs_symlink      = "report_all_logs_symlink.csv"
            , all_logs_non_symlink  = "report_all_logs_non_symlink.csv"
            , discrepancies         = "report_DISCREPANCIES.csv"
         ),


         ## Status updates / symlinks

         valid_actions = c(
            "create"
            , "promote_best"
            , "demote_best"
            , "promote_keep"
            , "demote_keep"
            , "promote_remove"
            , "demote_remove"
            , "delete_remove_folder"
         ),

         symlink_types = c(
            "best"
            , "keep"
            , "remove"
         ),

         # used in private$extract_symlink()
         # NOTE: There is also a set in private$assert_n_symlinks() that cannot be set here
         #       If you ever update symlink types, adjust it there as well
         #       Also: make_symlink_path()
         symlink_rgx_extract = list(
            best   = paste0("^best"),
            keep   = paste0("^keep_"),
            remove = paste0("^remove_")
         )

      ),

      # Dynamic fields ----------------------------------------------------

      # These are not true R6 active fields, but are dynamic within the tool.
      # I'm not exposing these to the user but they need to by dynamic
      # For each date_version operation, these will update e.g. file.path(private$DICT$ROOTS, date_version)
      # - updated by operation, not on instantiate
      # - for managing internal state of the tool, not for user input
      # - if they seem buggy, call `return_dynamic_fields()` to see if they are updating as expected
      DYNAMIC = list(

         # Field for logs - updated by tool, not user
         LOG = list(
            date_version = NA_character_,
            action       = NA_character_
         ),

         # Paths built from roots and intermediate path structures
         VERS_PATHS = list()
      ),


      # PRIVATE METHODS --------------------------------------------------------

      # Redefined functions ---------------------------------------------------

      csv_reader = NULL,

      # Validations ------------------------------------------------------------

      # Types of check functions:
      # 1. assert_x - stop if conditions are unmet
      # 2. validate_x - warn if conditions are unmet
      #               - Return TRUE/FALSE

      # Assert an element is atomic and length 1
      #
      #  @param x [any] Element to check
      #
      #  @return none
      assert_scalar = function(x){
         x_name <- deparse(substitute(x))
         if(!(is.atomic(x) && length(x) == 1L)){
            stop(x_name, " must be atomic and length 1L")
         }
      },

      # Ensure an object is not length 0, empty, blank etc.
      #
      # @param arg [any]
      #
      # @return [lgl] FALSE if empty in some way, TRUE otherwise
      # @export
      #
      # @examples
      validate_not_empty = function(arg) {
         # Check for missing arguments
         if (missing(arg)) return(FALSE)
         # Check for zero-length vectors or empty lists
         if (length(arg) == 0) return(FALSE)
         # Check for NULL, NA
         if (is.null(arg) || is.na(arg)) return(FALSE)
         # Check if the argument is only whitespace
         if (is.character(arg) && trimws(arg) == "") return(FALSE)
         # Check if the argument is a numeric value that is not finite (NaN, Inf, -Inf)
         if (is.numeric(arg) && !is.finite(arg)) return(FALSE)
         # Check for empty data frames
         if (is.data.frame(arg) && nrow(arg) == 0 && ncol(arg) == 0) return(FALSE)
         return(TRUE)
      },

      # Assert x is a scalar, and not empty in some way
      #
      # @param x [any]
      #
      # @return [none] stop if assertion fails
      # @export
      #
      # @examples
      assert_scalar_not_empty = function(x){
         private$assert_scalar(x)
         if(!private$validate_not_empty(x)){
            x_name <- deparse(substitute(x))
            stop(x_name, " is empty in some way.")
         }

      },

      # Assert an object is a scalar of a certain type
      #
      # @param x [any] Object to check
      # @param type [chr] Type to check against
      #
      # @return [none] stop if assertion fails
      assert_type = function(x, type){
         private$assert_scalar(type)
         stopifnot(is.character(type))
         if(!inherits(x, type)){
            x_name <- deparse(substitute(x))
            stop(x_name, " must be of type ", type)
         }
      },

      #  Assert an object is a list with named elements
      #
      #  Stops if:
      #   - x is not a list
      #   - x is a data.table
      #   - x has no names
      #   - x has any NA names
      #   - x has any zero-length names
      #   - x has any whitespace-only names
      #
      #  @param x [list] List to check
      #
      #  @return none
      assert_named_list = function(x){
         if(!is.null(x)){
            err_msg <- "x must be a named list, not vector or data.table (list names may not be whitespace)"
            if(!is.list(x))               stop(err_msg)
            if(is.data.table(x))          stop(err_msg)
            if(is.null(names(x)))         stop(err_msg)
            if(any(is.na(names(x))))      stop(err_msg)
            names(x) <- trimws(names(x))
            if(any(nchar(names(x)) == 0)) stop(err_msg)
         }
      },

      #  Assert the user's mark-operation entry matches the log schema
      #
      #  @param user_entry [named list] User's entry for a mark operation
      #  @param log_fields_user [character] Names of fields in the log schema
      #  @param log_schema [named list] Log schema
      #
      #  @return [none]
      assert_schema_vs_user_entry = function(user_entry,
                                             log_fields_user = private$DICT$log_fields_user,
                                             log_schema      = private$DICT$log_schema) {

         # check that user_entry is a named list
         private$assert_named_list(user_entry)
         private$assert_named_list(log_schema)

         # check that user_entry names are in log_fields_user
         if(!all(names(user_entry) %in% log_fields_user)){
            stop("user_entry names must be in log_fields_user names \n   ",
                 paste(log_fields_user, collapse = "\n   "))
         }

         # check that user_entry data types match log_fields_user data types
         for(arg_name in names(user_entry)){
            arg_type <- typeof(user_entry[[arg_name]])
            log_type <- typeof(log_schema[[arg_name]])
            if(arg_type != log_type){
               stop(paste0("arg_type must match log_type: \n  ",
                           arg_name, " is ", arg_type, "\n  ",
                           arg_name, " is ", log_type))
            }
         }
      },

      #  Enforce a data.table's schema with merge protection defaults.
      #
      #  Asserts the following in order (*all are OPTIONAL*):
      #  - required column names by exact name
      #  - forbidden column names by exact name
      #  - required column names by regex pattern
      #  - forbidden column names by regex pattern (default `\.x` and `\.y` to guard against invalid merges)
      #  - data types for selected columns
      #
      #  @param my_dt [data.table] your data
      #  @param varnames_strict [chr] strictly required vector of column names - all required, no others allowed
      #  @param strict_order_req [lgl] if varnames_strict is not NULL, should the order of columns be asserted?
      #  @param varnames_req [chr] vector of column names required in my_dt (include)
      #  @param varnames_forbid [chr] vector of column names forbidden in my_dt (exclude)
      #  @param regex_req [chr] regex pattern for required column names
      #  @param regex_forbid [chr] regex pattern for forbidden coumn names (default `\.x` and `\.y` to guard against invalid merges)
      #  @param data_types [named list] `key=value` pair list of column names and required data type
      #  @param verbose [lgl] confirmation message if passing all assertions
      #
      #  @return [none] message or error
      assert_data_schema = function(
      my_dt
      , varnames_strict  = NULL
      , strict_order_req = FALSE
      , varnames_req     = NULL
      , regex_req        = NULL
      , varnames_forbid  = NULL
      , regex_forbid     = c("\\.x", "\\.y")
      , data_types       = NULL
      , verbose          = FALSE
      ){

         # Prevent accidental in-place modification
         x          <- data.table::copy(my_dt)
         my_dt_name <- deparse(substitute(my_dt))

         if(!is.null(varnames_strict)){
            if(strict_order_req){
               varnames_have <- names(x)
               varnames_need <- varnames_strict
               strict_varname_success <- identical(varnames_have, varnames_need)
            } else {
               varnames_have <- sort(names(x))
               varnames_need <- sort(varnames_strict)
               strict_varname_success <- identical(varnames_have, varnames_need)
            }

            if(!strict_varname_success) {
               stop(my_dt_name, ": ",
                    "Strict varname check failure:\n",
                    "  strict_order_req = ", as.character(strict_order_req), "\n",
                    "  varnames_have = ", paste(varnames_have, collapse = ", "), "\n",
                    "  varnames_need = ", paste(varnames_need, collapse = ", "), "\n"
               )
            }
         }


         if(!is.null(varnames_req)) {
            missing_colnames <- paste(setdiff(varnames_req, names(x)), collapse = ", ")
            if(nchar(missing_colnames)) stop(my_dt_name, ": ", "Missing these required column names:\n", "    ", missing_colnames)
         }

         if(!is.null(regex_req)) {
            rgx_list <- lapply(regex_req, x = x, function(rgx, x){
               if(!any(grepl(rgx, names(x)))) return(rgx)
            })
            rgx_failures <- paste(unlist(rgx_list), collapse = ", ")
            if(nchar(rgx_failures)) stop(my_dt_name, ": ", "No column name contains the required pattern(s):\n", "    ", rgx_failures)
         }

         if(!is.null(varnames_forbid)){
            forbidden_colnames <- paste(names(x)[which(names(x) %in% varnames_forbid)], collapse = ", ")
            if(nchar(forbidden_colnames)) stop(my_dt_name, ": ", "Forbidden column names found:\n", "    ", forbidden_colnames)
         }

         if(!is.null(regex_forbid)){
            rgx_list <- lapply(regex_forbid, x = x, function(rgx, x){
               if(any(grepl(rgx, names(x)))) return(names(x)[grepl(rgx, names(x))])
            })
            rgx_failures <- paste(unlist(rgx_list), collapse = ", ")
            if(nchar(rgx_failures)) stop(my_dt_name, ": ", "Some column name(s) contain the forbidden pattern(s): ", paste(regex_forbid, collapse = ", "), "\n", "    ", rgx_failures)
         }

         if(!is.null(data_types)){

            data_types <- lapply(data_types, tolower)

            valid_data_types <-
               tolower(
                  c(
                     "Numeric",
                     "Integer",
                     "Logical",
                     "Character",
                     "Complex",
                     "Raw",
                     "Factor",
                     "Date",
                     "POSIXct",
                     "POSIXlt"
                  )
               )

            invalid_types <- unlist(data_types)[!unlist(data_types) %in% valid_data_types]
            if(length(invalid_types)) {
               invalid_types <- paste(invalid_types, collapse = ", ")
               if(!all(unlist(data_types)) %in% valid_data_types) {
                  stop("Some data_types are not supported:\n",
                       "    valid types: ", paste(valid_data_types, collapse = ", "), "\n",
                       "    invalid types: ", invalid_types)
               }
            }

            if(any(unlist(lapply(data_types, length)) > 1)) stop(my_dt_name, ": ", "data_types may only contain named `key = value` pairs")
            if(any(unlist(lapply(names(data_types), nchar)) == 0)) stop(my_dt_name, ": ", "data_types may only contain named `key = value` pairs")
            if(!all(names(data_types) %in% names(x))) {
               missing_varnames <- setdiff(names(data_types), names(x))
               stop(my_dt_name, ": ", "`names(data_types)` must all be columns in `my_dt` - missing: ", toString(missing_varnames))
            }

            coltypes_actual        <- unlist(lapply(names(data_types), function(dtype) typeof(x[[dtype]])))
            names(coltypes_actual) <- names(data_types)
            coltype_success_mask   <- unlist(purrr::map2(names(data_types), data_types, function(.key, .val){typeof(x[[.key]]) == .val}))

            if(any(coltype_success_mask == FALSE)){
               coltype_fail_mask <- !coltype_success_mask
               coltype_fails     <- coltypes_actual[coltype_fail_mask]
               stop(my_dt_name, ": ", "Columns failing type-matching.  Actual column data-types are:", "\n    ", paste(capture.output(coltype_fails), collapse = "\n    "))
            }
         }

         if(verbose) message(my_dt_name, ": ", "Passing data schema validation.")

      },

      #  @title Assert a directory exists on disk
      #
      #  @param x [path] A directory path
      #
      #  @return none
      assert_dir_exists = function(x){
         private$assert_scalar(x)
         root <- suppressWarnings(normalizePath(x))
         if(!dir.exists(root)) stop("root does not exist: ", x)
      },

      #  Validate whether a directory exists
      #
      #  @param x [path] A directory path
      #  @param verbose [lgl] message to std_out?
      #
      #  @return [lgl] TRUE if directory exists, FALSE otherwise
      validate_dir_exists = function(x, verbose = TRUE){
         private$assert_scalar(x)
         root <- suppressWarnings(normalizePath(x))
         if(!dir.exists(root)) {
            if(verbose) message("root does not exist: ", x)
            return(invisible(FALSE))
         } else {
            return(invisible(TRUE))
         }
      },

      #  Assert the date_selector shorthand is valid.
      #
      #  Examples:
      #  gt = 'greater_than'
      #  gte = 'greater_than_equal_to'
      #  lt = 'less_than'
      #  lte = 'less_than_equal_to'
      #  e = 'equal'
      #
      #  Used by `roundup_by_date`
      #
      #  @param date_selector [chr] One of 'gt', 'gte', 'lt', 'lte', 'e'
      #
      #  @return none
      assert_date_selector = function(date_selector){
         # format and validate inputs
         valid_date_selectors <- c("gt", "gte", "lt", "lte", "e")
         date_selectors_decoded <- c("greater_than",
                                     "greater_than_equal_to",
                                     "less_than",
                                     "less_than_equal_to",
                                     "equal")
         if(!date_selector %in% valid_date_selectors){
            stop(
               "Invalid date_selector. Must be one of (case-insensitive): \n  ",
               toString(valid_date_selectors), "\n  ",
               paste0("(", paste0(date_selectors_decoded, collapse = ", "), ")")
            )
         }
      },

      #  Assert the user_date is valid class and format
      #
      #  Stop if:
      #   - user_date is not one of the valid classes
      #   - user_date is not formatted as 'YYYY MM DD' with one of the delimiters [-/_]
      #
      #
      #  @param user_date [chr] a date string of class `c('character', 'Date', 'POSIXct', 'POSIXt')`
      #
      #  @return none
      assert_user_date_class_and_format = function(user_date){
         # user date should be formatted as YYYY MM DD with select delimiters between
         valid_date_classes <- c("character", "Date", "POSIXct", "POSIXt")
         if(!any(class(user_date) %in% valid_date_classes)){
            stop("Invalid user_date. Must be one of: ", toString(valid_date_classes), "\n",
                 "Received: ", class(user_date))
         }
         if(!grepl("^[0-9]{4}[-/_]{1}[0-9]{2}[-/_]{1}[0-9]{2}$", user_date)){
            stop("Invalid user_date. Must be formatted as YYYY MM DD, with one of these delimiters [-/_] between.\n  ",
                 "Example:  2020-01-01 or 2020_01_01 or 2020/01/01 \n  ",
                 "Received: ", user_date)
         }
      },

      #  Assert the user_date is in the PST timezone
      #
      #  @param date [chr] a date string
      #
      #  @return none
      assert_PST_date = function(date){
         private$assert_user_date_class_and_format(user_date = date)
         valid_tz = c("America/Los_Angeles", "US/Pacific", "PST8PDT")
         if(!lubridate::tz(date) %in% valid_tz){
            stop("Invalid timezone.\n",
                 "Allowed:  ", toString(valid_tz), "\n",
                 "Received: ", lubridate::tz(user_date))
         }

      },



      # Utils ------------------------------------------------------------------

      #  Determine if an object is an error
      #
      #  @param x [obj] some R object
      #
      #  @return [lgl] TRUE / FALSE
      is_an_error = function(x) {

         (inherits(x, "simpleError") | inherits(x, "try-error"))
      },



      # Implementations --------------------------------------------------------

      ## Symlinks --------------------------------------------------------------

      #  Find and count symlinks by gbd round, date_version, and symlink_type
      #
      #  @param root [path] full path to gbd round data
      #  @param date_version [chr] e.g. "2023_01_01"
      #  @param symlink_type [chr] e.g. "best"
      #
      #  @return [list] two elements symlinks - symlink path names (unresolved)
      #    and symlink_count of symlinks matching private$DICT$`symlink_types`
      find_count_symlinks = function(root, date_version, symlink_type){

         # validate inputs
         private$assert_scalar(root)
         private$assert_scalar(date_version)
         private$assert_scalar(symlink_type)

         # Define all eligible symlink regex patterns by type
         arrow_rgx <- " -> " # used in bash to show symlinks
         # We're ONLY monitoring symlinks of a certain pattern - users are free to make others
         symlink_rgx <- switch(
            symlink_type,
            best   = paste0("best", arrow_rgx, ".*", date_version)
            , keep = paste0("keep_", date_version, arrow_rgx, ".*", date_version)
            , remove = paste0("remove_", date_version, arrow_rgx, ".*", date_version)
         )
         # symlink_rgx <- list(
         #    best        = paste0("best", arrow_rgx, ".*", date_version)
         #    , keep      = paste0("keep_", date_version, arrow_rgx, ".*", date_version)
         #    , remove    = paste0("remove_", date_version, arrow_rgx, ".*", date_version)
         # )

         # validate
         if(!all(names(symlink_rgx) %in% private$DICT$symlink_types)){
            stop("Some of your symlink_rgx are not valid options - update 'private$DICT$symlink_types': \n  ",
                 paste0(names(symlink_rgx), collapse = "\n  ")
            )
         }

         # subset to user's chosen symlink_type
         # symlink_rgx <- symlink_rgx[names(symlink_rgx) %in% symlink_type]

         folder_contents <- system(paste("ls -l", root), intern = TRUE)
         folder_contents <- folder_contents[-1] # remove "total xxx"
         if(length(folder_contents) == 0) stop("No items found in your root folder - investigate")

         symlinks <- lapply(symlink_rgx, function(rgx){
            grep(rgx, folder_contents, value = TRUE)
         })
         # count symlinks by date_version
         symlinks_found_lgl <- lapply(symlink_rgx, function(rgx){
            grepl(rgx, folder_contents)
         })
         symlink_count <- unlist(lapply(symlinks_found_lgl, sum))

         return(list(
            symlinks      = symlinks,
            symlink_count = symlink_count
         ))
      },

      # Check if a date_version is already marked with a symlink of a certain type
      #
      # @param version_path [chr] full path to a version folder, e.g. /mnt/share/gbdxxxx/2023_01_01
      # @param symlink_type [chr] one of: private$DICT$symlink_types
      #
      # @return
      already_marked = function(version_path, symlink_type){
         # validate inputs
         private$assert_scalar(version_path)
         private$assert_scalar(symlink_type)
         root         <- dirname(version_path)
         date_version <- basename(version_path)

         symlink_list <- private$find_count_symlinks(root = root, date_version = date_version, symlink_type = symlink_type)
         ifelse(symlink_list$symlink_count > 0, TRUE, FALSE)
      },

      #  assert that some number of symlinks exist for some date_version - sums symlinks across all symlink_types specified
      #  use case: ensure you don't have the same date marked 'best' and 'remove' at the same time
      #
      #  @param root [path] full path to gbdxxxx data folder
      #  @param date_version [chr] a run date for the model
      #  @param symlink_type [chr] one of: c(private$DICT$symlink_types, "all")
      #  @param n_sym [int] number of symlinks to assert
      #  @param allow_fewer [lgl] if TRUE, allow fewer symlinks than n_sym symlinks (i.e. 0 is OK)
      #
      #  @return none
      assert_n_symlinks = function(root, date_version, symlink_type = "all", n_sym = 1L, allow_fewer = TRUE){

         # validate inputs
         private$validate_dir_exists(root, verbose = FALSE)
         private$assert_scalar(date_version)
         private$assert_scalar(n_sym)
         if(!is.integer(n_sym)) stop("n_sym must be an integer")
         if(n_sym < 0L) stop("n_sym must be non-negative")

         valid_symlink_types <- c(private$DICT$symlink_types, "all")
         if(!symlink_type %in% valid_symlink_types) {
            stop("symlink_type must be one of: \n  ",
                 valid_symlink_types, collapse = "\n  ")
         }

         if(symlink_type == "all") symlink_type <- private$DICT$symlink_types
         names(symlink_type) <- symlink_type # names for lapply

         symlink_list <- lapply(symlink_type, function(x){
            private$find_count_symlinks(root = root, date_version = date_version, symlink_type = x)
         })

         symlink_counts <- unlist(unname(purrr::map_depth(symlink_list, 1, "symlink_count")))

         version_path <- file.path(root, date_version)
         if(allow_fewer){
            if(sum(symlink_counts) > n_sym) {
               stop("You have more than ", n_sym, " symlink(s) for ", version_path, "\n  ",
                    "Please run 'unmark()' to remove all symlinks, then try again.\n  ",
                    paste(capture.output(symlink_counts), collapse = "\n  "))
            }
         } else {
            if(sum(symlink_counts) != n_sym) {
               stop("You do not have ", n_sym, " symlink(s) for ", version_path, "\n  ",
                    "Please run 'unmark()' to remove all symlinks, then try again.\n  ",
                    paste(capture.output(symlink_counts), collapse = "\n  "))
            }
         }

         return(TRUE)

      },


      #  Resolve a symlink to its full path
      #
      #  @param path [chr] a path to a symlink
      #
      #  @return [chr] the full path of the symlink
      resolve_symlink = function(path){
         path_resolved <- system(paste("realpath", path), intern = TRUE)
         if(file.exists(path_resolved)) {
            return(path_resolved)
         } else {
            message("Could not resolve symlink: ", path)
         }
      },

      #
      #  Make a symlink string of a certain type for a folder
      #
      #  @param version_path [chr] full path to a version folder
      #  @param symlink_type [chr] valid symlink type defined by class, e.g. 'best', 'keep', 'remove'
      #
      #  @return [chr] full path to the symlink
      make_symlink_path = function(version_path, symlink_type){
         private$assert_scalar(version_path)
         private$assert_scalar(symlink_type)
         if(!symlink_type %in% private$DICT$symlink_types) stop("Invalid symlink_type: ", symlink_type)
         root         <- DescTools::SplitPath(version_path)$dirname
         root         <- sub("/$", "", root) # remove trailing slash
         date_version <- DescTools::SplitPath(version_path)$fullfilename

         symlink_suffix <- switch(
            symlink_type,
            best   = "",
            keep   = paste0("_", date_version),
            remove = paste0("_", date_version)
         )

         return(file.path(root, paste0(symlink_type, symlink_suffix)))
      },

      #  Pull a symlink from a linux path string
      #
      #  e.g. pull "best" from:
      #  "lrwxrwxrwx 1 USER Domain Users  63 Feb  6 17:04 best -> /mnt/share/some/path"
      #
      #  @param symlink_string [chr] a string from a linux path e.g. "lrwxrwxrwx 1 USER Domain Users  63 Feb  6 17:04 best -> /mnt/share/some/path"
      #
      #  @return [chr] the symlink string
      extract_symlink = function(symlink_string){
         arrow_rgx <- " -> " # used by linux to show symlinks
         if(!grepl(arrow_rgx, symlink_string)) stop("No arrow found in symlink_string: ", symlink_string)
         match_idx <- regexpr(arrow_rgx, symlink_string)
         str_pre_arrow <- substr(symlink_string, 1, match_idx - 1)
         # grab the last string of non-whitespace characters
         str_symlink <- tail(strsplit(str_pre_arrow, " ")[[1]], 1)
         # validate against allowed patterns
         symlink_rgx <- private$DICT$symlink_rgx_extract
         symlink_rgx <- paste(unlist(symlink_rgx), collapse = "|")
         if(!grepl(symlink_rgx, str_symlink)) {
            stop("Invalid symlink found - inspect extract_symlink logic: ", str_symlink)
         }
         return(str_symlink)
      },

      #  Remove one symlink and message about which type was removed, append to log
      #
      #  @param root [chr] path to the root folder
      #  @param date_version [chr] date_version of the user's folder
      #  @param symlink_type [chr] valid symlink type defined by class, e.g. 'best', 'keep', 'remove'
      #  @param user_entry [named list] user entry from the user's folder
      #
      #  @return [none] side effect of removing a symlink
      remove_one_symlink = function(root, date_version, symlink_type, user_entry){
         private$assert_dir_exists(root)
         private$assert_scalar(date_version)
         private$assert_scalar(symlink_type)
         if(!symlink_type %in% private$DICT$symlink_types) stop("Invalid symlink_type; got - ", symlink_type, " - expected - ", toString(private$DICT$symlink_types))

         symlink_list <- private$find_count_symlinks(
            root         = root,
            date_version = date_version,
            symlink_type = symlink_type
         )
         symlink <- unlist(symlink_list$symlinks)

         if(length(symlink) > 0){
            symlink_clean              <- private$extract_symlink(symlink_string = symlink)
            symlink_full               <- file.path(root, symlink_clean)
            path_real                  <- private$resolve_symlink(path = symlink_full)
            private$DYNAMIC$LOG$action <- paste0("demote_", symlink_type)
            private$DYNAMIC$LOG$date_version <- date_version

            message("-- removing ", symlink_full)
            system(paste("unlink", symlink_full))

            private$append_to_log(version_path = path_real, user_entry = user_entry)
            private$append_to_central_log(version_path = path_real, user_entry = user_entry)


         } else {
            message("---- No symlink found for: ", date_version, " - ", symlink_type)
         }
      },

      #  Remove all symlinks for a date_version
      #
      #  Loops through all symlink types and removes them with `remove_one_symlink()`
      #
      #  @param root [chr] path to the root folder
      #  @param date_version [chr] date_version of the user's folder
      #  @param user_entry [named list] user entry from the user's folder
      #
      #  @return [none] removes symlinks on disk
      remove_all_symlinks = function(root, date_version, user_entry){
         private$assert_scalar(date_version)
         message("Removing symlinks for: ", date_version)
         for(symlink_type in private$DICT$symlink_types){
            private$remove_one_symlink(
               root         = root,
               date_version = date_version,
               symlink_type = symlink_type,
               user_entry   = user_entry
            )
         }
      },



      ## Creation / Deletion ---------------------------------------------------

      # Create a new real directory, not a symlink, to a versioned output folder, with new log
      # I want messages, not dir.create's default warning

      #  Create a new folder with a log file
      #
      #  @param version_path [chr] path to the versioned folder
      #
      #  @return [none] side effect of creating a log file on disk
      create_folder_with_log = function(version_path){

         dir_exists <- private$validate_dir_exists(version_path, verbose = FALSE)

         if(!dir_exists){
            dir.create(version_path, recursive = TRUE)
         } else {
            message("Directory already exists: ", version_path)
         }

         # Always write a log - if user uses the tool to try to create a folder
         # that already exists, it's  a signal they want a log to exist
         private$write_expected_log(version_path)

      },

      #  Delete a folder that's been marked `remove_`
      #
      #  @param root [chr] path to the root folder
      #  @param date_version [chr] date_version of the user's folder
      #  @param user_entry [named list] user entry from the user's folder
      #  @param require_user_input [lgl] TRUE if the user should be prompted to confirm deletion, FALSE if deletion should be automated e.g. in a loop.
      #
      #  @return [lgl] TRUE if the folder was deleted, FALSE if it was not
      delete_remove_folder = function(root, date_version, user_entry, require_user_input){
         private$assert_dir_exists(root)
         private$assert_scalar(date_version)
         version_path <- file.path(root, date_version)

         folder_dt <- private$query_root_folder_types(root = root)

         # Remove symlinks and base folders
         # ensure the folder is marked `remove_`
         # - the tool will not delete unmarked folders, that's the entire point of the tool
         folder_dt_removes      <- folder_dt[is_tool_symlink == TRUE & grepl(private$DICT$symlink_rgx_extract$remove, dir_leaf), ]
         deletion_symlink_exact <- paste0("remove_", date_version)
         deletion_dir_name      <- folder_dt[dir_leaf == deletion_symlink_exact, dir_name]

         if(!deletion_symlink_exact %in% folder_dt_removes$dir_leaf){
            message(
               "\n",
               "No valid `remove_` symlink found:\n",
               "  -- for: ", date_version, "\n",
               "  -- in root: ", root
            )

            ret_val_deleted_TF <- NULL

         } else if (require_user_input == TRUE) {

            dirnames_to_unlink <- folder_dt[dir_date_version == date_version, dir_name]

            message("") # newline for visual clarity

            user_input <- utils::menu(
               title = paste0("Do you want to delete the following folders?\n  ",
                              paste(dirnames_to_unlink, collapse = "\n  "))
               , choices = c("No", "Yes")
            )

            message("") # newline for visual clarity

            # Prompt user input to confirm deletion
            if(user_input == 2){
               private$DYNAMIC$LOG$action <- "delete_remove_folder"
               private$append_to_central_log(version_path = version_path, user_entry = user_entry)
               for(dir_name in dirnames_to_unlink){
                  message("Deleting ", dir_name)
                  unlink(x = dir_name, recursive = TRUE, force = TRUE)
               }
            }

            ret_val_deleted_TF <- ifelse(user_input == 2, TRUE, FALSE)

         } else if (require_user_input == FALSE) {

            dirnames_to_unlink <- folder_dt[dir_date_version == date_version, dir_name]

            message("") # newline for visual clarity

            private$DYNAMIC$LOG$action <- "delete_remove_folder"
            private$append_to_central_log(version_path = version_path, user_entry = user_entry)
            for(dir_name in dirnames_to_unlink){
               message("Deleting ", dir_name)
               unlink(x = dir_name, recursive = TRUE, force = TRUE)
            }

            ret_val_deleted_TF <- TRUE

         } else {
            stop("Unforeseen error in `delete_remove_folder` function. Please contact the developer.")
         }

         return(ret_val_deleted_TF)

      },




      ## Logs ------------------------------------------------------------------

      #  Make an empty data.table schema from a named list
      #
      #  @param schema [named list] named list of column names and data types
      #
      #  @return [data.table] empty data.table with schema columns and types
      make_schema_dt = function(schema){
         private$assert_named_list(schema)
         dt_schema <- data.table::data.table()
         for(s_name in names(schema)){
            dt_schema[[s_name]] <- vector(mode = schema[[s_name]], length = 0L)
         }
         private$assert_data_schema(dt_schema, data_types = schema) # from code_root/vaccination_pipeline_functions/validations.R
         return(dt_schema)
      },

      # Repair a log table to match the schema
      #
      # Intended only for the `read` family of functions for reading/writing
      # logs - not intended for the `query` family, which is for reading only
      # and not changing.
      #
      # Bind the log to the schema to fill it out with correct columns, in
      # order. If the SymlinkTool log schema changes, schema assertions cause
      # tool failure.
      # - This will NOT repair deleted rows
      # - There's no way around this unless the user strictly adheres to log schema for all time.
      # - This really gets in the way more than it helps.
      #
      # @param dt_log [data.table] log data.table
      # @param log_schema [list] e.g. private$DICT$log_schema
      #
      # @return [data.table] log data.table with schema columns in order
      repair_log_schema = function(dt_log, log_schema = private$DICT$log_schema, allow_schema_repair = private$DICT$FLAGS$allow_schema_repair){
         if(allow_schema_repair){
            private$assert_named_list(log_schema)
            dt_schema <- private$make_schema_dt(log_schema)
            dt_log <- data.table::rbindlist(list(dt_schema, dt_log), fill = TRUE)
         }
         return(dt_log)
      },

      #  Write a new log file with the schema defined in `private$DICT$log_schema`
      #
      #  Write a 'create' row if there are 0 rows in the log.
      #
      #  @param version_path [chr] path to the log file root
      #  @param log_schema [named list] named list of column names and data types
      #
      #  @return none
      write_new_log = function(version_path, log_schema = private$DICT$log_schema){
         fpath <- file.path(version_path, private$DICT$log_name)
         dt_log <- private$make_schema_dt(log_schema)
         # Safely write first 'create' row if it doesn't exist
         dt_log <- private$ensure_log_creation_entry(version_path, dt_log)
         data.table::fwrite(dt_log, fpath)
      },

      #   Return a time-stamp in "%Y_%m_%d_%H%M%S" format, e.g. "2024_02_21_104202"
      #
      #  @return [chr] time-stamp in "%Y_%m_%d_%H%M%S" format
      make_current_timestamp = function(){
         return(format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%Y_%m_%d_%H%M%S"))
      },

      #   Write a log creation row IF the log is empty
      #
      #  @param dt_log [data.table] log data.table
      #  @param date_version [chr] date version in format 'YYYY_MM_DD' default private$DYNAMIC$LOG$date_version
      #
      #  @return [data.table] log data.table with creation row if it was empty
      ensure_log_creation_entry = function(version_path, dt_log, date_version = private$DYNAMIC$LOG$date_version){
         if(is.na(date_version)) stop("date_version not set - check how you've initialized the date_version, if at all")
         # This will not rewrite the first log line if only that row was deleted - that's misleading
         if(nrow(dt_log) == 0) {
            first_row <- data.table(
               log_id       = 0L,
               timestamp    = private$make_current_timestamp(),
               user         = Sys.info()[["user"]],
               date_version = date_version,
               version_path = version_path,
               action       = "create",
               comment      = "log created"

            )
            dt_log <- rbind(first_row, dt_log)
         }
         return(dt_log)
      },

      #  Safely write an expected log with creation entry if it doesn't exist.
      #
      #  @param version_path [chr] path to the log file root
      #  @param log_schema [named list] named list of column names and data types
      #
      #  @return none
      write_expected_log = function(version_path, log_schema = private$DICT$log_schema){
         private$assert_scalar(version_path)
         fpath <- file.path(version_path, private$DICT$log_name)
         if(!file.exists(fpath)) {
            private$write_new_log(version_path)
         } else {
            dt_log <- private$read_log(fpath, log_schema)
            # Safely write first 'create' row if it doesn't exist
            dt_log <- private$ensure_log_creation_entry(version_path, dt_log)
            data.table::fwrite(dt_log, fpath)
         }
      },

      #  Safely correct a null log, if found (all dim == 0).
      #
      #  If log is null (all dim == 0), rebuild schema and write a new 'create' row.
      #
      #  Helper for `read_log()`
      #
      #  @param dt_log [data.table] log data.table
      #
      #  @return [data.table] log data.table
      correct_null_log = function(version_path, dt_log){
         if(all(dim(dt_log) == 0)){
            message("NULL log found (all dim = 0) - rebuilding schema and writing a new 'create' row.")
            dt_log <- private$make_schema_dt(private$DICT$log_schema)
            dt_log <- private$ensure_log_creation_entry(version_path, dt_log)
         }
         return(dt_log)
      },

      #  Reads log if it exists, makes a blank one if it doesn't.
      #
      #  Ensure correct schema.
      #
      #  @param fpath [chr] path to the log file
      #  @param log_schema [named list] named list of column names and data types
      #
      #  @return [data.table] log data.table
      read_log = function(fpath, log_schema = private$DICT$log_schema){

         col_classes        <- unlist(log_schema)
         dt_log <- private$csv_reader(fpath, colClasses = col_classes)

         # safely correct a null log, if found (all dim == 0)
         dt_log <- private$correct_null_log(dirname(fpath), dt_log)
         dt_log <- private$repair_log_schema(dt_log, log_schema = log_schema)
         private$assert_data_schema(dt_log, data_types = log_schema)
         return(dt_log)
      },

      #  Append a one-row entry to a log and write to disk
      #
      #  @param version_path [chr] path to a date_version folder
      #  @param user_entry [named list] named list of user entry values
      #
      #  @return none
      append_to_log = function(version_path, user_entry) {

         # validate inputs
         private$assert_named_list(user_entry)
         private$assert_scalar(version_path)

         # needs to read a log to bump the log_id number
         fpath <- file.path(version_path, private$DICT$log_name)
         private$write_expected_log(version_path)
         dt_log <- private$read_log(fpath)
         private$assert_schema_vs_user_entry(user_entry)

         # Safely write first 'create' row if it doesn't exist
         dt_log <- private$ensure_log_creation_entry(version_path, dt_log)

         last_row <- tail(dt_log, 1)

         log_entry <- data.table::data.table(
            log_id       = last_row$log_id + 1,
            timestamp    = private$make_current_timestamp(),
            user         = Sys.info()[["user"]],
            date_version = private$DYNAMIC$LOG$date_version,
            version_path = version_path,
            action       = private$DYNAMIC$LOG$action
         )

         for(varname in names(user_entry)){
            log_entry[[varname]] <- user_entry[[varname]]
         }

         dt_log <- rbindlist(list(dt_log, log_entry), fill = private$DICT$FLAGS$allow_schema_repair)
         data.table::setcolorder(dt_log, names(private$DICT$log_schema))

         message("---- Writing log to ", fpath)
         data.table::fwrite(dt_log, fpath)
      },


      ## Central Log ----------------------------------------------------------

      #  Safely write an expected central log with creation entry if it doesn't exist.
      #
      #  This differs from a date_version log since the first entry DOES NOT
      #  have a date_version by definition - we'll define it as "CENTRAL_LOG" for
      #  clarity, and write an append process to match
      #
      #  @param fpath [chr] path to the log file
      #  @param log_schema [named list] named list of column names and data types
      #
      #  @return none
      write_expected_central_log = function(fpath, log_schema = private$DICT$log_schema){
         private$assert_scalar(fpath)
         if(!file.exists(fpath)) {
            private$write_new_central_log(fpath, log_schema)
         } else {
            dt_log <- private$read_central_log(fpath, log_schema)
            data.table::fwrite(dt_log, fpath)
         }
      },

      #  Reads central log if it exists, makes a blank one if it doesn't.
      #
      #  Ensure correct schema.
      #
      #  @param fpath [chr] path to the log file
      #  @param log_schema [named list] named list of column names and data types
      #
      #  @return [data.table] log data.table
      read_central_log = function(fpath, log_schema = private$DICT$log_schema){

         col_classes        <- unlist(log_schema)

         dt_log <- private$csv_reader(fpath, colClasses = col_classes)

         # safely correct a null log, if found (all dim == 0)
         if(all(dim(dt_log) == 0)){
            message("NULL central log found (all dim = 0) - rebuilding schema and writing a new 'create' row.")
            dt_log <- private$make_schema_dt(private$DICT$log_schema)
            dt_log <- private$make_central_log_creation_entry(dt_log)
         }

         dt_log <- private$repair_log_schema(dt_log, log_schema = log_schema)
         private$assert_data_schema(dt_log, data_types = log_schema)
         return(dt_log)
      },

      #  Write a new central log with a 'create' row.
      #
      #  @param fpath [chr] path to the central log file
      #  @param log_schema [named list] named list of column names and data types
      #
      #  @return none
      write_new_central_log = function(fpath, log_schema = private$DICT$log_schema){
         dt_log <- private$make_schema_dt(log_schema)
         # Safely write first 'create' row if it doesn't exist
         dt_log <- private$make_central_log_creation_entry(dt_log)
         data.table::fwrite(dt_log, fpath)
      },

      #  Ensure a 'create' row exists in the central log.
      #
      #  Write a 'create' row if there are 0 rows in the log.
      #
      #  @param dt_log [data.table] log data.table
      #
      #  @return [data.table] log data.table with creation row
      make_central_log_creation_entry = function(dt_log){
         # This will not rewrite the first log line if only that row was deleted - that's misleading
         if(nrow(dt_log) == 0) {
            first_row <- data.table(
               log_id       = 0L,
               timestamp    = private$make_current_timestamp(),
               user         = Sys.info()[["user"]],
               date_version = "CENTRAL_LOG",
               version_path = private$DICT$LOG_CENTRAL$path,
               action       = "create",
               comment      = "log created"

            )
            dt_log <- rbind(first_row, dt_log)
         }
         return(dt_log)
      },

      #  Append a one-row entry to the central log and write to disk
      #
      #  Differs from `append_to_log` in that only promotions/deletions are recorded, not demotions.
      #  e.g. `demote_best` is not recorded, only the next `promote_best` will be.
      #  This helps keeps from cluttering the central log.
      #
      #  @param user_entry [named list] named list of user entry values
      #
      #  @return none
      append_to_central_log = function(version_path, user_entry) {

         # validate inputs
         private$assert_named_list(user_entry)
         private$assert_schema_vs_user_entry(user_entry)

         # needs to read a log to bump the log_id number
         fpath    <- private$DICT$LOG_CENTRAL$path
         private$write_expected_central_log(fpath, log_schema = private$DICT$log_schema)
         dt_log   <- private$read_log(fpath)

         # Safely write first 'create' row if it doesn't exist
         dt_log <- private$make_central_log_creation_entry(dt_log)

         if(!is.na(private$DYNAMIC$LOG$action)){

            last_row <- tail(dt_log, 1)

            log_entry <- data.table::data.table(
               log_id       = last_row$log_id + 1,
               timestamp    = private$make_current_timestamp(),
               user         = Sys.info()[["user"]],
               date_version = private$DYNAMIC$LOG$date_version,
               version_path = version_path,
               action       = private$DYNAMIC$LOG$action
            )

            for(varname in names(user_entry)){
               log_entry[[varname]] <- user_entry[[varname]]
            }

            dt_log <- rbindlist(list(dt_log, log_entry), fill = private$DICT$FLAGS$allow_schema_repair)
            data.table::setcolorder(dt_log, names(private$DICT$log_schema))

            message("---- Writing central log to ", fpath)
            data.table::fwrite(dt_log, fpath)

         } else {

            message("-- No action defined, not writing to central log.\n",
                    "---- This is expected if no symlinks were found or user-input did not produce an action for: ", private$DYNAMIC$LOG$date_version)
         }

      },


      ## Queries ---------------------------------------------------------------

      # each query_all_logs_* function should have a corresponding report

      # The 'query' function family is for reporting on the state of the symlinks.
      # It differs from the 'read' family, which is for promotion/demotion, and will create a new log

      #  Query one date-version folder for a log
      #
      #  `tryCatch()` to find a folder's log, read it if it exists, in a consistent format
      #
      #  @param version_path [chr] path to a date_version folder
      #  @param verbose [lgl] print message if no log found
      #
      #  @return [data.table] log data.table if found, else NULL
      try_query_log = function(version_path, verbose = TRUE){
         tryCatch(
            {
               log <- private$csv_reader(
                  file.path(version_path, private$DICT$log_name)
                  , colClasses = unlist(private$DICT$log_schema)
               )
            },
            error = function(e) if(verbose) message("No log found for folder: ", version_path)
         )

      },

      #  Query the first row of all logs in a list
      #
      #  Does not rely on the log entry index, uses `head()`
      #
      #  @param log_list [list] list of logs, each log is a data.table
      #
      #  @return [data.table] a data.table with the first row of each log
      query_logs_first_row = function(log_list){
         dt_query <- data.table::rbindlist(lapply(log_list, function(x) head(x, 1)), fill = TRUE)
         setorderv(dt_query, private$DICT$log_sort_varname)
         return(dt_query)
      },

      #  Return the last row from each log.
      #
      #  Does not rely on the log entry index, uses `tail()`
      #  - sort according to some variable
      #
      #  @param log_list [list] list of logs, each log is a data.table
      #
      #  @return [data.table] a data.table with the last row of each log
      query_logs_last_row = function(log_list){
         dt_query <- data.table::rbindlist(lapply(log_list, function(x) tail(x, 1)), fill = TRUE)
         setorderv(dt_query, private$DICT$log_sort_varname)
         return(dt_query)
      },

      #  Query the row with log_id == 0 from all logs in a list
      #
      #  This should be the creation line for each log.
      #
      #  @param log_list [list] list of logs, each log is a data.table
      #
      #  @return [data.table] a data.table with the row with log_id == 0 of each log
      query_log_id_0 = function(log_list){
         dt_query <- data.table::rbindlist(lapply(log_list, function(x) x[log_id == 0, ]), fill = TRUE)
         setorderv(dt_query, private$DICT$log_sort_varname)
         return(dt_query)

      },

      #  Query the folder types (bare of symlink) of all date_version folders in one `root`.
      #
      #  @param root [chr] path to a root folder defined at instantiation (`STL$new()`)
      #
      #  @return [data.table] table of all date_version folders and their folder types (e.g. bare folder or symlink)
      query_root_folder_types = function(root){
         # Find all first-level folders in the root (non-recursive)
         dir_name <- list.dirs(root, full.names = TRUE, recursive = FALSE)
         # final part of the path - implies symlink if active
         dir_leaf <- basename(dir_name)
         dir_name_resolved  <- unlist(lapply(dir_name, private$resolve_symlink))
         # table of all folders, resolved paths, and last part of the path
         tryCatch(
            {
               # I want this to be an error, not a warning
               folder_dt <- data.table::as.data.table(
                  cbind(
                     dir_name,
                     dir_name_resolved,
                     dir_leaf
                  )
               )
            },
            warning = function(w) stop("cbind operation failed in query_root_folder_types - inspect logic.")
         )
         # also add on the resolved leaf date_version for later queries and roundups
         # - keep name distinct to avoid data.table conflicts with function args
         folder_dt[, dir_date_version := basename(dir_name_resolved)]
         # define symlinks that could and couldn't have been created by this tool for later use
         tool_symlink_rgx  <- private$DICT$symlink_rgx_extract
         tool_symlink_rgx  <- paste(tool_symlink_rgx, collapse = "|")
         folder_dt[, is_symlink := dir_name != dir_name_resolved]
         folder_dt[, is_tool_symlink := grepl(tool_symlink_rgx, dir_leaf)]
         return(folder_dt)
      },


      #  Safely remove null logs, and account for zero-length logs if none are found for a date_version folder
      #
      #  @param log_list [list] list of log data.tables
      #
      #  @return [list] list of non-NULL log data.tables
      filter_null_logs_safely = function(log_list){
         if(length(log_list) == 0 || all(is.null(unlist(log_list)))){
            return(list(no_logs_found = private$make_schema_dt(schema = private$DICT$log_schema)))
         } else {
            return(log_list[!unlist(lapply(log_list, is.null))])
         }
      },

      #  Query all logs in a single `root`
      #
      #  @param root [chr] path to a root folder defined at instantiation (`STL$new()`)
      #
      #  @return [list] list of all non-NULL logs in the root, with the folder path as the list name
      query_all_logs = function(root){
         # find all folders and their types
         folder_dt            <- private$query_root_folder_types(root)
         # only read unique dir_name_resolved
         unique_version_paths <- unique(folder_dt$dir_name_resolved)
         # read all logs _if_ they exist
         log_list             <- lapply(unique_version_paths, private$try_query_log, verbose = FALSE)
         names(log_list)      <- unique_version_paths
         # remove any NULLs, result of the tryCatch in try_query_log
         log_list             <- private$filter_null_logs_safely(log_list)
         # lapply(log_list, private$assert_data_schema, data_types = private$DICT$log_schema)
         return(log_list)
      },

      #  Query all logs for any active symlinks in a single `root`
      #
      #  This finds symlinks _not_ made by the Symlink Tool (useful for diagnostics).
      #  E.g. if the user makes their own symlink not defined in private`$DICT$symlink_types`, this will find it.
      #
      #  @param root [chr] path to a root folder defined at instantiation (`STL$new()`)
      #
      #  @return [list] list of all non-NULL logs in the root, with the folder path as the list name
      query_all_logs_symlink = function(root){
         # find all folders and their types
         folder_dt <- private$query_root_folder_types(root)
         # query logs for active symlinks of any type
         unique_version_paths <- unique(folder_dt[is_symlink == TRUE, dir_name_resolved])
         log_list             <- lapply(unique_version_paths, private$try_query_log, verbose = FALSE)
         names(log_list)      <- unique_version_paths
         # remove any NULLs, result of the tryCatch in try_query_log
         log_list             <- private$filter_null_logs_safely(log_list)
         # lapply(log_list, private$assert_data_schema, data_types = private$DICT$log_schema)
         return(log_list)
      },

      #  Query all logs for active symlinks made by the tool in a single `root`
      #
      #  This finds symlinks _made_ by the Symlink Tool (useful for diagnostics).
      #  E.g. finds symlink patterns defined by `private$DICT$symlink_types`, this will find it.
      #
      #  @param root [chr] path to a root folder defined at instantiation (`STL$new()`)
      #  @param verbose [lgl] std_err message if no log found
      #
      #  @return [list] list of all non-NULL logs in the root, with the folder path as the list name
      query_all_logs_tool_symlink = function(root, verbose = TRUE){
         # find all folders and their types
         folder_dt            <- private$query_root_folder_types(root)
         # query logs for active symlinks made by the tool
         unique_version_paths <- unique(folder_dt[is_tool_symlink == TRUE, dir_name_resolved])
         # we SOMETIMES want to see messages if expected logs are not found
         # since the tool_symlink report _also_ now wraps up the discrepancy
         # report, that will handle this message so it's not repeated too often
         log_list             <- lapply(unique_version_paths, private$try_query_log, verbose = verbose)
         names(log_list)      <- unique_version_paths
         # remove any NULLs, result of the tryCatch in try_query_log
         log_list             <- private$filter_null_logs_safely(log_list)
         # lapply(log_list, private$assert_data_schema, data_types = private$DICT$log_schema)
         return(log_list)
      },

      #  Query all logs for non-symlink folders in a single `root`
      #
      #  This finds folders _not_ targeted by any symlink.
      #
      #  @param root [chr] path to a root folder defined at instantiation (`STL$new()`)
      #
      #  @return [list] list of all non-NULL logs in the root, with the folder path as the list name
      query_all_logs_non_symlink = function(root){
         # find all folders and their types
         folder_dt                 <- private$query_root_folder_types(root)
         # find unique paths not targeted by any symlink
         unique_dir_names_resolved <- unique(folder_dt$dir_name_resolved)
         unique_symlink_targets    <- unique(folder_dt[is_symlink == TRUE, dir_name_resolved])
         # This one is a little different - requires a setdiff, so has some different variable naming patterns
         unique_non_symlink_paths  <- setdiff(unique_dir_names_resolved, unique_symlink_targets)
         # query logs
         log_list                  <- lapply(unique_non_symlink_paths, private$try_query_log, verbose = FALSE)
         names(log_list)           <- unique_non_symlink_paths
         # remove any NULLs, result of the tryCatch in try_query_log
         log_list                  <- private$filter_null_logs_safely(log_list)
         # lapply(log_list, private$assert_data_schema, data_types = private$DICT$log_schema)
         return(log_list)
      },

      #  Return the row with the highest log_id from each log.
      #
      #  Does not pull the last row, relies on the log index.
      #
      #  @param log_list [list] list of logs, each log is a data.table
      #
      #  @return [data.table] a data.table with the row with the highest log_id of each log
      query_log_id_max = function(log_list){
         # find the row with highest non-missing log_id of each log
         return(
            data.table::rbindlist(lapply(log_list, function(x) x[max(x$log_id, na.rm = TRUE)]), fill = TRUE)
         )
      },

      # Query all folders with a "best_" symlink in a single `root`
      #
      # @param root [chr] path to a root folder defined at instantiation (`STL$new()`)
      #
      # @return [data.table] a data.table with the folder path, symlink name, and resolved path
      query_all_best_symlinks = function(root){
         # find all folders and their types
         folder_dt <- private$query_root_folder_types(root)
         # find all resolved paths attached to a "best_" symlink, and their symlink names
         # allow the user to decide how to deal with removal
         best_dirs_dt <- unique(folder_dt[is_symlink == TRUE & grepl("^best$", dir_leaf), .(dir_date_version, dir_name, dir_name_resolved)])
         return(best_dirs_dt)
      },

      #  Query all folders with a "keep_" symlink in a single `root`
      #
      #  @param root [chr] path to a root folder defined at instantiation (`STL$new()`)
      #
      #  @return [data.table] a data.table with the folder path, symlink name, and resolved path
      query_all_keep_symlinks = function(root){
         # find all folders and their types
         folder_dt <- private$query_root_folder_types(root)
         # find all resolved paths attached to a "keep_" symlink, and their symlink names
         # allow the user to decide how to deal with removal
         keep_dirs_dt <- unique(folder_dt[is_symlink == TRUE & grepl("^keep_", dir_leaf), .(dir_date_version, dir_name, dir_name_resolved)])
         return(keep_dirs_dt)
      },

      #  Query all folders with a "remove_" symlink in a single `root`
      #
      #  @param root [chr] path to a root folder defined at instantiation (`STL$new()`)
      #
      #  @return [data.table] a data.table with the folder path, symlink name, and resolved path
      query_all_remove_symlinks = function(root){
         # find all folders and their types
         folder_dt <- private$query_root_folder_types(root)
         # find all resolved paths attached to a "remove_" symlink, and their symlink names
         # allow the user to decide how to deal with removal
         remove_dirs_dt <- unique(folder_dt[is_symlink == TRUE & grepl("^remove_", dir_leaf), .(dir_date_version, dir_name, dir_name_resolved)])
         return(remove_dirs_dt)
      },

      query_all_unmarked = function(root){
         # find all folders and their types
         folder_dt     <- private$query_root_folder_types(root)
         marked_dirs   <- folder_dt[is_symlink == TRUE, dir_name_resolved]
         unmarked_dirs <- folder_dt[!dir_name_resolved %in% marked_dirs, dir_name_resolved]
         return(folder_dt[dir_name_resolved %in% unmarked_dirs, .(dir_date_version, dir_name, dir_name_resolved)])
      },

      #  Query all logs in one root by a date selector
      #
      #  This relies on date_version folders being formatted a certain way.  This
      #  formatting is enforced `assert_user_date_class_and_format()` and
      #  `assert_date_selector()`.
      #
      #  Returns date_version folders less than, greater than, and/or equal to a date.
      #
      #  @param root [chr] path to a root folder defined at instantiation
      #    (`STL$new()`)
      #  @param user_date_parsed [chr] a date in a format allowed by
      #    `assert_user_date_class_and_format()`
      #  @param date_selector [chr] a selector allowed by `assert_date_selector()`
      #
      #  @return [data.table] a data.table with the first row of each log
      query_by_date = function(root, user_date_parsed, date_selector){
         private$assert_dir_exists(root)
         private$assert_date_selector(date_selector)
         private$assert_PST_date(user_date_parsed)

         # query all logs for their creation lines
         folder_dt   <- private$query_root_folder_types(root)
         log_list    <- private$query_all_logs(root)
         log_id_0_dt <- private$query_log_id_0(log_list)

         # parse log creation dates, handle some messaging in case they're malformed
         ts_raw          <- log_id_0_dt$timestamp
         names(ts_raw)   <- ts_raw
         suppressWarnings(
            ts_parsed    <- lubridate::ymd_hms(log_id_0_dt$timestamp, tz ="America/Los_Angeles")
         )
         idx_failed_parse<- which(is.na(ts_parsed))
         dt_failed_parse <- data.table::data.table(
            dir_name_resolved      = folder_dt[idx_failed_parse]$dir_name_resolved,
            timestamp_failed_parse = ts_raw[idx_failed_parse]
         )
         tryCatch({
            lubridate::ymd_hms(log_id_0_dt$timestamp, tz ="America/Los_Angeles")
         }, warning = function(w) message("Some logs failed creation-date parsing (must be in yyyy_mm_dd format): \n  ",
                                          paste(capture.output(dt_failed_parse), collapse = "\n"),
                                          "\n")
         )

         # Strip time from date-time-stamp, retain timezone info
         # Formatting for direct comparison against user_date
         suppressWarnings(log_id_0_dt[, timestamp_parsed := lubridate::ymd_hms(timestamp, tz = "America/Los_Angeles")])
         log_id_0_dt[, timestamp_parsed := format(timestamp_parsed, "%Y-%m-%d")]
         log_id_0_dt[, timestamp_parsed := lubridate::ymd(timestamp_parsed, tz ="America/Los_Angeles")]

         # filter logs to all time-stamps less/greater than (or equal to) user date for later filtering
         logs_by_date <- switch(
            date_selector
            , "lt"  = log_id_0_dt[timestamp_parsed <  user_date_parsed, ]
            , "lte" = log_id_0_dt[timestamp_parsed <= user_date_parsed, ]
            , "gt"  = log_id_0_dt[timestamp_parsed >  user_date_parsed, ]
            , "gte" = log_id_0_dt[timestamp_parsed >= user_date_parsed, ]
            , "e"   = log_id_0_dt[timestamp_parsed == user_date_parsed, ]
         )

         # Match date_version to folder_dt and return required columns
         dv_by_date <- logs_by_date$date_version
         return(folder_dt[dir_date_version %in% dv_by_date, .(dir_date_version, dir_name, dir_name_resolved)])

      },

      ## Reports ---------------------------------------------------------------

      # Write a report, with special handling
      #
      # @param dt_report [data.table] the report
      # @param write_path [chr] full path with ext
      # @param order_cols_tf [lgl] (default = TRUE) order columns by log_schema?
      # @param schema [named_list] named list of column names and data types - used if `order_cols_tf` = TRUE
      #
      # @return [none] write to disk
      write_report = function(dt_report, write_path, order_cols_tf = TRUE, schema = private$DICT$log_schema){

          if(order_cols_tf){
            private$assert_named_list(schema)
            sortable_colnames <- intersect(names(schema), names(dt_report))
            data.table::setcolorder(dt_report, sortable_colnames)
          }

         data.table::fwrite(dt_report, write_path)
      },

      # each report function must have a corresponding query_all_logs_* function

      #  Report last row of all logs in one root
      #
      #  Writes a file to disk in `root`
      #
      #  @param root [chr] path to a root folder defined at instantiation
      #
      #  @return none
      report_all_logs = function(root){
         # query logs for active symlinks of any type
         log_list <- private$query_all_logs(root)
         last_row_dt <- private$query_logs_last_row(log_list)
         private$write_report(last_row_dt, file.path(root, private$DICT$report_fnames$all_logs))
      },

      #  Report last row of all symlinked folder logs in one root
      #
      #  Writes a file to disk in `root`
      #
      #  @param root [chr] path to a root folder defined at instantiation
      #
      #  @return none
      report_all_logs_symlink = function(root){
         # query logs for active symlinks of any type
         log_list <- private$query_all_logs_symlink(root)
         last_row_dt <- private$query_logs_last_row(log_list)
         private$write_report(last_row_dt, file.path(root, private$DICT$report_fnames$all_logs_symlink))
      },

      #  Report last row of all tool-symlinked folder logs in one root
      #
      #  Writes a file to disk in `root`
      #
      #  This runs each time a user runs a 'mark' function
      #  - Invoked by `handler_post_mark()`
      #  - It must provide an updated report of the current state of the symlinks
      #  - It must provide a discrepancy report as well
      #
      #  @param root [chr] path to a root folder defined at instantiation
      #
      #  @return none
      report_all_logs_tool_symlink = function(root){
         # query logs for active symlinks of any type
         log_list <- private$query_all_logs_tool_symlink(root, verbose = FALSE)
         last_row_dt <- private$query_logs_last_row(log_list)
         private$write_report(last_row_dt, file.path(root, private$DICT$report_fnames$all_logs_tool_symlink))

         private$report_discrepancies(root = root)

      },

      #  Report last row of all non-symlinked folder logs in one root
      #
      #  Writes a file to disk in `root`
      #
      #  @param root [chr] path to a root folder defined at instantiation
      #
      #  @return none
      report_all_logs_non_symlink = function(root){
         # query logs for active symlinks of any type
         log_list <- private$query_all_logs_non_symlink(root)
         last_row_dt <- private$query_logs_last_row(log_list)
         private$write_report(last_row_dt, file.path(root, private$DICT$report_fnames$all_logs_non_symlink))
      },

      #  Add a discrepancy column to a data.table if it has rows
      #
      #  @param report_dt [data.table] a data.table of discrepancies found with some log
      #  @param discrepancy_reason [chr] a string describing the discrepancy
      #
      #  @return a data.table with a new column `discrepancy` added
      add_discrepancy_to_dt = function(report_dt, discrepancy_reason){
         if(nrow(report_dt) > 0){
            report_dt[, discrepancy := discrepancy_reason]
         } else {
            report_dt[, discrepancy := NA_character_]}
         return(report_dt)
      },

      #  For discrepancies related to paths, build an empty report schema
      #
      #  @param path_dt [data.table] a data.table of paths
      #  @param discrepancy_reason [chr] a string describing the discrepancy
      #
      #  @return an empty data.table with a new column `discrepancy` added
      make_report_schema_for_discrepant_paths = function(path_dt, discrepancy_reason, log_schema = private$DICT$log_schema){
         # input validation
         if(!data.table::is.data.table(path_dt)) stop("path_dt must be a data.table")
         if(!is.character(discrepancy_reason))   stop("discrepancy_reason must be a character string")
         varname_dir <- grep("^dir_name", names(path_dt), value = TRUE)
         if(length(varname_dir) != 1)            stop("path_dt must have exactly one column starting with 'dir_name'")

         # FIXME SB - 2024 Feb 23 - OUTPUT A ZERO-ROW DATA TABLE IF THERE ARE NOT PATHS TO REPORT

         # add an all NA row to schema_dt
         schema_dt             <- private$make_schema_dt(log_schema)
         if(nrow(path_dt) > 0) {
            nan_row               <- as.data.table(t(rep(NA, ncol(schema_dt))))
            names(nan_row)        <- names(schema_dt)
            schema_dt             <- rbind(schema_dt, nan_row)
            report_dt             <- cbind(schema_dt, path_dt[ , ..varname_dir])
            report_dt$discrepancy <- discrepancy_reason
            # we'll eventually have a mix of resolved and unresolved paths, so set a consistent name for the final report
            setnames(report_dt, varname_dir, "dir_name")
         } else {
            report_dt <- data.table::copy(schema_dt)
            report_dt[, `:=`(dir_name = NA_character_, discrepancy = NA_character_)]
         }
         return(report_dt)
      },

      # Report if log and log_schema have different fields
      #
      # This function is only used to build the discrepancy report
      #
      # @param log [data.table] Log data.table queried from disk
      # @param log_schema [named list] Log schema
      #
      # @return [data.table] Log entries with new columns: vars_missing, vars_extra
      report_log_vs_schema_diffs = function(log, log_schema = private$DICT$log_schema){
         # check that log is a named list
         private$assert_named_list(log_schema)

         # find log varnames not in schema, and vise versa
         log_fields_user    <- names(log)
         log_fields_schema  <- names(log_schema)
         separator <- ";"
         log_fields_missing <- paste0(setdiff(log_fields_schema, log_fields_user), collapse = separator)
         log_fields_extra   <- paste0(setdiff(log_fields_user, log_fields_schema), collapse = separator)

         # add extra columns to the log
         log$vars_missing <- log_fields_missing
         log$vars_extra <- log_fields_extra

         return(log[nchar(log$vars_missing) > 0 | nchar(log$vars_extra) > 0, ])
      },

      #  Report discrepancies for any reason you can think a symlink or log is inaccurate
      #
      #  Writes a file to disk in `root`
      #
      #  Catch-all report for any reason you can think a symlink or log is inaccurate
      #  - start with the folder_dt, since we're looking both for weird folders and weird logs
      #
      #  @param root [chr] path to a root folder defined at instantiation
      #
      #  @return none
      report_discrepancies = function(root, verbose = TRUE){

         # find all folders in the `root` and their types
         folder_dt <- private$query_root_folder_types(root)

         # First and most important, tool-created symlinks with with no logs or malformed logs

         # Active tool symlink with no log (dirname only)
         unique_version_paths_tool_symlink        <- unique(folder_dt[is_tool_symlink == TRUE, dir_name_resolved])
         names(unique_version_paths_tool_symlink) <- unique_version_paths_tool_symlink
         tool_symlink_has_log                     <- unlist(lapply(unique_version_paths_tool_symlink, function(path){
            file.exists(file.path(path, private$DICT$log_name))
         }))
         tool_symlink_no_log                <- names(tool_symlink_has_log[tool_symlink_has_log == FALSE])
         dirs_tool_symlink_no_logs_dt       <- folder_dt[dir_name_resolved %in% tool_symlink_no_log, .N, by = .(dir_name_resolved)][N > 0]
         discrepant_dt_tool_symlink_no_logs <- private$make_report_schema_for_discrepant_paths(dirs_tool_symlink_no_logs_dt, "tool_symlink has no logs")

         # Multiple symlinks to the same folder (dir_name only)
         dirs_multiple_symlinks_dt   <- folder_dt[is_symlink == TRUE, .(dir_name, .N), by = dir_name_resolved][N > 1][, .(dir_name, N)]
         discrepant_dt_mult_symlinks <- private$make_report_schema_for_discrepant_paths(dirs_multiple_symlinks_dt, "multiple symlinks to the same folder")

         # Non-tool symlinks (dir_name only)
         dirs_non_tool_symlinks_dt       <- folder_dt[is_symlink == TRUE & is_tool_symlink == FALSE, .N, by = dir_name]
         discrepant_dt_non_tool_symlinks <- private$make_report_schema_for_discrepant_paths(dirs_non_tool_symlinks_dt, "non-tool symlinks in root folder")

         # Active tool symlink logs with `demote_` as last row (these should have been 'promoted)
         log_list_tool_symlinks    <- private$query_all_logs_tool_symlink(root, verbose = TRUE)
         last_row_dt_tool_symlinks <- private$query_logs_last_row(log_list_tool_symlinks)
         discrepant_dt_demote      <- last_row_dt_tool_symlinks[action %like% "^demote_"]
         discrepant_dt_demote      <- private$add_discrepancy_to_dt(discrepant_dt_demote, "active-tool-symlink logs with final 'demote' line")

         # Non-symlink folder logs with 'promote' as the first row (these should have been 'demoted')
         log_list_non_symlink    <- private$query_all_logs_non_symlink(root)
         last_row_dt_non_symlink <- private$query_logs_last_row(log_list_non_symlink)
         discrepant_dt_promote   <- last_row_dt_non_symlink[action %like% "^promote_"]
         discrepant_dt_promote   <- private$add_discrepancy_to_dt(discrepant_dt_promote, "non-active-symlink logs with final 'promote' line")


         log_list_all            <- private$query_all_logs(root)

         # schema differences
         # - note any logs with columns/types that differ from SLT log schema
         log_list_schema_diffs <- lapply(log_list_all, private$report_log_vs_schema_diffs)
         discrepant_dt_schema_diffs <- data.table::rbindlist(log_list_schema_diffs, fill = TRUE)
         discrepant_dt_schema_diffs <- private$add_discrepancy_to_dt(discrepant_dt_schema_diffs, "log schema differences - see vars_missing and vars_extra")

         # Ensure all logs are in a consistent format (only after checking schema diffs)
         # - repair_log_schema is OK here - it's just a report, were not actually writing any logs
         log_list_all            <- lapply(log_list_all, function(x) private$repair_log_schema(x, allow_schema_repair = TRUE))

         # Rows without `log_id`
         log_list_all_no_id      <- lapply(log_list_all, function(x) x[is.na(log_id)])
         discrepant_dt_no_log_id <- data.table::rbindlist(log_list_all_no_id, fill = TRUE)
         discrepant_dt_no_log_id <- private$add_discrepancy_to_dt(discrepant_dt_no_log_id, "rows without log_id")

         # Non-sequential log_ids
         # - logs should start at 0 (create line), then go up from there
         log_list_log_id_non_seq      <- lapply(log_list_all, function(x) x[log_id != 0:(.N-1)])
         discrepant_dt_log_id_non_seq <- data.table::rbindlist(log_list_log_id_non_seq, fill = TRUE)
         discrepant_dt_log_id_non_seq <- private$add_discrepancy_to_dt(discrepant_dt_log_id_non_seq, "non-sequential log_ids")

         # Invalid log actions
         log_list_invalid_actions      <- lapply(log_list_all, function(x) x[!(action %in% private$DICT$valid_actions)])
         discrepant_dt_invalid_actions <- data.table::rbindlist(log_list_invalid_actions, fill = TRUE)
         discrepant_dt_invalid_actions <- private$add_discrepancy_to_dt(discrepant_dt_invalid_actions, "invalid actions")

         # Combine all discrepancies into one report
         discrepancy_report_dt <- rbindlist(
            list(
               discrepant_dt_tool_symlink_no_logs = discrepant_dt_tool_symlink_no_logs
               , discrepant_dt_non_tool_symlinks  = discrepant_dt_non_tool_symlinks
               , discrepant_dt_mult_symlinks      = discrepant_dt_mult_symlinks
               , discrepant_dt_demote             = discrepant_dt_demote
               , discrepant_dt_promote            = discrepant_dt_promote
               , discrepant_dt_no_log_id          = discrepant_dt_no_log_id
               , discrepant_dt_log_id_non_seq     = discrepant_dt_log_id_non_seq
               , discrepant_dt_invalid_actions    = discrepant_dt_invalid_actions
               , discrepant_dt_schema_diffs       = discrepant_dt_schema_diffs
            )
            , fill = TRUE
         )

         # order columns so discrepancy is last
         discrepancy_report_dt <- discrepancy_report_dt[, c(setdiff(names(discrepancy_report_dt), "discrepancy"), "discrepancy"), with = FALSE]

         # If no discrepancies are found, then delete the current discrepancy report so it doesn't cause confusion
         path_discrepancy_report <- file.path(root, private$DICT$report_fnames$discrepancies)

         if(nrow(discrepancy_report_dt) == 0) {
            # If there's nothing to report, delete the discrepancy report, otherwise stay quiet
            if(file.exists(path_discrepancy_report)){
               if(verbose) message("-- No discrepancies found in ", root, ", removing ", private$DICT$report_fnames$discrepancies, " (if it exists now)")
               file.remove(path_discrepancy_report)
            }
         } else {
            if(verbose) message("-- DISCREPANCIES FOUND: Writing discrepancy report to ", path_discrepancy_report)
            private$write_report(discrepancy_report_dt, path_discrepancy_report)
         }
      },



      ## Handlers --------------------------------------------------------------

      # These functions handle the internal state of the tool in various ways
      # - before marking and/or folder creation
      # - after marking and/or folder deletion
      # - managing the DYNAMIC field list

      #  Handle pre-mark operation validations and updates
      #
      #  Mostly all `mark`, `create` and `delete` public functions should invoke this.
      #  - This invokes the Symlink Tool's `state machine`, only allowing one symlink type per date_version folder.
      #  - resets DYNAMIC private fields
      #  - updates DYNAMIC private fields
      #
      #  @param date_version [chr] Date version folder to mark
      #  @param user_entry [named list] User-supplied log entry
      #  @param roots [named list] All user-defined `root`s defined at instantiation
      #
      #  @return [none] Machine-state updates only
      #
      #
      #
      handler_pre_mark = function(date_version, user_entry, roots = private$DICT$ROOTS){
         # validate inputs
         private$assert_scalar(date_version)
         private$assert_named_list(user_entry)
         private$assert_schema_vs_user_entry(user_entry)
         for(user_entry_item in user_entry) private$assert_scalar_not_empty(user_entry_item)

         # This is the 'state machine' of the Symlink Tool
         # enforce one symlink per date_version
         # we do this to ensure users haven't hand-made symlinks
         # of the same form we expect to use with this tool
         for(root in roots){
            private$assert_n_symlinks(root         = root,
                                      date_version = date_version,
                                      n_sym        = 1L,
                                      symlink_type = "all",
                                      allow_fewer  = TRUE)
         }

         # clear out any logging cruft from prior mark/create/delete operations
         #
         # I'm doing this pre-mark so user can print dynamic fields after any
         # operation and see prior internal state of the tool - only reset if
         # bootstrapping a new operation
         private$handler_reset_dynamic_fields(field_types = "log")
         private$handler_update_dynamic_fields(date_version = date_version)
      },

      #  Handle post-mark operation validations and updates
      #
      #  **NOTE:** DYNAMIC fields are _not_ updated in this function - that is done by `handler_pre_mark()`.
      #  - This means the state of the machine is viewable by the user after every mark operation
      #  - DYNAMIC fields only reset when a new mark operation is underway
      #
      #  Mostly all `mark`, `create` and `delete` public functions should invoke this
      #  - This invokes the Symlink Tool's `state machine`, only allowing one symlink type per date_version folder.
      #  - ensure the tool_symlink report is up-to-date
      #  - handle central log updates
      #
      #  @param date_version [chr] Date version folder to mark
      #  @param user_entry [named list] User-supplied log entry
      #
      #  @return [none] no machine-state updates, only write to disk
      handler_post_mark = function(date_version, user_entry){
         # validate inputs
         private$assert_scalar(date_version)

         # enforce one symlink per date_version
         # make sure we haven't screwed up if we update this tool
         for(root in private$DICT$ROOTS){
            private$assert_n_symlinks(
               root         = root,
               date_version = date_version,
               n_sym        = 1L,
               symlink_type = "all",
               allow_fewer  = TRUE
            )

            # update tool_symlink report
            # - prints a discrepancy report if any active symlink logs have 'demote_*' as the last row's action
            private$report_all_logs_tool_symlink(root)
         }

      },

      #  Update the `private$DYNAMIC$VERS_PATHS` fields
      #
      #  Build versioned paths from roots, assert existence
      #
      #  @param date_version [chr] Date version folder to mark
      #
      #  @return [none] Updates `private$DYNAMIC$VERS_PATHS`
      handler_update_version_paths = function(date_version){
         private$assert_scalar(date_version)
         private$DYNAMIC$VERS_PATHS <- lapply(private$DICT$ROOTS, function(root) file.path(root, date_version))
         private$assert_named_list(private$DYNAMIC$VERS_PATHS)
         if(!length(private$DYNAMIC$VERS_PATHS)) stop("No version paths found")
         lapply(private$DYNAMIC$VERS_PATHS, private$validate_dir_exists, verbose = FALSE)
      },

      #  Update all `private$DYNAMIC` fields
      #
      #  update all dynamic fields except log action
      #
      #  @param date_version [chr] Date version folder
      #
      #  @return [none] Updates `private$DYNAMIC$LOG` and `private$DYNAMIC$VERS_PATHS`
      handler_update_dynamic_fields = function(date_version){
         private$assert_scalar(date_version)
         # Update dictionaries
         private$DYNAMIC$LOG$date_version <- date_version
         private$handler_update_version_paths(date_version = date_version)
      },

      #  Set dynamic fields to NA/blank to prepare for a new `mark_` operation
      #
      #  @param field_types [chr] Field types to reset e.g. 'log', 'vers_paths'
      #
      #  @return [none] Resets `private$DYNAMIC$LOG` and `private$DYNAMIC$VERS_PATHS`
      handler_reset_dynamic_fields = function(field_types){

         # validate inputs
         valid_field_types <- c("log", "vers_paths")
         if(!is.character(field_types)) stop("field_types must be character")
         field_types <- tolower(field_types)
         if(!all(field_types %in% valid_field_types)) stop("field_types must be one of: ", toString(valid_field_types))

         if("log" %in% field_types){
            private$DYNAMIC$LOG <- list(
               date_version = NA_character_,
               action       = NA_character_
            )
         }

         if("vers_paths" %in% field_types){
            private$DYNAMIC$VERS_PATHS = list()
         }
      },


      ## Promote / Demote ------------------------------------------------------

      # 'promote' and 'demote' are consistent terms 'elevate this folder to xxx status' or 'lower this folder from xxx status'
      # - xxx status is defined by `private$DICT$symlink_types`

      #  Demote a date_version folder from a symlink status.
      #
      #  Unlinks the symlink to a date_version folder.
      #
      #  @param version_path [chr] Path to a date_version folder
      #  @param date_version [chr] Date version folder to demote
      #  @param user_entry [named list] User-supplied log entry
      #  @param symlink_types [chr] `private$DICT$symlink_types`
      #
      #  @return [none] Unlinks the symlink on disk
      demote_existing_symlinks = function(version_path, date_version, user_entry, symlink_types = private$DICT$symlink_types){
         private$assert_scalar(version_path)
         private$assert_scalar(date_version)
         private$assert_named_list(user_entry)
         # find existing symlinks for a date_version
         names(symlink_types) <- symlink_types # names for `lapply()`

         root <- sub(date_version, "", version_path)

         symlink_list <- lapply(symlink_types, function(x){
            private$find_count_symlinks(root = root, date_version = date_version, symlink_type = x)
         })

         symlink      <- unlist(purrr::map_depth(symlink_list, 1, "symlinks"))
         symlink_type <- names(symlink)
         symlink      <- unname(symlink)

         if(length(symlink) > 1) {
            stop("More than one symlink found for ", date_version, " in: ", root, "\n  ",
                 paste(symlink, collapse = "\n  "),
                 "Please select one ", paste(symlink_types, collapse = "/"),  " symlink and delete all others manually.")
         }

         if(length(symlink) > 0) {
            message("-- Demoting existing symlink: ", symlink)
            symlink                    <- private$extract_symlink(symlink)
            symlink                    <- paste0(root, symlink)
            private$DYNAMIC$LOG$action <- paste0("demote_", symlink_type)
            private$append_to_log(version_path = version_path, user_entry = user_entry)
            private$append_to_central_log(version_path = version_path, user_entry = user_entry)

            # only unlink if logging is successful
            system(paste0("unlink ", symlink))
         } else {
            message("-- No existing symlinks found - moving on")
         }


      },

      #  Demote an existing 'best' symlink
      #
      #  Best symlinks are trickier than others
      #  - there can be only _one_ per GBD round
      #  - we're demoting a different version than we're promoting
      #  - we need to specifically update the demoted version's log
      #  - we need to simultaneously demote and promote
      #
      #  @param version_path [chr] Path to a date_version folder for `best_` demotion
      #  @param date_version [chr] Date version folder to demote from `best_`
      #  @param user_entry [named list] User-supplied log entry
      #
      #  @return [none] Unlinks the 'best' symlink on disk
      demote_previous_best = function(version_path, date_version, user_entry){
         private$assert_scalar(version_path)
         private$assert_dir_exists(version_path)
         path_best_sym <- private$make_symlink_path(version_path, "best")
         private$DYNAMIC$LOG$action <- "demote_best"

         # remove old symlink
         if(dir.exists(path_best_sym)) {
            # TODO SB - 2024 Feb 06 - Convert to `remove_one_symlink()` - requires arg updates
            path_best_real <- private$resolve_symlink(path_best_sym)
            date_version_to_remove <- basename(path_best_real)
            message("-- Demoting from 'best': ", path_best_real)

            # set version to the folder the log will be written to, then reset it after
            private$DYNAMIC$LOG$date_version <- date_version_to_remove
            private$append_to_log(version_path = path_best_real, user_entry = user_entry)
            private$append_to_central_log(version_path = path_best_real, user_entry = user_entry)
            private$DYNAMIC$LOG$date_version <- date_version

            # only unlink if logging is successful
            system(paste0("unlink ", path_best_sym))
         } else {
            message("-- No 'best' symlink found - moving on: ", path_best_sym)
         }
      },

      #  Promote a date_version folder to 'best' status.
      #
      #  @param version_path [chr] Path to a date_version folder for `best_` promotion
      #  @param date_version [chr] Date version folder to promote to `best_`
      #  @param user_entry [named list] User-supplied log entry
      #
      #  @return [none] Creates a symlink on disk
      promote_best = function(version_path, date_version, user_entry){
         private$assert_dir_exists(version_path)
         path_best_sym <- private$make_symlink_path(version_path, "best")
         path_best_new <- version_path
         # Highlander - there can be only one (best)
         private$demote_previous_best(version_path = version_path, date_version = date_version, user_entry = user_entry)
         # make new symlink
         message("-- Promoting to 'best': ", path_best_new)
         private$DYNAMIC$LOG$action <- "promote_best"
         private$append_to_log(version_path = path_best_new, user_entry = user_entry)
         private$append_to_central_log(version_path = path_best_new, user_entry = user_entry)

         # only symlink if logging is successful
         # force symlink to change in case the unlink is glitchy
         system(paste0("ln -nsf ", path_best_new, " ", path_best_sym))
      },

      #  Promote a date_version folder to 'keep' status.
      #
      #  @param version_path [chr] Path to a date_version folder for `keep_` promotion
      #  @param user_entry [named list] User-supplied log entry
      #
      #  @return [none] Creates a symlink on disk
      promote_keep = function(version_path, user_entry){
         private$DYNAMIC$LOG$action <- "promote_keep"
         path_keep_sym <- private$make_symlink_path(version_path = version_path, symlink_type = "keep")
         message("-- Promoting to 'keep': ", path_keep_sym)
         if(!dir.exists(path_keep_sym)){
            private$append_to_log(version_path = version_path, user_entry = user_entry)
            private$append_to_central_log(version_path = version_path, user_entry = user_entry)

            # only symlink if logging is successful
            system(paste0("ln -s ", version_path, " ", path_keep_sym))
         } else {
            message("---- Keep symlink already exists - moving on: ", path_keep_sym)
         }
      },

      #  Promote a date_version folder to 'remove' status.
      #
      #  @param version_path [chr] Path to a date_version folder for `remove_` promotion
      #  @param user_entry [named list] User-supplied log entry
      #
      #  @return [none] Creates a symlink on disk
      promote_remove = function(version_path, user_entry){
         private$DYNAMIC$LOG$action <- "promote_remove"
         path_remove_sym <- private$make_symlink_path(version_path = version_path, symlink_type = "remove")
         message("-- Promoting to 'remove': ", path_remove_sym)
         if(!dir.exists(path_remove_sym)){
            private$append_to_log(version_path = version_path, user_entry = user_entry)
            private$append_to_central_log(version_path = version_path, user_entry = user_entry)

            # only symlink if logging is successful
            system(paste0("ln -s ", version_path, " ", path_remove_sym))
         } else {
            message("---- Keep symlink already exists - moving on: ", path_remove_sym)
         }

      }


      # END OF PRIVATE LIST
      # ^ NO FINAL COMMA
      # NOTHING BELOW THIS LINE
      # CLOSING PARENTHEIS BELOW


   ),




   # PUBLIC METHODS ------------------------------------------------------------


   # These function names should be named pretty tersely and user-friendly

   public = list(


      # NEW -----------------------------------------------------------------------


      #' Initialize the SymlinkTool (an R6 object)
      #'
      #' @param user_root_list [list] Named list of root directories for
      #'   pipeline outputs. This is where `date_version` folders live (these
      #'   are iterative runs of an analysis pipeline.)
      #' @param user_central_log_root [path] Root directory for the central log.
      #'   If you have multiple roots in the `user_root_list`, you probably want
      #'   the central log to live one level above those roots.
      #' @param schema_repair [logical] Default `TRUE`.  If `TRUE`, the tool
      #'   will attempt to repair any schema mismatches it finds in the logs
      #'   when reading and writing (e.g.) add new columns if the tool schema
      #'   has columns that existing logs do not. If `FALSE`, the tool will stop and
      #'   throw an error if it finds a schema mismatch.
      #' @param csv_reader [chr] Default `fread_quiet`.  The CSV reader to use.
      #'   Options:
      #'   - 'fread_quiet' - suppress warnings (rely on tool's messaging instead)
      #'   - 'fread' - data.table standard
      #'
      #' @return [symlink_tool] A symlink tool object.  You can instantiate
      #'   (create) multiple version, each of which has different roots and
      #'   central logs.
      #'
      #' @export
      initialize = function(
      user_root_list          = NULL
      , user_central_log_root = NULL
      , schema_repair         = TRUE
      , csv_reader            = "fread_quiet"
      ) {


         # useful start up feedback
         if(is.null(user_root_list)){
            message("\n\nThis tool expects `user_root_list` to be a named list of root directories for pipeline outputs. \n\n  ",

                    "e.g.
                 list(
                   input_root  = '/mnt/share/my_team/input_data',
                   output_root = '/mnt/share/my_team/output_data'
                 ) \n\n  ",

                    "This tool assumes each root will have a `date_version` output folder (same folder name in each root). \n  ",
                    "  You may track outputs in one root, or across many roots in parallel (as long as the date_version is the same). \n  ",
                    "  It's recommended to create these folders with the tool so they get a log at time of creation. \n\n  ",

                    "Each output folder is defined by `file.path(user_root_list, date_version)`. \n  ",
                    "  The user can 'mark' or 'unmark' any `date_version` folder as best/keep/remove. \n  ",
                    "  This folder receives a log entry for all *demotion* and *promotion* actions (marking and unmarking). \n  ",
                    "  All the `date_version` folder logs are used for report generation. \n\n  "
            )
         }

         if(is.null(user_central_log_root)){
            message("\n\nThis tool expects `user_central_log_root` to be a single directory for the central log. \n\n  ",

                    "e.g.
                 '/mnt/share/my_team' \n\n  ",

                    "The central log receives all the marking actions of all the date-version logs across all roots, \n  ",
                    "  but is not used for report generation. \n\n  ",
                    "The central log is *created* on initialization i.e. when calling `SLT$new()`. \n\n  "
            )
         }

         if(any(is.null(user_root_list) || is.null(user_central_log_root))){
            stop("You must provide both user_root_list and user_central_log_root")
         }

         # libraries
         library(data.table)
         # only fread is currently supported due to how data types are defined when reading in logs
         private$csv_reader <- switch(
            csv_reader
            , "fread" = function(...) return(data.table::fread(...))
            , "fread_quiet" = function(...) return(suppressWarnings(data.table::fread(...)))
            , stop("csv_reader must be one of: fread, fread_quiet")
         )

         # Users must provide these fields

         ## ROOTS
         # validate inputs
         private$assert_named_list(user_root_list)
         lapply(user_root_list, private$assert_dir_exists)
         private$assert_scalar(schema_repair)
         private$assert_type(schema_repair, "logical")

         # set roots
         private$DICT$ROOTS <- user_root_list

         ## CENTRAL LOG
         # validate inputs
         private$assert_scalar(user_central_log_root)
         private$assert_dir_exists(user_central_log_root)
         # set
         private$DICT$LOG_CENTRAL$root <- user_central_log_root

         ## FLAGS
         # log schema
         private$DICT$FLAGS$allow_schema_repair <- schema_repair


         # User should not interact with these

         ## Log fields the user can set
         private$DICT$log_fields_user  <- setdiff(names(private$DICT$log_schema), private$DICT$log_fields_auto)
         ## CENTRAL LOG
         private$DICT$LOG_CENTRAL$path <- file.path(private$DICT$LOG_CENTRAL$root,
                                                    private$DICT$LOG_CENTRAL$fname)
         # Make sure this exists any time the class is initialized
         private$write_expected_central_log(fpath      = private$DICT$LOG_CENTRAL$path,
                                            log_schema = private$DICT$log_schema)
      },

      ## Show Internals --------------------------------------------------------

      #' Print the contents of all private dictionaries.
      #'
      #' @param item_names [chr] Default `NULL`.  If `NULL`, show all static
      #'   internal fields.  Otherwise, vector of static field names you want to
      #'   see.
      #'
      #' @return [std_out] Print static field values to std_out.
      #'
      #'
      #'
      return_dictionaries = function(item_names = NULL){
         dict_names <- names(private$DICT)
         return_list <- data.table::copy(private$DICT)
         if(!is.null(item_names)) {
            if(!all(item_names %in% dict_names)) stop("item_name(s) not in the symlink_tool dictionaries. \n ", toString(dict_names))
            return_list <- lapply(item_names, function(x) return(private$DICT[[x]]))
            names(return_list) <- item_names
         }
         return(return_list)
      },

      #' Print the contents of all dynamic fields.
      #'
      #' @param item_names [chr] Default `NULL`.  If `NULL`, show all dynamic
      #'   internal fields.  Otherwise, vector of dynamic field names you want
      #'   to see.
      #'
      #' @return [std_out] Print dynamic field values to std_out.
      #'
      #'
      #'
      return_dynamic_fields = function(item_names = NULL){
         dict_names <- names(private$DYNAMIC)
         return_list <- data.table::copy(private$DYNAMIC)
         if(!is.null(item_names)) {
            if(!all(item_names %in% dict_names)) stop("item_name(s) not in the symlink_tool dynamic files \n ", toString(dict_names))
            return_list <- lapply(item_names, function(x) return(private$DYNAMIC[[x]]))
            names(return_list) <- item_names
         }
         return(return_list)
      },

      ## Marks and Symlinks ----------------------------------------------------

      #' Mark an output folder with a "best" symlink.
      #'
      #' Enforces:
      #' - maximum of one best model
      #'   - does not go back through history to make a best model from a prior version (not capable, this is what log_tool is for)
      #'
      #' Writes:
      #' - appends to a log file in the output folder with a date and time stamp
      #' - appends a line to the central log file with a date and time stamp
      #'
      #' @param date_version [chr] The directory name of the output folder that
      #'   lives directly under one of the `root`s you define when you
      #'   instantiate the tool.
      #' @param user_entry [list] Named list of user-defined fields to append to
      #'   the log.  After making a tool called e.g. slt, call
      #'   `slt$return_dictionaries("log_fields_user")` to find which fields a
      #'   user may add.  If you want to make your own version of this class,
      #'   you may update `log_schema` in the `private$DICT` section to allow
      #'   for them.
      #'
      #' @return [ste_err] Messages about actions taken.
      #'
      #'
      #'
      mark_best = function(date_version, user_entry){

         # For all date_version folders, do the following:
         # 1. check if there is already a best model symlink
         # 2. if there is, demote it using `unlink()` & append to the log
         # 3. make a new best model symlink
         # 4. append to the date_version log
         # 5. append to the central log

         private$handler_pre_mark(
            date_version = date_version,
            user_entry   = user_entry
         )

         message("Marking best: ", date_version)
         # Manage symlinks and append logs
         for(version_path in private$DYNAMIC$VERS_PATHS){

            if(!private$validate_dir_exists(version_path)) next()
            if(private$already_marked(version_path, "best")) {
               message("-- ", version_path, " - already marked best - moving on.")
               next()
            }

            private$demote_existing_symlinks(
               version_path = version_path,
               date_version = date_version,
               user_entry   = user_entry
            )

            private$promote_best(
               version_path    = version_path,
               date_version    = date_version,
               user_entry      = user_entry
            )

         }

         private$handler_post_mark(date_version = date_version,
                                   user_entry = user_entry)
      },



      #' Mark an output folder with a "keep_<date_version>" symlink
      #'
      #' Writes:
      #' - appends to a log file in the output folder with a date and time stamp
      #' - appends a line to the central log file with a date and time stamp
      #'
      #' @param date_version [chr] The directory name of the output folder that
      #'   lives directly under one of the `root`s you define when you
      #'   instantiate the tool.
      #' @param user_entry [list] Named list of user-defined fields to append to
      #'   the log.  After making a tool called e.g. slt, call
      #'   `slt$return_dictionaries("log_fields_user")` to find which fields a
      #'   user may add.  If you want to make your own version of this class,
      #'   you may update `log_schema` in the `private$DICT` section to allow
      #'   for them.
      #'
      #' @return [std_err] Messages about actions taken.
      #'
      #'
      #'
      mark_keep = function(date_version, user_entry){

         # 1. make a new keep model symlink
         # 2. append to the log
         # 3. append to the central log

         private$handler_pre_mark(
            date_version = date_version,
            user_entry   = user_entry
         )

         for(version_path in private$DYNAMIC$VERS_PATHS){

            if(!private$validate_dir_exists(version_path)) next()
            if(private$already_marked(version_path, "keep")) {
               message("-- ", version_path, " - already marked keep - moving on.")
               next()
            }

            private$demote_existing_symlinks(
               version_path = version_path,
               date_version = date_version,
               user_entry   = user_entry
            )

            private$promote_keep(
               version_path    = version_path,
               user_entry      = user_entry
            )
         }

         private$handler_post_mark(date_version = date_version,
                                   user_entry = user_entry)
      },



      #' Mark an output folder with a "remove_<date_version>" symlink
      #'
      #' Indication that the results can be deleted
      #' - In the future, this will be used to remove old versions of the
      #'   output, and provide a list of ST-GPR models to delete
      #'
      #' Writes:
      #' - appends to a log file in the output folder with a date and time stamp
      #' - appends a line to the central log file with a date and time stamp
      #'
      #' @param date_version [chr] The directory name of the output folder that
      #'   lives directly under one of the `root`s you define when you
      #'   instantiate the tool.
      #' @param user_entry [list] Named list of user-defined fields to append to
      #'   the log.  After making a tool called e.g. slt, call
      #'   `slt$return_dictionaries("log_fields_user")` to find which fields a
      #'   user may add.  If you want to make your own version of this class,
      #'   you may update `log_schema` in the `private$DICT` section to allow
      #'   for them.
      #'
      #' @return [std_err] Messages about actions taken.
      #'
      #'
      #'
      mark_remove = function(date_version, user_entry){

         # 1. make a new remove model symlink
         # 2. append to the log
         # 3. append to the central log

         private$handler_pre_mark(
            date_version = date_version,
            user_entry   = user_entry
         )

         for(version_path in private$DYNAMIC$VERS_PATHS){

            if(!private$validate_dir_exists(version_path)) next()
            if(private$already_marked(version_path, "remove")) {
               message("-- ", version_path, " - already marked remove - moving on.")
               next()
            }

            private$demote_existing_symlinks(
               version_path = version_path,
               date_version = date_version,
               user_entry   = user_entry
            )

            private$promote_remove(
               version_path    = version_path,
               user_entry      = user_entry
            )
         }

         private$handler_post_mark(date_version = date_version,
                                   user_entry = user_entry)

      },



      #' Remove all symlinks for a single `date_version` in all `roots`
      #'
      #' Writes:
      #' - appends to a log file in the output folder with a date and time stamp
      #' - does _not_ append to the central log file
      #'
      #' @param date_version [chr] The directory name of the output folder that
      #'   lives directly under one of the `root`s you define when you
      #'   instantiate the tool.
      #' @param user_entry [list] Named list of user-defined fields to append to
      #'   the log.  After making a tool called e.g. slt, call
      #'   `slt$return_dictionaries("log_fields_user")` to find which fields a
      #'   user may add.  If you want to make your own version of this class,
      #'   you may update `log_schema` in the `private$DICT` section to allow
      #'   for them.
      #'
      #' @return [std_err] Messages about the symlinks removed.
      #'
      #'
      #'
      unmark = function(date_version, user_entry){

         # 1. remove all symlinks associated with a date_version
         # 2. append to the log
         for(root in private$DICT$ROOTS){
            private$remove_all_symlinks(root         = root,
                                        date_version = date_version,
                                        user_entry   = user_entry)
         }

         private$handler_post_mark(date_version = date_version,
                                   user_entry = user_entry)

      },


      ## Path Roundups ---------------------------------------------------------

      #' Find all `best_` symlinks in all `roots`
      #'
      #' Return both the symlink and the resolved symlink (folder the symlink
      #' points to)
      #'
      #' @return [list] list of data.tables - one for each `root`
      roundup_best = function(){
         return(lapply(private$DICT$ROOTS, private$query_all_best_symlinks))
      },

      #' Find all `keep_` symlinks in all `roots`
      #'
      #' Return both the symlink and the resolved symlink (folder the symlink
      #' points to)
      #'
      #' @return [list] list of data.tables - one for each `root`
      roundup_keep = function(){
         return(lapply(private$DICT$ROOTS, private$query_all_keep_symlinks))
      },

      #' Find all `remove_` symlinks in all `roots`
      #'
      #' Return both the symlink and the resolved symlink (folder the symlink
      #' points to)
      #'
      #' @return [list] list of data.tables - one for each `root`
      roundup_remove = function(){
         return(lapply(private$DICT$ROOTS, private$query_all_remove_symlinks))
      },

      #' Find all folders without symlinks in all `roots`
      #'
      #' Useful if you're rapidly iterating, have only marked a couple folders,
      #' and want to remove the rest.
      #'
      #' @return [list] list of data.tables - one for each `root`
      roundup_unmarked = function(){
         return(lapply(private$DICT$ROOTS, private$query_all_unmarked))
      },

      #' Find all `date_version` folders by creation date
      #'
      #' Only finds folders that _have a log_, and reads creation date on first
      #' row.  User may select dates by (using the `date_selector` argument):
      #' - greater than - `gt`
      #' - greater than or equal to - `gte`
      #' - less than - `nt`
      #' - less than or equal to `nte`
      #' - equal to `e`
      #'
      #' @param user_date [c("character", "Date", POSIXct", "POSIXt")] A date
      #'   with class requirements - must be formatted "2020-01-01 or 2020_01_01
      #'   or 2020/01/01"
      #' @param date_selector [chr] See docstring explanation.
      #' @param verbose [lgl] If `TRUE`, std_err message.
      #'
      #' @return [list] list of data.tables - one for each `root`
      #'
      #'
      #'
      roundup_by_date = function(user_date, date_selector, verbose = TRUE){


         # format inputs for assertion
         date_selector <- tolower(date_selector)
         # assert inputs
         private$assert_date_selector(date_selector)
         private$assert_user_date_class_and_format(user_date)

         if(verbose){
            message(paste0("Finding all folders with log creation dates that are", " '", date_selector, "' ", user_date), ". \n  ",
                    "NOTE! Log creation dates are used as the file-system does not record creation times. \n")
         }

         # format user_date to USA PST to align with cluster filesystem dates
         tzone = "America/Los_Angeles"
         message("Formatting date with time-zone: ", tzone, "\n")
         user_date_parsed <- lubridate::ymd(user_date, tz = tzone)

         # for(root in private$DICT$ROOTS){
         #    return(private$query_by_date(root, user_date_parsed, date_selector))
         # }
         message("Folders with symlinks will have duplicate rows by `date_version` (one row for each unique `dir_name`) - showing all for completeness.\n")
         return(
            lapply(private$DICT$ROOTS,
                   private$query_by_date,
                   user_date_parsed = user_date_parsed,
                   date_selector    = date_selector)
         )

      },



      ## Folder Creation -------------------------------------------------------

      #' Create a new `date_version` folder in _ALL THE TOOL'S ROOTS_
      #'
      #' Create a new log in each folder.  No symlinks are created.  No
      #' `user_entry` is used.
      #'
      #' @param date_version [chr] The directory name of the output folder that
      #'   lives directly under one of the `root`s you define when you
      #'   instantiate the tool.
      #'
      #' @return [std_err] Messages about the folder creation.
      #'
      #'
      #'
      create_date_version_folders_with_logs = function(date_version){

         private$assert_scalar(x = date_version)

         private$handler_update_dynamic_fields(date_version = date_version)

         for(version_path in private$DYNAMIC$VERS_PATHS){
            private$create_folder_with_log(version_path = version_path)
         }

      },

      #' Safely write an empty log file for first pipeline runs
      #'
      #' When you start a new pipeline run, make an empty log
      #' - helpful if you let this tool manage all your versions
      #' - you can roundup date_versions by creation date using the log's first entry
      #' - the file system doesn't track directory creation dates (at time of writing)
      #'
      #' @param date_version [chr] The directory name of the output folder that
      #'   lives directly under one of the `root`s you define when you
      #'   instantiate the tool.
      #'
      #' @return [std_err] Messages about the log creation.
      #'
      #'
      #'
      make_new_log = function(date_version){

         private$handler_update_dynamic_fields(date_version = date_version)

         # The read_log function will:
         # - check if a log exists
         # - write a new log with one "creation" row and date-stamp if not
         #
         # This is safer than blindly writing/overwriting an existing log
         for(version_path in private$DYNAMIC$VERS_PATHS){
            tryCatch(
               {
                  private$write_expected_log(version_path)
               },
               error = function(e) message("Error reading log: \n  ||---- ", e)
            )
         }

      },


      ## Folder Deletion -------------------------------------------------------

      #' Delete a `date_version` folder marked with a `remove_` symlink from
      #' _ALL ITS ROOTS_
      #'
      #' Removes the symlink(s) and the underlying folder(s), and updates
      #' central log if folders were removed.
      #'
      #' Writes:
      #' - appends a line to the central log file with a date and time stamp
      #'
      #' @param date_version [chr] The directory name of the output folder that
      #'   lives directly under one of the `root`s you define when you
      #'   instantiate the tool.
      #' @param user_entry [list] Named list of user-defined fields to append to
      #'   the log.  After making a tool called e.g. slt, call
      #'   `slt$return_dictionaries("log_fields_user")` to find which fields a
      #'   user may add.  If you want to make your own version of this class,
      #'   you may update `log_schema` in the `private$DICT` section to allow
      #'   for them.
      #' @param require_user_input [lgl] if `TRUE`, will prompt user to confirm
      #'   deletion.
      #'
      #' @return [std_err] Messages about deletion events.
      #'
      #'
      #'
      delete_date_version_folders = function(date_version, user_entry, require_user_input = TRUE){

         private$handler_pre_mark(
            date_version = date_version,
            user_entry   = user_entry
         )

         ret_val_deleted_TF <- lapply(
            X   = private$DICT$ROOTS,
            FUN = private$delete_remove_folder,
            date_version       = date_version,
            user_entry         = user_entry,
            require_user_input = require_user_input
         )

         ret_val_deleted_TF <- unlist(ret_val_deleted_TF)

         if(any(ret_val_deleted_TF)) {
            private$handler_post_mark(date_version = date_version,
                                      user_entry   = user_entry)
         }
      },


      ## Reports ---------------------------------------------------------------


      #' Make all reports
      #'
      #' Writes all reports to a summary .csv for every `root` defined in the
      #' tool.
      #'
      #' @return [std_err] Messages about where reports were written.
      #'
      #'
      #'
      reports = function(){

         message("Writing last-row log reports for:\n")
         for(root in private$DICT$ROOTS){
            message(root)
            private$report_all_logs(root = root)
            private$report_all_logs_symlink(root = root)
            private$report_all_logs_tool_symlink(root = root)
            private$report_all_logs_non_symlink(root = root)
            private$report_discrepancies(root = root, verbose = FALSE) # runs by default inside report_all_logs_tool_symlink, suppress extra messages
         }
      }


      # END OF PUBLIC LIST
      # ^ NO FINAL COMMA
      # NOTHING BELOW THIS LINE
      # CLOSING PARENTHEIS BELOW


   )


   # END OF CLASS DEFINITION
   # ^ NO FINAL COMMA
   # NOTHING BELOW THIS LINE
   # CLOSING PARENTHEIS BELOW

)


