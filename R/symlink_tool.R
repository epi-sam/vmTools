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
# - [ ] clean up function args that only pass around private$DICT values - use private$DICT directly


# LATER stuff - v2.0
# TODO SB - 2024 Feb 06
# - [ ] don't demote if the symlink is already the desired type
#       - clutters up the logs, but not a deal-breaker

# .username <- Sys.info()[["user"]]
# for assert_data_schema
# source(file.path("/share/code/vaccines", .username, "vaccines/vaccination_pipeline_functions/validations.R"))

# library(R6)
# library(DescTools)
# library(data.table)
# library(lubridate)


#' WARNING: DO NOT touch '.__enclos_env__' unless you want the tool to break
#'
#' NOTES:
#'   DEBUGGING:
#'     - The 'red dot' stop point will not work with R6 objects
#'     - Use `debug(my_fun)/undebug(my_fun)` or place a `browser()` in the code
#'     - See https://r6.r-lib.org/articles/Debugging.html
#'
#'   LIBRARIES
#'     - Depends on vaccines/vaccination_pipeline_functions/validations.R for assert_data_schema
#'     - Relies on libraries declared above, but calls on all functions by package namespace for clarity and robustness

# Required for vignette to work in Rmarkdown
# Known error: https://github.com/rstudio/rmarkdown/issues/187
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

         ## Initialize: user-defined

         ROOTS = NULL,

         LOG_CENTRAL = list(
            root  = NULL,
            fname = "log_symlinks_central.csv",
            path  = NULL
         ),



         ## Logs


         log_name = "log_version_history.csv",

         log_schema = list(
            log_id         = "integer"
            , timestamp    = "character"
            , user         = "character"
            , date_version = "character"
            , action       = "character"
            , comment      = "character"
         ),

         valid_log_field_types = c(
            "integer"
            , "character"
         ),

         log_fields_auto = c(
            "log_id"
            , "timestamp"
            , "user"
            , "date_version"
            , "action"
         ),

         ## Initialize: internally-defined
         # In `initialize()`, defined as `setdiff(names(private$DICT$log_schema), private$DICT$log_fields_auto)`
         # e.g. c("comment") at time of writing
         log_fields_user = NULL,


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

      # Validations ------------------------------------------------------------

      # Two types:
      # 1. assert_x - stop if conditions are unmet
      # 2. validate_x - warn if conditions are unmet
      #               - Return TRUE/FALSE

      assert_scalar = function(x){
         if(!(is.atomic(x) && length(x) == 1L)){
            stop("x must be atomic and length 1L")
         }
      },

      assert_named_list = function(x){
         if(!is.null(x)){
            err_msg <- "x must be a named list, not vector (list names may not be whitespace)"
            if(!is.list(x))               stop(err_msg)
            if(is.data.table(x))          stop(err_msg)
            if(is.null(names(x)))         stop(err_msg)
            if(any(is.na(names(x))))      stop(err_msg)
            names(x) <- trimws(names(x))
            if(any(nchar(names(x)) == 0)) stop(err_msg)
         }
      },

      assert_schema_vs_user_entry = function(user_entry,
                                             log_fields_user = private$DICT$log_fields_user,
                                             log_schema      = private$DICT$log_schema) {

         # check that user_entry is a named list
         private$assert_named_list(user_entry)

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

      #' Enforce a data.table's schema with merge protection defaults.
      #'
      #' Asserts the following in order (*all are OPTIONAL*):
      #' - required column names by exact name
      #' - forbidden column names by exact name
      #' - required column names by regex pattern
      #' - forbidden column names by regex pattern (default `\.x` and `\.y` to guard against invalid merges)
      #' - data types for selected columns
      #'
      #' @param my_dt [data.table] your data
      #' @param varnames_strict [chr] strictly required vector of column names - all required, no others allowed
      #' @param strict_order_req [lgl] if varnames_strict is not NULL, should the order of columns be asserted?
      #' @param varnames_req [chr] vector of column names required in my_dt (include)
      #' @param varnames_forbid [chr] vector of column names forbidden in my_dt (exclude)
      #' @param regex_req [chr] regex pattern for required column names
      #' @param regex_forbid [chr] regex pattern for forbidden coumn names (default `\.x` and `\.y` to guard against invalid merges)
      #' @param data_types [named list] `key=value` pair list of column names and required data type
      #' @param verbose [lgl] confirmation message if passing all assertions
      #'
      #' @return [std_err] message or error
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
            if(!all(names(data_types) %in% names(x))) stop(my_dt_name, ": ", "`names(data_types)` must all be columns in `x`")

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

      assert_dir_exists = function(x){
         private$assert_scalar(x)
         root <- suppressWarnings(normalizePath(x))
         if(!dir.exists(root)) stop("root does not exist: ", x)
      },

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

      assert_user_date = function(user_date){
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

      assert_PST_date = function(date){
         private$assert_user_date(user_date = date)
         valid_tz = c("America/Los_Angeles", "US/Pacific", "PST8PDT")
         if(!lubridate::tz(date) %in% valid_tz){
            stop("Invalid timezone.\n",
                 "Allowed:  ", toString(valid_tz), "\n",
                 "Received: ", lubridate::tz(user_date))
         }

      },



      # Utils ------------------------------------------------------------------

      is_an_error = function(x) {
         #' Determine if an object is an error
         #'
         #' @param x [obj] some R object
         #'
         #' @return [lgl] TRUE / FALSE

         (inherits(x, "simpleError") | inherits(x, "try-error"))
      },



      # Implementations --------------------------------------------------------

      ## Symlinks --------------------------------------------------------------

      #' Find and count symlinks by gbd round, date_version, and symlink_type
      #'
      #' @param root [path] full path to gbd round data
      #' @param date_version [chr] e.g. "2023_01_01"
      #' @param symlink_type [chr] e.g. "best"
      #'
      #' @return [list] two elements symlinks - symlink path names (unresolved)
      #'   and symlink_count of symlinks matching private$DICT$`symlink_types`
      #' @export
      #'
      #' @examples
      find_count_symlinks = function(root, date_version, symlink_type){

         # validate inputs
         private$assert_scalar(root)
         private$assert_scalar(date_version)
         private$assert_scalar(symlink_type)

         # Define all eligible symlink regex patterns by type
         arrow_rgx <- " -> " # used in bash to show symlinks
         # We're ONLY monitoring symlinks of a certain pattern - users are free to make others
         symlink_rgx <- list(
            best        = paste0("best", arrow_rgx, ".*", date_version)
            , keep      = paste0("keep_", date_version, arrow_rgx, ".*", date_version)
            , remove    = paste0("remove_", date_version, arrow_rgx, ".*", date_version)
         )

         # validate
         if(!all(names(symlink_rgx) %in% private$DICT$symlink_types)){
            stop("Some of your symlink_rgx are not valid options - update 'private$DICT$symlink_types': \n  ",
                 paste0(names(symlink_rgx), collapse = "\n  ")
            )
         }

         # subset to user's chosen symlink_type
         symlink_rgx <- symlink_rgx[names(symlink_rgx) %in% symlink_type]

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

      #' assert that some number of symlinks exist for some date_version - sums symlinks across all symlink_types specified
      #' use case: ensure you don't have the same date marked 'best' and 'remove' at the same time
      #'
      #' @param root [path] full path to gbdxxxx data folder
      #' @param date_version [chr] a run date for the model
      #' @param symlink_type [chr] one of: c(private$DICT$symlink_types, "all")
      #' @param n_sym [int] number of symlinks to assert
      #' @param allow_fewer [lgl] if TRUE, allow fewer symlinks than n_sym symlinks (i.e. 0 is OK)
      #'
      #' @return
      #' @export
      #'
      #' @examples
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


      resolve_symlink = function(path){
         path_resolved <- system(paste("realpath", path), intern = TRUE)
         if(file.exists(path_resolved)) {
            return(path_resolved)
         } else {
            message("Could not resolve symlink: ", path)
         }
      },

      # make a symlink string of a certain type for a folder
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

      extract_symlink = function(symlink_string){
         # pull a symlink from a linux path string e.g. pull "best" from:
         # "lrwxrwxrwx 1 USER Domain Users  63 Feb  6 17:04 best -> /mnt/share/some/path"

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

      # FIXME SB - 2024 Feb 06 - unused, maybe useful in the future
      # extract_symlink_target = function(symlink_string){
      #    # pull a symlink target from a linux path string e.g. pull "/mnt/share/some/path" from:
      #    # "lrwxrwxrwx 1 USER Domain Users  63 Feb  6 17:04 best -> /mnt/share/some/path"
      #
      #    arrow_rgx <- " -> " # used by linux to show symlinks
      #    if(!grepl(arrow_rgx, symlink_string)) stop("No arrow found in symlink_string: ", symlink_string)
      #    match_idx <- regexpr(arrow_rgx, symlink_string)
      #    start_idx <- as.integer(match_idx) + attr(match_idx, "match.length")
      #    str_post_arrow <- substr(symlink_string, start_idx, nchar(symlink_string))
      #    return(str_post_arrow)
      # },

      # remove one symlink and message about which type was removed, append to log
      remove_one_symlink = function(root, date_version, symlink_type, user_entry){
         private$assert_dir_exists(root)
         private$assert_scalar(date_version)
         private$assert_scalar(symlink_type)
         if(!symlink_type %in% private$DICT$symlink_types) stop("Invalid symlink_type")

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

            # Handle the case where only 'remove' demotions are centrally
            # logged, since they're the last `private$DICT$symlink_types`
            # element.  If we want to centrally log all demotions, we'll need to
            # restructure quite a bit of this code.
            private$handler_reset_dynamic_fields(field_types = "log")

         } else {
            # unset DYNAMIC log fields if there are no symlinks
            # - prevents central log from accidentally collecting extra rows
            # - `append_to_central_log()` is keyed to ignore adding rows with no defined action
            # TODO SB - 2024 Feb 28 - for coding roundtable
            # - this handoff is not really clear without this comment - think of how to improve it
            private$handler_reset_dynamic_fields(field_types = "log")
         }
      },

      # remove all symlinks for a date_version
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
      create_folder_with_log = function(vers_path){

         dir_exists <- private$validate_dir_exists(vers_path, verbose = FALSE)

         if(!dir_exists){
            dir.create(vers_path, recursive = TRUE)
            private$write_expected_log(fpath = file.path(vers_path, private$DICT$log_name))
         } else {
            message("Directory already exists: ", vers_path)
         }

      },

      delete_remove_folder = function(root, date_version, user_entry, require_user_input){
         private$assert_dir_exists(root)
         private$assert_scalar(date_version)

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

      # empty one-row dt to fill by user
      make_schema_dt = function(schema){
         private$assert_named_list(schema)
         dt_schema <- data.table::data.table()
         for(s_name in names(schema)){
            dt_schema[[s_name]] <- vector(mode = schema[[s_name]], length = 0L)
         }
         private$assert_data_schema(dt_schema, data_types = schema) # from code_root/vaccination_pipeline_functions/validations.R
         return(dt_schema)
      },

      write_new_log = function(fpath, log_schema = private$DICT$log_schema){
         dt_log <- private$make_schema_dt(log_schema)
         # Safely write first 'create' row if it doesn't exist
         dt_log <- private$write_log_creation_entry(dt_log)
         data.table::fwrite(dt_log, fpath)
      },

      # Return a time-stamp in "%Y_%m_%d_%H%M%S" format, e.g. "2024_02_21_104202"
      make_current_timestamp = function(){
         return(format(lubridate::with_tz(Sys.time(), tzone="America/Los_Angeles"), "%Y_%m_%d_%H%M%S"))
      },

      # Write a log creation row IF the log is empty
      write_log_creation_entry = function(dt_log){
         if(is.na(private$DYNAMIC$LOG$date_version)) stop("date_version not set - check how you've initialized the date_version, if at all")
         # This will not rewrite the first log line if only that row was deleted - that's misleading
         if(nrow(dt_log) == 0) {
            first_row <- data.table(
               log_id       = 0L,
               timestamp    = private$make_current_timestamp(),
               user         = Sys.info()[["user"]],
               date_version = private$DYNAMIC$LOG$date_version,
               action       = "create",
               comment      = "log created"

            )
            dt_log <- rbind(first_row, dt_log)
         }
         return(dt_log)
      },

      # safely write an expected log with creation entry if it doesn't exist
      write_expected_log = function(fpath, log_schema = private$DICT$log_schema){
         private$assert_scalar(fpath)
         if(!file.exists(fpath)) {
            private$write_new_log(fpath, log_schema)
         } else {
            dt_log <- private$read_log(fpath, log_schema)
            # Safely write first 'create' row if it doesn't exist
            dt_log <- private$write_log_creation_entry(dt_log)
            data.table::fwrite(dt_log, fpath)
         }
      },

      # safely correct a null log, if found (all dim == 0)
      correct_null_log = function(dt_log){
         if(all(dim(dt_log) == 0)){
            message("NULL log found (all dim = 0) - rebuilding schema and writing a new 'create' row.")
            dt_log <- private$make_schema_dt(private$DICT$log_schema)
            dt_log <- private$write_log_creation_entry(dt_log)
         }
         return(dt_log)
      },

      # reads log if it exists, makes a blank one if it doesn't
      read_log = function(fpath, log_schema = private$DICT$log_schema){

         fpath              <- file.path(fpath)
         col_classes        <- unlist(log_schema)
         names(col_classes) <- names(log_schema)
         dt_log             <- tryCatch(
            {
               data.table::fread(fpath, colClasses = col_classes)
            }, warning = function(w) message(
               "Warning reading log: \n ||----- ",
               w)
         )

         # safely correct a null log, if found (all dim == 0)
         dt_log <- private$correct_null_log(dt_log)
         private$assert_data_schema(dt_log, data_types = log_schema)
         return(dt_log)
      },

      append_to_log = function(version_path, user_entry) {

         # validate inputs
         private$assert_named_list(user_entry)
         private$assert_scalar(version_path)

         # needs to read a log to bump the log_id number
         fpath    <- file.path(version_path, private$DICT$log_name)
         private$write_expected_log(fpath, log_schema = private$DICT$log_schema)
         dt_log   <- private$read_log(fpath)
         private$assert_schema_vs_user_entry(user_entry)

         # Safely write first 'create' row if it doesn't exist
         dt_log <- private$write_log_creation_entry(dt_log)

         last_row <- tail(dt_log, 1)

         log_entry <- data.table::data.table(
            log_id       = last_row$log_id + 1,
            timestamp    = private$make_current_timestamp(),
            user         = Sys.info()[["user"]],
            date_version = private$DYNAMIC$LOG$date_version,
            action       = private$DYNAMIC$LOG$action
         )

         for(varname in names(user_entry)){
            log_entry[[varname]] <- user_entry[[varname]]
         }

         log_dt_new <- rbind(dt_log, log_entry)

         message("---- writing log to ", fpath)
         data.table::fwrite(log_dt_new, fpath)
      },


      ### Central Log ----------------------------------------------------------

      # This differs from a date_version log since the first entry DOES NOT
      # have a date_version by definition - we'll define it as "CENTRAL_LOG" for
      # clarity, and write an append process to match
      write_expected_central_log = function(fpath, log_schema = private$DICT$log_schema){
         private$assert_scalar(fpath)
         if(!file.exists(fpath)) {
            private$write_new_central_log(fpath, log_schema)
         } else {
            dt_log <- private$read_log(fpath, log_schema)
            # Safely write first 'create' row if it doesn't exist
            dt_log <- private$write_central_log_creation_entry(dt_log)
            data.table::fwrite(dt_log, fpath)
         }
      },

      write_new_central_log = function(fpath, log_schema = private$DICT$log_schema){
         dt_log <- private$make_schema_dt(log_schema)
         # Safely write first 'create' row if it doesn't exist
         dt_log <- private$write_central_log_creation_entry(dt_log)
         data.table::fwrite(dt_log, fpath)
      },

      write_central_log_creation_entry = function(dt_log){
         # This will not rewrite the first log line if only that row was deleted - that's misleading
         if(nrow(dt_log) == 0) {
            first_row <- data.table(
               log_id       = 0L,
               timestamp    = private$make_current_timestamp(),
               user         = Sys.info()[["user"]],
               date_version = "CENTRAL_LOG",
               action       = "create",
               comment      = "log created"

            )
            dt_log <- rbind(first_row, dt_log)
         }
         return(dt_log)
      },

      append_to_central_log = function(user_entry) {

         # validate inputs
         private$assert_named_list(user_entry)

         # needs to read a log to bump the log_id number
         fpath    <- private$DICT$LOG_CENTRAL$path
         private$write_expected_central_log(fpath, log_schema = private$DICT$log_schema)
         dt_log   <- private$read_log(fpath)
         private$assert_schema_vs_user_entry(user_entry)

         # Safely write first 'create' row if it doesn't exist
         dt_log <- private$write_central_log_creation_entry(dt_log)

         last_row <- tail(dt_log, 1)

         # We only want to append if there is a defined action
         # We clear actions in `remove_one_symlink` if there are no symlinks to a folder
         # - we don't want to fill up logs with junk rows
         # - this is handled within `remove_one_symlink` for date_version logs
         # - since the central log is folder-agnostic, we're handling it here instead
         # - this is a bit messy, hence the long comment
         # TODO SB - 2024 Feb 28 - for coding roundtable
         # - this handoff is not really clear without this comment - think of how to improve it

         message("Central log:")
         if(!is.na(private$DYNAMIC$LOG$action)){
            log_entry <- data.table::data.table(
               log_id       = last_row$log_id + 1,
               timestamp    = private$make_current_timestamp(),
               user         = Sys.info()[["user"]],
               date_version = private$DYNAMIC$LOG$date_version,
               action       = private$DYNAMIC$LOG$action
            )

            for(varname in names(user_entry)){
               log_entry[[varname]] <- user_entry[[varname]]
            }

            log_dt_new <- rbind(dt_log, log_entry)

            message("-- Writing central log to ", fpath)
            data.table::fwrite(log_dt_new, fpath)
         } else {
            message("-- No action defined, not writing to central log.\n",
                    "---- This is expected if no symlinks were found or user-input did not produce an action for: ", private$DYNAMIC$LOG$date_version)
         }

      },


      ## Queries ---------------------------------------------------------------

      # each query_all_logs_* function should have a corresponding report

      # the '_query_' function family is for reporting on the state of the symlinks
      # they differ from 'read_log', which is for promotion/demotion, and will create a new log

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

      # `try()` to find logs for all folders, read them if they exist, in a consistent format
      try_query_log = function(version_path, verbose = TRUE){
         tryCatch(
            {
               data.table::fread(
                  file.path(version_path, private$DICT$log_name)
                  , colClasses = unlist(private$DICT$log_schema
                  ))
            },
            error = function(e) if(verbose) message("No log found for folder: ", version_path)
         )
      },

      # safely remove null logs, and account for zero-length logs if none are found for a date_version folder
      filter_null_logs_safely = function(log_list){
         if(length(log_list) == 0 || all(is.null(unlist(log_list)))){
            return(list(no_logs_found = private$make_schema_dt(schema = private$DICT$log_schema)))
         } else {
            return(log_list[!unlist(lapply(log_list, is.null))])
         }
      },

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
         lapply(log_list, private$assert_data_schema, data_types = private$DICT$log_schema)
         return(log_list)
      },

      query_all_logs_symlink = function(root){
         # find all folders and their types
         folder_dt <- private$query_root_folder_types(root)
         # query logs for active symlinks of any type
         unique_version_paths <- unique(folder_dt[is_symlink == TRUE, dir_name_resolved])
         log_list             <- lapply(unique_version_paths, private$try_query_log, verbose = FALSE)
         names(log_list)      <- unique_version_paths
         # remove any NULLs, result of the tryCatch in try_query_log
         log_list             <- private$filter_null_logs_safely(log_list)
         lapply(log_list, private$assert_data_schema, data_types = private$DICT$log_schema)
         return(log_list)
      },

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
         lapply(log_list, private$assert_data_schema, data_types = private$DICT$log_schema)
         return(log_list)
      },

      # This one is a little different - requires a setdiff, so has some different variable naming patterns
      query_all_logs_non_symlink = function(root){
         # find all folders and their types
         folder_dt                 <- private$query_root_folder_types(root)
         # find unique paths not targeted by any symlink
         unique_dir_names_resolved <- unique(folder_dt$dir_name_resolved)
         unique_symlink_targets    <- unique(folder_dt[is_symlink == TRUE, dir_name_resolved])
         unique_non_symlink_paths  <- setdiff(unique_dir_names_resolved, unique_symlink_targets)
         # query logs
         log_list                  <- lapply(unique_non_symlink_paths, private$try_query_log)
         names(log_list)           <- unique_non_symlink_paths
         # remove any NULLs, result of the tryCatch in try_query_log
         log_list                  <- private$filter_null_logs_safely(log_list)
         lapply(log_list, private$assert_data_schema, data_types = private$DICT$log_schema)
         return(log_list)
      },

      query_logs_last_row = function(log_list){
         # find the last row of each log
         return(
            data.table::rbindlist(lapply(log_list, function(x) tail(x, 1)))
         )
      },

      query_log_id_max = function(log_list){
         # find the row with highest non-missing log_id of each log
         return(
            data.table::rbindlist(lapply(log_list, function(x) x[max(x$log_id, na.rm = TRUE)]))
         )
      },

      query_all_remove_symlinks = function(root){
         # find all folders and their types
         folder_dt            <- private$query_root_folder_types(root)
         # find all resolved paths attached to a "remove_" symlink, and their symlink names
         # allow the user to decide how to deal with removal
         remove_dirs_dt <- unique(folder_dt[is_symlink == TRUE & grepl("remove_", dir_leaf), .(dir_date_version, dir_name, dir_name_resolved)])
         return(remove_dirs_dt)
      },

      query_logs_first_row = function(log_list){
         return(
            data.table::rbindlist(lapply(log_list, function(x) head(x, 1)))
         )
      },

      query_log_id_0 = function(log_list){
         return(
            data.table::rbindlist(lapply(log_list, function(x) x[log_id == 0, ]))
         )

      },

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

      # each report function must have a corresponding query_all_logs_* function

      report_all_logs = function(root){
         # query logs for active symlinks of any type
         log_list <- private$query_all_logs(root)
         last_row_dt <- private$query_logs_last_row(log_list)
         data.table::fwrite(last_row_dt, file.path(root, "report_all_logs.csv"))
      },

      report_all_logs_symlink = function(root){
         # query logs for active symlinks of any type
         log_list <- private$query_all_logs_symlink(root)
         last_row_dt <- private$query_logs_last_row(log_list)
         data.table::fwrite(last_row_dt, file.path(root, "report_all_logs_symlink.csv"))
      },

      # This runs each time a user runs a 'mark' function
      # - Invoked by handler_post_mark
      # - It must provide an updated report of the current state of the symlinks
      # - It must provide a discrepancy report as well
      report_all_logs_tool_symlink = function(root){
         # query logs for active symlinks of any type
         log_list <- private$query_all_logs_tool_symlink(root, verbose = FALSE)
         last_row_dt <- private$query_logs_last_row(log_list)
         data.table::fwrite(last_row_dt, file.path(root, "report_all_logs_tool_symlink.csv"))

         private$report_discrepancies(root = root)

      },

      report_all_logs_non_symlink = function(root){
         # query logs for active symlinks of any type
         log_list <- private$query_all_logs_non_symlink(root)
         last_row_dt <- private$query_logs_last_row(log_list)
         data.table::fwrite(last_row_dt, file.path(root, "report_all_logs_non_symlink.csv"))
      },

      # add a discrepancy column to a data.table if it has rows
      add_discrepancy_to_dt = function(report_dt, discrepancy_reason){
         if(nrow(report_dt) > 0){
            report_dt[, discrepancy := discrepancy_reason]
         } else {
            report_dt[, discrepancy := NA_character_]}
         return(report_dt)
      },

      # for discrepancies related to paths, build an empty report schema
      make_report_schema_for_discrepant_paths = function(path_dt, discrepancy_reason){
         # input validation
         if(!data.table::is.data.table(path_dt)) stop("path_dt must be a data.table")
         if(!is.character(discrepancy_reason))   stop("discrepancy_reason must be a character string")
         varname_dir <- grep("^dir_name", names(path_dt), value = TRUE)
         if(length(varname_dir) != 1)            stop("path_dt must have exactly one column starting with 'dir_name'")

         # FIXME SB - 2024 Feb 23 - OUTPUT A ZERO-ROW DATA TABLE IF THERE ARE NOT PATHS TO REPORT

         # add an all NA row to schema_dt
         schema_dt             <- private$make_schema_dt(private$DICT$log_schema)
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

      # Catch-all report for any reason you can think a symlink or log is inaccurate
      # - start with the folder_dt, since we're looking both for weird folders and weird logs
      report_discrepancies = function(root){

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

         # Rows without `log_id`
         log_list_all            <- private$query_all_logs(root)
         log_list_all_no_id      <- lapply(log_list_all, function(x) x[is.na(log_id)])
         discrepant_dt_no_log_id <- data.table::rbindlist(log_list_all_no_id)
         discrepant_dt_no_log_id <- private$add_discrepancy_to_dt(discrepant_dt_no_log_id, "rows without log_id")

         # Non-sequential log_ids
         # - logs should start at 0 (create line), then go up from there
         log_list_log_id_non_seq      <- lapply(log_list_all, function(x) x[log_id != 0:(.N-1)])
         discrepant_dt_log_id_non_seq <- data.table::rbindlist(log_list_log_id_non_seq)
         discrepant_dt_log_id_non_seq <- private$add_discrepancy_to_dt(discrepant_dt_log_id_non_seq, "non-sequential log_ids")

         # Invalid log actions
         log_list_invalid_actions      <- lapply(log_list_all, function(x) x[!(action %in% private$DICT$valid_actions)])
         discrepant_dt_invalid_actions <- data.table::rbindlist(log_list_invalid_actions)
         discrepant_dt_invalid_actions <- private$add_discrepancy_to_dt(discrepant_dt_invalid_actions, "invalid actions")


         discrepant_list <- list(
            discrepant_dt_tool_symlink_no_logs = discrepant_dt_tool_symlink_no_logs
            , discrepant_dt_non_tool_symlinks  = discrepant_dt_non_tool_symlinks
            , discrepant_dt_mult_symlinks      = discrepant_dt_mult_symlinks
            , discrepant_dt_demote             = discrepant_dt_demote
            , discrepant_dt_promote            = discrepant_dt_promote
            , discrepant_dt_no_log_id          = discrepant_dt_no_log_id
            , discrepant_dt_log_id_non_seq     = discrepant_dt_log_id_non_seq
            , discrepant_dt_invalid_actions    = discrepant_dt_invalid_actions
         )

         discrepancy_report_dt <- rbindlist(discrepant_list, fill = TRUE)

         # If no discrepancies are found, then delete the current discrepancy report so it doesn't cause confusion
         path_discrepancy_report <- file.path(root, "REPORT_DISCREPANCIES.csv")

         if(nrow(discrepancy_report_dt) == 0) {
            message("No discrepancies found in ", root, ", removing REPORT_DISCREPANCIES.csv (if it exists now)")
            suppressWarnings(file.remove(path_discrepancy_report))
         } else {
            data.table::fwrite(discrepancy_report_dt, path_discrepancy_report)
         }
      },



      ## Handlers --------------------------------------------------------------

      # These functions handle the internal state of the tool in various ways
      # - before marking and/or folder creation
      # - after marking and/or folder deletion
      # - managing the DYNAMIC field list

      # Handle pre-mark operation validations and updates
      # - mostly all `mark`, `create` and `delete` public functions should invoke this
      handler_pre_mark = function(date_version, user_entry){
         # validate inputs
         private$assert_scalar(date_version)
         private$assert_named_list(user_entry)
         private$assert_schema_vs_user_entry(user_entry)

         # enforce one symlink per date_version
         # we do this to ensure users haven't hand-made symlinks
         # of the same form we expect to use with this tool
         for(root in private$DICT$ROOTS){
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

      # Handle post-mark operation validations and updates
      # - handle central log updates
      # - mostly all `mark`, `create` and `delete` public functions should invoke this
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

         # There is only one central log per instantiation of the tool
         private$append_to_central_log(user_entry = user_entry)

      },

      # build versioned paths from roots, assert existence
      handler_update_version_paths = function(date_version){
         private$assert_scalar(date_version)
         private$DYNAMIC$VERS_PATHS <- file.path(private$DICT$ROOTS, date_version)
         if(!length(private$DYNAMIC$VERS_PATHS)) stop("No version paths found")
         lapply(private$DYNAMIC$VERS_PATHS, private$validate_dir_exists, verbose = FALSE)
      },

      # update all dynamic fields except log action
      handler_update_dynamic_fields = function(date_version){
         private$assert_scalar(date_version)
         # Update dictionaries
         private$DYNAMIC$LOG$date_version <- date_version
         private$handler_update_version_paths(date_version = date_version)
      },

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

      # 'promote' and 'demote' are consistent terms to say 'carry out this action from the log schema'

      demote_existing_symlinks = function(version_path, date_version, user_entry){
         # find existing symlinks for a date_version
         symlink_types <- private$DICT$symlink_types
         names(symlink_types) <- symlink_types # names for lapply

         root <- sub(date_version, "", version_path)

         symlink_list <- lapply(symlink_types, function(x){
            private$find_count_symlinks(root = root, date_version = date_version, symlink_type = x)
         })

         symlink      <- unlist(unname(purrr::map_depth(symlink_list, 1, "symlinks")))
         symlink_type <- names(symlink)

         if(length(symlink) > 1) {
            stop("More than one symlink found for ", date_version, " in: ", root, "\n  ",
                 paste(symlink, collapse = "\n  "),
                 "Please select one ", paste(private$DICT$symlink_types, collapse = "/"),  " symlink and delete all others manually.")
         }

         if(length(symlink) > 0) {
            message("-- Demoting existing symlink: ", symlink)
            symlink                    <- private$extract_symlink(symlink)
            symlink                    <- paste0(root, symlink)
            private$DYNAMIC$LOG$action <- paste0("demote_", symlink_type)
            private$append_to_log(version_path = version_path, user_entry = user_entry)
            system(paste0("unlink ", symlink))
         } else {
            message("-- No existing symlinks found - moving on")
         }


      },

      #> Best symlinks are trickier than others
      #> - there can be only one per GBD round
      #> - we're demoting a different version than we're promoting
      #> - we need to specifically update the demoted verion's log
      #> - we need to simultaneously demote and promote
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
            private$DYNAMIC$LOG$date_version <- date_version

            system(paste0("unlink ", path_best_sym))
         } else {
            message("-- No 'best' symlink found - moving on: ", path_best_sym)
         }
      },

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
         # force symlink to change in case the unlink is glitchy
         system(paste0("ln -nsf ", path_best_new, " ", path_best_sym))
      },

      promote_keep = function(version_path, user_entry){
         private$DYNAMIC$LOG$action <- "promote_keep"
         path_keep_sym <- private$make_symlink_path(version_path = version_path, symlink_type = "keep")
         message("-- Promoting to 'keep': ", path_keep_sym)
         if(!dir.exists(path_keep_sym)){
            private$append_to_log(version_path = version_path, user_entry = user_entry)
            system(paste0("ln -s ", version_path, " ", path_keep_sym))
         } else {
            message("---- Keep symlink already exists - moving on: ", path_keep_sym)
         }
      },

      promote_remove = function(version_path, user_entry){
         private$DYNAMIC$LOG$action <- "promote_remove"
         path_remove_sym <- private$make_symlink_path(version_path = version_path, symlink_type = "remove")
         message("-- Promoting to 'remove': ", path_remove_sym)
         if(!dir.exists(path_remove_sym)){
            private$append_to_log(version_path = version_path, user_entry = user_entry)
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




   # User functions ------------------------------------------------------------

   # These function names should be named pretty tersely and user-friendly

   public = list(

      #' Initialize a symlink tool by GBD round
      #'
      #' Any time the tool is made, the tool is GBD-round specific.  Create one
      #' per round if you need more than one
      #'
      #'
      #' @param user_root_list [list] Named list of root directories for
      #'   pipeline outputs. This is where `date_version` folders live (these
      #'   are iterative runs of an analysis pipeline.)
      #' @param user_central_log_root [path] Root directory for the central log.
      #'   If you have multiple roots in the `user_root_list`, you probably want
      #'   the central log to live one level above those roots.
      #'
      #' @return [symlink_tool] A symlink tool object.  You can instantiate
      #'   (create) multiple version, each of which has different roots and
      #'   central logs.
      #' @export
      #'
      #' @examples See the symlink_tool_vignette.
      initialize = function(user_root_list = NULL, user_central_log_root = NULL) {

         # useful start up feedback
         if(is.null(user_root_list)){
            message("\n\nThis tool expects `user_root_list` to be a named list of root directories for pipeline outputs. \n\n  ",

                 "e.g.
                 list( input_root = '/mnt/share/my_team/input_data',
                      output_root = '/mnt/share/my_team/output_data' ) \n\n  ",

                 "This tool assumes each root has a matching `date_version` output folder. \n  ",
                 "  You may divert outputs to one root, or many roots in parallel. \n\n  ",

                 "Each output folder defined by `file.path(user_root_list, date_version)`. \n  ",
                 "  The `date_version` is defined when the user wants to 'mark' or 'unmark' a folder as best/keep/remove. \n  ",
                 "  This folder receives a log of all *demotion* and *promotion* actions (marking and unmarking). \n  ",
                 "  This `date_version` log is used for report generation. \n\n  "
            )
         }

         if(is.null(user_central_log_root)){
            message("\n\nThis tool expects `user_central_log_root` to be a single directory for the central log. \n\n  ",

                 "e.g.
                 '/mnt/share/my_team' \n\n  ",

                 "The central log is a summary record of all *promotion* (marking) actions done by this tool, \n  ",
                 "  but is not used for report generation. \n\n  ",
                 "The central log is *created* on initialization i.e. when calling `SLT$new()`. \n\n  ",

                 "Each output folder defined by `file.path(user_root_list, date_version)`. \n  ",
                 "  The `date_version` is defined when the user wants to 'mark' or 'unmark' a folder as best/keep/remove. \n  ",
                 "  This folder receives a log of all *demotion* and *promotion* actions (marking and unmarking). \n  ",
                 "  This `date_version` log is used for report generation. \n\n  "
            )
         }

         if(any(is.null(user_root_list) || is.null(user_central_log_root))){
            stop("You must provide both user_root_list and user_central_log_root")
         }

         library(data.table)

         # Users must provide these fields

         ## ROOTS
         # validate inputs
         private$assert_named_list(user_root_list)
         lapply(user_root_list, private$assert_dir_exists)
         # set
         private$DICT$ROOTS <- user_root_list

         ## CENTRAL LOG
         # validate inputs
         private$assert_scalar(user_central_log_root)
         private$assert_dir_exists(user_central_log_root)
         # set
         private$DICT$LOG_CENTRAL$root <- user_central_log_root


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
      #' @export
      #'
      #' @examples
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
      #' @export
      #'
      #' @examples
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
      #' @export
      #'
      #' @examples
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
      #' @export
      #'
      #' @examples
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
      #' @export
      #'
      #' @examples
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
      #' @return
      #' @export [std_err] Messages about the symlinks removed.
      #'
      #' @examples
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

      #' Find all `remove_` symlinks in all `roots`
      #'
      #' Return both the symlink and the resolved symlink (folder the symlink
      #' points to)
      #'
      #' @return [list] list of data.tables - one for each `root`
      #' @export
      #'
      #' @examples
      roundup_remove = function(){
         return(lapply(private$DICT$ROOTS, private$query_all_remove_symlinks))
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
      #' @export
      #'
      #' @examples
      roundup_by_date = function(user_date, date_selector, verbose = TRUE){


         # format inputs for assertion
         date_selector <- tolower(date_selector)
         # assert inputs
         private$assert_date_selector(date_selector)
         private$assert_user_date(user_date)

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
      #' @export
      #'
      #' @examples
      create_date_version_folders_with_logs = function(date_version){

         private$assert_scalar(x = date_version)

         private$handler_update_dynamic_fields(date_version = date_version)

         for(vers_path in private$DYNAMIC$VERS_PATHS){
            private$create_folder_with_log(vers_path = vers_path)
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
      #' @export
      #'
      #' @examples
      make_new_log = function(date_version){

         private$handler_update_dynamic_fields(date_version = date_version)

         # The read_log function will:
         # - check if a log exists
         # - write a new log with one "creation" row and date-stamp if not
         #
         # This is safer than blindly writing/overwriting an existing log
         for(version_path in private$DYNAMIC$VERS_PATHS){
            fpath_log <- file.path(version_path, private$DICT$log_name)
            tryCatch(
               {
                  private$write_expected_log(fpath_log)
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
      #' @export
      #'
      #' @examples
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
      #' @export
      #'
      #' @examples
      reports = function(){

         for(root in private$DICT$ROOTS){
            message("Writing last-row log reports for ", root)
            private$report_all_logs(root = root)
            private$report_all_logs_symlink(root = root)
            private$report_all_logs_tool_symlink(root = root)
            private$report_all_logs_non_symlink(root = root)
            private$report_discrepancies(root = root)
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


