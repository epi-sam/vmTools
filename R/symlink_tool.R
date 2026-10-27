# WARNING: DO NOT touch '.__enclos_env__' unless you want the tool to break
# ROXYGEN
#   - Roxygen docstrings cannot be added to private methods
#   - https://stackoverflow.com/questions/45431845/documenting-r6-classes-and-methods-within-r-package-in-rstudio

#' @title SymlinkTool R6 class
#' @description Class for lightweight file-system level data versioning, logs
#'   and reports without need for a database. Manages four symlink types: 'best'
#'   (validated production), 'latest' (most recent run), 'keep' (archive-worthy),
#'   and 'remove' (marked for deletion).
#' @import data.table
#' @importFrom R6 R6Class
#' @export
SLT <- R6::R6Class(

   "Symlink_Tool",

   private = list(

      # DICT -------------------------------------------------------------------

      # All private$___ items are functions, except for:
      # - constants stored in the DICT sub-list
      # - dynamic values stored in the DYNAMIC sub-list

      DICT = list(

         # The tool expects that each 'root' has a bunch of 'version_name' folders of pipeline output
         # - each 'version_name' represents a fresh run of a data pipeline
         # - each 'version_name' must be a folder one level under each 'root'
         # - the tool expects that each 'version_name' exists in each 'root',
         #   - i.e. you could split pipeline outputs from the same `version_name` into different 'root' folders
         # - the tool can work with heterogeneous 'root' folders
         #   - i.e. you don't need exactly the same 'version_names' in each, but the tool will try to manage `version_names` across `roots` in parallel
         #
         # Every time the tool runs a 'mark' operation, it will update the report on tool-generated active symlinks


         # Control-flow flags
         FLAGS = list(
            # TRUE if logs formats may differ over time
            # FALSE if user requires logs formats to be consistent
            allow_schema_repair = NULL
         )

         ## Initialize: user-defined

         , ROOTS = NULL

         # TRUE/FALSE per root: can the tool create symlinks (shortcuts) there?
         # - filled by a probe at construction (see R/links.R dir_supports_links)
         , ROOT_LINK_CAPABLE = NULL


         ## Logs

         , LOG_CENTRAL = list(
            root  = NULL,
            fname = "log_symlinks_central.csv",
            path  = NULL
         )

         , log_dir  = "logs"
         , log_name = "log_version_history.csv"
         , log_path = "logs/log_version_history.csv"

         , log_schema = list(
            log_id         = "integer"
            , timestamp    = "character"
            , user         = "character"
            , version_name = "character"
            , version_path = "character"
            , action       = "character"
            , comment      = "character"
         )

         , log_sort_varname = "timestamp"

         , valid_log_field_types = c(
            "integer"
            , "character"
         )

         , log_fields_auto = c(
            "log_id"
            , "timestamp"
            , "user"
            , "version_name"
            , "version_path"
            , "action"
         )

         # hard-setting instead of user-settable - avoid log discrepancies
         , datestamp_format = "%Y_%m_%d_%H%M%S"


         ## Initialize: internally-defined
         # In `initialize()`, defined as `setdiff(names(private$DICT$log_schema), private$DICT$log_fields_auto)`
         # e.g. c("comment") at time of writing
         , log_fields_user = NULL

         # Timezone
         , TZ = NULL


         # Reports - summary of last log entries across version_nameed folders
         # - Since these are the most visible/useful user summary, these could be set by `initialize()` in the future.

         , report_fnames = list(
            all_logs_tool_symlink   = "report_key_versions.csv"
            , all_logs              = "report_all_logs.csv"
            , all_logs_symlink      = "report_all_logs_symlink.csv"
            , all_logs_non_symlink  = "report_all_logs_non_symlink.csv"
            , discrepancies         = "report_DISCREPANCIES.csv"
         )

      ),


      # DYNAMIC ----------------------------------------------------------------

      # These are not true R6 active fields, but are dynamic within the tool.
      # I'm not exposing these to the user but they need to by dynamic
      # For each version_name operation, these will update e.g. clean_path(private$DICT$ROOTS, version_name)
      # - updated by operation, not on instantiate
      # - for managing internal state of the tool, not for user input
      # - if they seem buggy, call `return_dynamic_fields()` to see if they are updating as expected
      DYNAMIC = list(

         # Field for logs - updated by tool, not user
         LOG = list(
            version_name = NA_character_
            , action     = NA_character_
         )

         # Paths built from roots and intermediate path structures
         , VERS_PATHS = list()
      ),


      # PRIVATE METHODS --------------------------------------------------------

      # Redefined Functions ----------------------------------------------------
      # Functions under the user's control on instantiation
      # - requires NULL placeholder

      csv_reader     = NULL,
      csv_writer_raw = NULL,

      # Atomic CSV write: temp file in the target directory, then rename.
      #
      # All log and report writes go through here.  A plain `fwrite()` /
      # `write.csv()` truncates the target in place and then streams rows in, so
      # any concurrent reader (e.g. another array job) can observe an empty or
      # half-written file.  `file.rename()` within one filesystem is atomic, so
      # readers only ever see the complete old file or the complete new file.
      # The temp file MUST live in the same directory as the target: rename
      # across filesystems is not atomic (and may fail).
      #
      # If the target already exists, its permissions are copied onto the new
      # file so a rewrite does not silently change group/world access on
      # shared directories.
      #
      # @param x (data.frame) table to write
      # @param fpath (chr) target file path
      # @param ... passed to the underlying writer (`private$csv_writer_raw`)
      #
      # @return (none) writes to disk
      csv_writer = function(x, fpath, ...){
         assert_scalar(fpath)
         target_dir <- dirname(fpath)
         if(!dir.exists(target_dir)){
            stop(
               "Could not write the file: ", fpath,
               "\nIts folder does not exist: ", target_dir
            )
         }
         fpath_tmp <- tempfile(
            pattern   = paste0(".", basename(fpath), ".")
            , tmpdir  = target_dir
            , fileext = ".tmp"
         )
         # always remove the temp file if anything below fails
         on.exit(if(file.exists(fpath_tmp)) unlink(fpath_tmp), add = TRUE)

         private$csv_writer_raw(x, fpath_tmp, ...)

         if(file.exists(fpath)){
            Sys.chmod(fpath_tmp, mode = file.mode(fpath), use_umask = FALSE)
         }

         rename_ok <- file.rename(fpath_tmp, fpath)
         if(!isTRUE(rename_ok)){
            stop(
               "Could not finish writing the file: ", fpath,
               "\nThe original file (if any) was left untouched.\n",
               "Common causes: you do not have write permission on the folder, ",
               "the file is locked by another program, or the disk is full."
            )
         }
         invisible(NULL)
      },

      # Message Functions ------------------------------------------------------

      # Message if verbose, with option to warn even if verbosity is off
      #
      # If automating batch operations, large messge volumes may be undesired.
      # If so, suppress messages, but warn in cases where something might go
      # amiss.
      #
      # @param ... (...) passed to `message()`
      # @param verbose (lgl) send message to std_err?
      # @param always_message (lgl) send it anyway in select circumstances?
      #
      # @returns (std_err)
      msg_sometimes = function(..., verbose = private$DICT$verbose, always_message = FALSE){
         assert_scalar(verbose)
         assert_type(verbose, "logical")
         assert_scalar(always_message)
         assert_type(always_message, "logical")

         if(verbose == TRUE){
            message(...)
         } else if (verbose == FALSE & always_message == FALSE) {
            # do nothing
         } else if (verbose == FALSE & always_message == TRUE) {
            message(...)
         }
         return(invisible(NULL))
      },

      # Check Functions --------------------------------------------------------

      # Types of check functions:
      # 1. assert_x        - stop() if conditions are unmet
      # 2. validate_x/is_x - return() TRUE/FALSE



      #  Assert the user's mark-operation entry matches the log schema
      #
      #  @param user_entry (named list) User's entry for a mark operation
      #  @param log_fields_user (character) Names of fields in the log schema
      #  @param log_schema (named list) Log schema
      #
      #  @return (none)
      assert_schema_vs_user_entry = function(user_entry,
                                             log_fields_user = private$DICT$log_fields_user,
                                             log_schema      = private$DICT$log_schema) {

         # check that user_entry is a named list
         assert_named_list(user_entry)
         assert_named_list(log_schema)

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
      #  @param my_dt (data.table) your data
      #  @param varnames_strict (chr) strictly required vector of column names - all required, no others allowed
      #  @param strict_order_req (lgl) if varnames_strict is not NULL, should the order of columns be asserted?
      #  @param varnames_req (chr) vector of column names required in my_dt (include)
      #  @param varnames_forbid (chr) vector of column names forbidden in my_dt (exclude)
      #  @param regex_req (chr) regex pattern for required column names
      #  @param regex_forbid (chr) regex pattern for forbidden coumn names (default `\.x` and `\.y` to guard against invalid merges)
      #  @param data_types (named list) `key=value` pair list of column names and required data type
      #  @param verbose (lgl) confirmation message if passing all assertions
      #
      #  @return (none) message or error
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
            regex_list <- lapply(regex_req, x = x, function(regex, x){
               if(!any(grepl(regex, names(x)))) return(regex)
            })
            regex_failures <- paste(unlist(regex_list), collapse = ", ")
            if(nchar(regex_failures)) stop(my_dt_name, ": ", "No column name contains the required pattern(s):\n", "    ", regex_failures)
         }

         if(!is.null(varnames_forbid)){
            forbidden_colnames <- paste(names(x)[which(names(x) %in% varnames_forbid)], collapse = ", ")
            if(nchar(forbidden_colnames)) stop(my_dt_name, ": ", "Forbidden column names found:\n", "    ", forbidden_colnames)
         }

         if(!is.null(regex_forbid)){
            regex_list <- lapply(regex_forbid, x = x, function(regex, x){
               if(any(grepl(regex, names(x)))) return(names(x)[grepl(regex, names(x))])
            })
            regex_failures <- paste(unlist(regex_list), collapse = ", ")
            if(nchar(regex_failures)) stop(my_dt_name, ": ", "Some column name(s) contain the forbidden pattern(s): ", paste(regex_forbid, collapse = ", "), "\n", "    ", regex_failures)
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
            coltype_success_mask <- unlist(lapply(names(data_types), function(.key){typeof(x[[.key]]) == data_types[[.key]]}))

            if(any(coltype_success_mask == FALSE)){
               coltype_fail_mask <- !coltype_success_mask
               coltype_fails     <- coltypes_actual[coltype_fail_mask]
               stop(my_dt_name, ": ", "Columns failing type-matching.  Actual column data-types are:", "\n    ", paste(utils::capture.output(coltype_fails), collapse = "\n    "))
            }
         }

         if(verbose == TRUE) message(my_dt_name, ": ", "Passing data schema validation.")

      },

      #  Assert the date_selector shorthand is valid.
      #
      #  Examples:
      #  gt  = 'greater_than'
      #  gte = 'greater_than_equal_to'
      #  lt  = 'less_than'
      #  lte = 'less_than_equal_to'
      #  e   = 'equal'
      #
      #  Used by `roundup_by_date`
      #
      #  @param date_selector (chr) One of 'gt', 'gte', 'lt', 'lte', 'e'
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
               sprintf("(%s)", toString(date_selectors_decoded))
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
      #  @param user_date (chr) a date string of class `c('character', 'Date', 'POSIXct', 'POSIXt')`
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


      # Implementations --------------------------------------------------------

      ## Symlinks --------------------------------------------------------------

      #  Find and count symlinks by root, version_name, and symlink_type
      #
      #  @param root (path) root path with versioned data folders
      #  @param version_name (chr) e.g. "2023_01_01"
      #  @param symlink_type (chr) e.g. "best"
      #
      #  @return (list) two elements: symlinks - symlink names (the link's own
      #    name under root, e.g. "best" or "keep_1990_01_01") and symlink_count
      find_count_symlinks = function(root, version_name, symlink_type){

         # validate inputs
         assert_scalar(root)
         assert_scalar(version_name)
         assert_scalar(symlink_type)

         # The tool's symlink for a type + version has exactly one name (e.g.
         # `best`, `keep_1990_01_01`) and must point at that version folder.
         # We're ONLY monitoring symlinks of that pattern - users are free to make others
         leaf_expected <- basename(private$make_symlink_path(clean_path(root, version_name), symlink_type))

         links_dt <- link_list(root)
         links_dt <- links_dt[is_link == TRUE & leaf == leaf_expected, ]
         # A link whose target can be read must point at this version. A dangling
         # link whose target cannot be read (Windows, PowerShell silent) is
         # matched by name alone, but only for per-version types: `keep_<v>`
         # carries the version in its name, `best` does not and must never be
         # attributed to a version by guesswork.
         is_singleton   <- isTRUE(get_symlink_registry_entry(symlink_type)[["is_singleton"]])
         target_missing <- is.na(links_dt$target)
         keep_row       <- !target_missing & basename(links_dt$target) == version_name
         keep_row[target_missing] <- !is_singleton
         links_dt       <- links_dt[keep_row, ]

         return(list(
            symlinks      = links_dt$leaf,
            symlink_count = nrow(links_dt)
         ))
      },

      # Check if a version_name is already marked with a symlink of a certain type
      #
      # @param version_path (chr) full path to a version folder, e.g. /mnt/share/gbdxxxx/2023_01_01
      # @param symlink_type (chr) one of: get_symlink_types()
      #
      # @return (lgl) TRUE if symlink exists, FALSE if not
      already_marked = function(version_path, symlink_type){
         # validate inputs
         assert_scalar(version_path)
         assert_scalar(symlink_type)
         root         <- dirname(version_path)
         version_name <- basename(version_path)

         symlink_list <- private$find_count_symlinks(root = root, version_name = version_name, symlink_type = symlink_type)
         return(symlink_list$symlink_count > 0)
      },

      #  assert that some number of symlinks exist for some version_name - sums symlinks across all symlink_types specified
      #  use case: ensure you don't have the same date marked 'best' and 'remove' at the same time
      #
      #  @param root (path) full path to gbdxxxx data folder
      #  @param version_name (chr) a run date for the model
      #  @param symlink_type (chr) one of: c(get_symlink_types(), "all")
      #  @param n_sym (int) number of symlinks to assert
      #  @param allow_fewer (lgl) if TRUE, allow fewer symlinks than n_sym symlinks (i.e. 0 is OK)
      #
      #  @return none
      assert_n_symlinks = function(root, version_name, symlink_type = "all", n_sym = 1L, allow_fewer = TRUE){

         # validate inputs
         validate_dir_exists(root, verbose = FALSE)
         assert_scalar(version_name)
         assert_scalar(n_sym)
         if(!is.integer(n_sym)) stop("n_sym must be an integer")
         if(n_sym < 0L) stop("n_sym must be non-negative")

         valid_symlink_types <- c(get_symlink_types(), "all")
         if(!symlink_type %in% valid_symlink_types) {
            stop("symlink_type must be one of: \n  ",
                 valid_symlink_types, collapse = "\n  ")
         }

         if(symlink_type == "all") symlink_type <- get_symlink_types()
         names(symlink_type) <- symlink_type # names for lapply

         symlink_list <- lapply(symlink_type, function(x){
            private$find_count_symlinks(root = root, version_name = version_name, symlink_type = x)
         })

         symlink_counts <- unlist(unname(lapply_depth(symlink_list, 1, `[[`, "symlink_count")))

         version_path <- clean_path(root, version_name)
         if(allow_fewer){
            if(sum(symlink_counts) > n_sym) {
               stop("You have more than ", n_sym, " symlink(s) for ", version_path, "\n  ",
                    "Please run 'unmark()' to remove all symlinks, then try again.\n  ",
                    paste(utils::capture.output(symlink_counts), collapse = "\n  "))
            }
         } else {
            if(sum(symlink_counts) != n_sym) {
               stop("You do not have ", n_sym, " symlink(s) for ", version_path, "\n  ",
                    "Please run 'unmark()' to remove all symlinks, then try again.\n  ",
                    paste(utils::capture.output(symlink_counts), collapse = "\n  "))
            }
         }

         return(TRUE)

      },


      #  Resolve a symlink to the full path of its target
      #
      #  A dangling symlink (target folder gone) still resolves to that target
      #  path, with a message, so callers can log and remove it. A real folder
      #  resolves to its own normalized path.
      #
      #  @param path (chr) a path to a symlink (or a real folder)
      #
      #  @return (chr) the full path of the symlink's target
      resolve_symlink = function(path){
         info <- link_resolve(path)

         if (info$is_link && info$dangling) {
            private$msg_sometimes("Dangling symlink (target folder is gone): ", path, " -> ", info$target, always_message = TRUE)
            path_resolved <- info$target
         } else if (info$is_link) {
            path_resolved <- info$target
         } else {
            path_resolved <- clean_path(path, normalize = TRUE, mustWork = FALSE)
         }

         return(path_resolved)
      },

      #  Resolve a user-supplied `version_name` to the real folder name.
      #
      #  Users naturally pass a symlink name ("latest", "best",
      #  "keep_1990_01_01") where the API asks for a version_name
      #  ("1990_01_01").  Paths are already normalized through symlinks by
      #  `clean_path()`, but the *name* was not, so logs were written under the
      #  symlink's name and `ls`-regex lookups silently found nothing.
      #
      #  - A symlink name resolves to the version_name it points to.
      #  - A real version_name, or a name that does not exist, passes through
      #    unchanged so downstream "folder does not exist" errors still fire.
      #  - With several roots, the name must resolve to the SAME version in
      #    every root; otherwise the request is ambiguous & stop.
      #
      #  @param version_name (chr) user-supplied version_name or symlink name
      #
      #  @return (chr) the resolved version_name
      resolve_version_name = function(version_name){
         assert_scalar(version_name)
         roots    <- unlist(private$DICT$ROOTS)
         resolved <- basename(clean_path(roots, version_name)) # normalizes through symlinks
         names(resolved) <- names(roots)

         if(length(unique(resolved)) != 1L){
            stop(
               "'", version_name, "' points to different versioned folders in each root and is ambiguous:\n  ",
               paste0(names(resolved), ": ", resolved, collapse = "\n  "),
               "\nPlease pass the exact version_name instead of a symlink name."
            )
         }
         resolved <- unique(resolved)

         # Still a reserved name after resolving?  Then no such symlink exists
         # (user meant e.g. "latest" but nothing is marked latest), or a real
         # folder is using the name.  Either way there is no version to act on.
         if(!is.null(find_reserved_name_conflict(resolved))){
            stop(
               "'", version_name, "' is the name of a symlink (shortcut), not a versioned folder, ",
               "and no symlink with that name currently exists in:\n  ", paste(roots, collapse = "\n  "),
               "\nPlease pass the name of the versioned folder you want (e.g. '1990_01_01'). ",
               "Use roundup() to see which folders are currently marked."
            )
         }

         if(!identical(resolved, version_name)){
            private$msg_sometimes("Resolved '", version_name, "' -> '", resolved, "'")
         }
         return(resolved)
      },

      #
      #  Make a symlink string of a certain type for a folder
      #
      #  @param version_path (chr) full path to a version folder
      #  @param symlink_type (chr) valid symlink type defined by class, e.g. 'best', 'keep', 'remove'
      #
      #  @return (chr) full path to the symlink
      make_symlink_path = function(version_path, symlink_type){
         assert_scalar(version_path)
         assert_scalar(symlink_type)
         if(!symlink_type %in% get_symlink_types()) stop("Invalid symlink_type: ", symlink_type)
         root         <- dirname(clean_path(version_path))
         version_name <- basename(clean_path(version_path))

         symlink_suffix_fn <- get_symlink_registry_entry(symlink_type)[["symlink_suffix_fn"]]
         symlink_suffix    <- symlink_suffix_fn(version_name)

         # don't normalize - we want a symlink path
         return(clean_path(root, paste0(symlink_type, symlink_suffix), normalize = FALSE))
      },

      #  Remove one tool symlink (shortcut) of one type for one version, and log it
      #
      #  Looks for the link this type would give this version (`best`,
      #  `keep_<version>`) in one root. Three outcomes:
      #  - not there: message, no log row, no error
      #  - there but dangling (its folder was deleted by hand): the link is
      #    removed and the central log alone gets a `remove_dangling_<type>`
      #    row with a tool-authored comment plus the user's. The gone folder is
      #    never re-created to hold a version log
      #  - live: the link is removed and both the version log and the central
      #    log get a `demote_<type>` row, but only if the delete succeeded
      #
      #  A dangling link whose stored target cannot be read is matched by its own
      #  name for per-version types only; a dangling `best` is never attributed to
      #  a version by guesswork.
      #
      #  Messages about removals and failures are always shown, even with
      #  verbose off, because the tool is touching something the user did not
      #  name in the call.
      #
      #  @param root (chr) path to the root folder
      #  @param version_name (chr) resolved version_name the link should point at
      #  @param symlink_type (chr) one registered type, e.g. 'best', 'keep', 'remove'
      #  @param user_entry (named list) the caller's log entry
      #
      #  @return (none) side effects on disk and in the logs as above
      remove_one_symlink = function(root, version_name, symlink_type, user_entry){
         assert_dir_exists(root)
         assert_scalar(version_name)
         assert_scalar(symlink_type)
         if(!symlink_type %in% get_symlink_types())
         stop(
            "Invalid symlink_type; got - ", symlink_type
            , " - expected - ", toString(get_symlink_types())
         )

         symlink_list <- private$find_count_symlinks(
            root         = root,
            version_name = version_name,
            symlink_type = symlink_type
         )
         symlinks <- symlink_list$symlinks

         # nothing to do is not an error
         if (length(symlinks) == 0) {
            private$msg_sometimes(
               "     No symlink found for: ", version_name, " - ", symlink_type
               , always_message = TRUE
            )
            return(invisible(NULL))
         }

         # don't normalize - we want the symlink path itself, not its target
         symlink_full <- clean_path(root, symlinks, normalize = FALSE)

         # Dangling link: the version folder is gone, so there is no version log
         # to write and nothing to re-create. Central-log row only.
         # Must prevent creation of zombie folders by the append_to_log() route.
         if (isTRUE(link_resolve(symlink_full)$dangling)) {
            private$remove_dangling_symlink(
               root                  = root
               , symlink_leaf        = symlinks
               , symlink_type        = symlink_type
               , user_entry          = user_entry
               , append_user_comment = TRUE
            )
            return(invisible(NULL))
         }

         # Live link: demote
         path_real <- private$resolve_symlink(path = symlink_full)
         private$DYNAMIC$LOG$action       <- sprintf("demote_%s", symlink_type)
         private$DYNAMIC$LOG$version_name <- basename(path_real)

         private$msg_sometimes("   removing ", symlink_full)

         # Log only if the unlink succeeds (linux-style &&: act, then record the act)
         removed <- tryCatch(
            link_remove(symlink_full)
            , error = function(e) {
               private$msg_sometimes(
                  "   unlink failed for: ", symlink_full, "\n   ", conditionMessage(e)
                  , always_message = TRUE
               )
               return(FALSE)
            }
         )
         if (isTRUE(removed)) {
            private$append_to_log(version_path = path_real, user_entry = user_entry)
            private$append_to_central_log(version_path = path_real, user_entry = user_entry)
         }

         return(invisible(NULL))
      },

      #  Remove all symlinks for a version_name
      #
      #  Loops through all symlink types and removes them with `remove_one_symlink()`
      #
      #  @param root (chr) path to the root folder
      #  @param version_name (chr) version_name of the user's folder
      #  @param user_entry (named list) user entry from the user's folder
      #
      #  @return (none) removes symlinks on disk
      remove_all_symlinks = function(root, version_name, user_entry){
         assert_scalar(version_name)
         private$msg_sometimes("Removing symlinks for: ", version_name)
         for(symlink_type in get_symlink_types()){
            private$remove_one_symlink(
               root         = root,
               version_name = version_name,
               symlink_type = symlink_type,
               user_entry   = user_entry
            )
         }
      },

      #  Which version did a dangling link belong to?
      #
      #  From the target when it can be read. Otherwise from the link's own
      #  name for per-version types (`keep_1990_01_01` -> `1990_01_01`), and
      #  "unknown" for singletons (`best` says nothing about its version).
      #  - "unknown" should be a rare failure path when a dangling windows junction
      #    fails to find the junction target.
      #
      #  @param symlink_leaf (chr) the link's own name
      #  @param symlink_type (chr) registered type
      #  @param target (chr or NA) the link's target as reported by link_resolve()
      #
      #  @return (chr) a version_name, or "unknown"
      dangling_version_name = function(symlink_leaf, symlink_type, target){
         if (!is.na(target)) return(basename(target))
         reg <- get_symlink_registry_entry(symlink_type)
         if (isTRUE(reg[["is_singleton"]])) return("unknown")
         version_name <- sub(reg[["extract_regex"]], "", symlink_leaf)
         return(version_name)
      },

      #  Remove a dangling link by its own name (the unmark() path for link names)
      #
      #  `unmark("best")` normally resolves "best" to the version it points at
      #  and strips that version of every mark. A dangling link may have no
      #  readable version (Windows, PowerShell silent), so when the name the
      #  user passed is a tool link that is dangling, it is removed by name
      #  instead. Decide first, act second: this path is taken only when every
      #  root that has the link has it dangling. Dangling in one root and live in
      #  another is ambiguous (which version did the user mean?), so nothing is
      #  touched and the normal path raises its ambiguity error.
      #
      #  @param link_name (chr) what the user passed as version_name
      #  @param user_entry (named list) the caller's log entry
      #
      #  @return (lgl) TRUE if the request was handled here; FALSE to continue
      #    with normal resolution, nothing changed
      remove_dangling_link_by_name = function(link_name, user_entry){

         # not a tool link name (e.g. a plain version_name): nothing for this path to do
         symlink_type <- get_symlink_type_from_name(link_name)
         if (is.na(symlink_type)) return(FALSE)

         roots    <- unlist(private$DICT$ROOTS)
         info     <- lapply(roots, function(root) link_resolve(clean_path(root, link_name, normalize = FALSE)))
         is_link  <- vapply(info, `[[`, logical(1), "is_link")
         dangling <- vapply(info, `[[`, logical(1), "dangling")

         # no dangling link anywhere: normal resolution handles it
         if (!any(dangling)) return(FALSE)

         # dangling in one root, live in another: ambiguous, so touch nothing and let
         # the normal path raise its ambiguity error via `resolve_version_name()`
         if (any(is_link & !dangling)) return(FALSE)

         # unambiguous dangling link in all roots - remove and log
         for (root in roots[dangling]) {
            private$remove_dangling_symlink(
               root                  = root
               , symlink_leaf        = link_name
               , symlink_type        = symlink_type
               , user_entry          = user_entry
               , append_user_comment = TRUE
            )
         }
         for (root in roots) private$report_all_logs_tool_symlink(root)
         return(TRUE)
      },

      #  Remove a dangling tool symlink (shortcut) and record it in the central log only
      #
      #  The target folder is gone, so there is no version log to append to, and
      #  the tool never re-creates a folder to hold one. The central-log row is
      #  the whole record. Its comment is machine-prefixed so a reader can tell
      #  the tool wrote it:
      #    [vmTools auto] removed dangling symlink (shortcut) <leaf> pointing to missing folder <target leaf>. <user comment>
      #  No quotes, semicolons or commas in the generated text, so the csv
      #  round-trips on any delimiter. They may exist in user comments - their repsonsibility.
      #
      #  @param root (chr) root folder
      #  @param symlink_leaf (chr) the link's own name, e.g. `best`, `keep_1990_01_01`
      #  @param symlink_type (chr) registered type
      #  @param user_entry (named list) the caller's log entry
      #  @param append_user_comment (lgl) TRUE when the user asked for this removal
      #    (`unmark()`); FALSE when it is a side effect of clearing a dangling
      #    singleton slot during a mark, where the user's comment belongs on the
      #    promote row instead & removing a dangling symlink is just housekeeping
      #
      #  @return (lgl) TRUE if the link was removed and logged
      remove_dangling_symlink = function(root, symlink_leaf, symlink_type, user_entry, append_user_comment = TRUE){
         assert_dir_exists(root)
         assert_scalar(symlink_leaf)
         assert_scalar(symlink_type)
         assert_named_list(user_entry)
         checkmate::assert_flag(append_user_comment)
         if(!symlink_type %in% get_symlink_types()) stop("Invalid symlink_type: ", symlink_type)

         path_link <- clean_path(root, symlink_leaf, normalize = FALSE)
         info      <- link_resolve(path_link)
         if (!(info$is_link && info$dangling)) {
            stop(
               "Not a dangling symlink (shortcut): ", path_link
               , "\n  - remove_dangling_symlink() only removes links whose target folder is gone. Nothing has been changed."
            )
         }

         # version_path is always the version folder's hard path, as on every
         # demote row - the action column says what happened to the link, the
         # version columns say which version it was about. The folder is gone;
         # its address is still what the row is about.
         version_name <- private$dangling_version_name(symlink_leaf, symlink_type, info$target)
         if (!is.na(info$target)) {
            version_path <- info$target
            target_leaf  <- basename(info$target)
            link_label   <- symlink_leaf
         } else if (version_name != "unknown") {
            version_path <- clean_path(root, version_name, normalize = FALSE)   # per-version link: name carries the version
            target_leaf  <- version_name
            link_label   <- symlink_leaf
         } else {
            # dangling singleton whose stored target Windows could not report: no
            # hard path to give. NA in the column; the full link path goes in
            # the comment so the row still says which root it happened in.
            version_path <- NA_character_
            target_leaf  <- "unknown"
            link_label   <- path_link
         }

         comment_auto <- sprintf(
            "[vmTools auto] removed dangling symlink (shortcut) %s pointing to missing folder %s."
            , link_label, target_leaf
         )
         user_comment <- if (append_user_comment) user_entry[["comment"]] else NULL
         entry_auto   <- user_entry
         entry_auto[["comment"]] <- if (length(user_comment) == 1L && nzchar(user_comment)) {
            paste(comment_auto, user_comment)
         } else {
            comment_auto
         }

         private$DYNAMIC$LOG$action       <- sprintf("remove_dangling_%s", symlink_type)
         private$DYNAMIC$LOG$version_name <- version_name

         private$msg_sometimes(
            "   removing dangling symlink (shortcut): ", path_link, " (target folder missing: ", target_leaf, ")"
            , always_message = TRUE
         )

         # act, then record the act
         removed <- tryCatch(
            link_remove(path_link)
            , error = function(e) {
               private$msg_sometimes(
                  "   unlink failed for: ", path_link, "\n   ", conditionMessage(e)
                  , always_message = TRUE
               )
               return(FALSE)
            }
         )
         if (isTRUE(removed)) {
            private$append_to_central_log(version_path = version_path, user_entry = entry_auto)
         }

         return(isTRUE(removed))
      },



      ## Creation / Deletion ---------------------------------------------------

      # Create a new real directory, not a symlink, to a versioned output folder, with new log
      # I want messages, not dir.create's default warning

      #  Create a new folder with a log file
      #
      #  @param version_path (chr) path to a version_name folder
      #
      #  @return (none) side effect of creating a log file on disk
      make_folder_with_log = function(version_path){

         dir_exists <- validate_dir_exists(version_path, verbose = FALSE)

         if(!dir_exists){
            dir.create(version_path, recursive = TRUE)
         } else {
            private$msg_sometimes("Directory already exists: ", version_path, always_message = TRUE)
         }

         # Always write a log - if user uses the tool to try to create a folder
         # that already exists, it's  a signal they want a log to exist
         private$write_expected_log(version_path)

      },

      #  Delete a folder that's been marked `remove_`
      #
      #  @param root (chr) path to the root folder
      #  @param version_name (chr) version_name of the user's folder
      #  @param user_entry (named list) user entry from the user's folder
      #  @param require_user_input (lgl) TRUE if the user should be prompted to confirm deletion, FALSE if deletion should be automated e.g. in a loop.
      #
      #  @return (lgl) TRUE if the folder was deleted, FALSE if it was not
      delete_remove_folder = function(root, version_name, user_entry, require_user_input){
         assert_dir_exists(root)
         assert_scalar(version_name)
         version_name_ <- version_name # data.table accommodation
         version_path <- clean_path(root, version_name)

         folder_dt <- private$query_root_folder_types(root = root)

         # Remove symlinks and base folders
         # ensure the folder is marked `remove_`
         # - the tool will not delete unmarked folders, that's the entire point of the tool
         folder_dt_removes      <- folder_dt[is_tool_symlink == TRUE & grepl(get_symlink_regex_extract()[["remove"]], dir_leaf), ]
         deletion_symlink_exact <- sprintf("remove_%s", version_name)
         deletion_dir_name      <- folder_dt[dir_leaf == deletion_symlink_exact, dir_name]

         if(!deletion_symlink_exact %in% folder_dt_removes$dir_leaf){
            private$msg_sometimes(
               "\n",
               "No valid `remove_` symlink found:\n",
               "     for: ", version_name, "\n",
               "     in root: ", root
               , always_message = TRUE
            )

            ret_val_deleted_TF <- NULL

         } else if (require_user_input == TRUE) {

            dirnames_to_delete <- folder_dt[version_name == version_name_, dir_name]

            private$msg_sometimes("") # newline for visual clarity

            user_input <- utils::menu(
               title = paste0("Do you want to delete the following folders?\n  ",
                              paste(dirnames_to_delete, collapse = "\n  "))
               , choices = c("No", "Yes")
            )

            private$msg_sometimes("") # newline for visual clarity

            # Prompt user input to confirm deletion
            if(user_input == 2){
               private$DYNAMIC$LOG$action <- "delete_remove_folder"
               private$append_to_central_log(version_path = version_path, user_entry = user_entry)
               for(dir_name in dirnames_to_delete){
                  private$msg_sometimes("Deleting ", dir_name)
                  private$delete_folder_or_link(dir_name)
               }
            }

            ret_val_deleted_TF <- user_input == 2

         } else if (require_user_input == FALSE) {

            dirnames_to_delete <- folder_dt[version_name == version_name_, dir_name]

            private$msg_sometimes("") # newline for visual clarity

            private$DYNAMIC$LOG$action <- "delete_remove_folder"
            private$append_to_central_log(version_path = version_path, user_entry = user_entry)
            for(dir_name in dirnames_to_delete){
               private$msg_sometimes("Deleting ", dir_name, always_message = TRUE)
               private$delete_folder_or_link(dir_name)
            }

            ret_val_deleted_TF <- TRUE

         } else {
            stop("Unforeseen error in `delete_remove_folder` function. Please contact the developer.")
         }

         return(ret_val_deleted_TF)

      },

      #  Delete one version folder, or one of the tool's symlinks to it
      #
      #  A symlink is removed with `link_remove()`, which deletes the link only
      #  and never follows it into the folder it points to. A real folder is
      #  removed recursively. Used only by `delete_remove_folder()`.
      #
      #  @param path (chr) a real folder or a symlink directly under a root
      #
      #  @return (none) side effect of deletion
      delete_folder_or_link = function(path){
         assert_scalar(path)
         if (path_is_link(path)) {
            link_remove(path)
         } else {
            unlink(path, recursive = TRUE)
         }
      },




      ## Logs ------------------------------------------------------------------

      #  Make an empty data.table schema from a named list
      #
      #  @param schema (named list) named list of column names and data types
      #
      #  @return (data.table) empty data.table with schema columns and types
      make_schema_dt = function(schema){
         assert_named_list(schema)
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
      # @param dt_log (data.table) log data.table
      # @param log_schema (list) e.g. private$DICT$log_schema
      #
      # @return (data.table) log data.table with schema columns in order
      repair_log_schema = function(dt_log, log_schema = private$DICT$log_schema, allow_schema_repair = private$DICT$FLAGS$allow_schema_repair){
         if(allow_schema_repair){
            assert_named_list(log_schema)
            dt_schema <- private$make_schema_dt(log_schema)
            dt_log <- data.table::rbindlist(list(dt_schema, dt_log), fill = TRUE)
         }
         return(dt_log)
      },

      # If log_version_history.csv is not in a 'logs' subfolder, send it there.
      #
      # Otherwise, do nothing
      #
      # @param version_path (chr) path to a version_name folder
      #
      # @returns (none) renames the log file, or makes a new one in 'logs' subdirectory if rename fails
      move_log_to_subdir = function(version_path){

         path_log_bare   <- clean_path(version_path, private$DICT$log_name)
         path_log_subdir <- clean_path(version_path, private$DICT$log_path)

         # if path_log_bare exists and path_log_subdir does not, move it
         # - if renaming fails, make a log in the expected subfolder
         # if path_log_bare does not exist, move on doing nothing
         # if path_log_bare exists and path_log_subdir exists, move on and do nothing
         # if neither exists, move on and do nothing

         move_log_tf <- if(
            file.exists(path_log_bare)
            && !file.exists(path_log_subdir)
         ) TRUE else FALSE

         if(move_log_tf == TRUE){
            private$msg_sometimes("     Moving log file to 'logs' subfolder:\n"
                    , "     ", path_log_bare, " -> \n"
                    , "     ", path_log_subdir)
            dir.create(dirname(path_log_subdir), recursive = TRUE, showWarnings = FALSE)
            rename_flag <- file.rename(path_log_bare, path_log_subdir)

            if(rename_flag == FALSE){
               private$msg_sometimes("     Could not move existing log file, creating a new log in 'logs' subdirectory.", always_message = TRUE)
               private$write_new_log(version_path)
            }
         }

      },

      #  Return a time-stamp in private$DICT$datestamp_format format, e.g. "2024_02_21_104202"
      #
      #  @return (chr) time-stamp in private$DICT$datestamp_format format
      make_current_timestamp = function(){
         return(format(as.POSIXct(Sys.time(), tz = "UTC"), tz = private$DICT$TZ, format = private$DICT$datestamp_format))
      },

      #  Write a log creation row IF the log is empty
      #
      #  @param dt_log (data.table) log data.table
      #  @param version_name (chr) version_name in format 'YYYY_MM_DD.VV' default private$DYNAMIC$LOG$version_name
      #
      #  @return (data.table) log data.table with creation row if it was empty
      ensure_log_creation_entry = function(version_path, dt_log, version_name = private$DYNAMIC$LOG$version_name){
         if(is.na(version_name)) stop("version_name not set - check how you've initialized the version_name, if at all")
         # This will not rewrite the first log line if only that row was deleted - that's misleading
         if(nrow(dt_log) == 0) {
            first_row <- data.table(
               log_id       = 0L,
               timestamp    = private$make_current_timestamp(),
               user         = Sys.info()[["user"]],
               version_name = version_name,
               version_path = version_path,
               action       = "create",
               comment      = "log created"

            )
            dt_log <- rbind(first_row, dt_log)
         }
         return(dt_log)
      },

      #  Write a new log file with the schema defined in `private$DICT$log_schema`
      #
      #  Write a 'create' row if there are 0 rows in the log.
      #
      #  @param version_path (chr) path to a version_name folder
      #  @param log_schema (named list) named list of column names and data types
      #
      #  @return none
      write_new_log = function(version_path, log_schema = private$DICT$log_schema){
         fpath <- clean_path(version_path, private$DICT$log_path)
         dt_log <- private$make_schema_dt(log_schema)
         # Safely write first 'create' row if it doesn't exist
         dt_log <- private$ensure_log_creation_entry(version_path, dt_log)
         private$csv_writer(dt_log, fpath, row.names = FALSE)
      },

      #  Safely read a log, correcting a null log (all dim == 0), if found
      #
      #  If log is null (all dim == 0), rebuild schema and write a new 'create' row.
      #
      #  Helper for `read_log()` for safety in read-modify-write cycles; not
      #  really necessary for the `query_x` family of functions that are _only_
      #  readers.
      #
      #  @param dt_log (data.table) log data.table
      #
      #  @return (data.table) log data.table
      #  Read a log file from disk, refusing to mistake a damaged file for an
      #  empty one.
      #
      #  A genuinely empty (zero-byte) file is returned as a 0x0 data.table so
      #  the callers' existing "null log" repair can rebuild it.  A file that
      #  HAS bytes but cannot be read as a table (hand-edited, corrupted, or
      #  being simultaneously written by another process (race condition)) is an
      #  error: rebuilding it from scratch would silently erase its history.
      #
      #  @param fpath (chr) path to the log file
      #  @param log_schema (named list) named list of column names and data types
      #
      #  @return (data.table) raw log contents (0x0 if the file is zero bytes)
      read_log_file = function(fpath, log_schema = private$DICT$log_schema){
         assert_scalar(fpath)
         if(!file.exists(fpath)){
            stop("Could not read the log file - it does not exist: ", fpath)
         }
         if(file.size(fpath) == 0){
            return(data.table::data.table())
         }

         dt_log <- tryCatch(
            private$csv_reader(fpath, colClasses = unlist(log_schema))
            , error = function(e) e
         )
         read_failed <- inherits(dt_log, "error") || all(dim(dt_log) == 0)

         if(read_failed){
            stop(
               "Refusing to rebuild the log file: ", fpath,
               "\nIt is not empty (", file.size(fpath), " bytes) but could not be read as a table.",
               "\nThis usually means the file was hand-edited, corrupted, or was being written ",
               "by another process at the moment it was read (race condition).",
               "\nNothing has been written, to avoid destroying the log's history.",
               "\nPlease inspect the file. If it is truly unrecoverable, delete it and it ",
               "will be recreated on the next write.",
               if(inherits(dt_log, "error")) paste0("\nReader error: ", conditionMessage(dt_log)) else ""
            )
         }
         return(dt_log)
      },

      #  Rebuild a log that read as 0x0 (zero-byte file), adding a 'create' row.
      #
      #  `read_log_file()` guarantees we only get here for a genuinely empty
      #  file, never for a damaged one.
      #
      #  @param version_path (chr) path to a version_name folder
      #  @param dt_log (data.table) log as read from disk
      #
      #  @return (data.table) log data.table, rebuilt if it was null
      correct_null_log = function(version_path, dt_log){
         if(all(dim(dt_log) == 0)){
            private$msg_sometimes("Empty (zero-byte) log found - rebuilding schema and writing a new 'create' row.", always_message = TRUE)
            dt_log <- private$make_schema_dt(private$DICT$log_schema)
            dt_log <- private$ensure_log_creation_entry(version_path, dt_log)
         }
         return(dt_log)
      },

      #  Reads log if it exists, makes a blank one if it doesn't.
      #
      #  Ensure correct schema.
      #
      #  @param fpath (chr) path to the log file
      #  @param log_schema (named list) named list of column names and data types
      #
      #  @return (data.table) log data.table
      read_log = function(fpath, log_schema = private$DICT$log_schema){

         dt_log <- private$read_log_file(fpath, log_schema)

         # safely correct a null log, if found (all dim == 0, i.e. zero-byte file)
         dt_log <- private$correct_null_log(dirname(fpath), dt_log)
         dt_log <- private$repair_log_schema(dt_log, log_schema = log_schema)
         private$assert_data_schema(dt_log, data_types = log_schema)
         return(dt_log)
      },

      #  Safely write an expected log with creation entry if it doesn't exist.
      #
      #  Ensure log is in proper subfolder
      #
      #  @param version_path (chr) path to a version_name folder
      #  @param log_schema (named list) named list of column names and data types
      #
      #  @return none
      write_expected_log = function(version_path, log_schema = private$DICT$log_schema){
         assert_scalar(version_path)
         private$move_log_to_subdir(version_path)
         fpath <- clean_path(version_path, private$DICT$log_path)
         log_dir <- dirname(fpath)
         if(!file.exists(log_dir)) dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
         if(!file.exists(fpath)) {
            private$write_new_log(version_path)
         } else {
            dt_log <- private$read_log(fpath, log_schema)
            # Safely write first 'create' row if it doesn't exist
            dt_log <- private$ensure_log_creation_entry(version_path, dt_log)
            private$csv_writer(dt_log, fpath, row.names = FALSE)
         }
      },

      #  Append a one-row entry to a log and write to disk
      #
      #  @param version_path (chr) path to a version_name folder
      #  @param user_entry (named list) named list of user entry values
      #
      #  @return none
      append_to_log = function(version_path, user_entry) {

         # validate inputs
         assert_named_list(user_entry)
         assert_scalar(version_path)

         # needs to read a log to bump the log_id number
         fpath <- clean_path(version_path, private$DICT$log_path)
         private$write_expected_log(version_path)
         dt_log <- private$read_log(fpath)
         private$assert_schema_vs_user_entry(user_entry)

         # Safely write first 'create' row if it doesn't exist
         dt_log <- private$ensure_log_creation_entry(version_path, dt_log)

         last_row <- utils::tail(dt_log, 1)

         log_entry <- data.table::data.table(
            log_id       = last_row$log_id + 1,
            timestamp    = private$make_current_timestamp(),
            user         = Sys.info()[["user"]],
            version_name = private$DYNAMIC$LOG$version_name,
            version_path = clean_path(version_path),
            action       = private$DYNAMIC$LOG$action
         )

         for(varname in names(user_entry)){
            log_entry[[varname]] <- user_entry[[varname]]
         }

         dt_log <- rbindlist(list(dt_log, log_entry), fill = private$DICT$FLAGS$allow_schema_repair)
         data.table::setcolorder(dt_log, names(private$DICT$log_schema))

         private$msg_sometimes("  Writing log to ", fpath)
         private$csv_writer(dt_log, fpath, row.names = FALSE)
      },


      ## Central Log ----------------------------------------------------------

      #  Safely write an expected central log with creation entry if it doesn't exist.
      #
      #  This differs from a version_name log since the first entry DOES NOT
      #  have a version_name by definition - we'll define it as "CENTRAL_LOG" for
      #  clarity, and write an append process to match
      #
      #  @param fpath (chr) path to the log file
      #  @param log_schema (named list) named list of column names and data types
      #
      #  @return none
      write_expected_central_log = function(fpath, log_schema = private$DICT$log_schema){
         assert_scalar(fpath)
         if(!file.exists(fpath)) {
            private$write_new_central_log(fpath, log_schema)
         } else {
            dt_log <- private$read_central_log(fpath, log_schema)
            private$csv_writer(dt_log, fpath, row.names = FALSE)
         }
      },

      #  Reads central log if it exists, makes a blank one if it doesn't.
      #
      #  Ensure correct schema.
      #
      #  @param fpath (chr) path to the log file
      #  @param log_schema (named list) named list of column names and data types
      #
      #  @return (data.table) log data.table
      read_central_log = function(fpath, log_schema = private$DICT$log_schema){

         dt_log <- private$read_log_file(fpath, log_schema)

         # safely correct a null log, if found (all dim == 0, i.e. zero-byte file)
         if(all(dim(dt_log) == 0)){
            private$msg_sometimes("Empty (zero-byte) central log found - rebuilding schema and writing a new 'create' row.", always_message = TRUE)
            dt_log <- private$make_schema_dt(private$DICT$log_schema)
            dt_log <- private$make_central_log_creation_entry(dt_log)
         }

         dt_log <- private$repair_log_schema(dt_log, log_schema = log_schema)
         private$assert_data_schema(dt_log, data_types = log_schema)
         return(dt_log)
      },

      #  Write a new central log with a 'create' row.
      #
      #  @param fpath (chr) path to the central log file
      #  @param log_schema (named list) named list of column names and data types
      #
      #  @return none
      write_new_central_log = function(fpath, log_schema = private$DICT$log_schema){
         dt_log <- private$make_schema_dt(log_schema)
         # Safely write first 'create' row if it doesn't exist
         dt_log <- private$make_central_log_creation_entry(dt_log)
         private$csv_writer(dt_log, fpath, row.names = FALSE)
      },

      #  Ensure a 'create' row exists in the central log.
      #
      #  Write a 'create' row if there are 0 rows in the log.
      #
      #  @param dt_log (data.table) log data.table
      #
      #  @return (data.table) log data.table with creation row
      make_central_log_creation_entry = function(dt_log){
         # This will not rewrite the first log line if only that row was deleted - that's misleading
         if(nrow(dt_log) == 0) {
            first_row <- data.table(
               log_id       = 0L,
               timestamp    = private$make_current_timestamp(),
               user         = Sys.info()[["user"]],
               version_name = "CENTRAL_LOG",
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
      #  @param user_entry (named list) named list of user entry values
      #
      #  @return none
      append_to_central_log = function(version_path, user_entry) {

         # validate inputs
         assert_named_list(user_entry)
         private$assert_schema_vs_user_entry(user_entry)

         # needs to read a log to bump the log_id number
         fpath    <- private$DICT$LOG_CENTRAL$path
         private$write_expected_central_log(fpath, log_schema = private$DICT$log_schema)
         dt_log   <- private$read_log(fpath)

         # Safely write first 'create' row if it doesn't exist
         dt_log <- private$make_central_log_creation_entry(dt_log)

         if(!is.na(private$DYNAMIC$LOG$action)){

            last_row <- utils::tail(dt_log, 1)

            log_entry <- data.table::data.table(
               log_id       = last_row$log_id + 1,
               timestamp    = private$make_current_timestamp(),
               user         = Sys.info()[["user"]],
               version_name = private$DYNAMIC$LOG$version_name,
               version_path = version_path,
               action       = private$DYNAMIC$LOG$action
            )

            for(varname in names(user_entry)){
               log_entry[[varname]] <- user_entry[[varname]]
            }

            dt_log <- rbindlist(list(dt_log, log_entry), fill = private$DICT$FLAGS$allow_schema_repair)
            data.table::setcolorder(dt_log, names(private$DICT$log_schema))

            private$msg_sometimes("  Writing central log to ", fpath)
            private$csv_writer(dt_log, fpath, row.names = FALSE)

         } else {

            private$msg_sometimes("  No action defined, not writing to central log.\n",
                    "    This is expected if no symlinks were found or user-input did not produce an action for: ", private$DYNAMIC$LOG$version_name,
                    always_message = TRUE)
         }

      },


      ## Queries ---------------------------------------------------------------

      # each query_all_logs_* function should have a corresponding report

      # The 'query' function family is for reporting on the state of the symlinks.
      # It differs from the 'read' family, which is for promotion/demotion, and will create a new log

      #  Query one version_name folder for a log
      #
      #  `tryCatch()` to find a folder's log, read it if it exists, in a consistent format
      #
      #  @param version_path (chr) path to a version_name folder
      #  @param verbose (lgl) print message if no log found
      #
      #  @return (data.table) log data.table if found, else NULL
      try_query_log = function(version_path, verbose = TRUE){
         tryCatch(
            {
               log <- private$csv_reader(
                  clean_path(version_path, private$DICT$log_path)
                  , colClasses = unlist(private$DICT$log_schema)
               )
            },
            error = function(e) if(verbose == TRUE) private$msg_sometimes("No readable log found for folder: ", version_path, always_message = TRUE)
         )

      },

      #  Query the first row of all logs in a list
      #
      #  Does not rely on the log entry index, uses `head()`
      #
      #  @param log_list (list) list of logs, each log is a data.table
      #
      #  @return (data.table) a data.table with the first row of each log
      query_logs_first_row = function(log_list){
         dt_query <- data.table::rbindlist(lapply(log_list, function(x) utils::head(x, 1)), fill = TRUE)
         setorderv(dt_query, private$DICT$log_sort_varname)
         return(dt_query)
      },

      #  Return the last row from each log.
      #
      #  Does not rely on the log entry index, uses `tail()`
      #  - sort according to some variable
      #
      #  @param log_list (list) list of logs, each log is a data.table
      #
      #  @return (data.table) a data.table with the last row of each log
      query_logs_last_row = function(log_list){
         dt_query <- data.table::rbindlist(lapply(log_list, function(x) utils::tail(x, 1)), fill = TRUE)
         setorderv(dt_query, private$DICT$log_sort_varname)
         return(dt_query)
      },

      #  Query the row with log_id == 0 from all logs in a list
      #
      #  This should be the creation line for each log.
      #
      #  @param log_list (list) list of logs, each log is a data.table
      #
      #  @return (data.table) a data.table with the row with log_id == 0 of each log
      query_log_id_0 = function(log_list){
         dt_query <- data.table::rbindlist(lapply(log_list, function(x) x[log_id == 0, ]), fill = TRUE)
         setorderv(dt_query, private$DICT$log_sort_varname)
         return(dt_query)

      },

      #  FUNDAMENTAL QUERY METHOD FOR THE SYMLINK TOOL.
      #
      #  Query the folder types (bare of symlink) of all version_name folders in one `root`.
      #  - Moves all logs to subfolders for a proper query
      #
      #  @param root (chr) path to a root folder defined at instantiation (`STL$new()`)
      #
      #  @return (data.table) table of all version_name folders and their folder types (e.g. bare folder or symlink)
      query_root_folder_types = function(root){
         # Find all first-level folders and symlinks in the root (non-recursive)
         # - dangling symlinks (target folder gone) are left out, as `list.dirs()`
         #   did before `link_list()` - see Follow-ups in the plan
         # - the tool's own transient probe entries are left out
         links_dt <- link_list(root)
         links_dt <- links_dt[dangling == FALSE & !startsWith(leaf, ".vmtools_probe_"), ]

         # resolved path: a symlink's target, or a real folder's own path
         dir_name_resolved <- links_dt$target
         non_link_mask <- !links_dt$is_link
         dir_name_resolved[non_link_mask] <- clean_path(links_dt$path[non_link_mask], normalize = TRUE)

         # table of all folders, resolved paths, and last part of the path
         folder_dt <- data.table::data.table(
            dir_name            = links_dt$path
            , dir_name_resolved = dir_name_resolved
            , dir_leaf          = links_dt$leaf
         )
         # also add on the resolved leaf version_name for later queries and roundups
         # - keep name distinct to avoid data.table conflicts with function args
         folder_dt[, version_name := basename(dir_name_resolved)]
         # define symlinks that could and couldn't have been created by this tool for later use
         tool_symlink_regex  <- get_symlink_regex_extract()
         tool_symlink_regex  <- paste(tool_symlink_regex, collapse = "|")
         folder_dt[, is_symlink := links_dt$is_link]
         # a tool symlink must actually BE a symlink - a real folder named e.g.
         # `best` is a collision, not a mark, and is flagged separately
         folder_dt[, is_tool_symlink      := is_symlink & grepl(tool_symlink_regex, dir_leaf)]
         # - ignore case: on Windows and macOS a real folder `Best` sits in the
         #   `best` link's slot, so it is a collision there and worth reporting everywhere
         folder_dt[, is_reserved_name_dir := !is_symlink & grepl(tool_symlink_regex, dir_leaf, ignore.case = TRUE)]

         # reporting requires logs to be in a consistent location
         has_bare_log <- file.exists(clean_path(folder_dt$dir_name_resolved, basename(private$DICT$log_name)))
         catch        <- lapply(folder_dt[has_bare_log, dir_name_resolved], private$move_log_to_subdir)

         return(folder_dt)
      },


      #  Safely remove null logs, and account for zero-length logs if none are found for a version_name folder
      #
      #  @param log_list (list) list of log data.tables
      #
      #  @return (list) list of non-NULL log data.tables
      filter_null_logs_safely = function(log_list){
         if(length(log_list) == 0 || all(is.null(unlist(log_list)))){
            return(list(no_logs_found = private$make_schema_dt(schema = private$DICT$log_schema)))
         } else {
            return(log_list[!unlist(lapply(log_list, is.null))])
         }
      },

      #  Query all logs in a single `root`
      #
      #  @param root (chr) path to a root folder defined at instantiation (`STL$new()`)
      #
      #  @return (list) list of all non-NULL logs in the root, with the folder path as the list name
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
      #  @param root (chr) path to a root folder defined at instantiation (`STL$new()`)
      #
      #  @return (list) list of all non-NULL logs in the root, with the folder path as the list name
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
      #  E.g. finds symlink patterns defined by `get_symlink_types()`, this will find it.
      #
      #  @param root (chr) path to a root folder defined at instantiation (`STL$new()`)
      #  @param verbose (lgl) std_err message if no log found
      #
      #  @return (list) list of all non-NULL logs in the root, with the folder path as the list name
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
      #  @param root (chr) path to a root folder defined at instantiation (`STL$new()`)
      #
      #  @return (list) list of all non-NULL logs in the root, with the folder path as the list name
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
      #  @param log_list (list) list of logs, each log is a data.table
      #
      #  @return (data.table) a data.table with the row with the highest log_id of each log
      query_log_id_max = function(log_list){
         # find the row with highest non-missing log_id of each log
         return(
            data.table::rbindlist(lapply(log_list, function(x) x[max(x$log_id, na.rm = TRUE)]), fill = TRUE)
         )
      },


      query_all_symlink_type = function(root, symlink_type) {
         checkmate::assert_choice(symlink_type, names(get_symlink_registry()))
         symlink_reg <- get_symlink_registry_entry(symlink_type)
         folder_dt   <- private$query_root_folder_types(root)
         result_dt   <- unique(folder_dt[
            is_symlink == TRUE & grepl(symlink_reg$extract_regex, dir_leaf),
            .(version_name, dir_name, dir_name_resolved)
         ])
         return(result_dt)
      },


      query_all_unmarked = function(root){
         # find all folders and their types
         folder_dt     <- private$query_root_folder_types(root)
         marked_dirs   <- folder_dt[is_symlink == TRUE, dir_name_resolved]
         unmarked_dirs <- folder_dt[!dir_name_resolved %in% marked_dirs, dir_name_resolved]
         return(folder_dt[dir_name_resolved %in% unmarked_dirs, .(version_name, dir_name, dir_name_resolved)])
      },

      #  Query all logs in one root by a date selector
      #
      #  This relies on version_name folders being formatted a certain way.  This
      #  formatting is enforced `assert_user_date_class_and_format()` and
      #  `assert_date_selector()`.
      #
      #  Returns version_name folders less than, greater than, and/or equal to a date.
      #
      #  @param root (chr) path to a root folder defined at instantiation
      #    (`STL$new()`)
      #  @param user_date_parsed (chr) a date in a format allowed by
      #    `assert_user_date_class_and_format()`
      #  @param date_selector (chr) a selector allowed by `assert_date_selector()`
      #
      #  @return (data.table) a data.table with the first row of each log
      query_by_date = function(root, user_date_parsed, date_selector){
         assert_dir_exists(root)
         private$assert_date_selector(date_selector)

         # query all logs for their creation lines
         folder_dt   <- private$query_root_folder_types(root)
         log_list    <- private$query_all_logs(root)
         log_id_0_dt <- private$query_log_id_0(log_list)

         # parse log creation dates, handle some messaging in case they're malformed
         ts_raw          <- log_id_0_dt$timestamp
         names(ts_raw)   <- ts_raw
         suppressWarnings(
            ts_parsed    <- as.POSIXct(log_id_0_dt$timestamp, format = private$DICT$datestamp_format, tz = private$DICT$TZ)
         )
         idx_failed_parse <- which(is.na(ts_parsed))
         dt_failed_parse  <- data.table::data.table(
            dir_name_resolved      = folder_dt[idx_failed_parse]$dir_name_resolved,
            timestamp_failed_parse = ts_raw[idx_failed_parse]
         )
         tryCatch({
            as.POSIXct(log_id_0_dt$timestamp2, format = private$DICT$datestamp_format, tz = private$DICT$TZ)
         }, warning = function(w) message("Some logs failed creation-date parsing (must be in %Y_%m_%d_%H%M%S format): \n  ",
                                          paste(utils::capture.output(dt_failed_parse), collapse = "\n"),
                                          "\n")
         )

         # Strip time from date-time-stamp, retain timezone info, convert to
         # character, convert back to date for comparison against user_date

         suppressWarnings(log_id_0_dt[, timestamp_parsed := as.POSIXct(timestamp, format = private$DICT$datestamp_format, tz = private$DICT$TZ)])
         log_id_0_dt[, timestamp_parsed := format(timestamp_parsed, "%Y-%m-%d")]
         log_id_0_dt[, timestamp_parsed := as.POSIXct(timestamp_parsed, format = "%Y-%m-%d", tz = private$DICT$TZ)]

         # filter logs to all time-stamps less/greater than (or equal to) user date for later filtering
         logs_by_date <- switch_strict(
            date_selector
            , "lt"  = log_id_0_dt[timestamp_parsed <  user_date_parsed, ]
            , "lte" = log_id_0_dt[timestamp_parsed <= user_date_parsed, ]
            , "gt"  = log_id_0_dt[timestamp_parsed >  user_date_parsed, ]
            , "gte" = log_id_0_dt[timestamp_parsed >= user_date_parsed, ]
            , "e"   = log_id_0_dt[timestamp_parsed == user_date_parsed, ]
         )

         # Match version_name to folder_dt and return required columns
         dv_by_date <- logs_by_date$version_name
         return(folder_dt[version_name %in% dv_by_date, .(version_name, dir_name, dir_name_resolved)])

      },

      #  Combine roundup query results from all roots into a single data.table
      #
      #  Apply a query function to each root in
      #  `private$DICT$ROOTS`, adds a `root_name` column to each result, and
      #  combines all results into a single data.table ordered by root_name and
      #  version_name. Will fail if query results have inconsistent schemas
      #  across roots.
      #
      #  @param query_function (function) A private query function to apply to
      #    each root (e.g., `private$query_all_best_symlinks`)
      #  @param ... Additional arguments to pass to the query_function
      #
      #  @return (data.table) A single data.table with results from all roots,
      #    with `root_name` as the first column, ordered by root_name then
      #    version_name. Returns an empty data.table with proper schema if no
      #    results found.
      combine_roundup_results = function(query_function, ...){
         # Get named list of roots
         roots      <- private$DICT$ROOTS
         root_names <- names(roots)

         # Apply query function to each root, preserving names
         results_list <- lapply(root_names, function(root_name) {
            root <- roots[[root_name]]
            dt   <- query_function(root, ...)

            # Add root_name column even if dt is empty
            if (nrow(dt) == 0) {
               dt[, root_name := character(0)]
            } else {
               dt[, root_name := root_name]
            }

            return(dt)
         })

         # Combine all results - will fail if schemas differ
         combined_dt <- data.table::rbindlist(results_list)
         assert_x_in_y(c("root_name", "version_name"), names(combined_dt))

         # Reorder columns to put root_name first
         data.table::setcolorder(
               x          = combined_dt
               , neworder = "root_name"
               , before   = 1
         )

         # Order by root_name, then version_name
         if (nrow(combined_dt) > 0) {
            data.table::setorderv(combined_dt, c("root_name", "version_name"))
         }

         return(combined_dt)
      },


      ## Reports ---------------------------------------------------------------

      # Write a report, with special handling
      #
      # @param dt_report (data.table) the report
      # @param write_path (chr) full path with ext
      # @param order_cols_tf (lgl) (default = TRUE) order columns by log_schema?
      # @param schema (named_list) named list of column names and data types - used if `order_cols_tf` = TRUE
      #
      # @return (none) write to disk
      write_report = function(dt_report, write_path, order_cols_tf = TRUE, schema = private$DICT$log_schema){

         if(order_cols_tf){
            assert_named_list(schema)
            sortable_colnames <- intersect(names(schema), names(dt_report))
            data.table::setcolorder(dt_report, sortable_colnames)
         }

         private$csv_writer(dt_report, write_path, row.names = FALSE)
      },

      # each report function must have a corresponding query_all_logs_* function

      #  Report last row of all logs in one root
      #
      #  Writes a file to disk in `root`
      #
      #  @param root (chr) path to a root folder defined at instantiation
      #
      #  @return none
      report_all_logs = function(root){
         # query logs for active symlinks of any type
         log_list <- private$query_all_logs(root)
         last_row_dt <- private$query_logs_last_row(log_list)
         private$write_report(last_row_dt, clean_path(root, private$DICT$report_fnames$all_logs))
      },

      #  Report last row of all symlinked folder logs in one root
      #
      #  Writes a file to disk in `root`
      #
      #  @param root (chr) path to a root folder defined at instantiation
      #
      #  @return none
      report_all_logs_symlink = function(root){
         # query logs for active symlinks of any type
         log_list <- private$query_all_logs_symlink(root)
         last_row_dt <- private$query_logs_last_row(log_list)
         private$write_report(last_row_dt, clean_path(root, private$DICT$report_fnames$all_logs_symlink))
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
      #  @param root (chr) path to a root folder defined at instantiation
      #
      #  @return none
      report_all_logs_tool_symlink = function(root){
         # query logs for active symlinks of any type
         log_list <- private$query_all_logs_tool_symlink(root, verbose = FALSE)
         last_row_dt <- private$query_logs_last_row(log_list)
         private$write_report(last_row_dt, clean_path(root, private$DICT$report_fnames$all_logs_tool_symlink))
         private$report_discrepancies(root = root, verbose = private$DICT$verbose)
      },

      #  Report last row of all non-symlinked folder logs in one root
      #
      #  Writes a file to disk in `root`
      #
      #  @param root (chr) path to a root folder defined at instantiation
      #
      #  @return none
      report_all_logs_non_symlink = function(root){
         # query logs for active symlinks of any type
         log_list <- private$query_all_logs_non_symlink(root)
         last_row_dt <- private$query_logs_last_row(log_list)
         private$write_report(last_row_dt, clean_path(root, private$DICT$report_fnames$all_logs_non_symlink))
      },

      #  Add a discrepancy column to a data.table if it has rows
      #
      #  @param report_dt (data.table) a data.table of discrepancies found with some log
      #  @param discrepancy_reason (chr) a string describing the discrepancy
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
      #  @param path_dt (data.table) a data.table of paths
      #  @param discrepancy_reason (chr) a string describing the discrepancy
      #
      #  @return an empty data.table with a new column `discrepancy` added
      make_report_schema_for_discrepant_paths = function(path_dt, discrepancy_reason, log_schema = private$DICT$log_schema){
         # input validation
         if(!data.table::is.data.table(path_dt)) stop("path_dt must be a data.table")
         if(!is.character(discrepancy_reason))   stop("discrepancy_reason must be a character string")
         varname_dir <- grep("^dir_name", names(path_dt), value = TRUE)
         if(length(varname_dir) != 1)            stop("path_dt must have exactly one column starting with 'dir_name'")

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
      # @param log (data.table) Log data.table queried from disk
      # @param log_schema (named list) Log schema
      #
      # @return (data.table) Log entries with new columns: vars_missing, vars_extra
      report_log_vs_schema_diffs = function(log, log_schema = private$DICT$log_schema){
         # check that log is a named list
         assert_named_list(log_schema)

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
      #  @param root (chr) path to a root folder defined at instantiation
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
            file.exists(clean_path(path, private$DICT$log_path))
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

         # Real folders using a name reserved for a tool symlink, e.g. a directory literally called `best` (dir_name only)
         dirs_reserved_name_dt       <- folder_dt[is_reserved_name_dir == TRUE, .N, by = dir_name]
         discrepant_dt_reserved_name <- private$make_report_schema_for_discrepant_paths(dirs_reserved_name_dt, "real folder is using a name reserved for a tool symlink (shortcut)")

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
         log_list_invalid_actions      <- lapply(log_list_all, function(x) x[!(action %in% get_valid_actions())])
         discrepant_dt_invalid_actions <- data.table::rbindlist(log_list_invalid_actions, fill = TRUE)
         discrepant_dt_invalid_actions <- private$add_discrepancy_to_dt(discrepant_dt_invalid_actions, "invalid actions")

         # Combine all discrepancies into one report
         discrepancy_report_dt <- rbindlist(
            list(
               discrepant_dt_tool_symlink_no_logs = discrepant_dt_tool_symlink_no_logs
               , discrepant_dt_non_tool_symlinks  = discrepant_dt_non_tool_symlinks
               , discrepant_dt_reserved_name      = discrepant_dt_reserved_name
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
         path_discrepancy_report <- clean_path(root, private$DICT$report_fnames$discrepancies)

         if(nrow(discrepancy_report_dt) == 0) {
            # If there's nothing to report, delete the discrepancy report, otherwise stay quiet
            if(file.exists(path_discrepancy_report)){
               if(verbose == TRUE) message("  No discrepancies found in ", root, ", removing ", private$DICT$report_fnames$discrepancies, " (if it exists now)")
               file.remove(path_discrepancy_report)
            }
         } else {
            if(verbose == TRUE) message("  DISCREPANCIES FOUND: Writing discrepancy report to ", path_discrepancy_report)
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
      #  - This invokes the Symlink Tool's `state machine`, only allowing one symlink type per version_name folder.
      #  - resets DYNAMIC private fields
      #  - updates DYNAMIC private fields
      #
      #  @param version_name (chr) version_name folder to mark
      #  @param user_entry (named list) User-supplied log entry
      #  @param roots (named list) All user-defined `root`s defined at instantiation
      #
      #  @return (none) Machine-state updates only
      #
      #
      #
      handler_pre_mark = function(version_name, user_entry, roots = private$DICT$ROOTS){
         # validate inputs
         assert_scalar(version_name)
         assert_named_list(user_entry)
         private$assert_schema_vs_user_entry(user_entry)
         for(user_entry_item in user_entry) assert_scalar_not_empty(user_entry_item)

         # This is the 'state machine' of the Symlink Tool
         # enforce one symlink per version_name
         # we do this to ensure users haven't hand-made symlinks
         # of the same form we expect to use with this tool
         for(root in roots){
            private$assert_n_symlinks(root         = root,
                                      version_name = version_name,
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
         private$handler_update_dynamic_fields(version_name = version_name)
      },

      #  Handle post-mark operation validations and updates
      #
      #  **NOTE:** DYNAMIC fields are _not_ updated in this function - that is done by `handler_pre_mark()`.
      #  - This means the state of the machine is viewable by the user after every mark operation
      #  - DYNAMIC fields only reset when a new mark operation is underway
      #
      #  Mostly all `mark`, `create` and `delete` public functions should invoke this
      #  - This invokes the Symlink Tool's `state machine`, only allowing one symlink type per version_name folder.
      #  - ensure the tool_symlink report is up-to-date
      #  - handle central log updates
      #
      #  @param version_name (chr) version_name folder to mark
      #  @param user_entry (named list) User-supplied log entry
      #
      #  @return (none) no machine-state updates, only write to disk
      handler_post_mark = function(version_name, user_entry){
         # validate inputs
         assert_scalar(version_name)

         # enforce one symlink per version_name
         # make sure we haven't screwed up if we update this tool
         for(root in private$DICT$ROOTS){
            private$assert_n_symlinks(
               root         = root,
               version_name = version_name,
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
      #  @param version_name (chr) version_name folder to mark
      #
      #  @return (none) Updates `private$DYNAMIC$VERS_PATHS`
      handler_update_version_paths = function(version_name){
         assert_scalar(version_name)
         private$DYNAMIC$VERS_PATHS <- lapply(private$DICT$ROOTS, function(root) clean_path(root, version_name))
         assert_named_list(private$DYNAMIC$VERS_PATHS)
         if(!length(private$DYNAMIC$VERS_PATHS)) stop("No version paths found")
         lapply(private$DYNAMIC$VERS_PATHS, validate_dir_exists, verbose = FALSE)
      },

      #  Update all `private$DYNAMIC` fields
      #
      #  update all dynamic fields except log action
      #
      #  @param version_name (chr) version_name folder
      #
      #  @return (none) Updates `private$DYNAMIC$LOG` and `private$DYNAMIC$VERS_PATHS`
      handler_update_dynamic_fields = function(version_name){
         assert_scalar(version_name)
         # Update dictionaries
         private$DYNAMIC$LOG$version_name <- version_name
         private$handler_update_version_paths(version_name = version_name)
      },

      #  Set dynamic fields to NA/blank to prepare for a new `mark_` operation
      #
      #  @param field_types (chr) Field types to reset e.g. 'log', 'vers_paths'
      #
      #  @return (none) Resets `private$DYNAMIC$LOG` and `private$DYNAMIC$VERS_PATHS`
      handler_reset_dynamic_fields = function(field_types){

         # validate inputs
         valid_field_types <- c("log", "vers_paths")
         if(!is.character(field_types)) stop("field_types must be character")
         field_types <- tolower(field_types)
         if(!all(field_types %in% valid_field_types)) stop("field_types must be one of: ", toString(valid_field_types))

         if("log" %in% field_types){
            private$DYNAMIC$LOG <- list(
               version_name = NA_character_,
               action       = NA_character_
            )
         }

         if("vers_paths" %in% field_types){
            private$DYNAMIC$VERS_PATHS = list()
         }
      },


      ## Promote / Demote ------------------------------------------------------

      # 'promote' and 'demote' are consistent terms 'elevate this folder to
      # xxx status' or 'lower this folder from xxx status'
      # - xxx status is defined by `get_symlink_types()`

      #  Demote a version_name folder from a symlink status.
      #
      #  Unlinks the symlink to a version_name folder.
      #
      #  @param version_path (chr) Path to a version_name folder
      #  @param version_name (chr) version_name folder to demote
      #  @param user_entry (named list) User-supplied log entry
      #  @param symlink_types (chr) `get_symlink_types()`
      #
      #  @return (none) Unlinks the symlink on disk
      demote_existing_symlinks = function(
         version_path
         , version_name
         , user_entry
         , symlink_types = get_symlink_types()
      ){
         assert_scalar(version_path)
         assert_scalar(version_name)
         assert_named_list(user_entry)
         # find existing symlinks for a version_name
         names(symlink_types) <- symlink_types # names for `lapply()`

         root <- dirname(version_path)

         symlink_list <- lapply(symlink_types, function(x){
            private$find_count_symlinks(root = root, version_name = version_name, symlink_type = x)
         })

         symlink      <- unlist(lapply_depth(symlink_list, 1, `[[`, "symlinks"))
         symlink_type <- names(symlink)
         symlink      <- unname(symlink)

         if(length(symlink) > 1) {
            stop("More than one symlink found for ", version_name, " in: ", root, "\n  ",
                 paste(symlink, collapse = "\n  "),
                 "Please select one ", paste(symlink_types, collapse = "/"),  " symlink and delete all others manually.")
         }

         if(length(symlink) > 0) {
            private$msg_sometimes("Demoting existing symlink: ", symlink)
            # don't normalize - we want the symlink path itself, not its target
            symlink                    <- clean_path(root, symlink, normalize = FALSE)
            private$DYNAMIC$LOG$action <- sprintf("demote_%s", symlink_type)
            private$append_to_log(version_path = version_path, user_entry = user_entry)
            private$append_to_central_log(version_path = version_path, user_entry = user_entry)

            # only unlink if logging is successful
            link_remove(symlink)
         } else {
            private$msg_sometimes("No existing symlinks found - moving on")
         }


      },


      #  Check a chosen 'mark' slot before anything is changed
      #
      #  `mark()` should demote the version's other links first (e.g. 'best'),
      #  then promote. If the slot the new link needs is blocked, the promote
      #  fails after the demote has already run and the version is left with no
      #  mark at all. So the slot is checked up front:
      #  - a dangling link in the slot is removed and logged (auto comment only; the
      #    user's comment goes on the promote row that follows)
      #  - a real folder in the slot stops the mark before anything changes
      #    (e.g. the user created a real folder called 'best')
      #  - a live link is left alone for the normal mark path to handle
      #
      #  @param version_path (chr) version folder being marked
      #  @param symlink_type (chr) registered type
      #  @param user_entry (named list) the caller's log entry
      #
      #  @return (NULL) may remove a dangling link and write its central-log row
      validate_symlink_slot = function(version_path, symlink_type, user_entry){
         assert_scalar(version_path)
         path_symlink <- private$make_symlink_path(version_path, symlink_type)
         info         <- link_resolve(path_symlink)

         if (info$is_link && info$dangling) {
            private$remove_dangling_symlink(
               root                  = dirname(path_symlink)
               , symlink_leaf        = basename(path_symlink)
               , symlink_type        = symlink_type
               , user_entry          = user_entry
               , append_user_comment = FALSE
            )
            private$DYNAMIC$LOG$version_name <- basename(version_path)  # restore to the incoming version
            return(invisible(NULL))
         }

         if (!info$is_link && dir.exists(path_symlink)) {
            # A real directory, not a symlink, is using the name the symlink needs
            stop(
               "Cannot mark '", basename(version_path), "' as '", symlink_type, "'.",
               "\nThe tool needs to create a symlink (shortcut) named '", basename(path_symlink), "' here:\n  ", path_symlink,
               "\nbut a real folder with that name already exists.",
               "\nPlease move or rename that folder, then try again. Nothing has been changed."
            )
         }

         return(invisible(NULL))
      },

      demote_previous_singleton = function(version_path, version_name, user_entry, symlink_type) {
         assert_scalar(version_path)
         root         <- dirname(clean_path(version_path))
         path_symlink <- private$make_symlink_path(version_path, symlink_type)

         # a dangling link was already cleared by validate_symlink_slot(), so only a live link lands here
         if (path_is_link(path_symlink)) {
            old_version_name <- basename(private$resolve_symlink(path_symlink))
            private$remove_one_symlink(root, old_version_name, symlink_type, user_entry)
            private$DYNAMIC$LOG$version_name <- version_name  # restore to the incoming version
         } else {
            private$msg_sometimes("No '", symlink_type, "' symlink found - moving on: ", path_symlink)
         }
      },

      # Promote a version_name folder to a symlink status.
      #
      # Replaces legacy "promote_best" type private methods
      #
      # @param symlink_type (chr) Type of symlink to promote to, must be one of
      #   `get_symlink_types()`
      # @param version_name (chr) version_name folder to promote
      # @param version_path (chr) Path to a version_name folder for promotion
      # @param user_entry (named list) User-supplied log entry
      #
      # @returns (NULL) Creates a symlink on disk for the specified symlink_type, after
      #   demoting any existing symlink of that type for the version_name. Updates
      #   logs with the promotion and any demotion actions taken.
      promote_symlink_type = function(
         symlink_type
         , version_name
         , version_path
         , user_entry
      ){
         assert_dir_exists(version_path)

         symlink_reg  <- get_symlink_registry_entry(symlink_type)
         path_symlink <- private$make_symlink_path(version_path, symlink_type)

         # Singleton types need Highlander demotion of the previous symlink
         if (symlink_reg$is_singleton) {
            private$demote_previous_singleton(
               version_path   = version_path
               , version_name = version_name
               , user_entry   = user_entry
               , symlink_type = symlink_type
            )
         }

         private$msg_sometimes("Promoting to '", symlink_type, "': ", path_symlink)
         private$DYNAMIC$LOG$action <- sprintf("promote_%s", symlink_type)

         # a real folder or dangling link in the slot was handled by validate_symlink_slot() before any demotion
         if(path_is_link(path_symlink)){
            private$msg_sometimes("  ", symlink_type, " symlink already exists - moving on: ", path_symlink)
            return(invisible(NULL))
         }

         # Log only if the link succeeds (linux-style &&: act, then record the act)
         link_created <- tryCatch(
            {
               link_create(version_path, path_symlink)
               TRUE
            }
            , error = function(e) {
               private$msg_sometimes(
                  "   symlink creation failed for: ", path_symlink, "\n   ", conditionMessage(e)
                  , always_message = TRUE
               )
               FALSE
            }
         )
         if (link_created) {
            private$append_to_log(version_path = version_path, user_entry = user_entry)
            private$append_to_central_log(version_path = version_path, user_entry = user_entry)
         }

         return(invisible(NULL))

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


      #' @description Initialize the SymlinkTool object - an R6 class
      #'
      #' The constructor function.
      #'
      #' @param root_list (list) Named list of root directories for
      #'   pipeline outputs. This is where `version_name` folders live - these
      #'   are iterative runs of an analysis pipeline.
      #'   On Windows each root must be on a local disk (e.g. `C:`): the tool
      #'   makes junctions there, and network drives refuse them. The tool
      #'   checks every root when it is built and stops with a plain message
      #'   if one cannot hold links. Run from Linux for roots on a network
      #'   drive.
      #' @param central_log_root (path) Root directory for the central log.
      #'   If you have multiple roots in the `root_list`, you probably want
      #'   the central log to live one level above those roots.
      #' @param schema_repair (logical) Default `TRUE`.  If `TRUE`, the tool
      #'   will attempt to repair any schema mismatches it finds in the logs
      #'   when reading and writing e.g. add new columns if the tool schema has
      #'   columns that existing logs do not. If `FALSE`, the tool will stop and
      #'   throw an error if it finds a schema mismatch.
      #' @param verbose (lgl: default TRUE) control message verbosity - if TRUE,
      #'   standard message, if FALSE, warn only if something is irregular.
      #' @param verbose_startup (lgl) see start up warnings, if relevant?
      #' @param csv_reader (chr) The CSV reader to use (also assigns matching
      #'   CSV writer). CAUTION: DO NOT USE `data.table::fread` if you have any
      #'   quotation marks (") in log comments (these lead to exploding series
      #'   of quotations). https://github.com/Rdatatable/data.table/issues/4779.
      #'   Otherwise use `read.csv[2]`. Options:
      #' \itemize{
      #'  \item{fread_quiet - `data.table::fread` and suppress warnings (default)}
      #'  \item{fread       - `data.table::fread`}
      #'  \item{read.csv    - `utils::read.csv` - safer}
      #'  \item{read.csv2   - `utils::read.csv2` - safer, comma as decimal point, semicolon as field separator}
      #' }
      #' @param timezone (chr) Default `America/Los_Angeles`.  The timezone to
      #'   use for datestamps in logs. Must be a valid `OlsonNames()` string.
      #'
      #' @return (symlink_tool) A symlink tool object.  You can instantiate
      #'   a.k.a. create multiple objects, each of which has different roots and
      #'   central logs.
      #'
      #' @export
      #'
      #' @examples
      #'
      #' try(SLT$new()) # call with no arguments to see instructions
      #' # On Windows, roots must be on a local disk (e.g. C:) - the tool makes
      #' # junctions there, and network drives refuse them. Run from Linux for
      #' # roots on a network drive.
      initialize = function(
      root_list          = NULL
      , central_log_root = NULL
      , schema_repair    = TRUE
      , verbose          = FALSE
      , verbose_startup  = FALSE
      , csv_reader       = "fread_quiet"
      , timezone         = Sys.timezone()
      ) {

         assert_scalar(schema_repair)
         assert_type(schema_repair, "logical")
         assert_scalar(verbose)
         assert_type(verbose, "logical")
         assert_scalar(verbose_startup)
         assert_type(verbose_startup, "logical")

         # allow lazy defaults
         # - do this here to suppress startup messages
         # - assertions are done below
         if(!is.null(root_list) && is.null(central_log_root)){
            if(length(root_list) == 1){
               central_log_root <- data.table::copy(root_list)
            }
         }

         # Helpful start-up feedback
         if(is.null(root_list)){
            message("\n\nThis tool expects `root_list` to be a named list of root directories for pipeline outputs. \n\n  ",

                    "e.g.
                 list(
                   input_root  = '/mnt/share/my_team/input_data',
                   output_root = '/mnt/share/my_team/output_data'
                 ) \n\n  ",

                    "This tool assumes each root will have a `version_name` output folder (same folder name in each root). \n  ",
                    "  You may track outputs in one root, or across many roots in parallel (as long as the version_name is the same). \n  ",
                    "  It's recommended to create these folders with the tool so they get a log at time of creation. \n\n  ",

                    "Each output folder is defined by `clean_path(root_list, version_name)`. \n  ",
                    "  The user can 'mark' or 'unmark' any `version_name` folder as best/keep/remove. \n  ",
                    "  This folder receives a log entry for all *demotion* and *promotion* actions (marking and unmarking). \n  ",
                    "  All the `version_name` folder logs are used for report generation. \n\n  "
            )
         }

         if(is.null(central_log_root)){
            message("\n\nThis tool expects `central_log_root` to be a single directory for the central log. \n\n  ",

                    "e.g.
                 '/mnt/share/my_team' \n\n  ",

                    "The central log receives all the marking actions of all the version_name logs across all roots, \n  ",
                    "  but is not used for report generation. \n\n  ",
                    "The central log is *created* on initialization i.e. when calling `SLT$new()`. \n\n  "
            )
         }

         if(any(is.null(root_list) || is.null(central_log_root))){
            stop("You must provide both root_list and central_log_root")
         }

         stopifnot(is.character(timezone))
         if(!timezone %in% OlsonNames()){
            stop("timezone must be in `OlsonNames()`")
         }

         # ------------------------------------------------------------------#

         # libraries
         # library(data.table)
         # only fread is currently supported due to how data types are defined when reading in logs
         private$csv_reader <- switch_strict(
            csv_reader
            , "fread"       = function(...) return(data.table::fread(...))
            , "fread_quiet" = function(...) return(suppressWarnings(data.table::fread(...)))
            , "read.csv"    = function(...) return(data.table::setDT(utils::read.csv(...)))
            , "read.csv2"   = function(...) return(data.table::setDT(utils::read.csv2(...)))
            , stop("csv_reader must be one of: fread, fread_quiet, read.csv, read.csv2")
         )

         # raw writer - never call directly; `private$csv_writer()` wraps it
         # with an atomic temp-file + rename so concurrent readers never see a
         # truncated file
         private$csv_writer_raw <- switch_strict(
            csv_reader
            , "fread"       = function(...) return(data.table::fwrite(...))
            , "fread_quiet" = function(...) return(data.table::fwrite(...))
            , "read.csv"    = function(...) return(utils::write.csv(...))
            , "read.csv2"   = function(...) return(utils::write.csv2(...))
            , stop("csv_reader must be one of: fread, fread_quiet, read.csv, read.csv2")
         )

         if(csv_reader %like% "fread" & verbose_startup == TRUE) {
            message("WARNING: Do not use double quotation marks (\") in user comments - data.table::fread causes them to multiply.")
         }

         # ------------------------------------------------------------------#

         # Users must provide these fields

         ## ROOTS
         # clean roots
         root_list <- lapply(root_list, clean_path)
         # validate inputs
         assert_named_list(root_list)
         lapply(root_list, assert_dir_exists)
         assert_scalar(schema_repair)
         assert_type(schema_repair, "logical")

         # set roots
         private$DICT$ROOTS <- root_list

         # Can the tool create symlinks (shortcuts) in each root?
         # - probed once per root here and cached for the life of the tool
         # - whether a failed probe stops the tool is the link backend's call:
         #   Windows: yes - a network drive cannot even be read reliably
         #   Linux/macOS: no - links stay readable, so read-only use of roots
         #   you do not own (the SymlinkComposer case) still works; a mark
         #   fails later with a plain message if the root refuses it
         #   See `probe_failure_is_fatal` in R/links.R for the full reasoning.
         private$DICT$ROOT_LINK_CAPABLE <- vapply(root_list, dir_supports_links, logical(1))
         if (link_get_backend()$probe_failure_is_fatal && !all(private$DICT$ROOT_LINK_CAPABLE)) {
            bad_root <- root_list[[which(!private$DICT$ROOT_LINK_CAPABLE)[1]]]
            stop(
               "The Symlink Tool cannot create symlinks (shortcuts) in this root folder:\n  ", bad_root,
               "\n  - Reason: ", link_failure_reason(bad_root),
               "\n  - Nothing has been changed."
            )
         }

         ## Timezone
         private$DICT$TZ <- timezone

         ## FLAGS
         # log schema
         private$DICT$FLAGS$allow_schema_repair <- schema_repair

         ## Verbosity
         private$DICT$verbose <- verbose


         # ------------------------------------------------------------------#

         # User should not interact with these

         ## Log fields the user can set
         private$DICT$log_fields_user  <- setdiff(names(private$DICT$log_schema), private$DICT$log_fields_auto)

         ## CENTRAL LOG
         central_log_root <- clean_path(central_log_root)
         assert_scalar(central_log_root)
         assert_dir_exists(central_log_root)
         private$DICT$LOG_CENTRAL$root <- central_log_root

         private$DICT$LOG_CENTRAL$path <- clean_path(
            private$DICT$LOG_CENTRAL$root
            , private$DICT$LOG_CENTRAL$fname
         )
         # NOTE: the constructor must NOT touch the central log on disk.
         # Many jobs (e.g. job arrays) may instantiate this tool
         # concurrently against the same central_log_root; any read-modify-write
         # here is a race.  The central log is created/repaired lazily on the
         # first mark/delete via `append_to_central_log()`.
      },


      ## Show Internals --------------------------------------------------------

      #' @description
      #' Return the contents of all private dictionaries.
      #'
      #' @param item_names (chr) Default `NULL`.  If `NULL`, show all static
      #'   internal fields.  Otherwise, vector of static field names you want to
      #'   see.
      #'
      #' @return (list) of all static internal fields
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

      #' @description
      #' Print the contents of all dynamic fields.
      #'
      #' @param item_names (chr) Default `NULL`.  If `NULL`, show all dynamic
      #'   internal fields.  Otherwise, vector of dynamic field names you want
      #'   to see.
      #'
      #' @return (std_out) Print dynamic field values to std_out.
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

      #' @description Mark a version_name folder with a symlink of the specified
      #'   type.
      #'
      #'   Generic dispatch method for all symlink marking. Legacy per-type
      #'   methods (`mark_best`, `mark_latest`, etc.) are superceded thin
      #'   wrappers around this.
      #'
      #' @param symlink_type (chr) A registered symlink type, e.g. `"best"`,
      #'   `"keep"`, or any type added via `new_symlink_type()`.
      #' @param version_name (chr) The version_name folder to mark.
      #'   You may also pass the name of an existing symlink (shortcut) such
      #'   as `"latest"` or `"keep_1990_01_01"`; it is resolved to the real
      #'   version_name folder it points to.
      #' @param user_entry (list) Named list of user log fields.
      #'
      #' @return (std_err) Messages about actions taken.
      mark = function(symlink_type, version_name, user_entry) {
         version_name <- private$resolve_version_name(version_name)
         checkmate::assert_choice(symlink_type, names(get_symlink_registry()))

         private$handler_pre_mark(
            version_name = version_name,
            user_entry   = user_entry
         )

         private$msg_sometimes("Marking ", symlink_type, ": ", version_name)

         for (version_path in private$DYNAMIC$VERS_PATHS) {
            if (validate_dir_exists(version_path) == FALSE) next()
            if (private$already_marked(version_path, symlink_type)) {
               private$msg_sometimes(version_path, " - already marked ", symlink_type, " - moving on.")
               next()
            }

            # check the slot first so a blocked promote cannot leave the version unmarked
            private$validate_symlink_slot(
               version_path   = version_path,
               symlink_type   = symlink_type,
               user_entry     = user_entry
            )

            private$demote_existing_symlinks(
               version_path = version_path,
               version_name = version_name,
               user_entry   = user_entry
            )

            private$promote_symlink_type(
               symlink_type = symlink_type,
               version_name = version_name,
               version_path = version_path,
               user_entry   = user_entry
            )
         }

         private$handler_post_mark(
            version_name = version_name,
            user_entry   = user_entry
         )
      },

      #' @description
      #' `r lifecycle::badge('superseded')`
      #'
      #' Please use `mark("best", version_name, user_entry)` instead.
      #'
      #' Enforces:
      #' - maximum of one best model
      #'   - does not go back through history to make a best model from a prior version (not capable, this is what log_tool is for)
      #'
      #' Writes:
      #' - appends to a log file in the output folder with a date and time stamp
      #' - appends a line to the central log file with a date and time stamp
      #'
      #' @param version_name (chr) The directory name of the output folder that
      #'   lives directly under one of the `root`s you define when you
      #'   instantiate the tool.
      #'   You may also pass the name of an existing symlink (shortcut) such
      #'   as `"latest"` or `"keep_1990_01_01"`; it is resolved to the real
      #'   version_name folder it points to.
      #' @param user_entry (list) Named list of user-defined fields to append to
      #'   the log.  After making a tool called e.g. slt, call
      #'   `slt$return_dictionaries("log_fields_user")` to find which fields a
      #'   user may add.  If you want to make your own version of this class,
      #'   you may update `log_schema` in the `private$DICT` section to allow
      #'   for them.
      #'
      #' @return (ste_err) Messages about actions taken.
      #'
      #'
      #'
      mark_best = function(version_name, user_entry) {
         self$mark("best", version_name, user_entry)
      },


      #' @description
      #' `r lifecycle::badge('superseded')`
      #'
      #' Mark an output folder with a "latest" symlink.
      #'
      #' Please use `mark("latest", version_name, user_entry)` instead.
      #'
      #' Creates a 'latest' symlink (singleton) in each root. Auto-demotes
      #' current 'latest' if different. A folder cannot be both 'latest' and
      #' 'best' - this will auto-demote from the conflicting state.
      #'
      #' Writes:
      #' - appends to a log file in the output folder with a date and time stamp
      #' - appends a line to the central log file with a date and time stamp
      #'
      #' @param version_name (chr) The directory name of the output folder that
      #'   lives directly under one of the `root`s you define when you
      #'   instantiate the tool.
      #'   You may also pass the name of an existing symlink (shortcut) such
      #'   as `"latest"` or `"keep_1990_01_01"`; it is resolved to the real
      #'   version_name folder it points to.
      #' @param user_entry (list) Named list of user-defined fields to append to
      #'   the log.  After making a tool called e.g. slt, call
      #'   `slt$return_dictionaries("log_fields_user")` to find which fields a
      #'   user may add.  If you want to make your own version of this class,
      #'   you may update `log_schema` in the `private$DICT` section to allow
      #'   for them.
      #'
      #' @return (std_err) Messages about actions taken.
      #'
      #'
      #'
      mark_latest = function(version_name, user_entry) {
         self$mark("latest", version_name, user_entry)
      },


      #' @description
      #' `r lifecycle::badge('superseded')`
      #'
      #' Mark an output folder with a "keep_<version_name>" symlink
      #'
      #' Please use `mark("keep", version_name, user_entry)` instead.
      #'
      #' Writes:
      #' - appends to a log file in the output folder with a date and time stamp
      #' - appends a line to the central log file with a date and time stamp
      #'
      #' @param version_name (chr) The directory name of the output folder that
      #'   lives directly under one of the `root`s you define when you
      #'   instantiate the tool.
      #'   You may also pass the name of an existing symlink (shortcut) such
      #'   as `"latest"` or `"keep_1990_01_01"`; it is resolved to the real
      #'   version_name folder it points to.
      #' @param user_entry (list) Named list of user-defined fields to append to
      #'   the log.  After making a tool called e.g. slt, call
      #'   `slt$return_dictionaries("log_fields_user")` to find which fields a
      #'   user may add.  If you want to make your own version of this class,
      #'   you may update `log_schema` in the `private$DICT` section to allow
      #'   for them.
      #'
      #' @return (std_err) Messages about actions taken.
      #'
      #'
      #'
      mark_keep = function(version_name, user_entry) {
         self$mark("keep", version_name, user_entry)
      },


      #' @description
      #' `r lifecycle::badge('superseded')`
      #'
      #' Mark an output folder with a "remove_<version_name>" symlink
      #'
      #' Please use `mark("remove", version_name, user_entry)` instead.
      #'
      #' Indication that the results can be deleted
      #' - In the future, this will be used to remove old versions of the
      #'   output, and provide a list of ST-GPR models to delete
      #'
      #' Writes:
      #' - appends to a log file in the output folder with a date and time stamp
      #' - appends a line to the central log file with a date and time stamp
      #'
      #' @param version_name (chr) The directory name of the output folder that
      #'   lives directly under one of the `root`s you define when you
      #'   instantiate the tool.
      #'   You may also pass the name of an existing symlink (shortcut) such
      #'   as `"latest"` or `"keep_1990_01_01"`; it is resolved to the real
      #'   version_name folder it points to.
      #' @param user_entry (list) Named list of user-defined fields to append to
      #'   the log.  After making a tool called e.g. slt, call
      #'   `slt$return_dictionaries("log_fields_user")` to find which fields a
      #'   user may add.  If you want to make your own version of this class,
      #'   you may update `log_schema` in the `private$DICT` section to allow
      #'   for them.
      #'
      #' @return (std_err) Messages about actions taken.
      #'
      #'
      #'
      mark_remove = function(version_name, user_entry) {
         self$mark("remove", version_name, user_entry)
      },


      #' @description
      #' Remove all symlinks for a single `version_name` in all `roots`
      #'
      #' Writes:
      #' - appends a `demote_<type>` row to the log file in the output folder
      #' - appends the same row to the central log file
      #'
      #' A dangling symlink (shortcut), one whose version folder was deleted by
      #' hand, is removed too, by version name or by its own name. That writes
      #' a `remove_dangling_<type>` row to the central log only, with a comment
      #' prefixed `[vmTools auto]`; the deleted folder is never re-created.
      #'
      #' @param version_name (chr) The directory name of the output folder that
      #'   lives directly under one of the `root`s you define when you
      #'   instantiate the tool.
      #'   You may also pass the name of an existing symlink (shortcut) such
      #'   as `"latest"` or `"keep_1990_01_01"`; it is resolved to the real
      #'   version_name folder it points to, or removed directly if dangling.
      #' @param user_entry (list) Named list of user-defined fields to append to
      #'   the log.  After making a tool called e.g. slt, call
      #'   `slt$return_dictionaries("log_fields_user")` to find which fields a
      #'   user may add.  If you want to make your own version of this class,
      #'   you may update `log_schema` in the `private$DICT` section to allow
      #'   for them.
      #'
      #' @return (std_err) Messages about the symlinks removed.
      #'
      #'
      #'
      unmark = function(version_name, user_entry){

         # guard: edge case - a dangling link passed by its own name has no version to resolve to
         if (private$remove_dangling_link_by_name(version_name, user_entry)) return(invisible(NULL))

         version_name <- private$resolve_version_name(version_name)

         # 1. remove all symlinks associated with a version_name
         # 2. append to the log
         for(root in private$DICT$ROOTS){
            private$remove_all_symlinks(root         = root,
                                        version_name = version_name,
                                        user_entry   = user_entry)
         }

         private$handler_post_mark(version_name = version_name,
                                   user_entry = user_entry)

      },


      ## Path Roundups ---------------------------------------------------------

      #' @description
      #' Find all symlinks of a given type across all roots.
      #'
      #' Generic dispatch method for all roundup operations. Legacy per-type
      #' methods (`roundup_best`, `roundup_keep`, etc.) are superceded thin
      #' wrappers.
      #'
      #' @param symlink_type (chr) A registered symlink type.
      #' @param return_as (chr: default `"table"`) or `"version_name"`.
      #'
      #' @return (data.table/chr) A single data.table with results from all
      #'   roots, with `root_name` as the first column, or a vector of
      #'   `version_name`s if `return_as = version_name`.
      #'
      #' Table output schema
      #'
      #' ```r
      #' root_name version_name dir_name dir_name_resolved
      #'    <char>       <char>   <char>            <char>
      #' ```
      roundup = function(symlink_type, return_as = c("table", "version_name")) {
         return_as <- match.arg(return_as)
         checkmate::assert_choice(symlink_type, names(get_symlink_registry()))
         if (return_as == "table") {
            ret <- private$combine_roundup_results(query_function = private$query_all_symlink_type, symlink_type)
         } else if (return_as == "version_name") {
            ret <- unique(private$combine_roundup_results(query_function = private$query_all_symlink_type, symlink_type)$version_name)
         }
         return(ret)
      },

      #' @description
      #' `r lifecycle::badge('superseded')`
      #'
      #' Find all `best` symlinks in all `roots`
      #'
      #' Please use `roundup("best", return_as)` instead.
      #'
      #' @param return_as (chr) Default `table`.  Whether to return a data.table with
      #'   symlink and resolved paths (`table`) or just a vector of
      #'   `version_name`s (`version_name`) for easier automated round-ups.
      #'
      #' @return (data.table/chr) A single data.table with results from all
      #'   roots, with `root_name` as the first column, or a vector of
      #'   `version_name`s if `return_as = version_name`.
      #'
      #' Table output schema
      #'
      #' ```r
      #' root_name version_name dir_name dir_name_resolved
      #'    <char>       <char>   <char>            <char>
      #' ```
      #'
      roundup_best = function(return_as = c("table", "version_name")) {
         self$roundup("best", return_as)
      },

      #' @description
      #' `r lifecycle::badge('superseded')`
      #'
      #' Find all `latest` symlinks in all `roots`
      #'
      #' @param return_as (chr) Default `table`.  Whether to return a data.table with
      #'   symlink and resolved paths (`table`) or just a vector of
      #'   `version_name`s (`version_name`) for easier automated round-ups.
      #'
      #' @return (data.table/chr) A single data.table with results from all
      #'   roots, with `root_name` as the first column, or a vector of
      #'   `version_name`s if `return_as = version_name`.
      #'
      #' Table output schema
      #'
      #' ```r
      #' root_name version_name dir_name dir_name_resolved
      #'    <char>       <char>   <char>            <char>
      #' ```
      #'
      roundup_latest = function(return_as = c("table", "version_name")) {
         self$roundup("latest", return_as)
      },

      #' @description
      #' Find all `keep_` symlinks in all `roots`
      #'
      #' @param return_as (chr) Default `table`.  Whether to return a data.table with
      #'   symlink and resolved paths (`table`) or just a vector of
      #'   `version_name`s (`version_name`) for easier automated round-ups.
      #'
      #' @return (data.table/chr) A single data.table with results from all
      #'   roots, with `root_name` as the first column, or a vector of
      #'   `version_name`s if `return_as = version_name`.
      #'
      #' Table output schema
      #'
      #' ```r
      #' root_name version_name dir_name dir_name_resolved
      #'    <char>       <char>   <char>            <char>
      #' ```
      #'
      roundup_keep = function(return_as = c("table", "version_name")){
         self$roundup("keep", return_as)
      },

      #' @description
      #' `r lifecycle::badge('superseded')`
      #'
      #' Find all `remove_` symlinks in all `roots`
      #'
      #' @param return_as (chr) Default `table`.  Whether to return a data.table with
      #'   symlink and resolved paths (`table`) or just a vector of
      #'   `version_name`s (`version_name`) for easier automated round-ups.
      #'
      #' @return (data.table/chr) A single data.table with results from all
      #'   roots, with `root_name` as the first column, or a vector of
      #'   `version_name`s if `return_as = version_name`.
      #'
      #' Table output schema
      #'
      #' ```r
      #' root_name version_name dir_name dir_name_resolved
      #'    <char>       <char>   <char>            <char>
      #' ```
      #'
      roundup_remove = function(return_as = c("table", "version_name")){
         self$roundup("remove", return_as)
      },

      #' @description
      #' Find all folders without symlinks in all `roots`
      #'
      #' @param return_as (chr) Default `table`.  Whether to return a data.table with
      #'   symlink and resolved paths (`table`) or just a vector of
      #'   `version_name`s (`version_name`) for easier automated round-ups.
      #'
      #' @return (data.table/chr) A single data.table with results from all
      #'   roots, with `root_name` as the first column, or a vector of
      #'   `version_name`s if `return_as = version_name`.
      #'
      #' Table output schema
      #'
      #' ```r
      #' root_name version_name dir_name dir_name_resolved
      #'    <char>       <char>   <char>            <char>
      #' ```
      #'
      roundup_unmarked = function(return_as = c("table", "version_name")){
         return_as <- match.arg(return_as)
         if(return_as == "table"){
            ret <- private$combine_roundup_results(private$query_all_unmarked)
         } else if (return_as == "version_name"){
            ret <- unique(private$combine_roundup_results(private$query_all_unmarked)$version_name)
         }
         return(ret)
      },

      #' @description
      #' Find all `version_name` folders by creation date
      #'
      #' Only finds folders that _have a log_, and reads creation date on first
      #' row.  User may select dates by (using the `date_selector` argument):
      #' - greater than - `gt`
      #' - greater than or equal to - `gte`
      #' - less than - `nt`
      #' - less than or equal to `nte`
      #' - equal to `e`
      #'
      #' @param user_date (c("character", "Date", POSIXct", "POSIXt")) A date
      #'   with class requirements - must be formatted "2020-01-01 or 2020_01_01
      #'   or 2020/01/01"
      #' @param date_selector (chr) See docstring explanation.
      #'
      #' @param return_as (chr) Default `table`.  Whether to return a data.table with
      #' symlink and resolved paths (`table`) or just a vector of
      #' `version_name`s (`version_name`) for easier automated round-ups.
      #'
      #'
      #' @return (data.table/chr) A single data.table with results from all
      #'   roots, with `root_name` as the first column, or a vector of
      #'   `version_name`s if `return_as = version_name`.
      #'
      #' Table output schema
      #'
      #' ```r
      #' root_name version_name dir_name dir_name_resolved
      #'    <char>       <char>   <char>            <char>
      #' ```
      #'
      roundup_by_date = function(user_date, date_selector, return_as = c("table", "version_name")){

         return_as     <- match.arg(return_as)
         date_selector <- tolower(date_selector)

         private$assert_date_selector(date_selector)
         private$assert_user_date_class_and_format(user_date)

         private$msg_sometimes(paste0("Finding all folders with log creation dates that are", " '", date_selector, "' ", user_date), ". \n  ",
                    "NOTE! Log creation dates are used as the file-system does not record creation times. \n")

         # format user_date to USA PST to align with cluster filesystem dates
         tzone = private$DICT$TZ
         private$msg_sometimes("roundup_by_date: Formatting date with time-zone: ", tzone, "\n")
         user_date_parsed <- as.POSIXct(user_date, tz = tzone, tryFormats = c("%Y-%m-%d", "%Y_%m_%d", "%Y/%m/%d"))

         private$msg_sometimes("Folders with symlinks will have duplicate rows by `version_name` (one row for each unique `dir_name`) - showing all for completeness.\n")
         if(return_as == "table"){
            ret <- private$combine_roundup_results(
               query_function   = private$query_by_date,
               user_date_parsed = user_date_parsed,
               date_selector    = date_selector
            )
         } else if (return_as == "version_name"){
            ret <- unique(private$combine_roundup_results(
               query_function   = private$query_by_date,
               user_date_parsed = user_date_parsed,
               date_selector    = date_selector
            )$version_name)
         }
         return(ret)

      },



      ## Folder Creation -------------------------------------------------------

      #' @description Get a new YYYY_MM_DD.VV version compatible with _ALL THE
      #' TOOL'S ROOTS_
      #'
      #' If root1 has 2025_01_01.01 and root2 has 2025_01_01.03, then a new
      #' folder would need to be 2025_01_01.04
      #'
      #' @param root_list (list) named list of root directories for pipeline
      #'
      #' @param date (chr) Default "today".  The date to use for the new version
      #'   name.  Must be formatted "2020_01_01"
      #'
      #' @return (chr) format YYYY_MM_DD.VV
      #'
      #'
      #'
      get_common_new_version_name = function(date = "today", root_list = private$DICT$ROOTS){
         return(max(unlist(lapply(root_list, get_new_version_name, date = date))))
      },


      #' @description Create a new `version_name` folder in _ALL THE TOOL'S
      #'   ROOTS_
      #'
      #'   Create a new log in each folder.  No symlinks are created.  No
      #'   `user_entry` is used.
      #'
      #' @param version_name (chr) The directory name of the
      #'   output folder that lives directly under one of the `root`s you define
      #'   when you instantiate the tool.  For convenience, user may leave NULL
      #'   (default) and `get_common_new_version_name()` is used on that root.
      #'   Names used by the tool's own symlinks (shortcuts) are reserved and
      #'   refused: the name of any singleton type (e.g. `"best"`, `"latest"`)
      #'   and any name starting with a non-singleton type plus underscore
      #'   (e.g. `"keep_"`, `"remove_"`). Types added with `new_symlink_type()`
      #'   reserve their names the same way.
      #'
      #' @return (chr) new version_name (in YYYY_MM_DD.VV format if defaults are used)
      #'
      #'
      #'
      make_new_version_folder = function(version_name = self$get_common_new_version_name()){

         assert_scalar_not_empty(x = version_name)
         assert_version_name_not_reserved(version_name)

         private$handler_update_dynamic_fields(version_name = version_name)

         for(version_path in private$DYNAMIC$VERS_PATHS){
            private$make_folder_with_log(version_path = version_path)
         }

         return(version_name)

      },

      #' @description
      #' Safely write an empty log file for first pipeline runs
      #'
      #' When you start a new pipeline run, make an empty log
      #' - helpful if you let this tool manage all your versions
      #' - you can roundup version_names by creation date using the log's first entry
      #' - the file system doesn't track directory creation dates (at time of writing)
      #'
      #' @param version_name (chr) The directory name of the output folder that
      #'   lives directly under one of the `root`s you define when you
      #'   instantiate the tool.
      #'   You may also pass the name of an existing symlink (shortcut) such
      #'   as `"latest"` or `"keep_1990_01_01"`; it is resolved to the real
      #'   version_name folder it points to.
      #'
      #' @return (std_err) Messages about the log creation.
      #'
      #'
      #'
      make_new_log = function(version_name){
         version_name <- private$resolve_version_name(version_name)

         private$handler_update_dynamic_fields(version_name = version_name)

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
               error = function(e) message("Error reading log: \n  ||     ", e)
            )
         }

      },


      ## Folder Deletion -------------------------------------------------------

      #' @description
      #' Delete a `version_name` folder marked with a `remove_` symlink from
      #' _ALL ITS ROOTS_
      #'
      #' Removes the symlink(s) and the underlying folder(s), and updates
      #' central log if folders were removed.
      #'
      #' Writes:
      #' - appends a line to the central log file with a date and time stamp
      #'
      #' @param version_name (chr) The directory name of the output folder that
      #'   lives directly under one of the `root`s you define when you
      #'   instantiate the tool.
      #'   You may also pass the name of an existing symlink (shortcut) such
      #'   as `"latest"` or `"keep_1990_01_01"`; it is resolved to the real
      #'   version_name folder it points to.
      #' @param user_entry (list) Named list of user-defined fields to append to
      #'   the log.  After making a tool called e.g. slt, call
      #'   `slt$return_dictionaries("log_fields_user")` to find which fields a
      #'   user may add.  If you want to make your own version of this class,
      #'   you may update `log_schema` in the `private$DICT` section to allow
      #'   for them.
      #' @param require_user_input (lgl) if `TRUE`, will prompt user to confirm
      #'   deletion.
      #'
      #' @return (std_err) Messages about deletion events.
      #'
      #'
      #'
      delete_version_folders = function(version_name, user_entry, require_user_input = TRUE){
         version_name <- private$resolve_version_name(version_name)

         private$handler_pre_mark(
            version_name = version_name,
            user_entry   = user_entry
         )

         ret_val_deleted_TF <- lapply(
            X   = private$DICT$ROOTS,
            FUN = private$delete_remove_folder,
            version_name       = version_name,
            user_entry         = user_entry,
            require_user_input = require_user_input
         )

         ret_val_deleted_TF <- unlist(ret_val_deleted_TF)

         if(any(ret_val_deleted_TF)) {
            private$handler_post_mark(version_name = version_name,
                                      user_entry   = user_entry)
         }
      },


      ## Reports ---------------------------------------------------------------


      #' @description
      #' Make all reports
      #'
      #' Writes all reports to a summary .csv for every `root` defined in the
      #' tool.
      #'
      #' @return (std_err) Messages about where reports were written.
      #'
      #'
      #'
      make_reports = function(){

         private$msg_sometimes("Writing last-row log reports for:\n")
         for(root in private$DICT$ROOTS){
            private$msg_sometimes("  ", root)
            private$report_all_logs(root = root)
            private$report_all_logs_symlink(root = root)
            private$report_all_logs_tool_symlink(root = root)
            private$report_all_logs_non_symlink(root = root)
            private$report_discrepancies(root = root, verbose = FALSE) # runs by default inside report_all_logs_tool_symlink, suppress extra messages
            private$msg_sometimes("  ", root)
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

# Add a new class and print method
# - ORDER MATTERS for class additions
# - THE _NEW_ CLASS MUST COME FIRST for priting to function correctly when
#   calling the bare class
class(SLT) <- c("Symlink_Tool", class(SLT))


#' Symlink Tool custom print method
#'
#' @param x (Symlink_Tool) The SLT class
#' @param ... (any) Additional arguments to `print()`
#'
#' @return (stdout)
#' @exportS3Method print Symlink_Tool
#'
#' @examples SLT
print.Symlink_Tool <- function(x, ...) {
   NextMethod("print") # Prints the default class output, then cumstom message
   cat("\n\n    Call SLT$new() to make a Symlink Tool, with startup guidance messages!\n\n")
}

