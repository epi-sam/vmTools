
# Setup ------------------------------------------------------------------------

# use clean_path() liberally for github/cran checks

.user <- Sys.info()[['user']]
# system(paste("rm -rf", tempdir()))
make_directory <- function(path) dir.create(path, recursive = TRUE, showWarnings = FALSE)
root_base      <- clean_path(tempdir(), "slt")
root_list      <- list(
   root_input    = clean_path(root_base, "dir_1")
   , root_output = clean_path(root_base, "dir_2")
)
make_directory(root_list[["root_input"]])
make_directory(root_list[["root_output"]])
# dir_tree(root_base)
dv_list <- list(
   "1990_01_01"   = "1990_01_01"
   , "1990_01_02" = "1990_01_02"
   , "1990_01_03" = "1990_01_03"
)
path_list      <- lapply(root_list, function(x) clean_path(x, dv_list))
path_list      <- lapply(path_list, function(x) {names(x) <- names(dv_list); x})
path_list_logs <- lapply(path_list, function(x) clean_path(x, 'logs'))

ue_list <- list(
   best     = list(comment = "Testing mark best")
   , keep   = list(comment = "Testing mark keep")
   , remove = list(comment = "Testing mark remove")
   , unmark = list(comment = "Testing mark unmark")
)


# Windows Guard ------------------------------------------------------------

# All integration tests will fail on Windows unless the R process runs with
# administrative privileges, which is not possible on CRAN servers.
# - Right click on Rstudio > "Run as administrator" > Yes
#
# See description section of ?base::file.symlink.
#
# Symbolic links on Windows
# Symbolic links in the sense of POSIX file systems do not exist on Windows:
# however, NTFS file systems support two similar concepts.
#
# ...
#
# A version of symbolic linking to files/directories was implemented more
# recently, and file.symlink makes use of that interface. However, it has
# restrictions which are crippling. First, the user needs permission to make
# symbolic links, and that permission is not normally granted except to
# Administrator accounts (note: not users with Administrator rights): further
# many users report that whereas the Policy Editor appears to be able to grant
# such rights, the API still reports insufficient permissions. Second, the
# interface needs to know if from is a file or a directory (and it need not yet
# exist): we have implemented this to allow linking from a directory only if it
# currently exists.

if(tolower(.Platform$OS.type) == "windows" & vmTools:::is_windows_admin() == FALSE){

   test_that(
      "Windows non-admin startup produces correct error",
      {
         expect_error(
            SLT$new(), regexp = "Symbolic links are not supported on Windows without admin privileges."
         )
      }
   )

   testthat::skip("Symbolic links are not supported on Windows without admin privileges")

}

# Create tool ------------------------------------------------------------------


test_that(
   "Naive startup produces messages and error",
   {
      expect_message(
         expect_message(
            expect_error(
               SLT$new()
               , regexp = "You must provide both root_list and central_log_root"
            ) , regexp = "This tool expects `central_log_root` to be a single directory for the central log"
         ) , regexp = "This tool expects \\`root_list\\` to be a named list of root directories for pipeline outputs"
      )

   }
)


# Integration Tests ------------------------------------------------------------

# Create a real tool with two roots
# - doing integration tests with multiple roots to really torture test things
suppressMessages(
   suppressWarnings({ # idiosyncratic and benign cluster message

      slt <- SLT$new(
         root_list = list(
            root_input  = root_list[["root_input"]],
            root_output = root_list[["root_output"]]
         )
         , central_log_root = root_base
         , schema_repair = TRUE
         , verbose = FALSE
         , verbose_startup = FALSE
         , csv_reader = 'fread_quiet'
         , timezone = Sys.timezone()
      )
   })
)

# Central log - constructor is read-only --------------------------------------

# Many parallel jobs may instantiate SLT concurrently against one
# central_log_root.  The constructor must never read-modify-write that file.

test_that("Constructor does not create or modify the central log",
          {
             root_ctor    <- clean_path(tempdir(), "slt_ctor_readonly")
             roots_ctor   <- list(root_a = clean_path(root_ctor, "a"))
             central_ctor <- clean_path(root_ctor, "central")
             make_directory(roots_ctor[["root_a"]])
             make_directory(central_ctor)
             on.exit(unlink(root_ctor, recursive = TRUE), add = TRUE)

             make_slt_ctor <- function(){
                suppressMessages(suppressWarnings(
                   SLT$new(
                      root_list          = roots_ctor
                      , central_log_root = central_ctor
                      , verbose          = FALSE
                      , verbose_startup  = FALSE
                   )
                ))
             }

             # 1. Fresh root: construction creates no central log
             slt_ctor <- make_slt_ctor()
             fpath_central <- slt_ctor$return_dictionaries()$LOG_CENTRAL$path
             expect_false(file.exists(fpath_central))
             expect_identical(list.files(central_ctor), character(0))

             # 2. First mark creates it with a 'create' row plus the action row
             slt_ctor$make_new_version_folder(version_name = "1990_01_01")
             slt_ctor$mark_best(version_name = "1990_01_01", user_entry = list(comment = "first mark"))
             expect_true(file.exists(fpath_central))
             log_central <- data.table::fread(fpath_central)
             expect_identical(log_central$log_id, c(0L, 1L))
             expect_identical(log_central$action, c("create", "promote_best"))
             expect_identical(log_central$version_name, c("CENTRAL_LOG", "1990_01_01"))

             # 3. Re-constructing over a populated central log leaves it byte-identical
             md5_before   <- unname(tools::md5sum(fpath_central))
             mtime_before <- file.mtime(fpath_central)
             slt_ctor_2   <- make_slt_ctor()
             slt_ctor_3   <- make_slt_ctor()
             expect_identical(unname(tools::md5sum(fpath_central)), md5_before)
             expect_identical(file.mtime(fpath_central), mtime_before)

             # 4. A central log that does not exist at construction still
             #    gets written on the first mark of a *second* tool instance
             #    (i.e. nothing is cached at construction that depends on the file)
             unlink(fpath_central)
             slt_ctor_2$mark_keep(version_name = "1990_01_01", user_entry = list(comment = "second mark"))
             expect_true(file.exists(fpath_central))
             log_central <- data.table::fread(fpath_central)
             expect_identical(log_central$action, c("create", "demote_best", "promote_keep"))
          })


# Atomic CSV writer -------------------------------------------------------------

# All log/report writes go to a hidden temp file in the target directory and
# are then renamed into place, so concurrent readers never see a torn file.

test_that("csv_writer writes atomically and leaves no temp files",
          {
             dir_atomic <- clean_path(tempdir(), "slt_atomic_writer")
             make_directory(dir_atomic)
             on.exit(unlink(dir_atomic, recursive = TRUE), add = TRUE)

             csv_writer <- slt$.__enclos_env__$private$csv_writer
             fpath      <- clean_path(dir_atomic, "test.csv")
             dt_in      <- data.table::data.table(a = 1:3, b = c("x", "y", "z"))

             # 1. fresh write round-trips and leaves only the target behind
             csv_writer(dt_in, fpath, row.names = FALSE)
             expect_true(file.exists(fpath))
             expect_identical(data.table::fread(fpath), dt_in)
             expect_identical(list.files(dir_atomic, all.files = TRUE, no.. = TRUE), "test.csv")

             # 2. overwrite replaces content and still leaves no temp files
             dt_in_2 <- data.table::data.table(a = 4:5, b = c("p", "q"))
             csv_writer(dt_in_2, fpath, row.names = FALSE)
             expect_identical(data.table::fread(fpath), dt_in_2)
             expect_identical(list.files(dir_atomic, all.files = TRUE, no.. = TRUE), "test.csv")

             # 3. missing target directory errors cleanly
             expect_error(
                csv_writer(dt_in, clean_path(dir_atomic, "nope", "test.csv"), row.names = FALSE)
                , regexp = "folder does not exist"
             )
          })

test_that("csv_writer preserves file mode and cleans up on failure (POSIX only)",
          {
             skip_on_os("windows")
             skip_if(identical(Sys.info()[["user"]], "root"), "root ignores directory permissions")

             dir_atomic <- clean_path(tempdir(), "slt_atomic_writer_posix")
             make_directory(dir_atomic)
             on.exit({
                Sys.chmod(dir_atomic, mode = "0755", use_umask = FALSE)
                unlink(dir_atomic, recursive = TRUE)
             }, add = TRUE)

             csv_writer <- slt$.__enclos_env__$private$csv_writer
             fpath      <- clean_path(dir_atomic, "test.csv")
             dt_in      <- data.table::data.table(a = 1:3, b = c("x", "y", "z"))

             # 1. an existing file's mode is carried over to the rewritten file
             csv_writer(dt_in, fpath, row.names = FALSE)
             Sys.chmod(fpath, mode = "0640", use_umask = FALSE)
             csv_writer(dt_in, fpath, row.names = FALSE)
             expect_identical(as.character(file.mode(fpath)), "640")

             # 2. a write into an unwritable directory errors and leaves no temp file
             Sys.chmod(dir_atomic, mode = "0500", use_umask = FALSE)
             expect_error(csv_writer(dt_in, fpath, row.names = FALSE))
             Sys.chmod(dir_atomic, mode = "0755", use_umask = FALSE)
             expect_identical(list.files(dir_atomic, all.files = TRUE, no.. = TRUE), "test.csv")
             # original content untouched by the failed write
             expect_identical(data.table::fread(fpath), dt_in)
          })


# Null-log guard ----------------------------------------------------------------

# A zero-byte log is genuinely empty and may be rebuilt.  A log that has bytes
# but cannot be read as a table must NOT be rebuilt - that would erase history.

test_that("Zero-byte logs are rebuilt, unreadable logs are refused and left untouched",
          {
             root_guard    <- clean_path(tempdir(), "slt_null_log_guard")
             roots_guard   <- list(root_a = clean_path(root_guard, "a"))
             central_guard <- clean_path(root_guard, "central")
             make_directory(roots_guard[["root_a"]])
             make_directory(central_guard)
             on.exit(unlink(root_guard, recursive = TRUE), add = TRUE)

             slt_guard <- suppressMessages(suppressWarnings(
                SLT$new(
                   root_list          = roots_guard
                   , central_log_root = central_guard
                   , verbose          = FALSE
                   , verbose_startup  = FALSE
                )
             ))
             priv          <- slt_guard$.__enclos_env__$private
             fpath_central <- slt_guard$return_dictionaries()$LOG_CENTRAL$path
             slt_guard$make_new_version_folder(version_name = "1990_01_01")
             fpath_version <- clean_path(roots_guard[["root_a"]], "1990_01_01", slt_guard$return_dictionaries()$log_path)
             make_directory(dirname(fpath_version))

             # --- zero-byte files: rebuilt with a 'create' row -------------------
             file.create(fpath_central)
             file.create(fpath_version)
             expect_identical(file.size(fpath_central), 0)

             dt_central <- suppressMessages(priv$read_central_log(fpath_central))
             expect_identical(dt_central$action, "create")
             expect_identical(dt_central$version_name, "CENTRAL_LOG")

             dt_version <- suppressMessages(priv$read_log(fpath_version))
             expect_identical(dt_version$action, "create")
             expect_identical(dt_version$version_name, "1990_01_01")

             # end-to-end: a mark over a zero-byte central log rebuilds it
             suppressMessages(slt_guard$mark_best(version_name = "1990_01_01", user_entry = list(comment = "rebuild")))
             expect_identical(data.table::fread(fpath_central)$action, c("create", "promote_best"))

             # --- files with bytes that are not a table: refused ----------------
             writeLines(c("", "   ", ""), fpath_central)   # whitespace only
             writeLines(c("", "   ", ""), fpath_version)
             expect_gt(file.size(fpath_central), 0)
             md5_central <- unname(tools::md5sum(fpath_central))
             md5_version <- unname(tools::md5sum(fpath_version))

             expect_error(priv$read_central_log(fpath_central), regexp = "Refusing to rebuild the log file")
             expect_error(priv$read_log(fpath_version),         regexp = "Refusing to rebuild the log file")

             # end-to-end: marking refuses and writes nothing to the damaged central log
             expect_error(
                suppressMessages(slt_guard$mark_keep(version_name = "1990_01_01", user_entry = list(comment = "should fail")))
                , regexp = "Refusing to rebuild the log file"
             )
             expect_identical(unname(tools::md5sum(fpath_central)), md5_central)
             expect_identical(unname(tools::md5sum(fpath_version)), md5_version)
             expect_identical(list.files(central_guard, all.files = TRUE, no.. = TRUE), basename(fpath_central))

             # --- header-only file (0 rows, N cols) is NOT "null": read normally --
             writeLines(paste(names(slt_guard$return_dictionaries()$log_schema), collapse = ","), fpath_central)
             expect_no_error(dt_header <- priv$read_central_log(fpath_central))
             expect_identical(nrow(dt_header), 0L)  # 0 rows, full schema; 'create' row is added on append, not on read
             expect_true(all(names(slt_guard$return_dictionaries()$log_schema) %in% names(dt_header)))

             # --- missing file is a clear error, not a silent rebuild ----------
             unlink(fpath_central)
             expect_error(priv$read_central_log(fpath_central), regexp = "does not exist")
          })


# Parallel smoke test -----------------------------------------------------------

# Many processes constructing SLT and querying at once against one populated
# central log must leave that log byte-identical.

test_that("Concurrent construction and querying never modifies the central log",
          {
             skip_on_os("windows")   # forked workers (mclapply) unavailable
             skip_on_cran()

             root_par    <- clean_path(tempdir(), "slt_parallel_smoke")
             roots_par   <- list(root_a = clean_path(root_par, "a"), root_b = clean_path(root_par, "b"))
             central_par <- clean_path(root_par, "central")
             lapply(c(unlist(roots_par), central_par), make_directory)
             on.exit(unlink(root_par, recursive = TRUE), add = TRUE)

             make_slt_par <- function(){
                suppressMessages(suppressWarnings(
                   SLT$new(
                      root_list          = roots_par
                      , central_log_root = central_par
                      , verbose          = FALSE
                      , verbose_startup  = FALSE
                   )
                ))
             }

             # populate the central log with a few real entries
             slt_par <- make_slt_par()
             slt_par$make_new_version_folder(version_name = "1990_01_01")
             slt_par$make_new_version_folder(version_name = "1990_01_02")
             slt_par$mark_best(version_name = "1990_01_01", user_entry = list(comment = "setup"))
             slt_par$mark_keep(version_name = "1990_01_02", user_entry = list(comment = "setup"))
             fpath_central <- slt_par$return_dictionaries()$LOG_CENTRAL$path
             md5_before    <- unname(tools::md5sum(fpath_central))
             mtime_before  <- file.mtime(fpath_central)

             # IMPORTANT: every child must return, never error - a forked child
             # that errors can delete the session tempdir on exit.
             # R CMD check limits tests to 2 cores; 20 workers interleaved on 2
             # cores still exercises concurrent construction.
             n_workers <- 20L
             results <- parallel::mclapply(
                seq_len(n_workers)
                , mc.cores = 2L
                , function(i){
                   tryCatch({
                      slt_i <- make_slt_par()
                      rb    <- slt_i$roundup_best()
                      rk    <- slt_i$roundup_keep()
                      if(!all(c("1990_01_01") %in% rb$version_name)) stop("roundup_best missing version")
                      if(!all(c("1990_01_02") %in% rk$version_name)) stop("roundup_keep missing version")
                      "ok"
                   }, error = function(e) paste("worker", i, "error:", conditionMessage(e)))
                }
             )

             expect_identical(unique(unlist(results)), "ok")
             expect_identical(unname(tools::md5sum(fpath_central)), md5_before)
             expect_identical(file.mtime(fpath_central), mtime_before)
             expect_identical(list.files(central_par, all.files = TRUE, no.. = TRUE), basename(fpath_central))
          })


# Symlink names passed as version_name ------------------------------------------

# Users naturally pass "latest"/"best"/"keep_<v>" where a version_name is
# expected. These must resolve to the real folder name, never be used raw.

test_that("Symlink names passed as version_name resolve to the real version",
          {
             root_res    <- clean_path(tempdir(), "slt_resolve_names")
             roots_res   <- list(root_a = clean_path(root_res, "a"))
             central_res <- clean_path(root_res, "central")
             lapply(c(unlist(roots_res), central_res), make_directory)
             on.exit(unlink(root_res, recursive = TRUE), add = TRUE)

             slt_res <- suppressMessages(suppressWarnings(
                SLT$new(root_list = roots_res, central_log_root = central_res, verbose = TRUE, verbose_startup = FALSE)
             ))
             v1          <- "1990_01_01"
             fpath_vlog  <- clean_path(roots_res[["root_a"]], v1, slt_res$return_dictionaries()$log_path)
             fpath_clog  <- slt_res$return_dictionaries()$LOG_CENTRAL$path
             fpath_disc  <- clean_path(roots_res[["root_a"]], slt_res$return_dictionaries()$report_fnames$discrepancies)
             read_actions <- function(f) data.table::fread(f)[, .(version_name, action)]

             suppressMessages(slt_res$make_new_version_folder(version_name = v1))
             suppressMessages(slt_res$mark_latest(version_name = v1, user_entry = list(comment = "setup")))
             expect_true(file.exists(clean_path(roots_res[["root_a"]], "latest")))

             # --- A plain version_name is untouched and prints no "Resolved" message
             expect_no_message(
                suppressWarnings(slt_res$mark_keep(version_name = v1, user_entry = list(comment = "plain")))
                , message = "Resolved"
             )
             expect_true(file.exists(clean_path(roots_res[["root_a"]], paste0("keep_", v1))))

             # --- Symptom B: mark("best", "latest") - wait, keep_ is on; use the keep_ name
             #     non-singleton symlink name resolves too
             expect_message(
                slt_res$unmark(version_name = paste0("keep_", v1), user_entry = list(comment = "unmark via keep_ name"))
                , regexp = "Resolved 'keep_1990_01_01' -> '1990_01_01'"
             )
             expect_false(file.exists(clean_path(roots_res[["root_a"]], paste0("keep_", v1))))

             # re-mark latest for the next checks
             suppressMessages(slt_res$mark_latest(version_name = v1, user_entry = list(comment = "setup 2")))

             # --- Symptom B: mark("best", "latest") logs under the REAL version name
             expect_message(
                slt_res$mark("best", version_name = "latest", user_entry = list(comment = "best via latest"))
                , regexp = "Resolved 'latest' -> '1990_01_01'"
             )
             expect_true(file.exists(clean_path(roots_res[["root_a"]], "best")))
             vlog <- read_actions(fpath_vlog)
             clog <- read_actions(fpath_clog)
             expect_false("latest" %in% vlog$version_name)
             expect_false("latest" %in% clog$version_name)
             expect_identical(tail(vlog$action, 1), "promote_best")
             expect_identical(tail(vlog$version_name, 1), v1)
             expect_false(file.exists(fpath_disc))   # no discrepancy report triggered

             # --- Symptom A: unmark("best") actually removes the symlink and logs it
             expect_message(
                slt_res$unmark(version_name = "best", user_entry = list(comment = "unmark via best"))
                , regexp = "Resolved 'best' -> '1990_01_01'"
             )
             expect_false(file.exists(clean_path(roots_res[["root_a"]], "best")))
             vlog <- read_actions(fpath_vlog)
             expect_identical(tail(vlog$action, 1), "demote_best")
             expect_identical(tail(vlog$version_name, 1), v1)

             # --- A non-existent name passes through unchanged (resolution never invents a name)
             #     NOTE: mark() on a non-existent version is currently a silent skip (pre-existing,
             #     see plan finding G) - so test the helper directly rather than the public method.
             expect_identical(
                slt_res$.__enclos_env__$private$resolve_version_name("does_not_exist_1990")
                , "does_not_exist_1990"
             )
          })

test_that("A symlink name that points to different versions in different roots is refused",
          {
             root_amb    <- clean_path(tempdir(), "slt_resolve_ambiguous")
             roots_amb   <- list(root_a = clean_path(root_amb, "a"), root_b = clean_path(root_amb, "b"))
             central_amb <- clean_path(root_amb, "central")
             lapply(c(unlist(roots_amb), central_amb), make_directory)
             on.exit(unlink(root_amb, recursive = TRUE), add = TRUE)

             slt_amb <- suppressMessages(suppressWarnings(
                SLT$new(root_list = roots_amb, central_log_root = central_amb, verbose = FALSE, verbose_startup = FALSE)
             ))
             v1 <- "1990_01_01"; v2 <- "1990_01_02"
             for(v in c(v1, v2)) slt_amb$make_new_version_folder(version_name = v)
             slt_amb$mark_latest(version_name = v1, user_entry = list(comment = "setup"))

             # hand-repoint root_b's `latest` at v2 so the two roots disagree
             latest_b <- clean_path(roots_amb[["root_b"]], "latest", normalize = FALSE)
             unlink(latest_b)
             file.symlink(clean_path(roots_amb[["root_b"]], v2), latest_b)

             logs <- c(
                clean_path(roots_amb[["root_a"]], v1, slt_amb$return_dictionaries()$log_path)
                , clean_path(roots_amb[["root_b"]], v1, slt_amb$return_dictionaries()$log_path)
                , clean_path(roots_amb[["root_b"]], v2, slt_amb$return_dictionaries()$log_path)
             )
             md5_before <- unname(tools::md5sum(logs))

             expect_error(
                slt_amb$unmark(version_name = "latest", user_entry = list(comment = "ambiguous"))
                , regexp = "points to different versioned folders in each root"
             )
             expect_error(
                slt_amb$mark_best(version_name = "latest", user_entry = list(comment = "ambiguous"))
                , regexp = "root_a: 1990_01_01.*root_b: 1990_01_02"
             )

             # nothing changed on disk
             expect_identical(unname(tools::md5sum(logs)), md5_before)
             expect_true(file.exists(clean_path(roots_amb[["root_a"]], "latest")))
             expect_true(file.exists(latest_b))
             expect_false(file.exists(clean_path(roots_amb[["root_a"]], "best")))
          })


# Reserved folder names ---------------------------------------------------------

test_that("Reserved symlink names are refused as version_name folders",
          {
             root_rsv    <- clean_path(tempdir(), "slt_reserved_names")
             roots_rsv   <- list(root_a = clean_path(root_rsv, "a"))
             central_rsv <- clean_path(root_rsv, "central")
             lapply(c(unlist(roots_rsv), central_rsv), make_directory)
             on.exit(unlink(root_rsv, recursive = TRUE), add = TRUE)
             slt_rsv <- suppressMessages(suppressWarnings(
                SLT$new(root_list = roots_rsv, central_log_root = central_rsv, verbose = FALSE, verbose_startup = FALSE)
             ))
             slt_rsv$make_new_version_folder(version_name = "1990_01_01")

             # creating a folder with a reserved name
             expect_error(slt_rsv$make_new_version_folder(version_name = "best"), regexp = "cannot be used as a version_name")
             expect_false(dir.exists(clean_path(roots_rsv[["root_a"]], "best")))

             # passing a symlink name when no such symlink exists
             expect_error(
                slt_rsv$mark_best(version_name = "latest", user_entry = list(comment = "c"))
                , regexp = "is the name of a shortcut \\(symlink\\), not a versioned folder"
             )
          })

test_that("A real folder using a reserved symlink name blocks marking and is reported",
          {
             root_occ    <- clean_path(tempdir(), "slt_reserved_occupied")
             roots_occ   <- list(root_a = clean_path(root_occ, "a"))
             central_occ <- clean_path(root_occ, "central")
             lapply(c(unlist(roots_occ), central_occ), make_directory)
             on.exit(unlink(root_occ, recursive = TRUE), add = TRUE)
             slt_occ <- suppressMessages(suppressWarnings(
                SLT$new(root_list = roots_occ, central_log_root = central_occ, verbose = FALSE, verbose_startup = FALSE)
             ))
             slt_occ$make_new_version_folder(version_name = "1990_01_01")
             dir.create(clean_path(roots_occ[["root_a"]], "best"))   # hand-made real folder

             expect_error(
                suppressMessages(slt_occ$mark_best(version_name = "1990_01_01", user_entry = list(comment = "c")))
                , regexp = "a real folder with that name already exists"
             )
             expect_identical(slt_occ$roundup_best()$version_name, character(0))   # not mistaken for a mark

             suppressMessages(slt_occ$.__enclos_env__$private$report_discrepancies(roots_occ[["root_a"]], verbose = FALSE))
             disc <- data.table::fread(clean_path(roots_occ[["root_a"]], slt_occ$return_dictionaries()$report_fnames$discrepancies))
             expect_true(any(grepl("real folder is using a name reserved", disc$discrepancy)))
          })


# Empty Directories ---------------------------------------------------------

test_that("SLT roundups run cleanly on empty directories",
          {
             expect_no_error({
                slt$roundup_best()
                slt$roundup_keep()
                slt$roundup_remove()
                slt$roundup_latest()
                slt$roundup_unmarked()
                # expect_message(
                slt$roundup_by_date(Sys.Date(), "lt")
                # , regexp = "Formatting date with time-zone"
                # )
             })
          })


# Paths ---------------------------------------------------------------------

# file names used in tests - draw from within tool to ensure consistency
fname_dv_log <- slt$return_dictionaries()$log_name
fname_report_key_versions <- slt$return_dictionaries()$report_fnames$all_logs_tool_symlink
fname_discrepnacy_report <- slt$return_dictionaries()$report_fnames$discrepancies

fpaths_dv_logs <- lapply(path_list_logs, function(path) clean_path(path, fname_dv_log))
fpaths_dv_logs <- lapply(fpaths_dv_logs, function(x) {names(x) <- names(dv_list); x})


# Integration - Make new folders -----------------------------------------------


test_that("SLT creates new folders",
          {
             lapply(dv_list, function(dv){
                slt$make_new_version_folder(version_name = dv)
             })
             dirlist <- unlist(lapply(root_list, function(root) clean_path(root, dv_list)))
             expect_true(
                all(file.exists(dirlist))
             )
          }
)



# Integration - Marking operations  --------------------------------------------

test_that("Mark best works",
          {
             slt$mark_best(version_name = dv_list[["1990_01_01"]], user_entry = ue_list[["best"]])
             expect_true(
                all(file.exists(clean_path(root_list, "best")))
             )
          })


test_that("Mark keep works",
          {
             slt$mark_keep(version_name = dv_list[["1990_01_01"]], user_entry = ue_list[["keep"]])
             expect_true(
                all(file.exists(clean_path(root_list, "keep_1990_01_01")))
             )
             # catch issues with multiple symlinks
             expect_false(
                all(file.exists(clean_path(root_list, "best")))
             )
          })

test_that("Mark remove works",
          {
             slt$mark_remove(version_name = dv_list[["1990_01_01"]], user_entry = ue_list[["remove"]])
             expect_true(
                all(file.exists(clean_path(root_list, "remove_1990_01_01")))
             )
             # catch issues with multiple symlinks
             expect_false(
                all(file.exists(clean_path(root_list, "keep_1990_01_01")))
             )
          })

test_that("Key version report has expected content",
          {
             report_key_versions <- data.table::fread(clean_path(root_list[[1]], fname_report_key_versions))
             # timestamp won't test well - keep testable columns
             report_key_versions <- report_key_versions[, .(log_id, user, version_name, version_path, action, comment)]
             report_key_versions_test <- structure(
                list(
                   log_id = 5L,
                   user = Sys.info()[["user"]],
                   version_name = "1990_01_01",
                   version_path = clean_path(root_base, "dir_1/1990_01_01"),
                   action = "promote_remove",
                   comment = "Testing mark remove"
                ),
                row.names = c(NA,
                              -1L),
                class = c("data.table", "data.frame")
             )

             expect_identical(report_key_versions, report_key_versions_test)

          })


test_that("There is no discrepancy report after SLT marking",
          {
             slt$make_reports()
             expect_false(
                file.exists(clean_path(root_list[[1]], fname_discrepnacy_report))
             )
          })

test_that("Mark unmark works",
          {
             slt$unmark(version_name = dv_list[["1990_01_01"]] , user_entry = ue_list[["unmark"]])
             expect_true(
                all(!file.exists(clean_path(root_list, "remove_1990_01_01")))
             )
          })

test_that("Key version report is empty after unmarking",
          {
             report_key_versions <- data.table::fread(clean_path(root_list[[1]], fname_report_key_versions))
             # timestamp won't test well - keep testable columns
             report_key_versions <- report_key_versions[, .(log_id, user, version_name, version_path, action, comment)]
             expect_equal(nrow(report_key_versions), 0)

          })


test_that("Expected folders exist after marking, unmarking, and running reports",
          {
             expect_equal(
                list.files(root_list[[1]])
                , c(
                   "1990_01_01"
                   , "1990_01_02"
                   , "1990_01_03"
                   , "report_all_logs.csv"
                   , "report_all_logs_non_symlink.csv"
                   , "report_all_logs_symlink.csv"
                   , "report_key_versions.csv"
                )
             )
          })

# Integration - Logs  ----------------------------------------------------------

test_that("Only logs exist so far",
          {
             dv_content_list <- lapply(path_list_logs, list.files, full.names = TRUE)

             dv_log_list <- lapply(path_list_logs, function(dv_content){
                fnames_logs <- list.files(dv_content, pattern = "log", full.names = TRUE)
             })
             expect_equal(dv_content_list, dv_log_list)
          })

test_that("Marked logs have correct structure",
          {
             dv_log_list <- lapply(path_list_logs, function(dv_content){
                fnames_logs <- list.files(dv_content, pattern = "log", full.names = TRUE)
             })
             expect_no_error(
                log_list <- lapply(dv_log_list, function(logs) lapply(logs, fread))
             )
             # First log should go through best, keep, remove and finally unmark steps correctly
             for(root in names(root_list)){
                expect_equal(
                   log_list[[root]][[1]][, .(log_id, user, version_name, version_path, action, comment)],
                   data.table(
                      log_id = 0:6
                      , user = rep(Sys.info()[["user"]], 7)
                      , version_name = rep(dv_list[[1]], 7)
                      , version_path = rep(clean_path(root_list[[root]], dv_list[[1]]), 7)
                      , action = c("create", "promote_best", "demote_best", "promote_keep", "demote_keep", "promote_remove", "demote_remove")
                      , comment = c("log created", "Testing mark best", "Testing mark keep", "Testing mark keep", "Testing mark remove", "Testing mark remove", "Testing mark unmark")
                   )
                )
             }
          })

test_that("Central log has expected content",
          {
             fpath_central_log <- slt$return_dictionaries()$LOG_CENTRAL$path
             log_central <- data.table::fread(fpath_central_log)
             # Timstamps won't test well, trim to testable columns
             log_central <- log_central[, .(log_id, user, version_name, version_path, action, comment)]
             log_central_test <- structure(
                list(
                   log_id = 0:12,
                   user = rep(Sys.info()[["user"]], 13),
                   version_name = c(
                      "CENTRAL_LOG",
                      "1990_01_01",
                      "1990_01_01",
                      "1990_01_01",
                      "1990_01_01",
                      "1990_01_01",
                      "1990_01_01",
                      "1990_01_01",
                      "1990_01_01",
                      "1990_01_01",
                      "1990_01_01",
                      "1990_01_01",
                      "1990_01_01"
                   ),
                   version_path = clean_path(
                      root_base,
                      c(
                         "log_symlinks_central.csv",
                         "dir_1/1990_01_01",
                         "dir_2/1990_01_01",
                         "dir_1/1990_01_01",
                         "dir_1/1990_01_01",
                         "dir_2/1990_01_01",
                         "dir_2/1990_01_01",
                         "dir_1/1990_01_01",
                         "dir_1/1990_01_01",
                         "dir_2/1990_01_01",
                         "dir_2/1990_01_01",
                         "dir_1/1990_01_01",
                         "dir_2/1990_01_01"
                      )
                   ),
                   action = c(
                      "create",
                      "promote_best",
                      "promote_best",
                      "demote_best",
                      "promote_keep",
                      "demote_best",
                      "promote_keep",
                      "demote_keep",
                      "promote_remove",
                      "demote_keep",
                      "promote_remove",
                      "demote_remove",
                      "demote_remove"
                   ),
                   comment = c(
                      "log created",
                      "Testing mark best",
                      "Testing mark best",
                      "Testing mark keep",
                      "Testing mark keep",
                      "Testing mark keep",
                      "Testing mark keep",
                      "Testing mark remove",
                      "Testing mark remove",
                      "Testing mark remove",
                      "Testing mark remove",
                      "Testing mark unmark",
                      "Testing mark unmark"
                   )
                ),
                row.names = c(NA,
                              -13L),
                class = c("data.table", "data.frame")
             )

             expect_identical(log_central, log_central_test)
          })


# Integration - Latest symlink -------------------------------------------------
# 2026 Mar 02 - new feature
# - testing after logs to avoid refactoring existing expectations

test_that("Mark latest works (creates singleton 'latest' symlink)",
          {
             slt$mark_latest(version_name = dv_list[["1990_01_02"]], user_entry = list(comment = "Testing mark latest"))
             expect_true(
                all(file.exists(clean_path(root_list, "latest")))
             )
          })

test_that("Latest singleton enforcement works (only one latest per root)",
          {
             # Mark a different version as latest
             slt$mark_latest(version_name = dv_list[["1990_01_03"]], user_entry = list(comment = "Testing latest singleton"))
             # Check that only one 'latest' symlink exists and points to new version
             expect_true(
                all(file.exists(clean_path(root_list, "latest")))
             )
             # Verify it points to the new version
             latest_resolved <- sapply(root_list, function(root) {
                latest_path <- clean_path(root, "latest")
                if(file.exists(latest_path)) {
                   target <- normalizePath(latest_path, mustWork = TRUE)
                   basename(target)
                } else {
                   NA
                }
             })
             expect_true(
                all(latest_resolved == "1990_01_03")
             )
          })

test_that("Auto-demotion from latest when marking keep",
          {
             # Start with latest marked (already on 1990_01_03 from prior test)
             expect_true(all(file.exists(clean_path(root_list, "latest"))))

             # Mark as keep - should auto-demote from latest
             slt$mark_keep(version_name = dv_list[["1990_01_03"]], user_entry = list(comment = "Testing auto-demote from latest"))
             expect_true(all(file.exists(clean_path(root_list, "keep_1990_01_03"))))
             expect_true(all(!file.exists(clean_path(root_list, "latest"))))

             # Clean up
             slt$unmark(version_name = dv_list[["1990_01_03"]], user_entry = list(comment = "Cleanup"))
          })

test_that("Auto-demotion from best when marking latest",
          {
             # Mark as best first
             slt$mark_best(version_name = dv_list[["1990_01_02"]], user_entry = list(comment = "Testing auto-demote from best"))
             expect_true(all(file.exists(clean_path(root_list, "best"))))

             # Mark as latest - should auto-demote from best
             slt$mark_latest(version_name = dv_list[["1990_01_02"]], user_entry = list(comment = "Testing auto-demote from best"))
             expect_true(all(file.exists(clean_path(root_list, "latest"))))
             expect_true(all(!file.exists(clean_path(root_list, "best"))))

             # Clean up
             slt$unmark(version_name = dv_list[["1990_01_02"]], user_entry = list(comment = "Cleanup"))
          })



# Integration - CSV readers ----------------------------------------------------

suppressMessages(
   suppressWarnings({ # idiosyncratic and benign cluster message

      slt_readcsv <- SLT$new(
         root_list = list(
            root_input  = root_list[["root_input"]],
            root_output = root_list[["root_output"]]
         )
         , central_log_root = root_base
         , csv_reader = "read.csv"
         , verbose = FALSE
      )
   })
)

test_that("bad csv_reader option produces expected error",
          {
             expect_error(
                SLT$new(
                   root_list = list(
                      root_input  = root_list[["root_input"]],
                      root_output = root_list[["root_output"]]
                   )
                   , central_log_root = root_base
                   , csv_reader = "readr"
                   , verbose = FALSE
                )
                , regexp = "csv_reader must be one of: fread, fread_quiet, read.csv, read.csv2"
             )
          })

test_that("read.csv alternate CSV writer round-trips through the atomic wrapper",
          {
             dir_atomic <- clean_path(tempdir(), "slt_atomic_writer_readcsv")
             make_directory(dir_atomic)
             on.exit(unlink(dir_atomic, recursive = TRUE), add = TRUE)
             fpath <- clean_path(dir_atomic, "test.csv")
             dt_in <- data.table::data.table(a = 1:3, b = c("x", "y", "z"))
             slt_readcsv$.__enclos_env__$private$csv_writer(dt_in, fpath, row.names = FALSE)
             expect_identical(data.table::setDT(utils::read.csv(fpath)), dt_in)
             expect_identical(list.files(dir_atomic, all.files = TRUE, no.. = TRUE), "test.csv")
          })

test_that("read.csv alternate CSV reader works",
          {
             expect_equal(
                slt$roundup_by_date(user_date = "2023-01-01", date_selector = "gte")[root_name == "root_input"]
                , slt_readcsv$roundup_by_date(user_date = "2023-01-01", date_selector = "gte")[root_name == "root_input"]
             )
          })



# Integration - Reports --------------------------------------------------------



test_that("Discrepancy report has expected structure after manual edits",
          {
             # Make some bad changes to logs
             # - this should create a discrepancy report
             log_list <- lapply(fpaths_dv_logs$root_input,  data.table::fread)
             # insert an erroneous line
             log_list[["1990_01_01"]] <- rbindlist(list(
                log_list[["1990_01_01"]]
                , data.table(
                   log_id         = 12
                   , timestamp    = ""
                   , user         = Sys.info()[["user"]]
                   , version_name = dv_list[["1990_01_01"]]
                   , version_path = path_list$root_input[["1990_01_01"]]
                   , action       = "non_slt_event"
                   , comment      = "This line was added by hand"
                )
             ))
             # delete a log
             system(paste("rm -f", fpaths_dv_logs$root_input[["1990_01_02"]]))
             log_list[["1990_01_02"]] <- NULL
             # add a new column
             log_list[["1990_01_03"]] <- log_list[["1990_01_03"]][, bad_column := "This column was not created by the tool"]
             # write back to disk
             lapply(names(log_list), function(log_name){
                fwrite(log_list[[log_name]], fpaths_dv_logs$root_input[[log_name]])
             })
             # make a bad symlink
             file.symlink(
                path_list$root_input[["1990_01_01"]]
                , clean_path(root_list$root_input, "bad_symlink")
             )

             # run reports
             slt$make_reports()
             # read discrepancy report
             discrepancy_report <- data.table::fread(clean_path(root_list[[1]], fname_discrepnacy_report))
             discrepancy_report[, timestamp := NULL]
             discrepancy_report[, version_path := clean_path(version_path)]

             expect_identical(
                discrepancy_report
                , structure(
                   list(
                      log_id         = c(NA, 12L, 12L, 0L, 1L, 2L, 3L, 4L)
                      , user         = c("", .user, .user, .user, .user, .user, .user, .user)
                      , version_name = c("", "1990_01_01", "1990_01_01", "1990_01_03", "1990_01_03", "1990_01_03", "1990_01_03", "1990_01_03")
                      , version_path = c(
                         ""
                         , clean_path(path_list$root_input[["1990_01_01"]])
                         , clean_path(path_list$root_input[["1990_01_01"]])
                         , clean_path(path_list$root_input[["1990_01_03"]])
                         , clean_path(path_list$root_input[["1990_01_03"]])
                         , clean_path(path_list$root_input[["1990_01_03"]])
                         , clean_path(path_list$root_input[["1990_01_03"]])
                         , clean_path(path_list$root_input[["1990_01_03"]])
                      )
                      , action       = c("", "non_slt_event", "non_slt_event", "create", "promote_latest", "demote_latest", "promote_keep", "demote_keep")
                      , comment      = c("", "This line was added by hand", "This line was added by hand", "log created", "Testing latest singleton", "Testing auto-demote from latest", "Testing auto-demote from latest", "Cleanup")
                      , dir_name     = c(file.path(clean_path(root_list$root_input), "bad_symlink"), "", "", "", "", "", "", "")
                      , bad_column   = c("", "", "", "This column was not created by the tool", "This column was not created by the tool", "This column was not created by the tool", "This column was not created by the tool", "This column was not created by the tool")
                      , vars_missing = c(NA, NA, NA, NA, NA, NA, NA, NA)
                      , vars_extra   = c("", "", "", "bad_column", "bad_column", "bad_column", "bad_column", "bad_column")
                      , discrepancy  = c(
                         "non-tool symlinks in root folder"
                         , "non-sequential log_ids"
                         , "invalid actions"
                         , "log schema differences - see vars_missing and vars_extra"
                         , "log schema differences - see vars_missing and vars_extra"
                         , "log schema differences - see vars_missing and vars_extra"
                         , "log schema differences - see vars_missing and vars_extra"
                         , "log schema differences - see vars_missing and vars_extra")
                   ), row.names = c(NA, -8L), class = c("data.table", "data.frame")
                )
             )
          })


test_that("demoting bad symlink causes no error and makes no log entry", {
   expect_no_error(
      slt$unmark(version_name = "1990_01_01", user_entry = list(comment = "testing unmark on hand-made symlink"))
   )
   expect_identical(
      data.table::fread(fpaths_dv_logs$root_input[["1990_01_01"]])[log_id == 12, action]
      , "non_slt_event"
   )
})

test_that("promoting folder with handmade symlink does not alter the handmade symlink", {
   expect_no_error(
      slt$mark_remove(version_name = "1990_01_01", user_entry = list(comment = "testing mark_remove on hand-made symlinked folder"))
   )
   expect_equal(
      normalizePath(clean_path(root_list$root_input, "bad_symlink"))
      , normalizePath(clean_path(root_list$root_input, "remove_1990_01_01"))
   )
})


test_that("make_new_log works when promoting an existing folder without a log",
          {
             # ensure no log before mark
             expect_false(file.exists(fpaths_dv_logs$root_input[["1990_01_02"]]))
             expect_no_error(
                {
                   # make the mark
                   slt$mark_keep(version_name = "1990_01_02", user_entry = list(comment = "testing mark_keep on folder without log"))
                   log_list <- lapply(fpaths_dv_logs$root_input, data.table::fread)
                }
             )
             # expect a new log with two rows
             expect_equal(
                log_list[["1990_01_02"]][, action]
                , c("create", "promote_keep")
             )
          })




# Integration - Roundups -------------------------------------------------------

test_that("Roundup by date throws expected errors",
          {
             expect_error(
                slt$roundup_by_date(user_date = "01-01-2023", date_selector = "gte")
                , regexp = "Invalid user_date. Must be formatted as YYYY MM DD, with one of these delimiters \\[-/_\\] between."
             )
             expect_error(
                slt$roundup_by_date(user_date = "2023-01-01", date_selector = "bte")
                , regexp = "Invalid date_selector. Must be one of \\(case-insensitive\\):.*\n  gt, gte, lt, lte, e"
             )
          })

test_that("Roundup by date works",
          {
             expect_equal(
                slt$roundup_by_date(user_date = "2023-01-01", date_selector = "gte")[root_name == "root_input", ]
                , data.table(root_name = rep("root_input", 6)
                             , version_name = c(rep(dv_list[[1]],3)
                                                , rep(dv_list[[2]],2)
                                                , rep(dv_list[[3]],1))
                             , dir_name = clean_path( clean_path(root_list$root_input) # for Mac
                                                      , c(
                                                         dv_list[[1]]
                                                         , "bad_symlink"
                                                         , sprintf("remove_%s", dv_list[[1]])
                                                         , dv_list[[2]]
                                                         , sprintf("keep_%s", dv_list[[2]])
                                                         , dv_list[[3]]
                                                      )
                                                      , normalize = FALSE)
                             , dir_name_resolved = c(rep(clean_path(path_list$root_input[1]),3)
                                                     , rep(clean_path(path_list$root_input[2]),2)
                                                     , rep(clean_path(path_list$root_input[3]),1))
                )
             )
          })

test_that("Roundup by date version_name return_as works",
          {
             expect_equal(
                slt$roundup_by_date(user_date = "2023-01-01", date_selector = "gte", return_as = "version_name")
                , c("1990_01_01", "1990_01_02", "1990_01_03")
             )
          })



slt$mark_best(version_name = "1990_01_03", user_entry = list(comment = "prepping for roundup best"))

test_that("Roundup best works",
          {
             expect_equal(
                slt$roundup_best()[root_name == "root_input", version_name]
                ,"1990_01_03"
             )
             # and check version_name return structure - only testing once for 'best'
             expect_equal(
                slt$roundup_best(return_as = "version_name")
                ,"1990_01_03"
             )
          })

test_that("Roundup keep works",
          {
             expect_equal(
                slt$roundup_keep()[root_name == "root_input", version_name]
                ,"1990_01_02"
             )
          })

test_that("Roundup remove works",
          {
             expect_equal(
                slt$roundup_remove()[root_name == "root_input", version_name]
                ,"1990_01_01"
             )
          })

slt$mark_latest(version_name = "1990_01_02", user_entry = list(comment = "prepping for roundup latest"))

test_that("Roundup latest works",
          {
             expect_equal(
                slt$roundup_latest()[root_name == "root_input", version_name]
                ,"1990_01_02"
             )
          })

lapply(dv_list, function(dv) slt$unmark(dv, list(comment = "unmarking for roundup test")))

test_that("Roundup unmarked works, even with a handmande-symlink pointing to a folder",
          {
             unmarked_dt <- slt$roundup_unmarked()
             expect_equal(
                unmarked_dt[root_name == "root_input", version_name]
                , c("1990_01_02", "1990_01_03")
             )

             expect_equal(
                unmarked_dt[root_name == "root_output", version_name]
                , c("1990_01_01", "1990_01_02", "1990_01_03")
             )

          })


# Integration - Deletion -------------------------------------------------------

test_that("Folder deletion works, and only for a folder marked _remove",
          {
             expect_message(
                slt$delete_version_folders(
                   version_name        = "1990_01_02"
                   , user_entry        = list(comment = "testing folder deletion without marking")
                   , require_user_input = FALSE
                )
                , regexp = "No valid `remove_` symlink found:"
             )
             expect_true(
                file.exists(path_list$root_input[["1990_01_02"]])
             )
             slt$mark_remove(version_name = "1990_01_02", user_entry = list(comment = "testing folder deletion"))
             expect_message(
                slt$delete_version_folders(
                   version_name         = "1990_01_02"
                   , user_entry         = list(comment = "testing folder deletion")
                   , require_user_input = FALSE
                )
                , regexp = paste0("Deleting ", clean_path(path_list$root_input[["1990_01_02"]]))
             )
             expect_false(
                file.exists(path_list$root_input[["1990_01_02"]])
             )
          })


# Integration - New Versions ---------------------------------------------------

test_that("get_common_new_version_name works",
          {
             expect_equal(
                slt$get_common_new_version_name()
                , format(Sys.Date(), "%Y_%m_%d.01")
             )
          })

new_version <- slt$get_common_new_version_name()
slt$make_new_version_folder(new_version)
dir.create(clean_path(root_list$root_input, format(Sys.Date(), "%Y_%m_%d.04")))

test_that("get_common_new_version_name finds the max possible version across all roots",
          {
             expect_equal(
                slt$get_common_new_version_name()
                , format(Sys.Date(), "%Y_%m_%d.05")
             )
          })

# Test private methods ---------------------------------------------------------

# 2024 Oct 16 - put this off for now and focus on some integration tests first




# Clean up ---------------------------------------------------------------------

# need files to persist during tests, so cannot use `withr::local_file()`
# - instead, clean up after tests run, or tests will fail on subsequent runs
test_that(
   "Cleanup is complete",
   {
      # unlink(root_base, recursive = TRUE)
      system(paste("rm -rf", root_base))
      expect_error(dir_tree(root_base), regexp = "Failed to search directory.*no such file or directory")
   }
)


