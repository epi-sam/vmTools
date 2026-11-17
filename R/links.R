# Cross-platform directory links ----------------------------------------------
#
# The ONLY file in the package that knows about operating systems.
# - POSIX (Linux, macOS): symlinks via file.symlink()
# - Windows:              junctions via Sys.junction() - no privilege needed,
#                         local NTFS disks only (network drives refuse them)
#
# Backend registry (`.link_backends`): one entry per link mechanism, each a
# named list of the OS-specific functions. `.onLoad()` picks one by OS and
# stores it; every public function below delegates to it. Adding a mechanism
# = one new registry entry + one row in the OS map. No `if`/`switch` on OS
# anywhere else.
#
# Dispatch: one OS lookup at load time, none afterwards
#
#   library(vmTools)
#         |
#         v
#   .onLoad()                    R/zzz.R
#         |
#         v
#   link_set_backend()           Sys.info()[["sysname"]]
#         |                        Linux   -> "symlink"
#         |                        Darwin  -> "symlink"
#         |                        Windows -> "junction"
#         |                        other   -> stop()
#         v
#   .link_backends[[name]]       registry: one list of functions per mechanism
#         |
#         |   member                  $symlink (POSIX)        $junction (Windows)
#         |   ----------------------  ----------------------  ------------------------
#         |   name                    "symlink"               "junction"
#         |   probe_failure_is_fatal  FALSE                   TRUE
#         |   is_link(path)           Sys.readlink() != ""    normalizePath() differs, or dead
#         |   get_target(path)        Sys.readlink()          normalizePath(), or PowerShell
#         |   create(target, link)    file.symlink()          Sys.junction()
#         |   failure_reason(dir)     fixed text              drive letter + drive type
#         |   drive_type(path)        "Unknown"               PowerShell DriveInfo
#         |
#         |   Windows-only free helpers used by the junction backend:
#         |   windows_junction_is_dead(path)   dir.exists() but file.mtime() is NA
#         |   windows_junction_target(path)    PowerShell (Get-Item).Target
#         |
#         |   shape check: assert_x_in_y(.link_backend_members, names(backend))
#         v
#   .link_state$backend          stored once, never changed
#         ^
#         |   link_get_backend()   read on every call; stops if unset
#         |
#   internal-public wrappers (OS-agnostic, same body on every platform):
#   path_is_link(path) ->  backend$is_link(path)
#   etc.
#   link_resolve / link_list / link_remove / dir_supports_links build on these
#
#   wrappers validate arguments and clean_path() first; a backend function
#   receives a cleaned path and performs only the OS-specific task.
#
# Link primitives (internal-public contract for the rest of the package):
#   link_resolve(path)          list(is_link, target, dangling)
#   link_create(target, link)   never overwrites; cleans up after a failed attempt
#   link_remove(link)           refuses anything that is not a link; never touches the target
#
# "Dangling" = the link still exists but its target folder is gone (e.g. a
# version folder deleted by hand without unmarking). file.exists() says FALSE
# because it follows the link; Sys.readlink() still returns the old target.
# link_resolve() reports it so callers can warn or remove it; link_remove()
# works on a dangling link.
#
# Windows: base R cannot read a junction's stored target (Sys.readlink() is
# always ""), and a dead junction looks like a real empty folder to
# dir.exists(), file.exists() and normalizePath(). The one base R tell is
# file.mtime(): NA on a dead junction because Windows cannot open it, a date
# on a real folder. The junction backend uses that as the detector and then
# asks PowerShell for the stored target, on that rare branch only. Probed on
# Windows 2026-10-22; history in .claude/DANGLING_MATRIX.md.
#
# Conventions:
# - no system()/system2() except two PowerShell calls in the junction backend,
#   both off the main path: drive_type() after a link has failed, and
#   windows_junction_target() after a dead junction has been detected
#   No base R altenatives exist for these two PowerShell calls, and this
#   avoids an additional fs dependency (using libuv).
# - link_remove() never recurses; link_create() never overwrites
# - every stop() names the path, the cause, and what to do

# package-level state, filled once in .onLoad()
.link_state <- new.env(parent = emptyenv())

# every backend must provide exactly these members
.link_backend_members <- c(
   "name"
   , "probe_failure_is_fatal"
   , "is_link"
   , "get_target"
   , "create"
   , "failure_reason"
   , "drive_type"
)

# Backend registry ------------------------------------------------------------
# Each function receives paths already validated and cleaned by its public
# wrapper, and does only the OS-specific part of the job.
.link_backends <- list(

   ## POSIX: real symlinks ----------------------------------------------------
   symlink = list(

      name = "symlink"

      # Why a failed probe is NOT fatal here: on POSIX the probe can only fail
      # because the root is not writable (a colleague's folder, a read-only
      # share). Symlinks in it are still readable with readlink(), so
      # roundups and reports work - the SymlinkComposer use case: build a
      # tool on roots you do not own, read-only. A mark on such a root fails
      # later, at the moment it is attempted, with a plain message.
      , probe_failure_is_fatal = FALSE


      # beware: .link_backends$symlink$is_link("/some/path/link/") with a
      # trailing slash returns FALSE - always use the public wrapper
      # path_is_link()

      # readlink is authoritative, including dangling links
      , is_link = function(path){
         raw <- Sys.readlink(path)             # NA = path missing, "" = not a link
         return(!is.na(raw) && nzchar(raw))    # nzchar(NA) is TRUE, so test NA first
      }

      # raw link text, made absolute if relative; unchanged if dangling
      , get_target = function(path){
         raw <- Sys.readlink(path)
         if (!startsWith(raw, "/")) raw <- file.path(dirname(path), raw)
         return(clean_path(raw, normalize = TRUE, mustWork = FALSE))
      }

      , create = function(target, link){
         return(isTRUE(suppressWarnings(file.symlink(target, link))))
      }

      , failure_reason = function(dir){
         return(sprintf("the file system refused to create a symlink (shortcut) in: %s", dir))
      }

      , drive_type = function(path) {
         return("Unknown")
      }
   )

   ## Windows: NTFS junctions -------------------------------------------------
   , junction = list(

      name = "junction"

      # Why a failed probe IS fatal here: a root that refuses junctions is a
      # network drive, and Windows cannot READ links on those either - links
      # made from Linux appear as missing (Qumulo) or as plain folders (some
      # mounts). Roundups would be silently empty or wrong, so the tool
      # refuses to build. Contrast the symlink backend above.
      , probe_failure_is_fatal = TRUE

      # base R cannot read reparse-point tags, but normalizePath() follows
      # junctions and symlinks: a link is a path whose resolved form differs
      # from its parent's resolved form plus its own leaf name.
      # A dead junction has nothing to follow, so it resolves to itself like
      # a real folder; windows_junction_is_dead() tells the two apart and
      # windows_junction_target() reads the stored target. A blank answer from
      # PowerShell means Windows does not call it a junction (a real folder R
      # could not open), so the heuristic is overruled; no answer at all keeps
      # the heuristic's verdict.
      # `winslash = "/"` so both sides of the compare use the same separator;
      # the public wrapper must already strip any trailing slash using
      # clean_path().
      , is_link = function(path){
         if (!dir.exists(path)) return(FALSE)
         resolved <- normalizePath(path, winslash = "/", mustWork = FALSE)
         expected <- file.path(
            normalizePath(dirname(path), winslash = "/", mustWork = FALSE)
            , basename(path)
         )
         if (!identical(tolower(resolved), tolower(expected))) return(TRUE)   # live: resolves elsewhere
         if (!windows_junction_is_dead(path)) return(FALSE)                 # real folder
         target <- windows_junction_target(path)
         return(is.na(target) || nzchar(target))                            # dead junction, or unanswerable
      }

      # dead junction: the stored target from PowerShell, NA if it cannot answer
      # (is_link() is called first, so a blank answer never reaches here)
      , get_target = function(path){
         if (windows_junction_is_dead(path)) {
            target <- windows_junction_target(path)
            if (is.na(target) || !nzchar(target)) return(NA_character_)
            return(target)
         }
         return(normalizePath(path, winslash = "/", mustWork = FALSE))
      }

      # Sys.junction() exists only in Windows builds of R - fetch it by name
      # so POSIX R CMD check does not flag an undefined function
      , create = function(target, link){
         return(isTRUE(suppressWarnings(get("Sys.junction", envir = baseenv())(target, link))))
      }

      , failure_reason = function(dir){
         letter <- toupper(substr(clean_path(dir), 1, 1))
         type   <- windows_drive_type(dir)
         reason <- switch(
            type
            , Network   = sprintf("the folder is on a network drive (%s:) - Windows cannot create symlinks (shortcuts) or junctions on network drives. Use a folder on a local disk such as C:, or run the Symlink Tool from the Linux cluster against this folder", letter)
            , Removable = sprintf("the folder is on a removable drive (%s:) that does not support junctions", letter)
            , Fixed     = sprintf("the local drive (%s:) refused to create a junction", letter)
            , sprintf("the drive (%s:) does not support junctions", letter)
         )
         return(reason)
      }

      # PowerShell, failure path only, used to word an error message after a
      # link has already failed. The other shell call: windows_junction_target().
      , drive_type = function(path){
         letter <- toupper(substr(clean_path(path), 1, 1))
         if (!grepl("^[A-Z]$", letter)) return("Unknown")   # UNC path, no drive letter
         out <- tryCatch(
            system2(
               "powershell"
               , c("-NoProfile", "-Command", shQuote(sprintf("([System.IO.DriveInfo]'%s:').DriveType", letter)))
               , stdout = TRUE
               , stderr = FALSE
            )
            , error   = function(e) character(0)
            , warning = function(w) character(0)
         )
         out <- trimws(out[1])

         if (is.na(out) || !out %in% c("Fixed", "Network", "Removable")) return("Unknown")

         return(out)
      }
   )
)

# ---- Critical OS Backend -----------------------------------------------------------------

#' Pick the link backend for this operating system and store it
#'
#' Called once by `.onLoad()`. The only place in the package that inspects the
#' operating system. Supported: Linux, macOS (Darwin) and Windows; anything
#' else stops. Windows uses junctions, all others use symlinks.
#'
#' @return (list) the stored backend, invisibly - see `link_get_backend()`
#' @family links
#' @keywords internal
link_set_backend <- function(){
   os           <- Sys.info()[["sysname"]]
   os_supported <- c(
      Linux     = "symlink"
      , Darwin  = "symlink"
      , Windows = "junction"
   )
   if (!os %in% names(os_supported)) {
      stop(
         sprintf("vmTools supports Linux, macOS and Windows, but this is: %s", os)
         , "\n    - Symlinks (shortcuts) are created differently on each operating system, and this one is untested."
      )
   }
   backend <- .link_backends[[os_supported[[os]]]]
   assert_x_in_y(.link_backend_members, names(backend))
   assign("backend", backend, envir = .link_state)
   return(invisible(backend))
}


#' Return list of OS-specific backend utilities
#'
#' Reads the backend stored by `link_set_backend()` in `.onLoad()`. Never
#' inspects the operating system itself.
#'
#' @return (list) the backend: `name` (chr) plus the functions `is_link`,
#'   `get_target`, `create`, `failure_reason`, `drive_type`
#' @family links
#' @keywords internal
link_get_backend <- function(){
   backend <- .link_state$backend
   if (is.null(backend)) {
      stop(
         "The link backend has not been set - vmTools was not loaded normally."
         , "\n    - Load the package with library(vmTools) or devtools::load_all() so .onLoad() runs."
      )
   }
   return(backend)
}


# ---- Private Helpers-----------------------------------------------------------------


#' Is this path a symlink (shortcut) or junction?
#'
#' @param path (chr) one path
#'
#' @return (lgl) TRUE if `path` is a link; FALSE for real folders, files and
#'   paths that do not exist. Dangling links count as links on every OS
#' @family links
#' @keywords internal
path_is_link <- function(path){
   assert_scalar_not_empty(path)
   path    <- clean_path(path, normalize = FALSE)
   is_link <- link_get_backend()$is_link(path)
   return(is_link)
}

#' Where does a link point?
#'
#' @param path (chr) one path
#'
#' @return (chr) absolute path of the link's target, normalized when the target
#'   exists, or `NA` if `path` is not a link (also `NA` for a dead junction
#'   whose stored target Windows could not report)
#' @family links
#' @keywords internal
link_get_target <- function(path){
   if (!path_is_link(path)) return(NA_character_)
   path   <- clean_path(path, normalize = FALSE)
   target <- link_get_backend()$get_target(path)
   return(target)
}


#' List the folders and links directly under a root
#'
#' Replaces parsing `ls -l`. Files are excluded; dangling links are included
#' so callers can report or remove them.
#'
#' @param root (chr) one existing folder
#'
#' @return (data.table) one row per entry: `leaf` (chr), `path` (chr),
#'   `is_link` (lgl), `target` (chr or NA), `is_dangling` (lgl)
#' @family links
#' @keywords internal
link_list <- function(root){
   assert_scalar_not_empty(root)
   assert_dir_exists(root)
   root    <- clean_path(root)
   entries <- clean_path(
      list.files(root, full.names = TRUE, all.files = TRUE, no.. = TRUE)
      , normalize = FALSE
   )
   is_link <- vapply(entries, path_is_link, logical(1), USE.NAMES = FALSE)
   entries <- entries[dir.exists(entries) | is_link]
   info    <- lapply(entries, link_resolve)

   return(
      data.table::data.table(
         leaf       = basename(entries)
         , path     = entries
         , is_link  = vapply(info, `[[`, logical(1),   "is_link")
         , target   = vapply(info, `[[`, character(1), "target")
         , is_dangling = vapply(info, `[[`, logical(1), "is_dangling")
      )
   )
}



#' Can links be created inside this folder?
#'
#' Probe: make a throwaway folder and a link to it inside `dir`, check the
#' link resolves, then remove both. Uniquely named so parallel jobs probing
#' the same root never collide. Cleans up on exit, including the litter a
#' failed junction leaves behind.
#'
#' @param dir (chr) folder to probe, e.g. a tool root
#'
#' @return (lgl) TRUE if a link round-trip works in `dir`
#' @family links
#' @export
#'
#' @examples
#' dir_supports_links(tempdir())
dir_supports_links <- function(dir){
   assert_scalar_not_empty(dir)
   if (!dir.exists(dir)) return(FALSE)
   dir    <- clean_path(dir)
   stem   <- basename(tempfile(pattern = ".vmtools_probe_"))
   target <- file.path(dir, sprintf("%s_target", stem))
   link   <- file.path(dir, sprintf("%s_link", stem))

   on.exit(
      {
         if (path_is_link(link)) try(link_remove(link), silent = TRUE)   # link first, non-recursive
         unlink(c(link, target), recursive = TRUE)                       # litter + probe folder
      }
      , add = TRUE
   )

   dir.create(target, showWarnings = FALSE)
   if (!dir.exists(target)) return(FALSE)

   link_created <- tryCatch({
      link_create(target, link)
      TRUE
   }
      , error = function(e) FALSE
   )

   links_supported <- link_created && path_is_link(link) && dir.exists(link)
   return(links_supported)
}


#' Plain-language reason a link could not be created in a folder
#'
#' Called on the failure path only. Generic causes (missing folder, no write
#' permission) are checked here; anything else is explained by the backend.
#'
#' @param dir (chr) the folder the link was meant to live in
#'
#' @return (chr) one sentence for an error message
#' @family links
#' @keywords internal
link_failure_reason <- function(dir){
   dir <- clean_path(dir, normalize = FALSE)

   if (!dir.exists(dir)) {
      reason <- sprintf("the folder does not exist: %s", dir)
   } else if (file.access(dir, mode = 2) != 0) {
      reason <- sprintf("you do not have permission to write in: %s", dir)
   } else {
      reason <- link_get_backend()$failure_reason(dir)
   }

   return(reason)
}


# ---- Windows Backend Helpers -----------------------------------------------------------------

#' What kind of drive is a path on?
#'
#' Windows answers `Fixed`, `Network` or `Removable` via the junction backend;
#' every other backend answers `Unknown`.
#'
#' @param path (chr) one path
#'
#' @return (chr) `"Fixed"`, `"Network"`, `"Removable"`, or `"Unknown"`
#' @family links
#' @keywords internal
windows_drive_type <- function(path){
   assert_scalar_not_empty(path)
   type <- link_get_backend()$drive_type(path)
   return(type)
}

#' Is this Windows folder entry a junction whose target is gone?
#'
#' Base R's one tell for a dead junction: the entry passes `dir.exists()`
#' (Windows lists its attributes without following it) but `file.mtime()` is
#' `NA` because Windows cannot open it to read its times. A real folder and a
#' live junction both report a date. Only meaningful under the junction
#' backend; on POSIX a dead symlink fails `dir.exists()` first.
#'
#' @param path (chr) one path, already cleaned
#'
#' @return (lgl) TRUE for a dead junction (or, rarely, a real folder R cannot
#'   open - `windows_junction_target()` settles that)
#' @family links
#' @keywords internal
windows_junction_is_dead <- function(path){
   assert_scalar_not_empty(path)
   is_dead <- dir.exists(path) && is.na(suppressWarnings(file.mtime(path)))
   return(is_dead)
}

#' Ask Windows where a junction points
#'
#' PowerShell reads the target stored inside the junction without following
#' it, so a dead junction still answers. Called only after
#' `windows_junction_is_dead()` fires, so the shell cost (about half a second)
#' is paid once per dead junction, never during a normal scan.
#'
#' @param path (chr) one path, already cleaned
#'
#' @return (chr) the stored target with forward slashes; `""` when Windows says
#'   the entry is not a junction; `NA` when PowerShell gave no usable answer
#' @family links
#' @keywords internal
windows_junction_target <- function(path){
   assert_scalar_not_empty(path)
   path_win <- gsub("/", "\\", path, fixed = TRUE)
   path_win <- gsub("'", "''", path_win, fixed = TRUE)   # PowerShell single-quote escape
   cmd      <- sprintf("(Get-Item -LiteralPath '%s' -Force).Target", path_win)
   out <- tryCatch(
      system2(
         "powershell"
         , c("-NoProfile", "-NonInteractive", "-Command", shQuote(cmd))
         , stdout = TRUE
         , stderr = FALSE
      )
      , error   = function(e) NULL
      , warning = function(w) NULL   # non-zero exit status arrives as a warning
   )
   if (is.null(out)) return(NA_character_)
   status <- attr(out, "status")
   if (!is.null(status) && status != 0) return(NA_character_)
   out <- trimws(out)
   out <- out[nzchar(out)]
   if (length(out) == 0) return("")
   target <- clean_path(out[1], normalize = FALSE)
   return(target)
}



# ---- Internal-Public Primitives -----------------------------------------------------------------

# Contracts with the rest of the package

#' Describe a path: is it a link, where does it go, is the target missing?
#'
#' @param path (chr) one path
#'
#' @return (list) `is_link` (lgl), `target` (chr or NA), `is_dangling` (lgl)
#' @family links
#' @keywords internal
link_resolve <- function(path){
   is_link <- path_is_link(path)
   target  <- if (is_link) link_get_target(path) else NA_character_
   return(
      list(
         is_link    = is_link
         , target   = target
         , is_dangling = is_link && !file.exists(target)
      )
   )
}

#' Create a link to a folder
#'
#' Never overwrites. A failed junction attempt leaves an empty real folder at
#' `link`; that litter is removed before the error is raised.
#'
#' @param target (chr) existing folder the link should point to
#' @param link (chr) path of the link to create; must not exist
#'
#' @return (chr) `link`, invisibly. Stops with a plain message if the target
#'   is missing, the link path is taken, or the file system refuses the link.
#' @family links
#' @keywords internal
link_create <- function(target, link){
   assert_scalar_not_empty(target)
   assert_scalar_not_empty(link)
   target <- clean_path(target, normalize = TRUE)
   link   <- clean_path(link, normalize = FALSE)

   if (!dir.exists(target)) {
      stop(
         sprintf("Cannot create a symlink (shortcut) at: %s", link)
         , sprintf("\n    - The folder it should point to does not exist: %s", target)
         , "\n    - Nothing has been changed."
      )
   }
   if (path_is_link(link)) {
      stop(
         sprintf("Cannot create a symlink (shortcut) at: %s", link)
         , sprintf("\n    - A symlink with that name already exists and points to: %s", link_get_target(link))
         , "\n    - Remove it first with link_remove(). Nothing has been changed."
      )
   }
   if (file.exists(link)) {
      stop(
         sprintf("Cannot create a symlink (shortcut) at: %s", link)
         , "\n    - A real folder or file with that name already exists there."
         , "\n    - Rename or remove it first. Nothing has been changed."
      )
   }
   if (!dir.exists(dirname(link))) {
      stop(
         sprintf("Cannot create a symlink (shortcut) at: %s", link)
         , sprintf("\n    - The folder it should live in does not exist: %s", dirname(link))
         , "\n    - Nothing has been changed."
      )
   }

   ok <- link_get_backend()$create(target, link)

   if (!ok) {
      # Sys.junction() creates the folder first, then fails to attach the
      # reparse point - remove that empty folder so nothing is left behind.
      if (dir.exists(link) && !path_is_link(link) && length(list.files(link, all.files = TRUE, no.. = TRUE)) == 0) {
         unlink(link, recursive = TRUE)
      }
      stop(
         sprintf("Could not create a symlink (shortcut) at: %s", link)
         , sprintf("\n    - Reason: %s", link_failure_reason(dirname(link)))
         , "\n    - Nothing has been left behind."
      )
   }

   return(invisible(link))
}


#' Remove a link, leaving its target untouched
#'
#' Refuses anything that is not a link, so it can never delete a real folder.
#' Uses a non-recursive `unlink()`, which removes the link itself and never
#' follows it into the target.
#'
#' @param link (chr) path of the link to remove
#'
#' @return (lgl) TRUE, invisibly. Stops with a plain message if `link` is a
#'   real folder or file, does not exist, or cannot be removed.
#' @family links
#' @keywords internal
link_remove <- function(link){
   assert_scalar_not_empty(link)
   link <- clean_path(link, normalize = FALSE)

   if (!path_is_link(link)) {
      if (file.exists(link)) {
         stop(
            sprintf("Refusing to remove: %s", link)
            , "\n    - It is a real folder or file, not a symlink (shortcut)."
            , "\n    - This tool only removes symlinks. Nothing has been changed."
         )
      }
      stop(
         sprintf("Cannot remove the symlink (shortcut) at: %s", link)
         , "\n    - Nothing exists at that path. Nothing has been changed."
      )
   }

   status <- suppressWarnings(unlink(link, recursive = FALSE))

   if (status != 0 || path_is_link(link)) {
      stop(
         sprintf("Could not remove the symlink (shortcut) at: %s", link)
         , "\n    - R was not able to delete it. On Windows this happens for symlinks made by other programs;"
         , "\n      this tool only makes junctions, which it can remove."
         , "\n    - Remove it by hand (Windows: `rmdir` in a command prompt). Nothing has been changed."
      )
   }

   return(invisible(TRUE))
}
