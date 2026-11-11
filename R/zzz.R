# Load hook --------------------------------------------------------------------
# The symlink registry itself (environment, factories, accessors) is in
# R/registry_session.R; its on-disk, per-root counterpart in R/registry_disk.R.
#
# Created when the package loads, persists for the session.
# CRAN-compliant: no global assignment, scoped to package namespace.
# Built-in bindings are locked so they cannot be overwritten by users.
.onLoad <- function(libname, pkgname){

   # Symlink-based Finite State Machine
   #
   # State Transitions:
   # [unmarked] <-> [latest] <-> [best]
   #     ^            ^           ^
   #     v            v           v
   #   [keep]       [keep]      [keep]
   #     ^            ^           ^
   #     v            v           v
   #  [remove]     [remove]    [remove]
   #
   # Rules:
   # 1. Only ONE symlink per folder at any time
   # 2. Only ONE 'best' folder per project root (Highlander rule)
   # 3. Only ONE 'latest' folder per project root (Highlander rule)
   # 4. Multiple 'keep' folders allowed
   # 5. Multiple 'remove' folders allowed
   # 6. 'best' <-> 'latest' transitions auto-demote the conflicting state
   #
   # Symlink Naming Convention:
   # - 'best':   singleton symlink named "best" (no version in name)
   # - 'latest': singleton symlink named "latest" (no version in name)
   # - 'keep':   multiple symlinks named "keep_<version_name>"
   # - 'remove': multiple symlinks named "remove_<version_name>"

   # Singleton-version marks (best, latest) follow the Highlander rule: one per
   # root directory. Multiple-version marks (keep, remove) may be applied to
   # many versions in the same root. Shapes come from `get_builtin_symlink_types()`
   # (R/registry_session.R).
   builtin_types <- get_builtin_symlink_types()
   builtin_names <- names(builtin_types)
   symlink_registry_builtins <- lapply(builtin_names, function(symlink_name) {
      return(make_symlink_registry_entry(
         symlink_name
         , builtin_types[[symlink_name]]
         , is_builtin = TRUE
      ))
   })
   names(symlink_registry_builtins) <- builtin_names

   for(symlink_name in builtin_names){
      assign(
         symlink_name
         , symlink_registry_builtins[[symlink_name]]
         , envir = .symlink_registry
      )
   }

   # Decide the link mechanism for this OS once per session (symlink vs
   # junction) - see R/links.R; this is the only OS check in the package
   link_set_backend()

   # Prevent user from overwriting built-in entries
   lock_some_bindings(
      objs  = builtin_names
      , env = .symlink_registry
   )

}
