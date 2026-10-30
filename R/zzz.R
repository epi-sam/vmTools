# ---- Load hook --------------------------------------------------------------
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

    symlink_registry_builtins <- list(
        # Singleton-version marks
        # - These marks follow the Higlander rule: there can be only one per root directory
        best     = list(is_singleton = TRUE)
        , latest = list(is_singleton = TRUE)

        # Multiple-version marks
        # - These marks can be applied to multiple versions within the same root directory
        , keep   = list(is_singleton = FALSE)
        , remove = list(is_singleton = FALSE)
    )

    for(idx in seq_along(symlink_registry_builtins)) {

        entry        <- symlink_registry_builtins[[idx]]
        symlink_name <- names(symlink_registry_builtins)[idx]

        entry$symlink_suffix_fn <- make_symlink_suffix_fn(entry$is_singleton)
        entry$extract_regex     <- make_extract_regex(entry$is_singleton, symlink_name)

        symlink_registry_builtins[[idx]] <- entry
    }

    for(symlink_name in names(symlink_registry_builtins)){
        assign(
            symlink_name
            , symlink_registry_builtins[[symlink_name]]
            , envir = .symlink_registry
        )
    }

    # Decide the link mechanism for this OS once per session (symlink vs junction)
    # - see R/links.R; this is the only OS check in the package
    link_set_backend()

    # Prevent user from overwriting built-in entries
    lock_some_bindings(
        objs  = names(symlink_registry_builtins)
        , env = .symlink_registry
    )

}

