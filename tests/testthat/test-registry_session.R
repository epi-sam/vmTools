# R/registry_session.R - the built-in symlink types and the registry helpers

test_that("get_symlink_registry() returns the four built-in types and their is_singleton", {
   expect_identical(
      vapply(get_symlink_registry(), `[[`, logical(1), "is_singleton")
      , c(best = TRUE, keep = FALSE, latest = TRUE, remove = FALSE)
   )
})

test_that("A log action is judged by its form, not against the current symlink types", {
   actions <- c("create", "promote_any_type", "demote_any_type", "non_slt_event")
   expect_identical(vmTools:::is_valid_action(actions), c(TRUE, TRUE, TRUE, FALSE))
})
