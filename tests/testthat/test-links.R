# R/links.R - cross-platform link primitives and helpers
# One test per helper, one round trip. Runs on every CI OS; Windows uses
# junctions on the (local) runner tempdir.

root_links <- make_test_root("links")

test_that("link_set_backend picks by OS and link_get_backend returns it", {
   expect_identical(link_set_backend(), link_get_backend())   # set returns what get reads
   backend <- link_get_backend()
   expect_identical(backend$name, if (.Platform$OS.type == "windows") "junction" else "symlink")
   expect_setequal(names(backend), vmTools:::.link_backend_members)
   expect_type(backend$probe_failure_is_fatal, "logical")
   fns <- setdiff(names(backend), c("name", "probe_failure_is_fatal"))
   expect_true(all(vapply(backend[fns], is.function, logical(1))))
   expect_true(Sys.info()[["sysname"]] %in% c("Linux", "Darwin", "Windows"))
})

test_that("dir_supports_links probes a folder and leaves it empty", {
   dir_probe <- make_test_root("links", "probe")
   expect_true(dir_supports_links(dir_probe))
   expect_identical(list.files(dir_probe, all.files = TRUE, no.. = TRUE), character(0))
   expect_false(dir_supports_links(file.path(dir_probe, "does_not_exist")))
})

test_that("link_create refuses a missing target and an occupied link path", {
   dir_c  <- make_test_root("links", "create")
   target <- make_test_root("links", "create", "target")
   real   <- make_test_root("links", "create", "real_folder")
   expect_error(link_create(file.path(dir_c, "missing"), file.path(dir_c, "lnk")), regexp = "does not exist")
   expect_error(link_create(target, real), regexp = "real folder or file with that name already exists")
   expect_false(path_is_link(real))   # untouched
   link_create(target, file.path(dir_c, "lnk"))
   expect_error(link_create(target, file.path(dir_c, "lnk")), regexp = "symlink with that name already exists")
})

test_that("path_is_link and link_get_target tell links from real folders", {
   dir_i  <- make_test_root("links", "is")
   target <- make_test_root("links", "is", "target")
   lnk    <- file.path(dir_i, "lnk")
   expect_false(path_is_link(target))
   expect_false(path_is_link(file.path(dir_i, "nothing_here")))
   expect_identical(link_get_target(target), NA_character_)
   link_create(target, lnk)
   expect_true(path_is_link(lnk))
   expect_identical(link_get_target(lnk), target)
   # a trailing slash means "the folder behind the link" to the OS - must not look through it
   expect_true(path_is_link(paste0(lnk, "/")))
   expect_identical(link_get_target(paste0(lnk, "/")), target)
})

test_that("link_resolve reports a dangling link", {
   dir_d  <- make_test_root("links", "dangling")
   target <- make_test_root("links", "dangling", "target")
   lnk    <- file.path(dir_d, "lnk")
   link_create(target, lnk)
   unlink(target, recursive = TRUE)
   res <- link_resolve(lnk)
   expect_true(res$is_link)
   expect_true(res$is_dangling)
   expect_identical(res$target, target)
   expect_true(link_remove(lnk))   # dangling links are still removable
   expect_false(path_is_link(lnk))
})

test_that("junction backend: dead junction detected and read back, real folder left alone", {
   skip_if_not(link_get_backend()$name == "junction", "junction backend only")
   dir_j  <- make_test_root("links", "junction_dead")
   target <- make_test_root("links", "junction_dead", "target")
   real   <- make_test_root("links", "junction_dead", "real")
   lnk    <- file.path(dir_j, "lnk")
   link_create(target, lnk)
   expect_false(windows_junction_is_dead(lnk))
   unlink(target, recursive = TRUE)
   expect_true(windows_junction_is_dead(lnk))
   expect_false(windows_junction_is_dead(real))
   expect_identical(windows_junction_target(lnk), target)
   expect_identical(windows_junction_target(real), "")
   expect_false(path_is_link(real))
})

test_that("link_list returns one row per folder or link, files excluded", {
   dir_l  <- make_test_root("links", "list")
   target <- make_test_root("links", "list", "target")
   link_create(target, file.path(dir_l, "lnk"))
   writeLines("x", file.path(dir_l, "a_file.txt"))
   dt <- link_list(dir_l)
   expect_identical(names(dt), c("leaf", "path", "is_link", "target", "is_dangling"))
   expect_setequal(dt$leaf, c("target", "lnk"))
   expect_identical(dt[leaf == "lnk", target], target)
   expect_identical(dt[leaf == "target", is_link], FALSE)
   expect_identical(nrow(link_list(make_test_root("links", "list_empty"))), 0L)
})

test_that("link_remove refuses real folders and files", {
   dir_r <- make_test_root("links", "remove_refuse")
   real  <- make_test_root("links", "remove_refuse", "real")
   writeLines("keep me", file.path(real, "file.txt"))
   expect_error(link_remove(real), regexp = "real folder or file, not a symlink")
   expect_error(link_remove(file.path(dir_r, "nothing_here")), regexp = "Nothing exists at that path")
   expect_true(file.exists(file.path(real, "file.txt")))
})

test_that("link_failure_reason and windows_drive_type return one string", {
   expect_type(link_failure_reason(root_links), "character")
   expect_match(link_failure_reason(file.path(root_links, "nope")), "does not exist")
   expect_true(windows_drive_type(root_links) %in% c("Fixed", "Network", "Removable", "Unknown"))
})

test_that("round trip: create, resolve, remove; target and its contents survive", {
   dir_rt <- make_test_root("links", "round_trip")
   target <- make_test_root("links", "round_trip", "target")
   lnk    <- file.path(dir_rt, "lnk")
   writeLines("payload", file.path(target, "file.txt"))

   expect_identical(link_create(target, lnk), lnk)
   expect_true(path_is_link(lnk))
   expect_identical(link_get_target(lnk), target)
   expect_true(file.exists(file.path(lnk, "file.txt")))   # usable through the link

   expect_true(link_remove(lnk))
   expect_false(path_is_link(lnk))
   expect_false(file.exists(lnk))
   expect_true(dir.exists(target))
   expect_true(file.exists(file.path(target, "file.txt")))   # removing the link never touches the target

   # a recursive delete of a folder that CONTAINS a link must not follow the
   # link into its target - this is the "erases the folder it points to" failure mode
   holder <- make_test_root("links", "round_trip", "holder")
   link_create(target, file.path(holder, "lnk"))
   unlink(holder, recursive = TRUE)
   expect_false(dir.exists(holder))
   expect_true(file.exists(file.path(target, "file.txt")))
})

test_that("Cleanup is complete", {
   unlink(root_links, recursive = TRUE)
   expect_false(dir.exists(root_links))
})
