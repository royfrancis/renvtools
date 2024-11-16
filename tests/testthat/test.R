library(testthat)
library(renvtools)

Sys.setenv("LANG" = "C")

# with names
paths <- list.files(system.file("extdata", package = "renvtools"), full.names = TRUE)
names(paths) <- gsub(".lock", "", basename(paths))

# without names
paths_1 <- list.files(system.file("extdata", package = "renvtools"), full.names = TRUE)

# read_lock --------------------------------------------------------------------

testthat::context("read_lock")

test_that("general", {
  # no input
  expect_error(suppressWarnings(read_lock()))
  # input path doesn't exist
  expect_error(suppressWarnings(read_lock("/usr/renv.lock")))
  # format is null
  expect_error(read_lock(paths[1], format = NULL))
  # format is incorrect
  expect_error(read_lock(paths[1], format = "data.frame"))
  # method is null
  expect_error(read_lock(paths[1], method = NULL))
  # method is incorrect
  expect_error(read_lock(paths[1], method = "bla"))
  # expect list with multiple paths
  expect_contains(class(read_lock(paths)), "list")
  # is list length correct?
  expect_equal(length(read_lock(paths)), length(paths))
  # expect generated names if paths have no names
  expect_type(names(read_lock(paths_1)), "character")
})

test_that("test individual lock files", {
  # run across all lock files
  for (path in paths) {
    # default read
    expect_no_error(read_lock(path))
    # read as tibble
    expect_no_error(read_lock(path, format = "tibble"))
    # read as list
    expect_no_error(read_lock(path, format = "list"))
    # method renv
    expect_no_error(read_lock(path, format = "renv"))
    # expect pkg names and order to be identical in tibble and list
    expect_equal(read_lock(path, format = "tibble")$Package, names(read_lock(path, format = "list")$Packages))
    # output has class rt_tibble
    expect_contains(class(read_lock(path, format = "tibble")), "rt_tibble")
    # output has class rt_list
    expect_contains(class(read_lock(path, format = "list")), "rt_list")
    # output has class rt_tibble
    expect_contains(class(read_lock(path, format = "renv")), "rt_list")
  }
})

# print.rt_tibble --------------------------------------------------------------

testthat::context("print.rt_tibble")

test_that("print", {
  expect_no_error(print(read_lock(paths[1], format = "tibble")))
  expect_no_error(print(read_lock(paths[5], format = "tibble")))
})

# is.rt_tibble -----------------------------------------------------------------

testthat::context("is.rt_tibble")

test_that("is.rt_tibble", {
  l <- read_lock(paths[1], format = "tibble")
  expect_s3_class(l, "rt_tibble")
  expect_true(is.rt_tibble(l))
  expect_false(is.rt_tibble("bla"))
})

# print.rt_list --------------------------------------------------------------

testthat::context("print.rt_list")

test_that("print list", {
  l <- read_lock(paths[1], format = "list")
  expect_no_error(invisible(print(l)))
  l <- read_lock(paths[5], format = "list")
  expect_no_error(invisible(print(l)))
})

test_that("print renv", {
  l <- read_lock(paths[1], format = "renv")
  expect_no_error(invisible(print(l)))
  l <- read_lock(paths[5], format = "renv")
  expect_no_error(invisible(print(l)))
})

# is.rt_list -----------------------------------------------------------------

testthat::context("is.rt_list")

test_that("is.rt_list list", {
  l <- read_lock(paths[1], format = "list")
  expect_s3_class(l, "rt_list")
  expect_true(is.rt_list(l))
  expect_false(is.rt_list("bla"))
})

test_that("is.rt_list renv", {
  l <- read_lock(paths[1], format = "renv")
  expect_s3_class(l, "rt_list")
  expect_true(is.rt_list(l))
})

# list_to_tbl ------------------------------------------------------------------

testthat::context("list_to_tbl")

test_that("list_to_tbl", {
  l <- read_lock(paths, format = "list")
  # Incorrect input
  expect_error(renvtools:::list_to_tbl("bla"))
  # default usage
  expect_no_error(renvtools:::list_to_tbl(l[[1]]))
  # expect tibble
  expect_s3_class(renvtools:::list_to_tbl(l[[1]]), "tbl")
  # without names
  l <- read_lock(paths_1, format = "list")
  expect_s3_class(renvtools:::list_to_tbl(l[[1]]), "tbl")
  # error if a tibble list is provided
  expect_error(renvtools:::list_to_tbl(read_lock(paths, format = "tibble")))
  expect_error(renvtools:::list_to_tbl(read_lock(paths[[1]], format = "tibble")))
})

# get_version_r ----------------------------------------------------------------

testthat::context("get_version_r")

test_that("list", {
  l <- read_lock(paths, format = "list")
  expect_equal(get_version_r(l[[1]]), "3.5.3")
  expect_equal(get_version_r(l[[2]]), "3.6.1")
})

test_that("renv", {
  l <- read_lock(paths, format = "renv")
  expect_equal(get_version_r(l[[1]]), "3.5.3")
  expect_equal(get_version_r(l[[2]]), "3.6.1")
})

test_that("tibble", {
  l <- read_lock(paths, format = "tibble")
  expect_equal(get_version_r(l[[1]]), "3.5.3")
  expect_equal(get_version_r(l[[2]]), "3.6.1")
})

test_that("list of list", {
  expect_error(get_version_r(read_lock(paths, format = "list")))
  expect_error(get_version_r(read_lock(paths, format = "renv")))
  expect_error(get_version_r(read_lock(paths, format = "tibble")))
})

# get_version_bioc -------------------------------------------------------------

testthat::context("get_version_bioc")

test_that("list", {
  l <- read_lock(paths, format = "list")
  expect_equal(get_version_bioc(l[[4]]), NA)
  expect_equal(get_version_bioc(l[[5]]), "3.16")
})

test_that("renv", {
  l <- read_lock(paths, format = "renv")
  expect_equal(get_version_bioc(l[[4]]), NA)
  expect_equal(get_version_bioc(l[[5]]), "3.16")
})

test_that("tibble", {
  l <- read_lock(paths, format = "tibble")
  expect_equal(get_version_bioc(l[[4]]), NA)
  expect_equal(get_version_bioc(l[[5]]), "3.16")
})

test_that("list of list", {
  expect_error(get_version_bioc(read_lock(paths, format = "list")))
  expect_error(get_version_bioc(read_lock(paths, format = "renv")))
  expect_error(get_version_bioc(read_lock(paths, format = "tibble")))
})

# get_version_renv -------------------------------------------------------------

testthat::context("get_version_renv")

test_that("list", {
  l <- read_lock(paths, format = "list")
  expect_equal(get_version_renv(l[[1]]), "0.9.2")
  expect_equal(get_version_renv(l[[3]]), "0.16.0")
})

test_that("renv", {
  l <- read_lock(paths, format = "renv")
  expect_equal(get_version_renv(l[[1]]), "0.9.2")
  expect_equal(get_version_renv(l[[3]]), "0.16.0")
})

test_that("tibble", {
  l <- read_lock(paths, format = "tibble")
  expect_equal(get_version_renv(l[[1]]), "0.9.2")
  expect_equal(get_version_renv(l[[3]]), "0.16.0")
})

test_that("list of list", {
  expect_error(get_version_renv(read_lock(paths, format = "list")))
  expect_error(get_version_renv(read_lock(paths, format = "renv")))
  expect_error(get_version_renv(read_lock(paths, format = "tibble")))
})

# get_version_pkg --------------------------------------------------------------

testthat::context("get_version_pkg")

test_that("list", {
  l <- read_lock(paths, format = "list")
  expect_equal(get_version_pkg(l[[1]], "renv"), NA)
  expect_equal(get_version_pkg(l[[2]], "renv"), "0.11.0")
})

test_that("renv", {
  l <- read_lock(paths, format = "renv")
  expect_equal(get_version_pkg(l[[1]], "renv"), NA)
  expect_equal(get_version_pkg(l[[2]], "renv"), "0.11.0")
})

test_that("tibble", {
  l <- read_lock(paths, format = "tibble")
  expect_equal(get_version_pkg(l[[1]], "renv"), NA)
  expect_equal(get_version_pkg(l[[2]], "renv"), "0.11.0")
})

test_that("list of list", {
  expect_error(get_version_pkg(read_lock(paths, format = "list"), "renv"))
  expect_error(get_version_pkg(read_lock(paths, format = "renv"), "renv"))
  expect_error(get_version_pkg(read_lock(paths, format = "tibble"), "renv"))
})

# tbl_to_list ------------------------------------------------------------------

testthat::context("tbl_to_list")

test_that("tbl_to_list", {
  l <- read_lock(paths, format = "tibble")
  # Incorrect input
  expect_error(renvtools:::tbl_to_list("bla"))
  # default usage
  expect_no_error(renvtools:::tbl_to_list(l[[1]]))
  # expect list
  expect_type(renvtools:::tbl_to_list(l[[1]]), "list")
  # without names
  l <- read_lock(paths_1, format = "tibble")
  expect_type(renvtools:::tbl_to_list(l[[1]]), "list")
  # error if a list is provided
  expect_error(renvtools:::tbl_to_list(read_lock(paths, format = "list")))
  expect_error(renvtools:::tbl_to_list(read_lock(paths, format = "tibble")))
})

# write_lock -------------------------------------------------------------------

testthat::context("write_lock")

test_that("general", {
  tbl <- read_lock(paths[1], format = "tibble")

  # no input provided
  expect_error(write_lock())
  # incorrect input provided
  expect_error(write_lock("bla"))
  # input with many lockfiles
  expect_error(write_lock(read_lock(paths)))
  # write path not provided
  expect_error(write_lock(tbl))
  # write path doesn't exist
  expect_error(suppressWarnings(write_lock(tbl, "path/does/not/exist/renv.lock")))
  # default write
  expect_no_error(suppressWarnings(write_lock(tbl, "temp.lock")))
  unlink("temp.lock")
  # list of files
  expect_error(write_lock(read_lock(paths, format = "tibble")))
  expect_error(write_lock(read_lock(paths, format = "list")))
})

test_that("check identical io", {
  # write and read are identical
  for (path in paths) {
    tbl1 <- read_lock(path, format = "tibble")
    write_lock(tbl1, "temp.lock")
    tbl2 <- read_lock("temp.lock", format = "tibble")
    expect_equal(tbl1$Package, tbl2$Package)
    unlink("temp.lock")
  }
})

# summarize_lock ---------------------------------------------------------------

testthat::context("summarize_locks")

test_that("lists", {
  # no input specified
  expect_error(summarize_locks())
  # input is not a list
  expect_error(summarize_locks("bla"))
  # default usage
  l <- read_lock(paths, format = "list")
  expect_no_error(summarize_locks(l))
  # output is a tibble
  expect_s3_class(summarize_locks(l), "tbl")
  # without names
  l <- read_lock(paths_1, format = "list")
  expect_no_error(summarize_locks(l))
  names(l) <- NULL
  expect_no_error(summarize_locks(l))
  # output is a tibble
  expect_s3_class(summarize_locks(l), "tbl")
})

test_that("tibbles", {
  # when tibbles are provided
  l <- read_lock(paths, format = "tibble")
  expect_no_error(summarize_locks(l))
  expect_s3_class(summarize_locks(l), "tbl")
  expect_no_error(summarize_locks(l[[1]]))
  expect_s3_class(summarize_locks(l[[1]]), "tbl")
})

# not identical results
# test_that("compare lists and tibbles", {
#  # when tibbles are provided
#  l1 <- read_lock(paths, format = "tibble")
#  l2 <- read_lock(paths, format = "list")
#  expect_equal(summarize_locks(l1),summarize_locks(l2))
# })

# compare_locks_pair ------------------------------------------------------------

testthat::context("compare_locks_pair")

test_that("bad input", {
  # no input specified
  expect_error(compare_locks_pair())
  # input is not a list
  expect_error(compare_locks_pair("bla"))
  # input contains more than two items
  l <- read_lock(paths, format = "list")
  expect_error(compare_locks_pair(l))
})

test_that("general", {
  # default usage
  l <- read_lock(paths[1:2], format = "list")
  expect_no_error(compare_locks_pair(l))
  # output is a tibble
  expect_s3_class(compare_locks_pair(l), "tbl")
  # without names
  l <- read_lock(paths_1[1:2], format = "list")
  expect_no_error(compare_locks_pair(l))
  names(l) <- NULL
  expect_no_error(compare_locks_pair(l))
  # output is a tibble
  expect_s3_class(compare_locks_pair(l), "tbl")
  # error when tibbles are provided
  l <- read_lock(paths[1:2], format = "tibble")
  expect_no_error(compare_locks_pair(l))
})

# compare_locks ----------------------------------------------------------------

testthat::context("compare_locks")

test_that("bad input", {
  # no input specified
  expect_error(compare_locks())
  # input is not a list
  expect_error(compare_locks("bla"))
})

test_that("general", {
  # default usage
  l <- read_lock(paths, format = "list")
  expect_no_error(compare_locks(l))
  # output is a tibble
  expect_s3_class(compare_locks(l), "tbl")
  # without names
  l <- read_lock(paths_1, format = "list")
  expect_no_error(compare_locks(l))
  names(l) <- NULL
  expect_no_error(compare_locks(l))
  # output is a tibble
  expect_s3_class(compare_locks(l), "tbl")
  # error when tibbles are provided
  l <- read_lock(paths, format = "tibble")
  expect_no_error(compare_locks(l))
})
