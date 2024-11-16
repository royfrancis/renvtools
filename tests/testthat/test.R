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
  # type is null
  expect_error(read_lock(paths[1], type = NULL))
  # type is incorrect
  expect_error(read_lock(paths[1], type = "data.frame"))
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

test_that("test all lock files", {
  # run across all lock files
  for (path in paths) {
    # default read
    expect_no_error(read_lock(path))
    # read as tibble
    expect_no_error(read_lock(path, type = "tibble"))
    # read as list
    expect_no_error(read_lock(path, type = "list"))
    # method renv
    expect_no_error(read_lock(path, method = "renv"))
    # method renv and type list
    expect_no_error(read_lock(path, type = "list", method = "renv"))
    # expect pkg names and order to be identical in tibble and list
    expect_equal(read_lock(path, type = "tibble")$Package, names(read_lock(path, type = "list")$Packages))
    # output has class rt_tibble
    expect_contains(class(read_lock(path, type = "tibble")), "rt_tibble")
  }
})

# print.rt_tibble --------------------------------------------------------------

testthat::context("print.rt_tibble")

test_that("print", {
  l <- read_lock(paths[1], type = "tibble")
  expect_no_error(invisible(print(l)))
})

# is.rt_tibble -----------------------------------------------------------------

testthat::context("is.rt_tibble")

test_that("is.rt_tibble", {
  l <- read_lock(paths[1], type = "tibble")
  expect_s3_class(l, "rt_tibble")
  expect_true(is.rt_tibble(l))
  expect_false(is.rt_tibble("bla"))
})

# list_to_tbl ------------------------------------------------------------------

testthat::context("list_to_tbl")

test_that("list_to_tbl", {
  l <- read_lock(paths, type = "list")
  # Incorrect input
  expect_error(renvtools:::list_to_tbl("bla"))
  # default usage
  expect_no_error(renvtools:::list_to_tbl(l[[1]]))
  # expect tibble
  expect_s3_class(renvtools:::list_to_tbl(l[[1]]), "tbl")
  # without names
  l <- read_lock(paths_1, type = "list")
  expect_s3_class(renvtools:::list_to_tbl(l[[1]]), "tbl")
  # error if a tibble list is provided
  expect_error(renvtools:::list_to_tbl(read_lock(paths, type = "tibble")))
  expect_error(renvtools:::list_to_tbl(read_lock(paths[[1]], type = "tibble")))
})

# tbl_to_list ------------------------------------------------------------------

testthat::context("tbl_to_list")

test_that("tbl_to_list", {
  l <- read_lock(paths, type = "tibble")
  # Incorrect input
  expect_error(renvtools:::tbl_to_list("bla"))
  # default usage
  expect_no_error(renvtools:::tbl_to_list(l[[1]]))
  # expect list
  expect_type(renvtools:::tbl_to_list(l[[1]]), "list")
  # without names
  l <- read_lock(paths_1, type = "tibble")
  expect_type(renvtools:::tbl_to_list(l[[1]]), "list")
  # error if a list is provided
  expect_error(renvtools:::tbl_to_list(read_lock(paths, type = "list")))
})

# write_lock -------------------------------------------------------------------

testthat::context("write_lock")

test_that("general", {
  tbl <- read_lock(paths[1], type = "tibble")

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
  expect_error(write_lock(read_lock(paths, type = "tibble")))
  expect_error(write_lock(read_lock(paths, type = "list")))
})

test_that("check identical io", {
  # write and read are identical
  for (path in paths) {
    tbl1 <- read_lock(path, type = "tibble")
    write_lock(tbl1, "temp.lock")
    tbl2 <- read_lock("temp.lock", type = "tibble")
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
  l <- read_lock(paths, type = "list")
  expect_no_error(summarize_locks(l))
  # output is a tibble
  expect_s3_class(summarize_locks(l), "tbl")
  # without names
  l <- read_lock(paths_1, type = "list")
  expect_no_error(summarize_locks(l))
  names(l) <- NULL
  expect_no_error(summarize_locks(l))
  # output is a tibble
  expect_s3_class(summarize_locks(l), "tbl")
})

test_that("tibbles", {
  # when tibbles are provided
  l <- read_lock(paths, type = "tibble")
  expect_no_error(summarize_locks(l))
  expect_s3_class(summarize_locks(l), "tbl")
  expect_no_error(summarize_locks(l[[1]]))
  expect_s3_class(summarize_locks(l[[1]]), "tbl")
})

# not identical results
#test_that("compare lists and tibbles", {
#  # when tibbles are provided
#  l1 <- read_lock(paths, type = "tibble")
#  l2 <- read_lock(paths, type = "list")
#  expect_equal(summarize_locks(l1),summarize_locks(l2))
#})

# compare_locks_pair ------------------------------------------------------------

testthat::context("compare_locks_pair")

test_that("compare_locks_pair", {
  # no input specified
  expect_error(compare_locks_pair())
  # input is not a list
  expect_error(compare_locks_pair("bla"))
  # input contains more than two items
  l <- read_lock(paths, type = "list")
  expect_error(compare_locks_pair(l))
  # default usage
  l <- read_lock(paths[1:2], type = "list")
  expect_no_error(compare_locks_pair(l))
  # output is a tibble
  expect_s3_class(compare_locks_pair(l), "tbl")
  # without names
  l <- read_lock(paths_1[1:2], type = "list")
  expect_no_error(compare_locks_pair(l))
  names(l) <- NULL
  expect_no_error(compare_locks_pair(l))
  # output is a tibble
  expect_s3_class(compare_locks_pair(l), "tbl")
  # error when tibbles are provided
  l <- read_lock(paths[1:2], type = "tibble")
  expect_no_error(compare_locks_pair(l))
})

# compare_locks ----------------------------------------------------------------

testthat::context("compare_locks")

test_that("compare_locks", {
  # no input specified
  expect_error(compare_locks())
  # input is not a list
  expect_error(compare_locks("bla"))
  # default usage
  l <- read_lock(paths, type = "list")
  expect_no_error(compare_locks(l))
  # output is a tibble
  expect_s3_class(compare_locks(l), "tbl")
  # without names
  l <- read_lock(paths_1, type = "list")
  expect_no_error(compare_locks(l))
  names(l) <- NULL
  expect_no_error(compare_locks(l))
  # output is a tibble
  expect_s3_class(compare_locks(l), "tbl")
  # error when tibbles are provided
  l <- read_lock(paths, type = "tibble")
  expect_no_error(compare_locks(l))
})
