#' @title Get R version
#' @description Get R version from lockfile
#' @param x A 'rt_list' or 'rt_tibble' class object. An single output from `read_lock()`.
#' @return A character string with version or NA
#' @examples
#' paths <- file.path(system.file("extdata", package = "renvtools"), "renv-r4.4.1.lock")
#' l <- read_lock(paths, format = "list")
#' get_version_r(l)
#' @export
#'
get_version_r <- function(x) {
  if (!is.rt(x)) stop("Input must be a single rt_tibble (tibble) or a single rt_list (list) item.")
  if (is.rt_tibble(x)) x <- tbl_to_list(x)
  testthat::expect_true(!is.null(x$R$Version))
  x$R$Version
}

#' @title Get Bioc version
#' @description Get Bioconductor version from lockfile
#' @param x A 'rt_list' or 'rt_tibble' class object. An single output from `read_lock()`.
#' @return A character string with version or NA
#' @examples
#' paths <- file.path(system.file("extdata", package = "renvtools"), "renv-r4.4.1.lock")
#' l <- read_lock(paths, format = "list")
#' get_version_bioc(l)
#' @export
#'
get_version_bioc <- function(x) {
  if (!is.rt(x)) stop("Input must be a single rt_tibble (tibble) or a single rt_list (list) item.")
  if (is.rt_tibble(x)) x <- tbl_to_list(x)
  testthat::expect_true(!is.null(x$Bioconductor))
  x$Bioconductor$Version
}

#' @title Get renv version
#' @description Get renv version from lockfile
#' @param x A 'rt_list' or 'rt_tibble' class object. An single output from `read_lock()`.
#' @return A character string with version or NA
#' @examples
#' paths <- file.path(system.file("extdata", package = "renvtools"), "renv-r4.4.1.lock")
#' l <- read_lock(paths, format = "list")
#' get_version_renv(l)
#' @export
#'
get_version_renv <- function(x) {
  if (!is.rt(x)) stop("Input must be a single rt_tibble (tibble) or a single rt_list (list) item.")
  if (is.rt_tibble(x)) x <- tbl_to_list(x)

  if ("renv" %in% names(x$Packages)) {
    return(x$Packages$renv$Version)
  }
  testthat::expect_true(!is.null(x$renv$Version))
  x$renv$Version
}

#' @title Get package version
#' @description Get package version from lockfile
#' @param x A 'rt_list' or 'rt_tibble' class object. An single output from `read_lock()`.
#' @param pkg package name with correct case
#' @return A character string with version or NA
#' @examples
#' paths <- file.path(system.file("extdata", package = "renvtools"), "renv-r4.4.1.lock")
#' l <- read_lock(paths, format = "list")
#' get_version_pkg(l, "renv")
#' @export
#'
get_version_pkg <- function(x, pkg = "renv") {
  if (!is.rt(x)) stop("Input must be a single rt_tibble (tibble) or a single rt_list (list) item.")
  if (is.rt_tibble(x)) x <- tbl_to_list(x)

  if (pkg %in% names(x$Packages)) {
    return(x$Packages[[pkg]]$Version)
  } else {
    return(NA)
  }
}
