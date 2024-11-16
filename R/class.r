#' @title Print function for rt_tibble
#' @description Print function for rt_tibble
#' @param x An object of class rt_tibble
#' @param ... Other
#' @details
#' Prints R version along with table view.
#'
#' @method print rt_tibble
#' @export
#'
print.rt_tibble <- function(x, ...) {
  if (!is.null(attr(x, "metadata")$R$Version)) cat(paste("R version:", attr(x, "metadata")$R$Version, "\n"))
  if (!is.null(attr(x, "metadata")$Bioconductor$Version)) cat(paste("Bioc version:", attr(x, "metadata")$Bioconductor$Version, "\n"))
  NextMethod(x)
  return(invisible(x))
}

#' @title Check if input is rt_tibble class
#' @description Check if input is rt_tibble class
#' @param x An object of class rt_tibble
#' @return TRUE or FALSE
#' @export
#'
is.rt_tibble <- function(x) {
  if ("rt_tibble" %in% class(x)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @title Print function for rt_list
#' @description Print function for rt_list
#' @param x An object of class rt_list
#' @param ... Other
#' @details
#' Prints key features of the lockfile.
#'
#' @method print rt_list
#' @export
#'
print.rt_list <- function(x, ...) {
  if (!is.na(get_version_r(x))) cat(paste("R version:", get_version_r(x), "\n"))
  if (!is.na(get_version_bioc(x))) cat(paste("Bioc version:", get_version_bioc(x), "\n"))
  if (!is.na(get_version_pkg(x, "renv"))) cat(paste("renv version:", get_version_pkg(x, "renv"), "\n"))
  if (!is.null(x$Packages)) cat(paste("Packages:", length(x$Packages)))
  cat("\n")
  return(invisible(x))
}

#' @title Check if input is rt_list class
#' @description Check if input is rt_list class
#' @param x An object of class rt_list
#' @return TRUE or FALSE
#' @export
#'
is.rt_list <- function(x) {
  if ("rt_list" %in% class(x)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @title Check if input is single item
#' @description Check if input is a single rt_list or a single rt_tibble class
#' @param x An object of class rt_list or rt_tibble
#' @return TRUE or FALSE
#' @export
#'
is.rt <- function(x) {
  if (!is.list(x)) stop("Input is not a list.")
  r <- TRUE
  if (is.rt_list(x[[1]]) || is.rt_tibble(x[[1]])) r <- FALSE
  return(r)
}
