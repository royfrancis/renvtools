#' @title Read lock files
#' @description Read lock files as tibble or list
#' @param paths Path to lock files
#' @param type Return object type. Either 'tibble' or 'list'
#' @param method Read method. Either 'default' or 'renv'. See details.
#' @param encoding Text encoding, See `jsonlite::fromJSON()`.
#' @return A tibble or a list
#' @details
#' If argument `paths` is longer than one, then a list is returned.
#' Argument `type` denotes whether to return a tibble or a list.
#' Argument `method` denotes which method to use to read a lockfile. 'default'
#' reads the lockfile and preserves it as it is as much as possible. 'renv' uses
#' the read function from renv. It might update the lockfile structure to be current.
#'
#' @examples
#' # read one lock file as a tibble
#' path <- file.path(system.file("extdata", package = "renvtools"), "renv-r4.4.1.lock")
#' read_lock(path, type = "tibble")
#'
#' # read one lock file as a list
#' x <- read_lock(path, type = "list")
#'
#' # read several lock files as a list of tibbles
#' paths <- list.files(system.file("extdata", package = "renvtools"), full.names = TRUE)
#' names(paths) <- basename(paths)
#' x <- read_lock(paths, type = "tibble")
#'
#' # read several lock files as a list of lists
#' x <- read_lock(paths, type = "list")
#'
#' @importFrom jsonlite fromJSON
#' @importFrom renv lockfile_read
#' @importFrom purrr map
#' @export
#'
read_lock <- function(paths = "renv.lock", type = "list", method = "default", encoding = "UTF-8") {
  if (!type %in% c("tibble", "list")) stop("Argument 'type' must be either 'tibble' or 'list'.")
  if (!method %in% c("default", "renv")) stop("Argument 'method' must be either 'default' or 'renv'.")

  fn <- function(pa, type, method) {
    if (method == "default") out <- fromJSON(readLines(pa, warn = FALSE, encoding = encoding))
    if (method == "renv") {
      out <- lockfile_read(pa)
      class(out) <- c(class(out), "list")
    }

    if (type == "tibble") {
      return(list_to_tbl(out))
    } else if (type == "list") {
      return(out)
    }
  }

  if (length(paths) > 1) {
    l <- map(paths, ~ fn(.x, type = type, method = method))
    if (is.null(names(paths))) {
      padded_nums <- sprintf(paste0("%0", nchar(as.character(length(paths))), "d"), 1:length(paths))
      names(paths) <- paste0("lf_", padded_nums)
    }
    names(l) <- names(paths)
  } else {
    l <- fn(paths, type = type, method = method)
  }

  return(l)
}

#' @title Convert a list to a rt_tibble
#' @description Convert a list to a rt_tibble
#' @param x A list containing one lockfile. A list from `read_lock()`.
#' @return A tibble of class 'rt_tibble'.
#' @importFrom tibble tibble
#' @importFrom tidyr unnest_wider
#'
list_to_tbl <- function(x) {
  if (!is.list(x)) stop("Input is not a list.")
  if (is.rt_tibble(x)) stop("Given input is of class 'rt_tibble'. Input must be a list.")
  if (is.rt_tibble(x[[1]])) stop("Given input contains class 'rt_tibble'. Input must be a list.")

  metadata <- x
  metadata$Packages <- NULL

  o <- x$Packages
  # Convert packages list to tibble
  out <- tibble(o) |> unnest_wider(o)

  attr(out, "metadata") <- metadata
  class(out) <- c("rt_tibble", class(out))

  return(out)
}

#' @title Convert a rt_tibble to a list
#' @description Convert a rt_tibble to a list
#' @param x A tibble of class 'rt_tibble' from one lockfile. A tibble from `read_lock()`.
#' @return A list
#' @importFrom purrr pmap set_names discard
#'
tbl_to_list <- function(x) {
  if (!is.rt_tibble(x)) stop("Input must be a tibble with class 'rt_tibble'.")

  # Convert the tibble back to a list
  nested_list <- x |>
    pmap(~ {
      row_list <- list(...)
      row_list <- discard(row_list, is.null)
      return(row_list)
    }) |>
    set_names(x$Package)

  # Remove all NAs
  remove_na_recursive <- function(x) {
    if (is.list(x)) {
      x <- lapply(x, remove_na_recursive) # Recursively apply to each element if it's a list
      x <- x[!sapply(x, function(y) length(y) == 0 && !is.list(y))] # Remove empty elements if they are not lists
    } else if (!is.atomic(x) || anyNA(x)) {
      x <- x[!is.na(x)] # Remove NA
    }
    return(x)
  }
  clean_list <- remove_na_recursive(nested_list)

  # Combine metadata and package data
  # if(is.null(attr(tbl,"metadata")))
  final_list <- attr(x, "metadata")
  final_list$Packages <- clean_list

  return(final_list)
}

#' @title Export a lock file
#' @description Write a rt_tibble or a list as a lock file
#' @param x A single list or an 'rt_tibble'. An single file output from `read_lock()`.
#' @param path Output path with filename
#' @return None
#' @importFrom jsonlite toJSON
#' @export
#'
write_lock <- function(x, path) {
  if (!is.list(x)) stop("Input is not a rt_tibble (tibble) or a list.")
  if (is.rt_tibble(x[[1]])) stop("Input must be a single rt_tibble (tibble) or a single list item.")

  if (is.rt_tibble(x)) {
    x <- tbl_to_list(x)
  }

  write(toJSON(x, pretty = TRUE, auto_unbox = TRUE), path)

  return(invisible(TRUE))
}

#' @title Summarize lock files
#' @description Summarize lock files
#' @param x A list of lists or list of 'rt_tibble'. An output from `read_lock()`.
#' @return A tibble
#' @examples
#' paths <- c(
#'   file.path(system.file("extdata", package = "renvtools"), "renv-r4.4.1.lock"),
#'   file.path(system.file("extdata", package = "renvtools"), "renv-r4.3.2.lock")
#' )
#' l <- read_lock(paths, type = "list")
#' summarize_locks(l)
#'
#' @importFrom purrr map map_chr map_int
#' @importFrom tibble tibble
#' @importFrom dplyr mutate select relocate
#' @export
#'
summarize_locks <- function(x) {
  if (!is.list(x)) stop("Input is not a list.")

  if (is.rt_tibble(x)) {
    lst <- list(tbl_to_list(x))
  } else if (is.rt_tibble(x[[1]])) {
    lst <- lapply(x, tbl_to_list)
    names(lst) <- names(x)
  } else {
    lst <- x
  }

  if (is.null(names(lst))) {
    padded_nums <- sprintf(paste0("%0", nchar(as.character(length(lst))), "d"), 1:length(lst))
    names(lst) <- paste0("lf_", padded_nums)
  }

  lock_files <- NULL
  label <- NULL

  tib <- tibble(lock_files = lst) |>
    mutate(
      label = names(lst),
      rver = map_chr(lock_files, ~ .x$R$Version),
      pkgs_len = map_int(lock_files, ~ length(.x$Packages)),
      repositories = map(lst, ~ as.data.frame(table(unlist(map(.x$Packages, "Repository"))))),
      sources = map(lst, ~ as.data.frame(table(unlist(map(.x$Packages, "Source"))))),
      pkgs = map(lock_files, ~ names(.x$Packages)),
      pkgs_req = map(lock_files, ~ map(.x$Packages, "Requirements"))
    ) |>
    select(-lock_files)

  return(tib)
}

#' @title Compare two lockfiles
#' @description Compare two lockfiles
#' @param x A list of lists or list of 'rt_tibble' with two lockfiles. An output from `read_lock()`.
#' @return A tibble
#' @examples
#' paths <- c(
#'   file.path(system.file("extdata", package = "renvtools"), "renv-r4.4.1.lock"),
#'   file.path(system.file("extdata", package = "renvtools"), "renv-r4.3.2.lock")
#' )
#' l <- read_lock(paths, type = "list")
#' compare_locks_pair(l)
#' @importFrom tibble tibble
#' @importFrom purrr map_chr
#' @export
#'
compare_locks_pair <- function(x) {
  if (!is.list(x)) stop("Input is not a list.")
  if (length(x) != 2) stop("Input must be a list of length of 2.")

  if (is.rt_tibble(x[[1]])) {
    lst <- lapply(x, tbl_to_list)
    names(lst) <- names(x)
  } else {
    lst <- x
  }

  if (is.null(names(lst))) names(lst) <- c("lf_1", "lf_2")

  jaccard <- function(a, b) {
    intersection <- length(intersect(a, b))
    union <- length(a) + length(b) - intersection
    return(intersection / union)
  }

  a_pkgs <- as.character(map_chr(lst[[1]]$Packages, ~ .x$Package))
  b_pkgs <- as.character(map_chr(lst[[2]]$Packages, ~ .x$Package))

  dfr <- tibble(
    a = names(lst)[1],
    b = names(lst)[2],
    a_rver = lst[[1]]$R$Version,
    b_rver = lst[[2]]$R$Version,
    jaccard = jaccard(a_pkgs, b_pkgs),
    a_pkgs_len = length(a_pkgs),
    b_pkgs_len = length(lst[[2]]$Packages),
    a_pkgs_len_only = length(setdiff(a_pkgs, b_pkgs)),
    b_pkgs_len_only = length(setdiff(b_pkgs, a_pkgs)),
    pkgs_total_len = length(union(a_pkgs, b_pkgs)),
    a_pkgs = list(a_pkgs),
    b_pkgs = list(b_pkgs),
    a_pkgs_only = list(setdiff(a_pkgs, b_pkgs)),
    b_pkgs_only = list(setdiff(b_pkgs, a_pkgs)),
    pkgs_common = list(intersect(a_pkgs, b_pkgs)),
    pkgs_total = list(union(a_pkgs, b_pkgs))
  )

  return(dfr)
}

#' @title Compare multiple lockfiles
#' @description Compare multiple lockfiles
#' @param x A list of lists or list of 'rt_tibble'. An output from `read_lock()`.
#' @return A tibble
#' @examples
#' paths <- c(
#'   file.path(system.file("extdata", package = "renvtools"), "renv-r4.4.1.lock"),
#'   file.path(system.file("extdata", package = "renvtools"), "renv-r4.3.2.lock")
#' )
#' l <- read_lock(paths, type = "list")
#' compare_locks(l)
#' @importFrom purrr map_dfr
#' @importFrom dplyr bind_rows
#' @importFrom utils combn
#' @export
#'
#'
compare_locks <- function(x) {
  if (!is.list(x)) stop("Input is not a list.")

  elements <- names(x)
  if (is.null(elements)) elements <- seq_along(x) # Handle unnamed lists

  combns <- combn(length(x), 2)

  # Apply compare_locks_pair on each pair
  fn <- function(indices) {
    pair_names <- elements[indices]
    pair_list <- x[indices]
    names(pair_list) <- pair_names
    compare_locks_pair(pair_list)
  }

  res_list <- apply(combns, 2, fn)
  return(bind_rows(res_list))
}

#' @title Print function for rt_tibble
#' @description Print function for rt_tibble
#' @param x An object of class rt_tibble
#' @param ... Other
#' @details
#' Displays R version along with table view for now.
#'
#' @method print rt_tibble
#' @export
#'
print.rt_tibble <- function(x, ...) {
  if (!is.null(attr(x, "metadata")$R$Version)) cat(paste("R version:", attr(x, "metadata")$R$Version, "\n"))
  if (!is.null(attr(x, "metadata")$Bioconductor$Version)) cat(paste("Bioc version:", attr(x, "metadata")$Bioconductor$Version, "\n"))
  NextMethod(x)
  invisible(x)
}

#' @title Check if an object is rt_tibble class
#' @description Check if an object is rt_tibble class
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

utils::globalVariables()
