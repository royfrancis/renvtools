#' @title Read lock files
#' @description Read lock files as tibble or list
#' @param paths Path to lock files
#' @param format Return object format. Either 'list', 'tibble' or 'renv'. See details.
#' @param encoding Text encoding, See `jsonlite::fromJSON()`.
#' @return A tibble or a list
#' @details
#' If argument `paths` is longer than one, then a list is returned.
#' Argument `format` denotes return object format for the lockfile.
#' Argument `method` denotes which method to use to read a lockfile. 'list'
#' reads the lockfile and preserves it as it is as much as possible. 'tibble'
#' returns a tibble.'renv' uses the read function from renv
#' (`renv::lockfile_read()`) which returns a list.
#' This option might update the lockfile structure to be current.
#'
#' @examples
#' # read one lock file as a tibble
#' path <- file.path(system.file("extdata", package = "renvtools"), "renv-r4.4.1.lock")
#' read_lock(path, format = "tibble")
#'
#' # read one lock file as a list
#' x <- read_lock(path, format = "list")
#'
#' # read several lock files as a list of tibbles
#' paths <- list.files(system.file("extdata", package = "renvtools"), full.names = TRUE)
#' names(paths) <- basename(paths)
#' x <- read_lock(paths, format = "tibble")
#'
#' # read several lock files as a list of lists
#' x <- read_lock(paths, format = "list")
#'
#' @importFrom jsonlite fromJSON
#' @importFrom renv lockfile_read
#' @importFrom purrr map
#' @export
#'
read_lock <- function(paths = "renv.lock", format = "list", encoding = "UTF-8") {
  fn <- function(pa, format) {
    if (format == "list") {
      out <- fromJSON(readLines(pa, warn = FALSE, encoding = encoding))
      class(out) <- c("rt_list", class(out))
    } else if (format == "tibble") {
      out <- fromJSON(readLines(pa, warn = FALSE, encoding = encoding))
      out <- list_to_tbl(out)
    } else if (format == "renv") {
      out <- lockfile_read(pa)
      class(out) <- c(class(out), "rt_list", "list")
    } else {
      stop("Argument 'format' must be one of 'list', 'tibble' or 'renv'.")
    }
    return(out)
  }

  if (length(paths) > 1) {
    l <- map(paths, ~ fn(.x, format = format))
    if (is.null(names(paths))) {
      padded_nums <- sprintf(paste0("%0", nchar(as.character(length(paths))), "d"), 1:length(paths))
      names(paths) <- paste0("lf_", padded_nums)
    }
    names(l) <- names(paths)
  } else {
    l <- fn(paths, format = format)
  }

  return(l)
}

#' @title Convert an rt_list to an rt_tibble
#' @description Convert a single rt_list to an rt_tibble
#' @param x A list containing one lockfile. A list from `read_lock()`.
#' @return A tibble of class 'rt_tibble'.
#' @importFrom tibble tibble
#' @importFrom tidyr unnest_wider
#'
list_to_tbl <- function(x) {
  if (!is.list(x)) stop("Input is not a list.")
  if (is.rt_tibble(x)) stop("Given input is of class 'rt_tibble'. Input must be a single 'rt_list' class (list).")
  if (is.rt_tibble(x[[1]])) stop("Given input contains class 'rt_tibble'. Input must be a single 'rt_list' class (list).")

  metadata <- x
  metadata$Packages <- NULL

  o <- x$Packages
  # Convert packages list to tibble
  out <- tibble(o) |> unnest_wider(o)

  attr(out, "metadata") <- metadata
  class(out) <- c("rt_tibble", class(out))

  return(out)
}

#' @title Convert a rt_tibble to an rt_list
#' @description Convert an rt_tibble to a rt_list
#' @param x A tibble of class 'rt_tibble' from one lockfile. A tibble from `read_lock()`.
#' @return A list
#' @importFrom purrr pmap set_names discard
#'
tbl_to_list <- function(x) {
  if (is.rt_tibble(x[[1]])) stop("Input must be a single tibble with class 'rt_tibble'.")
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
  class(final_list) <- c("rt_list", class(final_list))

  return(final_list)
}

#' @title Export a lock file
#' @description Write a rt_tibble or a list as a lock file
#' @param x A single list or a single 'rt_tibble'. An single file output from `read_lock()`.
#' @param path Output path with filename
#' @return None
#' @importFrom jsonlite toJSON
#' @export
#'
write_lock <- function(x, path) {
  if (!is.rt(x)) stop("Input must be a single rt_tibble (tibble) or a single rt_list (list) item.")
  if (is.rt_tibble(x)) x <- tbl_to_list(x)
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
#' l <- read_lock(paths, format = "list")
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
      renvver = map_chr(lock_files, get_version_renv),
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
#' l <- read_lock(paths, format = "list")
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
    a_renvver = get_version_renv(lst[[1]]),
    b_renvver = get_version_renv(lst[[2]]),
    jaccard = jaccard(a_pkgs, b_pkgs),
    a_pkgs_len = length(a_pkgs),
    b_pkgs_len = length(lst[[2]]$Packages),
    a_pkgs_len_unique = length(setdiff(a_pkgs, b_pkgs)),
    b_pkgs_len_unique = length(setdiff(b_pkgs, a_pkgs)),
    pkgs_total_len = length(union(a_pkgs, b_pkgs)),
    a_pkgs = list(a_pkgs),
    b_pkgs = list(b_pkgs),
    a_pkgs_unique = list(setdiff(a_pkgs, b_pkgs)),
    b_pkgs_unique = list(setdiff(b_pkgs, a_pkgs)),
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
#' l <- read_lock(paths, format = "list")
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

utils::globalVariables()
