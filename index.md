# renvtools <img src="logo.webp" alt="pixture-logo" style="width:120px;float:right;" align="right" /> 

[![ci_badge](https://github.com/royfrancis/renvtools/workflows/r-cmd-check/badge.svg)](https://github.com/royfrancis/renvtools/actions?workflow=r-cmd-check) [![codecov](https://codecov.io/gh/royfrancis/renvtools/branch/main/graph/badge.svg?token=4DOQ8HNQFK)](https://app.codecov.io/gh/royfrancis/renvtools/) [![lifecycle_badge](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

An R package to read, write and compare renv lock files.

- Read lock files as lists or tibbles
- Write lock files
- Summarize and compare lock files

## Installation

```
# install dependencies
install.packages(c("dplyr", "jsonlite", "purrr", "renv", "tibble", "tidyr", "remotes"))

# install from github
remotes::install_github("royfrancis/renvtools")
```

For usage and demonstration, refer to the articles linked in the menu.

## Quick start

A quick start for the impatient.

```r
library(renvtools)

# path to lock file
path <- file.path(system.file("extdata", package = "renvtools"), "renv-r4.4.1.lock")
# read as list
lst <- read_lock(path)
# read as tibble
tbl <- read_lock(path, format = "tibble")
# write lockfile
write_lock(tbl, "renv-mod.lock")

# read multiple lock files
paths <- list.files(file.path(system.file("extdata", package = "renvtools")), full.names = TRUE)
# read as tibble
tbl <- read_lock(paths, format = "tibble")
# read as list
lst <- read_lock(paths, format = "list")

# summarize lock files
d <- summarize_lock(lst)
# compare lock files pairwise
d <- compare_lock(lst)
```

### Disclaimer

This R package is offered free and without warranty of any kind, either expressed or implied. I will not be held liable to you for any damage arising out of the use, modification or inability to use this program. This R package can be used, redistributed and/or modified freely for non-commercial purposes subject to the original source being properly cited. Licensed under GPL-3.  
