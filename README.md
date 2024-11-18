<a href="https://github.com/royfrancis/renvtools">
  <img src="pkgdown/favicon/logo.webp" alt="renvtools logo" align="right" width="130px" style="position:relative;z-index:2;padding:5px;float:right;">
</a>

# renvtools

[![ci_badge](https://github.com/royfrancis/renvtools/workflows/r-cmd-check/badge.svg)](https://github.com/royfrancis/renvtools/actions?workflow=r-cmd-check) [![codecov](https://codecov.io/gh/royfrancis/renvtools/branch/main/graph/badge.svg?token=4DOQ8HNQFK)](https://app.codecov.io/gh/royfrancis/renvtools/) [![lifecycle_badge](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

An R package to read, write, summarize and compare renv lock files.

- Read lock files as lists or tibbles
- Write lock files
- Summarize and compare lock files

```r
# installation
install.packages(c("dplyr", "jsonlite", "purrr", "renv", "tibble", "tidyr", "remotes"))
remotes::install_github('royfrancis/renvtools')
```

See the [project website](http://royfrancis.github.io/renvtools) for a walkthrough.

Quick start for the impatient.

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

## Disclaimer

This R package is offered free and without warranty of any kind, either expressed or implied. I will not be held liable to you for any damage arising out of the use, modification or inability to use this program. This R package can be used, redistributed and/or modified freely for non-commercial purposes subject to the original source being properly cited. Licensed under GPL-3.  

## Contact

If you have an comments, suggestions, corrections or enchancements, kindly submit an issue on the [Github issues page](https://github.com/royfrancis/renvtools/issues).  

---

2024 • Roy Francis
