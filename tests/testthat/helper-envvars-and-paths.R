on_codecov <- function() {
  identical(Sys.getenv("R_COVR"), "true")
}

on_ci <- function() {
  isTRUE(as.logical(Sys.getenv("CI")))
}

not_on_cran <- function() {
  on_ci() || identical(Sys.getenv("NOT_CRAN"), "true")
}

test_release_url <- function() {
  "https://github.com/stan-dev/cmdstan/releases/download/v2.25.0/cmdstan-2.25.0.tar.gz"
}

delete_extensions <- function() {
  if (os_is_windows()) {
    c(".exe", ".o", ".hpp")
  } else {
    c("", ".o",".hpp")
  }
}
