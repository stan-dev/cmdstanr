beroulli_example_file <- function() {
  file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
}

bernoulli_example_data <- function() {
  list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))
}

on_appveyor <- function() {
  identical(tolower(Sys.getenv("APPVEYOR")), "true")
}

on_travis <- function() {
  identical(Sys.getenv("TRAVIS"), "true")
}

on_codecov <- function() {
  identical(Sys.getenv("R_COVR"), "true")
}

not_on_cran <- function() {
  on_travis() || on_appveyor() || identical(Sys.getenv("NOT_CRAN"), "true")
}

delete_extensions <- function() {
  if (os_is_windows()) {
    c(".exe", ".o", ".hpp")
  } else {
    c("", ".o",".hpp")
  }
}
