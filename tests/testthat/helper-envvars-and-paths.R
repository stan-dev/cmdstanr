on_codecov <- function() {
  identical(Sys.getenv("R_COVR"), "true")
}

on_ci <- function() {
  isTRUE(as.logical(Sys.getenv("CI")))
}

not_on_cran <- function() {
  on_ci() || identical(Sys.getenv("NOT_CRAN"), "true")
}

mpi_toolchain_present <- function() {
  tryCatch(
    processx::run(command = "mpicxx", args = "--version")$status == 0 &&
    processx::run(command = "mpiexec", args = "--version")$status == 0,
    error=function(cond) {
      FALSE
    }
  )
}

delete_extensions <- function() {
  if (os_is_windows()) {
    c(".exe", ".o", ".hpp")
  } else {
    c("", ".o",".hpp")
  }
}
