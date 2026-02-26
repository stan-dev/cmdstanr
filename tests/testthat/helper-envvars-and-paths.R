on_codecov <- function() {
  identical(Sys.getenv("R_COVR"), "true")
}

on_ci <- function() {
  isTRUE(as.logical(Sys.getenv("CI")))
}

mpi_toolchain_present <- function() {
  tryCatch(
    wsl_compatible_run(command = "mpicxx", args = "--version")$status == 0 &&
    wsl_compatible_run(command = "mpiexec", args = "--version")$status == 0,
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

skip_if_legacy_win41_pareto <- function() {
  if (tolower(Sys.getenv("CMDSTANR_SKIP_PARETO_SMOOTH_INIT_TESTS")) %in% c("1", "true")) {
    skip("Skipping tests requiring posterior::pareto_smooth on windows-2022 R 4.1 CI.")
  }
}
