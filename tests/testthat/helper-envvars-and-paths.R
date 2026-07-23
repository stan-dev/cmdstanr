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

# Write cpp_options to the make/local of the current CmdStan installation and
# restore its original contents (or absence) when `envir` exits. Called at the
# top level of a test file, the restore runs after all tests in that file.
local_cmdstan_make_local <- function(cpp_options, envir = parent.frame()) {
  make_local_path <- file.path(cmdstan_path(), "make", "local")
  make_local_orig <- if (file.exists(make_local_path)) {
    readBin(make_local_path, "raw", file.size(make_local_path))
  } else {
    NULL
  }
  withr::defer(
    {
      if (is.null(make_local_orig)) {
        unlink(make_local_path)
      } else {
        writeBin(make_local_orig, make_local_path)
      }
    },
    envir = envir
  )
  cmdstan_make_local(cpp_options = cpp_options)
}
