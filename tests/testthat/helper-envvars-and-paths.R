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

# Tracks whether an outer local_make_local_backup() is holding the on-disk
# backup, so nested calls restore in memory without disturbing it.
make_local_backup <- new.env(parent = emptyenv())
make_local_backup$held <- FALSE

make_local_backup_path <- function() {
  # make/ is included file by file, with no globs, so this is inert to make.
  file.path(cmdstan_path(), "make", "local.cmdstanr-test-backup")
}

# Restore make/local from the on-disk backup and remove it. An empty backup
# stands for "no make/local", which make treats the same as an empty one.
restore_cmdstan_make_local <- function() {
  backup_path <- make_local_backup_path()
  if (!file.exists(backup_path)) {
    return(invisible(NULL))
  }
  make_local_path <- file.path(cmdstan_path(), "make", "local")
  if (file.size(backup_path) == 0) {
    unlink(make_local_path)
  } else {
    file.copy(backup_path, make_local_path, overwrite = TRUE)
  }
  unlink(backup_path)
  invisible(NULL)
}

# Restore the make/local of the current CmdStan installation when `envir` exits,
# without writing to it. Use this directly when the test does its own writing;
# use local_cmdstan_make_local() below when it just wants options set.
#
# The outermost call also copies make/local aside for the duration. That copy is
# what makes a killed run recoverable: the restore below never runs, but the next
# call heals from the backup before taking its own snapshot, instead of adopting
# the residue as its baseline and compounding it.
local_make_local_backup <- function(envir = parent.frame()) {
  make_local_path <- file.path(cmdstan_path(), "make", "local")
  outermost <- !isTRUE(make_local_backup$held)
  if (outermost) {
    restore_cmdstan_make_local()
    backup_path <- make_local_backup_path()
    if (file.exists(make_local_path)) {
      file.copy(make_local_path, backup_path, overwrite = TRUE)
    } else {
      file.create(backup_path)
    }
    make_local_backup$held <- TRUE
  }
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
      if (outermost) {
        unlink(make_local_backup_path())
        make_local_backup$held <- FALSE
      }
    },
    envir = envir
  )
  invisible(NULL)
}

# Write cpp_options to the make/local of the current CmdStan installation and
# restore its original contents (or absence) when `envir` exits. Called at the
# top level of a test file, the restore runs after all tests in that file.
local_cmdstan_make_local <- function(cpp_options, envir = parent.frame(),
                                     append = TRUE) {
  local_make_local_backup(envir = envir)
  cmdstan_make_local(cpp_options = cpp_options, append = append)
}
