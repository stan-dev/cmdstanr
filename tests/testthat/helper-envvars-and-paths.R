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

read_make_local_contents <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  tryCatch(
    readBin(path, "raw", file.size(path)),
    error = function(e) FALSE
  )
}

# Restore make/local from the on-disk backup and remove it. An empty backup
# stands for "no make/local", which make treats the same as an empty one.
restore_cmdstan_make_local <- function() {
  backup_path <- make_local_backup_path()
  if (!file.exists(backup_path)) {
    return(invisible(NULL))
  }
  make_local_path <- file.path(cmdstan_path(), "make", "local")
  backup_contents <- read_make_local_contents(backup_path)
  if (!is.raw(backup_contents)) {
    restored <- FALSE
  } else if (length(backup_contents) == 0) {
    suppressWarnings(unlink(make_local_path))
    restored <- !file.exists(make_local_path)
  } else {
    tryCatch(
      suppressWarnings(file.copy(backup_path, make_local_path, overwrite = TRUE)),
      error = function(e) FALSE
    )
    restored <- identical(
      read_make_local_contents(make_local_path),
      backup_contents
    )
  }
  if (!isTRUE(restored)) {
    stop(
      "Could not restore CmdStan's 'make/local'. The recovery backup has ",
      "been retained at '", backup_path, "'.",
      call. = FALSE
    )
  }
  suppressWarnings(unlink(backup_path))
  if (file.exists(backup_path)) {
    stop(
      "CmdStan's 'make/local' was restored, but the recovery backup could ",
      "not be removed from '", backup_path, "'.",
      call. = FALSE
    )
  }
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
    make_local_orig <- read_make_local_contents(make_local_path)
    if (is.raw(make_local_orig)) {
      created <- tryCatch(
        suppressWarnings(
          file.copy(make_local_path, backup_path, overwrite = TRUE)
        ),
        error = function(e) FALSE
      )
    } else {
      created <- tryCatch(
        suppressWarnings(file.create(backup_path)),
        error = function(e) FALSE
      )
    }
    backup_contents <- read_make_local_contents(backup_path)
    expected_backup <- if (is.null(make_local_orig)) raw() else make_local_orig
    if (!isTRUE(created) || !identical(backup_contents, expected_backup)) {
      suppressWarnings(unlink(backup_path))
      stop(
        "Could not create a verified recovery backup of CmdStan's ",
        "'make/local' at '", backup_path, "'. 'make/local' was not modified.",
        call. = FALSE
      )
    }
    make_local_backup$held <- TRUE
  } else {
    make_local_orig <- read_make_local_contents(make_local_path)
    if (!is.null(make_local_orig) && !is.raw(make_local_orig)) {
      stop(
        "Could not snapshot CmdStan's 'make/local' for nested restoration. ",
        "The recovery backup is at '", make_local_backup_path(), "'.",
        call. = FALSE
      )
    }
  }
  withr::defer(
    {
      if (outermost) {
        # Clear this before restoration so an error cannot poison later calls.
        make_local_backup$held <- FALSE
        restore_cmdstan_make_local()
      } else {
        tryCatch(
          {
            if (is.null(make_local_orig)) {
              suppressWarnings(unlink(make_local_path))
            } else {
              writeBin(make_local_orig, make_local_path)
            }
          },
          error = function(e) FALSE
        )
        if (!identical(
          read_make_local_contents(make_local_path),
          make_local_orig
        )) {
          stop(
            "Could not restore nested CmdStan 'make/local' state. The ",
            "recovery backup has been retained at '",
            make_local_backup_path(), "'.",
            call. = FALSE
          )
        }
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
