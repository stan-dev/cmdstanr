# install_executable(): the last step of a build, putting the executable and its
# record in place as a pair. No C++ is compiled, we just use text files to stand
# in for the executables.

local_exe_fixture <- function(destination_exists = TRUE,
                              .local_envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .local_envir)
  fixture <- list(
    dir = dir,
    from = file.path(dir, "compiled-exe"),
    to = file.path(dir, "model-exe")
  )
  writeLines("new executable", fixture$from)
  # Compiled by make, so executable. Installation has to preserve that.
  Sys.chmod(fixture$from, "0755", use_umask = FALSE)
  fixture$record <- example_record(fixture$from)
  if (destination_exists) {
    writeLines("old executable", fixture$to)
    write_build_record(example_record(fixture$to), fixture$to)
  }
  fixture
}

# POSIX execute permissions are not available through Windows R, including WSL.
expect_installed_executable <- function(path) {
  expect_identical(readLines(path), "new executable")
  if (!os_is_windows()) {
    expect_identical(file.access(path, mode = 1)[[1]], 0L)
  }
}

# Replace platform-specific directory spellings and random filenames without
# hiding separator regressions in paths created by install_executable().
exe_path_transform <- function(fixture) {
  dirs <- unique(c(
    fixture$dir,
    repair_path(fixture$dir),
    gsub("\\\\", "/", fixture$dir)
  ))
  function(lines) {
    for (dir in dirs) {
      lines <- gsub(dir, "<dir>", lines, fixed = TRUE)
    }
    gsub("(exe-new|exe-old|record-old)-[0-9a-f]+", "\\1-<random>", lines)
  }
}

# Make the n-th file.rename() call fail, optionally warning first, as base does.
local_failing_file_rename <- function(fail_on,
                                      warn = FALSE,
                                      .local_envir = parent.frame()) {
  real_file_rename <- base::file.rename
  calls <- 0
  local_mocked_bindings(
    file.rename = function(from, to) {
      calls <<- calls + 1
      if (calls %in% fail_on) {
        if (warn) warning("cannot rename file")
        return(FALSE)
      }
      real_file_rename(from, to)
    },
    .package = "base",
    .env = .local_envir
  )
}

test_that("install_executable() installs when there is no existing executable", {
  fixture <- local_exe_fixture(destination_exists = FALSE)

  expect_null(install_executable(fixture$from, fixture$to, fixture$record))
  expect_installed_executable(fixture$to)
  expect_setequal(list.files(fixture$dir), basename(c(fixture$from, fixture$to)))
})

test_that("install_executable() replaces an executable and removes the backup", {
  fixture <- local_exe_fixture()

  expect_null(install_executable(fixture$from, fixture$to, fixture$record))
  expect_installed_executable(fixture$to)
  expect_setequal(list.files(fixture$dir), basename(c(fixture$from, fixture$to)))
})

test_that("install_executable() refuses to install over a directory", {
  fixture <- local_exe_fixture(destination_exists = FALSE)
  dir.create(fixture$to)
  writeLines("important", file.path(fixture$to, "data.txt"))

  # Directories satisfy file.exists(), so reject them before staging or renaming.
  # exe_file= can pass a directory here.
  expect_error(
    install_executable(fixture$from, fixture$to, fixture$record),
    "is a directory",
    fixed = TRUE
  )
  expect_true(dir.exists(fixture$to))
  expect_identical(readLines(file.path(fixture$to, "data.txt")), "important")
  expect_setequal(
    list.files(fixture$dir),
    basename(c(fixture$from, fixture$to))
  )
})

test_that("install_executable() leaves the destination alone if staging fails", {
  fixture <- local_exe_fixture()
  local_mocked_bindings(file.copy = function(...) FALSE, .package = "base")

  expect_snapshot(
    error = TRUE,
    install_executable(fixture$from, fixture$to, fixture$record),
    transform = exe_path_transform(fixture)
  )
  expect_identical(readLines(fixture$to), "old executable")
  expect_equal(read_build_record(fixture$to)$record, example_record(fixture$to))
  expect_setequal(list.files(fixture$dir), basename(c(fixture$from, fixture$to)))
})

test_that("install_executable() leaves the destination alone if the backup fails", {
  fixture <- local_exe_fixture()
  # The first rename is the record's own staging write.
  local_failing_file_rename(fail_on = 2)

  expect_snapshot(
    error = TRUE,
    install_executable(fixture$from, fixture$to, fixture$record),
    transform = exe_path_transform(fixture)
  )
  expect_identical(readLines(fixture$to), "old executable")
  expect_equal(read_build_record(fixture$to)$record, example_record(fixture$to))
  expect_setequal(list.files(fixture$dir), basename(c(fixture$from, fixture$to)))
})

test_that("install_executable() restores the backup if the install fails", {
  fixture <- local_exe_fixture()
  # The renames are the record's staging write, the executable backup, the
  # record backup, then the install.
  local_failing_file_rename(fail_on = 4)

  expect_snapshot(
    error = TRUE,
    install_executable(fixture$from, fixture$to, fixture$record),
    transform = exe_path_transform(fixture)
  )
  expect_identical(readLines(fixture$to), "old executable")
  expect_equal(read_build_record(fixture$to)$record, example_record(fixture$to))
  expect_setequal(list.files(fixture$dir), basename(c(fixture$from, fixture$to)))
})

test_that("install_executable() keeps the backup if it cannot be restored", {
  fixture <- local_exe_fixture()
  # The install fails, the record goes back, and the executable cannot follow.
  local_failing_file_rename(fail_on = c(4, 6))

  expect_snapshot(
    error = TRUE,
    install_executable(fixture$from, fixture$to, fixture$record),
    transform = exe_path_transform(fixture)
  )
  # The destination is gone, so the error has to name a real recovery path.
  expect_false(file.exists(fixture$to))
  leftover <- setdiff(list.files(fixture$dir), basename(fixture$from))
  expect_match(leftover, "^exe-old-")
  expect_identical(readLines(file.path(fixture$dir, leftover)), "old executable")
})

test_that("install_executable() rolls back when warnings are errors", {
  fixture <- local_exe_fixture()
  # file.rename() warnings must not interrupt rollback when warn = 2.
  local_failing_file_rename(fail_on = 4, warn = TRUE)
  withr::local_options(warn = 2)

  expect_error(
    install_executable(fixture$from, fixture$to, fixture$record),
    "are as they were",
    fixed = TRUE
  )
  expect_identical(readLines(fixture$to), "old executable")
  expect_equal(read_build_record(fixture$to)$record, example_record(fixture$to))
})

test_that("install_executable() reports every backup it could not remove", {
  fixture <- local_exe_fixture()
  local_mocked_bindings(unlink = function(...) 1L, .package = "base")

  # Return the backups without warning so the caller can commit state first.
  expect_no_warning(
    leftover <- install_executable(fixture$from, fixture$to, fixture$record)
  )
  expect_identical(readLines(fixture$to), "new executable")
  expect_equal(read_build_record(fixture$to)$record, fixture$record)
  expect_length(leftover, 2)
  expect_match(basename(leftover[1]), "^exe-old-")
  expect_match(basename(leftover[2]), "^record-old-")
  expect_identical(readLines(leftover[1]), "old executable")
})

test_that("install_executable() writes the record beside a fresh install", {
  fixture <- local_exe_fixture(destination_exists = FALSE)

  expect_null(install_executable(fixture$from, fixture$to, fixture$record))
  expect_equal(read_build_record(fixture$to)$status, "available")
  expect_setequal(
    list.files(fixture$dir, all.files = TRUE, no.. = TRUE),
    basename(c(fixture$from, fixture$to, build_record_path(fixture$to)))
  )
})

test_that("install_executable() replaces the record with the executable", {
  fixture <- local_exe_fixture()

  expect_null(install_executable(fixture$from, fixture$to, fixture$record))
  expect_equal(read_build_record(fixture$to)$record, fixture$record)
  expect_setequal(
    list.files(fixture$dir, all.files = TRUE, no.. = TRUE),
    basename(c(fixture$from, fixture$to, build_record_path(fixture$to)))
  )
})

test_that("install_executable() restores both files if the record cannot be written", {
  fixture <- local_exe_fixture()
  local_mocked_bindings(write_build_record = function(...) stop("disk full"))

  expect_error(
    install_executable(fixture$from, fixture$to, fixture$record),
    "disk full",
    fixed = TRUE
  )
  expect_identical(readLines(fixture$to), "old executable")
  expect_equal(read_build_record(fixture$to)$record, example_record(fixture$to))
  expect_setequal(
    list.files(fixture$dir, all.files = TRUE, no.. = TRUE),
    basename(c(fixture$from, fixture$to, build_record_path(fixture$to)))
  )
})

test_that("install_executable() restores both files if the pair fails verification", {
  fixture <- local_exe_fixture()
  # A record that describes some other executable still writes, so only the
  # verification at the end of the transaction can catch it.
  fixture$record$executable_hash <- "deadbeef"

  expect_error(
    install_executable(fixture$from, fixture$to, fixture$record),
    "executable_mismatch",
    fixed = TRUE
  )
  expect_identical(readLines(fixture$to), "old executable")
  expect_equal(read_build_record(fixture$to)$record, example_record(fixture$to))
  expect_setequal(
    list.files(fixture$dir, all.files = TRUE, no.. = TRUE),
    basename(c(fixture$from, fixture$to, build_record_path(fixture$to)))
  )
})
