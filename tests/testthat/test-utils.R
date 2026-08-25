set_cmdstan_path()
fit_mcmc <- testing_fit("logistic", method = "sample",
                        seed = 123, chains = 2)
fit_mle <- testing_fit("logistic", method = "opt", seed = 123)



# diagnostic checks -------------------------------------------------------

test_that("check_divergences() works", {
  csv_files <- c(test_path("resources", "csv", "model1-2-no-warmup.csv"))
  csv_output <- read_cmdstan_csv(csv_files)
  output <- "14 of 100 \\(14.0%\\) transitions ended with a divergence."
  expect_message(divs <- check_divergences(csv_output$post_warmup_sampler_diagnostics), output)
  expect_equal(divs, 14)

  csv_files <- c(test_path("resources", "csv", "model1-2-no-warmup.csv"),
                 test_path("resources", "csv", "model1-2-no-warmup.csv"))
  csv_output <- read_cmdstan_csv(csv_files)
  output <- "28 of 200 \\(14.0%\\) transitions ended with a divergence."
  expect_message(divs <- check_divergences(csv_output$post_warmup_sampler_diagnostics), output)
  expect_equal(divs, c(14, 14))

  # force different number of divergences per chain just to test
  csv_output$post_warmup_sampler_diagnostics[1, 1:2, "divergent__"] <- c(0, 1)
  output <- "27 of 200 \\(14.0%\\) transitions ended with a divergence."
  expect_message(divs <- check_divergences(csv_output$post_warmup_sampler_diagnostics), output)
  expect_equal(divs, c(13, 14))

  csv_files <- c(test_path("resources", "csv", "model1-2-warmup.csv"))
  csv_output <- read_cmdstan_csv(csv_files)
  output <- "1 of 100 \\(1.0%\\) transitions ended with a divergence."
  expect_message(check_divergences(csv_output$post_warmup_sampler_diagnostics), output)


  fit_wramup_no_samples <- testing_fit("logistic", method = "sample",
                          seed = 123, chains = 1,
                          iter_sampling = 0,
                          iter_warmup = 10,
                          save_warmup = TRUE,
                          diagnostics = "")
  csv_output <- read_cmdstan_csv(fit_wramup_no_samples$output_files())
  expect_message(divs <- check_divergences(csv_output$post_warmup_sampler_diagnostics), regexp = NA)
  expect_null(divs)
})

test_that("check_max_treedepth() works", {
  csv_files <- c(test_path("resources", "csv", "model1-2-no-warmup.csv"))
  csv_output <- read_cmdstan_csv(csv_files)
  output <- "16 of 100 \\(16.0%\\) transitions hit the maximum treedepth limit of 5."
  expect_message(
    max_tds <- check_max_treedepth(
      csv_output$post_warmup_sampler_diagnostics,
      csv_output$metadata),
    output
  )
  expect_equal(max_tds, 16)

  csv_files <- c(test_path("resources", "csv", "model1-2-no-warmup.csv"),
                 test_path("resources", "csv", "model1-2-no-warmup.csv"))
  csv_output <- read_cmdstan_csv(csv_files)
  output <- "32 of 200 \\(16.0%\\) transitions hit the maximum treedepth limit of 5."
  expect_message(
    max_tds <- check_max_treedepth(
      csv_output$post_warmup_sampler_diagnostics,
      csv_output$metadata),
    output
  )
  expect_equal(max_tds, c(16, 16))

  # force different number of max treedepths per chain just to test
  csv_output$post_warmup_sampler_diagnostics[1, 1:2, "treedepth__"] <- c(1, 15)
  output <- "31 of 200 \\(16.0%\\) transitions hit the maximum treedepth limit of 5."
  expect_message(
    max_tds <- check_max_treedepth(
      csv_output$post_warmup_sampler_diagnostics,
      csv_output$metadata),
    output
  )
  expect_equal(max_tds, c(15, 16))

  csv_files <- c(test_path("resources", "csv", "model1-2-warmup.csv"))
  csv_output <- read_cmdstan_csv(csv_files)
  output <- "1 of 100 \\(1.0%\\) transitions hit the maximum treedepth limit of 5."
  expect_message(
    check_max_treedepth(
      csv_output$post_warmup_sampler_diagnostics,
      csv_output$metadata),
    output
  )
})

test_that("check_ebfmi and computing ebfmi works", {
  set.seed(1)
  energy_df <- data.frame("energy__" = rnorm(1000))
  expect_error(suppressWarnings(check_ebfmi(posterior::as_draws(energy_df))), NA)
  expect_error(suppressWarnings(ebfmi(posterior::as_draws(energy_df))), NA)
  energy_df[1] <- 0
  for(i in 1:999){
    energy_df$energy__[i+1] <- energy_df$energy__[i] + rnorm(1, 0, 0.01)
  }
  energy_df <- posterior::as_draws(energy_df)
  expect_message(check_ebfmi(energy_df), "had an E-BFMI less than")
  energy_vec <- energy_df$energy__
  check_val <- (sum(diff(energy_vec)^2) / length(energy_vec)) / stats::var(energy_vec)
  expect_equal(as.numeric(ebfmi(energy_df)), check_val)
  expect_equal(as.numeric(ebfmi(posterior::as_draws_array(energy_df))), check_val)
  expect_equal(as.numeric(ebfmi(posterior::as_draws_list(energy_df))), check_val)
  expect_equal(as.numeric(ebfmi(posterior::as_draws_matrix(energy_df))), check_val)
  energy_df <- posterior::as_draws(data.frame("energy__" = 0))
  expect_warning(check_ebfmi(energy_df), "E-BFMI not computed because it is undefined for posterior chains of length less than 3.")
  expect_warning(ebfmi(energy_df), "E-BFMI not computed because it is undefined for posterior chains of length less than 3.")

  energy_df <- posterior::as_draws(data.frame("somethingelse" = 0))
  expect_warning(check_ebfmi(energy_df), "E-BFMI not computed because the 'energy__' diagnostic could not be located.")
  expect_warning(ebfmi(energy_df), "E-BFMI not computed because the 'energy__' diagnostic could not be located.")
})


# cmdstan utilities -------------------------------------------------------

test_that("cmdstan_summary works if bin/stansummary deleted file", {
  delete_and_run <- function() {
    file.remove(file.path(cmdstan_path(), "bin", cmdstan_ext("stansummary")))
    fit_mcmc$cmdstan_summary()
  }
  expect_output(delete_and_run(), "Inference for Stan model: logistic_model")
})

test_that("cmdstan_diagnose works if bin/diagnose deleted file", {
  delete_and_run <- function() {
    file.remove(file.path(cmdstan_path(), "bin", cmdstan_ext("diagnose")))
    fit_mcmc$cmdstan_diagnose()
  }
  expect_output(delete_and_run(), "Checking sampler transitions treedepth")
})

test_that("get_standalone_hpp() reports stanc failures", {
  model_dir <- withr::local_tempdir()
  stan_file <- file.path(model_dir, "model.stan")
  hpp_file <- file.path(model_dir, "model.hpp")
  writeLines("parameters { real y; } model { y ~ std_normal(); }", stan_file)
  writeLines("// partial output", hpp_file)
  local_mocked_bindings(
    wsl_compatible_run = function(...) {
      list(
        status = 124L,
        stdout = "",
        stderr = "stanc: invalid canonicalize value"
      )
    }
  )

  expect_snapshot(
    error = TRUE,
    get_standalone_hpp(
      stan_file,
      "--canonicalize='deprecations'"
    )
  )
  expect_false(file.exists(hpp_file))
})

test_that("get_standalone_hpp() suggests formatting deprecated syntax", {
  stan_file <- withr::local_tempfile(fileext = ".stan")
  local_mocked_bindings(
    wsl_compatible_run = function(...) {
      list(
        status = 1L,
        stdout = "",
        stderr = "Syntax error: Use the auto-format flag to stanc"
      )
    }
  )

  expect_snapshot(
    error = TRUE,
    get_standalone_hpp(stan_file, character())
  )
})


# misc --------------------------------------------------------------------

test_that("generate_file_names() zero-pads IDs for lexicographic sorting", {
  expect_equal(
    generate_file_names(
      basename = "output",
      ids = 1:10,
      timestamp = FALSE,
      random = FALSE
    ),
    paste0("output-", sprintf("%02d", 1:10), ".csv")
  )

  file_names <- generate_file_names(
    basename = "output",
    ids = 1:100,
    timestamp = FALSE,
    random = FALSE
  )
  expect_equal(
    file_names[c(1, 9, 10, 100)],
    paste0("output-", c("001", "009", "010", "100"), ".csv")
  )
  expect_equal(sort(file_names), file_names)
})

test_that("copy_temp_files retains sources if any copy fails", {
  source_dir <- withr::local_tempdir()
  destination_dir <- withr::local_tempdir()
  source_paths <- file.path(source_dir, c("one.csv", "two.csv"))
  writeLines("one", source_paths[1])
  writeLines("two", source_paths[2])
  # Simulate a partial copy failure without relying on platform-specific file
  # permissions. The original binding is restored at the end of the test.
  local_mocked_bindings(
    file.copy = function(...) c(TRUE, FALSE),
    .package = "base"
  )

  expect_snapshot(
    error = TRUE,
    copy_temp_files(
      current_paths = source_paths,
      new_dir = destination_dir,
      new_basename = "output",
      ids = 1:2,
      timestamp = FALSE,
      random = FALSE
    )
  )
  expect_identical(file.exists(source_paths), c(TRUE, TRUE))
})

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
  if (destination_exists) {
    writeLines("old executable", fixture$to)
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
    gsub("exe-(new|old)-[0-9a-f]+", "exe-\\1-<random>", lines)
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

  expect_null(install_executable(fixture$from, fixture$to))
  expect_installed_executable(fixture$to)
  expect_setequal(list.files(fixture$dir), basename(c(fixture$from, fixture$to)))
})

test_that("install_executable() replaces an executable and removes the backup", {
  fixture <- local_exe_fixture()

  expect_null(install_executable(fixture$from, fixture$to))
  expect_installed_executable(fixture$to)
  expect_setequal(list.files(fixture$dir), basename(c(fixture$from, fixture$to)))
})

test_that("install_executable() refuses to install over a directory", {
  fixture <- local_exe_fixture(destination_exists = FALSE)
  dir.create(fixture$to)
  writeLines("important", file.path(fixture$to, "data.txt"))

  # Directories satisfy file.exists(), so reject them before staging or renaming.
  # Both $exe_file(path) and exe_file= can pass a directory here.
  expect_error(
    install_executable(fixture$from, fixture$to),
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
    install_executable(fixture$from, fixture$to),
    transform = exe_path_transform(fixture)
  )
  expect_identical(readLines(fixture$to), "old executable")
  expect_setequal(list.files(fixture$dir), basename(c(fixture$from, fixture$to)))
})

test_that("install_executable() leaves the destination alone if the backup fails", {
  fixture <- local_exe_fixture()
  local_failing_file_rename(fail_on = 1)

  expect_snapshot(
    error = TRUE,
    install_executable(fixture$from, fixture$to),
    transform = exe_path_transform(fixture)
  )
  expect_identical(readLines(fixture$to), "old executable")
  expect_setequal(list.files(fixture$dir), basename(c(fixture$from, fixture$to)))
})

test_that("install_executable() restores the backup if the install fails", {
  fixture <- local_exe_fixture()
  local_failing_file_rename(fail_on = 2)

  expect_snapshot(
    error = TRUE,
    install_executable(fixture$from, fixture$to),
    transform = exe_path_transform(fixture)
  )
  expect_identical(readLines(fixture$to), "old executable")
  expect_setequal(list.files(fixture$dir), basename(c(fixture$from, fixture$to)))
})

test_that("install_executable() keeps the backup if it cannot be restored", {
  fixture <- local_exe_fixture()
  local_failing_file_rename(fail_on = c(2, 3))

  expect_snapshot(
    error = TRUE,
    install_executable(fixture$from, fixture$to),
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
  local_failing_file_rename(fail_on = 2, warn = TRUE)
  withr::local_options(warn = 2)

  expect_error(
    install_executable(fixture$from, fixture$to),
    "previously compiled executable has been restored",
    fixed = TRUE
  )
  expect_identical(readLines(fixture$to), "old executable")
})

test_that("install_executable() reports a backup it could not remove", {
  fixture <- local_exe_fixture()
  local_mocked_bindings(unlink = function(...) 1L, .package = "base")

  # Return the backup without warning so the caller can commit state first.
  expect_no_warning(leftover <- install_executable(fixture$from, fixture$to))
  expect_identical(readLines(fixture$to), "new executable")
  expect_true(file.exists(leftover))
  expect_identical(readLines(leftover), "old executable")
})

test_that("repair_path() fixes slashes", {
  # all slashes should be single "/", and no trailing slash
  expect_equal(repair_path("a//b\\c/"), "a/b/c")
  # but leading double slash is needed for UNC paths on  (e.g. the cmdstan path on WSL)
  expect_equal(repair_path("\\\\wsl//my-project//"), "//wsl/my-project")
})

test_that("repair_path works with zero length path or non-string path", {
  expect_equal(repair_path(""), "")
  expect_equal(repair_path(5), 5)
})

test_that("repair_path works with multiple paths", {
  expect_equal(repair_path(c("a//b\\c/", "d\\e//f")), c("a/b/c", "d/e/f"))
})

test_that("wsl_safe_path() works with multiple paths", {
  with_mocked_bindings(
    {
      expect_equal(
        wsl_safe_path(
          c(
            "/mnt/c/project/init-1.json",
            "/mnt/d/project/init-2.json",
            "relative/init-3.json"
          ),
          revert = TRUE
        ),
        c(
          "C:/project/init-1.json",
          "D:/project/init-2.json",
          "relative/init-3.json"
        )
      )
      expect_equal(
        wsl_safe_path(
          c(
            "//wsl$/Ubuntu/tmp/init-1.json",
            "//wsl$/Ubuntu/tmp/init-2.json"
          )
        ),
        c("/tmp/init-1.json", "/tmp/init-2.json")
      )
    },
    os_is_wsl = function() TRUE,
    wsl_dir_prefix = function(...) "//wsl$/Ubuntu"
  )
})

test_that("wsl_compatible_run() preserves arguments containing spaces", {
  skip_if_not(os_is_wsl())
  arg <- "--filename-in-msg=model filename with spaces.stan"
  result <- wsl_compatible_run(
    command = "printf",
    args = c("%s", arg),
    wd = cmdstan_path()
  )

  expect_equal(result$status, 0L)
  expect_equal(result$stdout, arg)
})

test_that("list_to_array works with empty list", {
  expect_equal(list_to_array(list()), NULL)
})

test_that("list_to_array fails for non-numeric values", {
  expect_error(list_to_array(list(k = "test"), name = "test-list"),
               "All elements in list 'test-list' must be numeric or logical!")
})

test_that("cmdstan_make_local() works", {
  exisiting_make_local <- cmdstan_make_local()
  make_local_path <- file.path(cmdstan_path(), "make", "local")
  if (file.exists(make_local_path)) {
    file.remove(make_local_path)
  }
  expect_equal(cmdstan_make_local(), NULL)
  cpp_options = list(
   "CXX" = "clang++",
   "CXXFLAGS+= -march=native",
   TEST1 = TRUE,
   "TEST2" = FALSE
  )
  expect_equal(cmdstan_make_local(cpp_options = cpp_options),
               c(
                 "CXX=clang++",
                 "CXXFLAGS+= -march=native",
                 "TEST1=true",
                 "TEST2=false"
                 ))
  expect_equal(cmdstan_make_local(cpp_options = list("TEST3" = TRUE)),
               c(
                 "CXX=clang++",
                 "CXXFLAGS+= -march=native",
                 "TEST1=true",
                 "TEST2=false",
                 "TEST3=true"
               ))
  expect_equal(cmdstan_make_local(cpp_options = list("TEST4" = TRUE), append = FALSE),
               c("TEST4=true"))
  cmdstan_make_local(cpp_options = as.list(exisiting_make_local), append = FALSE)
})

test_that("cmdstan_make_local() preserves empty make/local behavior", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "make"), recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(dir, "make", "local"))

  expect_identical(cmdstan_make_local(dir = dir), "")
})

test_that("cmdstan_make_local() reads back written make flags", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "make"), recursive = TRUE, showWarnings = FALSE)

  expect_null(cmdstan_make_local(dir = dir))
  expect_equal(
    cmdstan_make_local(
      dir = dir,
      cpp_options = list("CXX" = "clang++", STAN_THREADS = TRUE)
    ),
    c("CXX=clang++", "STAN_THREADS=true")
  )
  expect_equal(
    cmdstan_make_local(dir = dir, cpp_options = list("PRECOMPILED_HEADERS" = FALSE)),
    c("CXX=clang++", "STAN_THREADS=true", "PRECOMPILED_HEADERS=false")
  )
  expect_equal(
    cmdstan_make_local(dir = dir, cpp_options = list("CXX" = "g++"), append = FALSE),
    "CXX=g++"
  )
})

test_that("matching_variables() works", {
  ret <- matching_variables(c("beta"),  c("alpha", "beta[1]", "beta[2]", "beta[3]"))
  expect_equal(
    ret$matching,
    c("beta[1]", "beta[2]", "beta[3]")
  )
  expect_equal(length(ret$not_found), 0)

  ret <- matching_variables(c("alpha"),  c("alpha", "beta[1]", "beta[2]", "beta[3]"))
  expect_equal(
    ret$matching,
    c("alpha")
  )
  expect_equal(length(ret$not_found), 0)

  ret <- matching_variables(c("alpha", "theta"),  c("alpha", "beta[1]", "beta[2]", "beta[3]"))
  expect_equal(
    ret$matching,
    c("alpha")
  )
  expect_equal(
    ret$not_found,
    c("theta")
  )

  ret <- matching_variables(c("alpha", "beta"),  c("alpha", "beta[1]", "beta[2]", "beta[3]"))
  expect_equal(
    ret$matching,
    c("alpha", "beta[1]", "beta[2]", "beta[3]")
  )
  expect_equal(length(ret$not_found), 0)
})

test_that("require_suggested_package() works", {
  expect_error(
    require_suggested_package("not_a_real_package"),
    "Please install the 'not_a_real_package' package to use this function."
  )
})

test_that("use_spinner() respects the cmdstanr_spinner option", {
  # rlang::is_interactive() is FALSE while testing, so simulate an interactive
  # session. The option and env var are cleared so that the tests don't inherit
  # them from the session running the tests.
  withr::local_options(list(rlang_interactive = TRUE, cmdstanr_spinner = NULL))
  withr::local_envvar(IN_PKGDOWN = NA)
  expect_true(use_spinner())
  withr::with_options(list(cmdstanr_spinner = FALSE), expect_false(use_spinner()))
  withr::with_options(list(cmdstanr_spinner = TRUE), expect_true(use_spinner()))
})

test_that("use_spinner() is FALSE unless interactive", {
  withr::local_options(list(cmdstanr_spinner = NULL))
  withr::local_envvar(IN_PKGDOWN = NA)

  withr::local_options(rlang_interactive = FALSE)
  expect_false(use_spinner())
  withr::with_options(list(cmdstanr_spinner = TRUE), expect_false(use_spinner()))

  withr::local_options(rlang_interactive = TRUE)
  withr::local_envvar(IN_PKGDOWN = "true")
  expect_false(use_spinner())
})

test_that("as_mcmc.list() works", {
  x <- as_mcmc.list(fit_mcmc)
  expect_length(x, fit_mcmc$num_chains())
  expect_s3_class(x, "mcmc.list")
  expect_s3_class(x[[1]], "mcmc")

  draws <- fit_mcmc$draws()
  x1 <- x[[1]]
  expect_equal(dim(x1), c(posterior::niterations(draws), posterior::nvariables(draws)))
  expect_equal(dimnames(x1)$variable, posterior::variables(draws))

  expect_error(
    as_mcmc.list(fit_mle),
    "Currently only CmdStanMCMC objects can be converted to mcmc.list"
  )
})

test_that("get_cmdstan_flags() can be used recursively in `make`", {
  mkfile <- repair_path(test_path("resources", "recursive-cmdstan-flags.mk"))
  nonrecursive_flags <- get_cmdstan_flags("STANCFLAGS")
  recursive_run <- processx::run(
    command = "make",
    args = sprintf("--file=%s", mkfile),
    error_on_status = FALSE
  )
  if (recursive_run$status != 0) {
    fail(
      paste(
        "Recursive make failed.",
        paste0("status: ", recursive_run$status),
        "stdout:",
        recursive_run$stdout,
        "stderr:",
        recursive_run$stderr,
        sep = "\n"
      )
    )
    return(invisible())
  }
  expected_stdout <- paste(capture.output(cat(nonrecursive_flags)), collapse = "\n")
  expect_equal(recursive_run$stdout, expected_stdout)
})

test_that("parse_make_print_flag() ignores unrelated make output", {
  stdout <- paste(
    "make: Entering directory '/tmp/cmdstan'",
    "STANCFLAGS = --O1 --warn-pedantic --allow-undefined",
    "make: Leaving directory '/tmp/cmdstan'",
    sep = "\n"
  )

  expect_equal(
    parse_make_print_flag("STANCFLAGS", stdout),
    "--O1 --warn-pedantic --allow-undefined"
  )
})

test_that("parse_make_print_flag() errors if no matching flag line is found", {
  expect_error(
    parse_make_print_flag("STANCFLAGS", "make: Entering directory '/tmp/cmdstan'"),
    "Failed to parse `STANCFLAGS`",
    fixed = TRUE
  )
})

test_that("parse_make_print_flag() errors if multiple matching flag lines are found", {
  stdout <- paste(
    "STANCFLAGS = --O1",
    "STANCFLAGS = --warn-pedantic",
    sep = "\n"
  )
  expect_error(
    parse_make_print_flag("STANCFLAGS", stdout),
    "Found multiple `STANCFLAGS` lines",
    fixed = TRUE
  )
})

test_that("get_cmdstan_flags() returns empty STANCFLAGS as character(0)", {
  with_mocked_bindings(
    {
      expect_equal(get_cmdstan_flags("STANCFLAGS"), character(0))
    },
    wsl_compatible_run = function(...) {
      list(stdout = "STANCFLAGS =\n")
    }
  )
})

test_that("get_cmdstan_flags() preserves empty non-STANCFLAGS values", {
  with_mocked_bindings(
    {
      expect_equal(get_cmdstan_flags("CPPFLAGS"), "")
    },
    wsl_compatible_run = function(...) {
      list(stdout = "CPPFLAGS =\n")
    }
  )
})

test_that("get_cmdstan_flags() handles line-continuation STANCFLAGS in make/local", {
  tmpdir <- withr::local_tempdir()
  # Build a minimal make setup so we can exercise real make line continuations.
  writeLines(
    c(
      "print-%: ; @echo $* = $($*)",
      "-include local"
    ),
    file.path(tmpdir, "Makefile")
  )
  writeLines(
    c(
      "STANCFLAGS += --O1 \\",
      "  --warn-pedantic \\",
      "  --allow-undefined"
    ),
    file.path(tmpdir, "local")
  )
  make_run <- processx::run(
    command = "make",
    args = c("-s", "print-STANCFLAGS"),
    wd = tmpdir,
    error_on_status = FALSE
  )
  if (make_run$status != 0) {
    fail(
      paste(
        "Mini make failed.",
        paste0("status: ", make_run$status),
        "stdout:",
        make_run$stdout,
        "stderr:",
        make_run$stderr,
        sep = "\n"
      )
    )
    return(invisible())
  }

  with_mocked_bindings(
    {
      expect_equal(
        get_cmdstan_flags("STANCFLAGS"),
        c("--O1", "--warn-pedantic", "--allow-undefined")
      )
    },
    wsl_compatible_run = function(...) {
      list(stdout = make_run$stdout)
    }
  )
})
