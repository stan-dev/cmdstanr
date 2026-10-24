# With NOT_CRAN=true, the first worker to get the lock prepares the CmdStan
# installation once, so parallel workers do not build in it at the same time:
# it adds -fPIC to make/local and rebuilds CmdStan if Linux needs it, then
# builds the precompiled headers and main objects, with and without threads.
# Tests that change the installation use a per-worker copy (see
# local_private_cmdstan()). Each worker runs two chains at a time, so four
# workers sample eight chains at once.
options(mc.cores = 2)

stan_dir <- stan_fixture()
dir.create(stan_dir, recursive = TRUE, showWarnings = FALSE)
file.copy(
  list.files(test_path("resources", "stan"), full.names = TRUE),
  stan_dir,
  recursive = TRUE,
  overwrite = TRUE
)

cleanup_stan_artifacts <- function() {
  all_files_in_stan <- list.files(
    stan_dir,
    full.names = TRUE,
    recursive = TRUE
  )
  files_to_remove <- all_files_in_stan[!grepl("\\.stan$", all_files_in_stan)]

  if (length(files_to_remove) > 0) {
    unlink(files_to_remove, force = TRUE)
  }

  invisible(files_to_remove)
}

cleanup_stan_artifacts()
withr::defer(cleanup_stan_artifacts(), testthat::teardown_env())

if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  local({
    message("Preparing CmdStan in ", cmdstan_path(), " for the tests")
    lock_dir <- tools::R_user_dir("cmdstanr", "cache")
    dir.create(lock_dir, recursive = TRUE, showWarnings = FALSE)
    lock <- filelock::lock(file.path(lock_dir, "test-warm-up.lock"))
    withr::defer(filelock::unlock(lock))

    make_variable <- function(make_args, name) {
      trimws(
        sub(
          ".*=",
          "",
          run_make(
            c("-s", make_args, paste0("print-", name)),
            quiet = TRUE
          )$stdout
        )
      )
    }
    withr::with_options(
      list(mc.cores = parallel::detectCores()),
      check_sundials_fpic(get_cmdstan_flags("CPPFLAGS_SUNDIALS"), FALSE)
    )
    for (make_args in list(character(), "STAN_THREADS=true")) {
      header <- make_variable(make_args, "PRECOMPILED_MODEL_HEADER")
      main_o <- make_variable(make_args, "CMDSTAN_MAIN_O")
      targets <- c(
        header,
        sub("\\.hpp\\.gch$", ".d", header),
        main_o,
        sub("\\.o$", ".d", main_o)
      )
      targets <- targets[nzchar(targets)]
      missing <- targets[!file.exists(file.path(cmdstan_path(), targets))]
      if (length(missing) > 0) {
        run_make(c(missing, make_args), quiet = TRUE)
      }
    }
  })
}
