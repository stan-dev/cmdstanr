# Each worker runs two chains at a time; CI runs up to four workers.
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

# Build what every worker would otherwise build at once in the shared
# installation: the -fPIC SUNDIALS flag, and the precompiled headers and main
# objects for plain and threaded models. Concurrent builds corrupt these files,
# so the first worker to take the lock builds them and the rest wait.
if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  local({
    message("Preparing CmdStan in ", cmdstan_path(), " for the tests")
    lock_dir <- tools::R_user_dir("cmdstanr", "cache")
    dir.create(lock_dir, recursive = TRUE, showWarnings = FALSE)
    lock <- filelock::lock(file.path(lock_dir, "test-warm-up.lock"))
    withr::defer(filelock::unlock(lock))

    # Workers killed during teardown leave their CmdStan copies behind
    stale <- Sys.glob(
      file.path(dirname(tempdir()), "Rtmp*", "cmdstan-private-*")
    )
    unlink(stale[file.mtime(stale) < Sys.time() - 86400], recursive = TRUE)

    withr::with_options(
      list(mc.cores = parallel::detectCores()),
      check_sundials_fpic(get_cmdstan_flags("CPPFLAGS_SUNDIALS"), FALSE)
    )
    for (make_args in list(character(), "STAN_THREADS=true")) {
      flags <- c("PRECOMPILED_MODEL_HEADER", "CMDSTAN_MAIN_O")
      output <- run_make(
        c("-s", make_args, paste0("print-", flags)),
        quiet = TRUE
      )$stdout
      header <- parse_make_print_flag(flags[1], output)
      main_o <- parse_make_print_flag(flags[2], output)
      targets <- c(
        header,
        sub("\\.hpp\\.gch$", ".d", header),
        main_o,
        sub("\\.o$", ".d", main_o)
      )
      targets <- targets[nzchar(targets)]
      # make fails on Windows when a target is already built
      missing <- targets[!file.exists(file.path(cmdstan_path(), targets))]
      if (length(missing) > 0) {
        run_make(c("-j2", missing, make_args), quiet = TRUE)
      }
    }
  })
}
