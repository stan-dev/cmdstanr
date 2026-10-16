# CI runners have four cores, so run two chains at a time
options(mc.cores = 2)

stan_dir <- file.path(tempdir(), "stan")
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
  if (identical(Sys.getenv("CALLR_IS_RUNNING"), "true")) {
    root <- dirname(tempdir())
    runs <- Sys.glob(file.path(root, "cmdstanr-tests-*"))
    run_pids <- as.integer(sub("^cmdstanr-tests-", "", basename(runs)))
    unlink(runs[!run_pids %in% ps::ps_pids()], recursive = TRUE)
    run_dir <- file.path(root, paste0("cmdstanr-tests-", ps::ps_ppid()))
    dir.create(run_dir, showWarnings = FALSE)
    Sys.setenv(CMDSTANR_TEST_SHARED = run_dir)
  }
  with_test_lock("prebuild", {
    withr::with_options(
      list(mc.cores = parallel::detectCores()),
      check_sundials_fpic(get_cmdstan_flags("CPPFLAGS_SUNDIALS"), FALSE)
    )
    for (make_args in list(character(), "STAN_THREADS=true")) {
      header <- trimws(
        sub(
          ".*=",
          "",
          run_make(
            c("-s", make_args, "print-PRECOMPILED_MODEL_HEADER"),
            quiet = TRUE
          )$stdout
        )
      )
      if (nzchar(header) && !file.exists(file.path(cmdstan_path(), header))) {
        run_make(c(basename(header), make_args), quiet = TRUE)
      }
    }
  })
}
