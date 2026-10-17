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
  warmup <- file.path(dirname(tempdir()), "cmdstanr-warmup")
  dir.create(warmup, showWarnings = FALSE)
  lock <- filelock::lock(file.path(warmup, "lock"))
  withr::with_options(
    list(mc.cores = parallel::detectCores()),
    check_sundials_fpic(get_cmdstan_flags("CPPFLAGS_SUNDIALS"), FALSE)
  )
  for (threads in c(FALSE, TRUE)) {
    dir <- file.path(warmup, threads)
    dir.create(dir, showWarnings = FALSE)
    cmdstan_model(
      testing_stan_file("bernoulli"),
      dir = dir,
      cpp_options = list(stan_threads = threads)
    )
  }
  filelock::unlock(lock)
}
