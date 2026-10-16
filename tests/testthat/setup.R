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

shared <- tempdir()
if (identical(Sys.getenv("CALLR_IS_RUNNING"), "true")) {
  root <- dirname(tempdir())
  runs <- list.files(root, "^cmdstanr-tests-", full.names = TRUE)
  alive <- paste0("cmdstanr-tests-", ps::ps_pids())
  unlink(runs[!basename(runs) %in% alive], recursive = TRUE)
  shared <- file.path(root, paste0("cmdstanr-tests-", ps::ps_ppid()))
  dir.create(shared, showWarnings = FALSE)
}
Sys.setenv(CMDSTANR_TEST_SHARED = shared)

if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  with_test_lock("prebuild", {
    withr::with_options(
      list(mc.cores = parallel::detectCores()),
      check_sundials_fpic(get_cmdstan_flags("CPPFLAGS_SUNDIALS"), FALSE)
    )
    testing_model("bernoulli")
    testing_model("bernoulli", cpp_options = list(stan_threads = TRUE))
  })
}
