# Save original state so we can restore it after tests finish
original_cmdstan_env <- Sys.getenv("CMDSTAN")
original_cmdstan_path_var <- .cmdstanr$PATH
original_cmdstan_version_var <- .cmdstanr$VERSION
original_cmdstan_wsl_var <- .cmdstanr$WSL
original_cmdstan_tempdir_var <- .cmdstanr$TEMP_DIR

# Set up isolated CmdStan installation for parallel test workers
original_cmdstan_path <- cmdstanr::cmdstan_path()
worker_cmdstan_dir <- file.path(tempdir(), "cmdstan", basename(original_cmdstan_path))
dir.create(worker_cmdstan_dir, recursive = TRUE, showWarnings = FALSE)
file.copy(original_cmdstan_path, worker_cmdstan_dir, recursive = TRUE)

cmdstanr::set_cmdstan_path(worker_cmdstan_dir)
Sys.setenv("CMDSTAN" = worker_cmdstan_dir)

# Copy test Stan files to worker temp dir for isolation
original_stan_dir <- testthat::test_path("resources", "stan")
worker_stan_dir <- file.path(tempdir(), "stan")
dir.create(worker_stan_dir, recursive = TRUE, showWarnings = FALSE)
file.copy(original_stan_dir, worker_stan_dir, recursive = TRUE)
.cmdstanr$TESTING_STAN_DIR <- worker_stan_dir


cleanup_stan_artifacts <- function() {
  stan_dir <- .cmdstanr$TESTING_STAN_DIR %||% test_path("resources", "stan")
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

# Restore original global state after tests finish (runs before cleanup_stan_artifacts
# in LIFO order, so also clean up worker stan artifacts here while paths are valid)
withr::defer({
  # Clean up worker stan artifacts before restoring paths
  if (!is.null(.cmdstanr$TESTING_STAN_DIR)) {
    all_files <- list.files(.cmdstanr$TESTING_STAN_DIR, full.names = TRUE, recursive = TRUE)
    artifact_files <- all_files[!grepl("\\.stan$", all_files)]
    if (length(artifact_files) > 0) {
      unlink(artifact_files, force = TRUE)
    }
  }

  # Restore CMDSTAN environment variable
  if (nzchar(original_cmdstan_env)) {
    Sys.setenv("CMDSTAN" = original_cmdstan_env)
  } else {
    Sys.unsetenv("CMDSTAN")
  }

  # Restore original cmdstanr internal state
  .cmdstanr$PATH <- original_cmdstan_path_var
  .cmdstanr$VERSION <- original_cmdstan_version_var
  .cmdstanr$WSL <- original_cmdstan_wsl_var
  .cmdstanr$TEMP_DIR <- original_cmdstan_tempdir_var
  .cmdstanr$TESTING_STAN_DIR <- NULL
}, testthat::teardown_env())
