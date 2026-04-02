cleanup_stan_artifacts <- function() {
  all_files_in_stan <- list.files(
    test_path("resources", "stan"),
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
