# CI runners have four cores, so run two chains at a time
options(mc.cores = 2)

stan_dir <- file.path(tempdir(), "stan")
dir.create(stan_dir, recursive = TRUE, showWarnings = FALSE)
file.copy(
  list.files(test_path("resources", "stan"), full.names = TRUE),
  stan_dir,
  recursive = TRUE
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

if (!os_is_wsl()) {
  shared_cmdstan <- cmdstan_path()
  worker_cmdstan <- file.path(tempdir(), "cmdstan", basename(shared_cmdstan))
  dir.create(dirname(worker_cmdstan), recursive = TRUE, showWarnings = FALSE)
  if (os_is_windows()) {
    system2(
      "robocopy",
      c(
        shQuote(shared_cmdstan),
        shQuote(worker_cmdstan),
        "/E", "/MT:16", "/NFL", "/NDL", "/NJH", "/NJS", "/NP"
      ),
      stdout = FALSE
    )
  } else {
    system2("cp", c("-a", shQuote(shared_cmdstan), shQuote(worker_cmdstan)))
  }
  Sys.setenv(CMDSTAN = worker_cmdstan)
  suppressMessages(set_cmdstan_path())
}
