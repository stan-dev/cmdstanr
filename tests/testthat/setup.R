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
  if (
    identical(Sys.getenv("CALLR_IS_RUNNING"), "true") &&
      !nzchar(Sys.getenv("CMDSTANR_TEST_SHARED"))
  ) {
    root <- dirname(tempdir())
    for (dir in Sys.glob(file.path(root, "cmdstanr-tests-*"))) {
      pid <- as.integer(sub("^cmdstanr-tests-", "", basename(dir)))
      alive <- tryCatch(
        ps::ps_is_running(ps::ps_handle(pid)),
        error = function(e) FALSE
      )
      if (!alive) {
        unlink(dir, recursive = TRUE)
      }
    }
    run_dir <- file.path(root, paste0("cmdstanr-tests-", ps::ps_ppid()))
    dir.create(run_dir, showWarnings = FALSE)
    Sys.setenv(CMDSTANR_TEST_SHARED = run_dir)
  }
  tryCatch(
    {
      with_test_lock("sundials-fpic", {
        withr::with_options(
          list(mc.cores = min(parallel::detectCores(), 4)),
          check_sundials_fpic(
            get_cmdstan_flags("CPPFLAGS_SUNDIALS"),
            verbose = FALSE
          )
        )
      })
      pending <- list(character(), "STAN_THREADS=true")
      for (timeout in c(0, 900)) {
        built <- vapply(
          pending,
          function(make_args) {
            printed <- run_make(
              c("-s", make_args, "print-PRECOMPILED_MODEL_HEADER"),
              quiet = TRUE
            )$stdout
            line <- grep(
              "^PRECOMPILED_MODEL_HEADER",
              trimws(readLines(textConnection(printed))),
              value = TRUE
            )
            header <- trimws(
              sub("^PRECOMPILED_MODEL_HEADER[[:space:]]*=", "", line)
            )
            if (length(header) != 1 || !nzchar(header)) {
              return(TRUE)
            }
            header_path <- file.path(cmdstan_path(), header)
            if (file.exists(header_path)) {
              return(TRUE)
            }
            with_test_lock(
              paste0("pch-", rlang::hash(make_args)),
              {
                if (!file.exists(header_path)) {
                  run_make(c(basename(header_path), make_args), quiet = TRUE)
                }
                TRUE
              },
              timeout,
              on_timeout = FALSE
            )
          },
          logical(1)
        )
        pending <- pending[!built]
      }
    },
    error = function(e) message(conditionMessage(e))
  )
}
