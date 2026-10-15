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

ensure_sundials_fpic <- function() {
  with_test_lock("sundials-fpic", {
    withr::with_options(
      list(mc.cores = min(parallel::detectCores(), 4)),
      check_sundials_fpic(
        get_cmdstan_flags("CPPFLAGS_SUNDIALS"),
        verbose = FALSE
      )
    )
  })
}

precompiled_header_path <- function(make_args) {
  printed <- run_make(
    c("-s", make_args, "print-PRECOMPILED_MODEL_HEADER"),
    quiet = TRUE
  )$stdout
  line <- grep(
    "^PRECOMPILED_MODEL_HEADER",
    trimws(strsplit(printed, "\n")[[1]]),
    value = TRUE
  )
  header <- trimws(sub("^PRECOMPILED_MODEL_HEADER[[:space:]]*=", "", line))
  if (length(header) != 1 || !nzchar(header)) {
    return(NULL)
  }
  file.path(cmdstan_path(), header)
}

build_precompiled_header <- function(make_args, timeout) {
  header_path <- precompiled_header_path(make_args)
  if (is.null(header_path) || file.exists(header_path)) {
    return(TRUE)
  }
  lock <- NULL
  if (!is.null(test_run_dir())) {
    lock <- acquire_test_lock(paste0("pch-", rlang::hash(make_args)), timeout)
    if (is.null(lock)) {
      return(FALSE)
    }
    on.exit(release_test_lock(lock))
  }
  if (!file.exists(header_path)) {
    run_make(c(basename(header_path), make_args), quiet = TRUE)
  }
  TRUE
}

ensure_precompiled_headers <- function() {
  pending <- list(character(), "STAN_THREADS=true")
  for (timeout in c(0, 900)) {
    built <- vapply(pending, build_precompiled_header, logical(1), timeout)
    pending <- pending[!built]
  }
}

remove_dead_run_dirs <- function(root) {
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
}

if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  if (
    identical(Sys.getenv("CALLR_IS_RUNNING"), "true") &&
      !nzchar(Sys.getenv("CMDSTANR_TEST_SHARED"))
  ) {
    root <- dirname(tempdir())
    remove_dead_run_dirs(root)
    run_dir <- file.path(root, paste0("cmdstanr-tests-", ps::ps_ppid()))
    dir.create(run_dir, showWarnings = FALSE)
    Sys.setenv(CMDSTANR_TEST_SHARED = run_dir)
  }
  tryCatch(
    {
      ensure_sundials_fpic()
      ensure_precompiled_headers()
    },
    error = function(e) message(conditionMessage(e))
  )
}
