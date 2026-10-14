library(testthat)
library(cmdstanr)

prebuild_precompiled_headers <- function() {
  variants <- list(character(), "STAN_THREADS=true")
  headers <- vapply(variants, function(make_args) {
    header <- cmdstanr:::get_cmdstan_flags(
      "PRECOMPILED_MODEL_HEADER", make_args = make_args
    )
    gsub("\"", "", header, fixed = TRUE)
  }, "")
  missing <- nzchar(headers) & !file.exists(headers)
  if (any(missing)) {
    targets <- sub("^.*/(stan/src/stan/model/)", "\\1", headers)
    cluster <- parallel::makeCluster(sum(missing))
    on.exit(parallel::stopCluster(cluster))
    parallel::clusterMap(
      cluster,
      function(target, make_args) {
        cmdstanr:::run_make(c(target, make_args), quiet = TRUE)
      },
      targets[missing], variants[missing]
    )
  }
}

shared <- NULL
if (identical(Sys.getenv("NOT_CRAN"), "true") &&
    !identical(Sys.getenv("TESTTHAT_PARALLEL"), "false")) {
  shared <- tempfile("cmdstanr-tests-")
  dir.create(shared)
  Sys.setenv(CMDSTANR_TEST_SHARED = shared)
  if (!nzchar(Sys.getenv("TESTTHAT_CPUS"))) {
    Sys.setenv(TESTTHAT_CPUS = min(parallel::detectCores(), 4))
  }
  prebuild_precompiled_headers()
}

tryCatch(
  test_check("cmdstanr"),
  finally = if (!is.null(shared)) unlink(shared, recursive = TRUE)
)
