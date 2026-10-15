library(testthat)
library(cmdstanr)

prebuild_precompiled_headers <- function() {
  variants <- list(character(), "STAN_THREADS=true")
  cluster <- parallel::makeCluster(length(variants), outfile = "")
  on.exit(parallel::stopCluster(cluster))
  parallel::clusterApply(
    cluster,
    variants,
    function(make_args) {
      output <- character()
      # jarl-ignore internal_function: deliberate use of internals
      tryCatch(
        withCallingHandlers(
          {
            printed <- cmdstanr:::run_make(
              c("-s", make_args, "print-PRECOMPILED_MODEL_HEADER"),
              quiet = TRUE
            )$stdout
            line <- grep(
              "^PRECOMPILED_MODEL_HEADER",
              trimws(strsplit(printed, "\n")[[1]]),
              value = TRUE
            )
            header <- trimws(sub("^PRECOMPILED_MODEL_HEADER\\s*=", "", line))
            if (
              length(header) == 1 &&
                nzchar(header) &&
                !file.exists(file.path(cmdstanr::cmdstan_path(), header))
            ) {
              cmdstanr:::run_make(c(header, make_args), quiet = TRUE)
            }
          },
          message = function(m) {
            output <<- c(output, conditionMessage(m))
            invokeRestart("muffleMessage")
          }
        ),
        error = function(e) {
          message(paste(c(output, conditionMessage(e)), collapse = "\n"))
        }
      )
      invisible()
    }
  )
  invisible()
}

ensure_sundials_fpic <- function() {
  # jarl-ignore internal_function: deliberate use of internals
  withr::with_options(
    list(mc.cores = min(parallel::detectCores(), 4)),
    cmdstanr:::check_sundials_fpic(
      cmdstanr:::get_cmdstan_flags("CPPFLAGS_SUNDIALS"),
      verbose = FALSE
    )
  )
}

shared <- NULL
if (
  identical(Sys.getenv("NOT_CRAN"), "true") &&
    !identical(Sys.getenv("TESTTHAT_PARALLEL"), "false")
) {
  shared <- tempfile("cmdstanr-tests-")
  dir.create(shared)
  Sys.setenv(CMDSTANR_TEST_SHARED = shared)
  if (!nzchar(Sys.getenv("TESTTHAT_CPUS"))) {
    Sys.setenv(TESTTHAT_CPUS = min(parallel::detectCores(), 4))
  }
  ensure_sundials_fpic()
  prebuild_precompiled_headers()
}

tryCatch(
  test_check("cmdstanr"),
  finally = if (!is.null(shared)) unlink(shared, recursive = TRUE)
)
