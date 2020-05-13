if (not_on_cran()) {
  set_cmdstan_path()
  stan_program <- test_path("resources/stan/chain_fails.stan")
  stan_program_init_warnings <- test_path("resources/stan/init_warnings.stan")

  mod <- cmdstan_model(stan_file = stan_program)
  mod_init_warnings <- cmdstan_model(stan_file = stan_program_init_warnings)

  make_all_fail <- function(x) {
    utils::capture.output(
      all_fail <- x$sample(data = list(pr_fail = 1),
                           save_latent_dynamics = TRUE)
    )
    all_fail
  }

  make_some_fail <- function(x) {
    num_files <- 0
    while (num_files == 0 || num_files == 4) {
      utils::capture.output(
        check_some_fail <- x$sample(
          data = list(pr_fail = 0.5),
          save_latent_dynamics = TRUE
        )
      )
      num_files <- length(check_some_fail$output_files())
    }
    check_some_fail
  }

  # called here and also in tests below
  suppressWarnings(
    utils::capture.output(
      fit_all_fail <- make_all_fail(mod),
      fit_some_fail <- make_some_fail(mod)
    )
  )
}

test_that("correct warnings are thrown when all chains fail", {
  skip_on_cran()
  expect_warning(
    make_all_fail(mod),
    "All chains finished unexpectedly!"
  )
  for (i in 1:4) {
    expect_output(fit_all_fail$output(i), "Location parameter is inf")
  }
})

test_that("correct warnings are thrown when some chains fail", {
  skip_on_cran()
  expect_warning(
     fit_tmp <- make_some_fail(mod),
     paste(4 - length(fit_tmp$output_files()), "chain(s) finished unexpectedly"),
     fixed = TRUE
  )

  failed <- !fit_some_fail$runset$procs$is_finished()
  for (i in which(failed)) {
    expect_output(fit_some_fail$output(i), "Location parameter is inf")
  }
})

test_that("$save_* methods save all files regardless of chain failure", {
  skip_on_cran()
  expect_message(
    fit_all_fail$save_output_files(dir = tempdir()),
    "Moved 4 files"
  )
  expect_message(
    fit_some_fail$save_output_files(dir = tempdir()),
    "Moved 4 files"
  )

  expect_message(
    fit_all_fail$save_latent_dynamics_files(dir = tempdir()),
    "Moved 4 files"
  )
  expect_message(
    fit_some_fail$save_latent_dynamics_files(dir = tempdir()),
    "Moved 4 files"
  )
})

test_that("errors when using draws after all chains fail", {
  skip_on_cran()
  expect_error(fit_all_fail$summary(), "No chains finished successfully")
  expect_error(fit_all_fail$draws(), "No chains finished successfully")
  expect_error(fit_all_fail$cmdstan_summary(), "Unable to run bin/stansummary")
  expect_error(fit_all_fail$cmdstan_diagnose(), "Unable to run bin/diagnose")
})

test_that("can use draws after some chains fail", {
  skip_on_cran()
  expect_s3_class(fit_some_fail$summary(), "draws_summary")
  expect_s3_class(fit_some_fail$draws(), "draws_array")
  expect_output(fit_some_fail$cmdstan_summary(), "Inference for Stan model")
  expect_output(fit_some_fail$cmdstan_diagnose(), "Processing complete")
})

test_that("init warnings are shown", {
  skip_on_cran()
  suppressWarnings(
    expect_message(
      utils::capture.output(
        mod_init_warnings$sample(chains = 1)
      ),
      "Rejecting initial value"
    )
  )
})
