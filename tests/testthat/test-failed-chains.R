if (not_on_cran()) {
  set_cmdstan_path()
  stan_program <- testing_stan_file("chain_fails")
  stan_program_init_warnings <- testing_stan_file("init_warnings")

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
    attempt <- 1
    while (num_files == 0 || num_files == 4) {
      utils::capture.output(
        check_some_fail <- x$sample(
          data = list(pr_fail = 0.5),
          save_latent_dynamics = TRUE,
          seed = base::sample(.Machine$integer.max, 4)
        )
      )
      num_files <- length(check_some_fail$output_files(include_failed = FALSE))
      attempt <- attempt + 1
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
     paste(4 - length(fit_tmp$output_files(include_failed = FALSE)), "chain(s) finished unexpectedly"),
     fixed = TRUE
  )

  failed <- !fit_some_fail$runset$procs$is_finished()
  for (i in which(failed)) {
    expect_output(fit_some_fail$output(i), "Location parameter is inf")
  }
})

test_that("$output_files() and latent_dynamic_files() returns path to all files regardless of chain failure", {
  skip_on_cran()
  expect_equal(
    length(fit_all_fail$output_files(include_failed = TRUE)),
    4
  )
  expect_equal(
    length(fit_all_fail$output_files(include_failed = FALSE)),
    0
  )
  expect_equal(
    length(fit_some_fail$output_files(include_failed = TRUE)),
    4
  )
  expect_equal(
    length(fit_all_fail$latent_dynamics_files(include_failed = TRUE)),
    4
  )
  expect_equal(
    length(fit_all_fail$latent_dynamics_files(include_failed = FALSE)),
    0
  )
  expect_equal(
    length(fit_some_fail$latent_dynamics_files(include_failed = TRUE)),
    4
  )
  expect_equal(
    length(fit_all_fail$output_files()),
    0
  )
  expect_equal(
    length(fit_all_fail$latent_dynamics_files()),
    0
  )
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
  expect_error(fit_all_fail$sampler_diagnostics(), "No chains finished successfully")
  expect_error(fit_all_fail$cmdstan_summary(), "Unable to run bin/stansummary")
  expect_error(fit_all_fail$cmdstan_diagnose(), "Unable to run bin/diagnose")
  expect_error(fit_all_fail$print(), "Fitting failed. Unable to print")
  expect_error(fit_all_fail$inv_metric(), "No chains finished successfully")
  expect_error(fit_all_fail$metadata(), "Fitting failed. Unable to retrieve the metadata")
  expect_error(fit_all_fail$inv_metric(), "No chains finished successfully")
})

test_that("can use draws after some chains fail", {
  skip_on_cran()
  expect_s3_class(fit_some_fail$summary(), "draws_summary")
  expect_s3_class(fit_some_fail$draws(), "draws_array")
  expect_output(fit_some_fail$cmdstan_summary(), "Inference for Stan model")
  expect_output(fit_some_fail$cmdstan_diagnose(), "Processing complete")
  expect_output(fit_some_fail$print(), "variable")
})

test_that("init warnings are shown", {
  skip_on_cran()
  suppressWarnings(
    expect_message(
      utils::capture.output(
        fit <- mod_init_warnings$sample(chains = 1)
      ),
      "Rejecting initial value"
    )
  )
})

test_that("optimize error on bad data", {
  mod <- testing_model("bernoulli")
  expect_error(
    mod$optimize(data = list(a = c(1,2,3)), seed = 123),
    "Missing input data for the following data variables: N, y."
  )
})

test_that("errors when using draws after variational fais", {
  expect_warning(
    utils::capture.output(
      fit <- mod$variational(data = list(pr_fail = 1))
    ),
    "Fitting finished unexpectedly!"
  )
  expect_error(fit$print(), "Fitting failed. Unable to print.")
  expect_error(fit$summary(), "Fitting failed. Unable to retrieve the draws.")
  expect_error(fit$draws(), "Fitting failed. Unable to retrieve the draws.")
  expect_error(fit$cmdstan_summary(), "Unable to run bin/stansummary")
  expect_error(fit$cmdstan_diagnose(), "Unable to run bin/diagnose")
  expect_error(fit$metadata(), "Fitting failed. Unable to retrieve the metadata.")
})

test_that("gq chains error on wrong input CSV", {
  skip_on_cran()
  fit_bernoulli <- testing_fit("bernoulli", method = "sample", seed = 123, chains = 2)
  fit_logistic <- testing_fit("logistic", method = "sample", seed = 123, chains = 4)
  mod <- testing_model("bernoulli_ppc")
  data_list <- testing_data("bernoulli_ppc")
  suppressWarnings(
    expect_message(
      mod$generate_quantities(data = data_list, fitted_params = fit_logistic$output_files()),
      "Mismatch between model and fitted_parameters csv"
    )
  )
  err_msg <- "Mismatch between model and fitted_parameters csv file"
  suppressWarnings(
    expect_message(
      mod$generate_quantities(data = data_list, fitted_params = test_path("resources", "csv", "bernoulli-fail.csv")),
      err_msg
    )
  )
  expect_warning(
    utils::capture.output(
      fit <- mod$generate_quantities(data = data_list, fitted_params = test_path("resources", "csv", "bernoulli-fail.csv"))
    ),
    "Chain 1 finished unexpectedly"
  )

  expect_error(
    fit$draws(),
    "Generating quantities for all MCMC chains failed. Unable to retrieve the generated quantities."
  )
  expect_error(
    fit$metadata(),
    "Fitting failed. Unable to retrieve the metadata."
  )
  expect_error(
    fit$print(),
    "Fitting failed. Unable to print."
  )
  expect_warning(
    utils::capture.output(
      fit <- mod$generate_quantities(data = data_list, fitted_params = c(fit_bernoulli$output_files(), fit_logistic$output_files()))
    ),
    "4 chain(s) finished unexpectedly",
    fixed = TRUE
  )
})



