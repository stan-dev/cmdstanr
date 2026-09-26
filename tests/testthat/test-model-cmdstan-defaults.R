skip_on_cran()

set_cmdstan_path()
mod <- testing_model("bernoulli")

expected_cmdstan_defaults <- list(
  sample = list(
    iter_sampling = 1000L,
    iter_warmup = 1000L,
    save_warmup = FALSE,
    thin = 1L,
    adapt_engaged = TRUE,
    adapt_delta = 0.8,
    init_buffer = 75L,
    term_buffer = 50L,
    window = 25L,
    save_metric = FALSE,
    max_treedepth = 10L,
    metric = "diag_e",
    metric_file = "",
    step_size = 1L
  ),
  optimize = list(
    algorithm = "lbfgs",
    init_alpha = 0.001,
    tol_obj = 1e-12,
    tol_rel_obj = 10000L,
    tol_grad = 1e-08,
    tol_rel_grad = 1e+07,
    tol_param = 1e-08,
    history_size = 5L,
    jacobian = FALSE,
    iter = 2000L
  ),
  variational = list(
    algorithm = "meanfield",
    iter = 10000L,
    grad_samples = 1L,
    elbo_samples = 100L,
    eta = 1L,
    adapt_engaged = TRUE,
    adapt_iter = 50L,
    tol_rel_obj = 0.01,
    eval_elbo = 100L,
    draws = 1000L
  ),
  pathfinder = list(
    init_alpha = 0.001,
    tol_obj = 1e-12,
    tol_rel_obj = 10000L,
    tol_grad = 1e-08,
    tol_rel_grad = 1e+07,
    tol_param = 1e-08,
    history_size = 5L,
    draws = 1000L,
    num_paths = 4L,
    save_single_paths = FALSE,
    psis_resample = TRUE,
    calculate_lp = TRUE,
    max_lbfgs_iters = 1000L,
    single_path_draws = 1000L,
    num_elbo_draws = 25L
  ),
  laplace = list(
    jacobian = TRUE,
    draws = 1000L
  )
)

expect_cmdstan_defaults <- function(method, expected) {
  args <- mod$cmdstan_defaults(method)
  expect_type(args, "list")
  expect_named(args)
  expect_setequal(names(args), names(expected))

  for (name in names(expected)) {
    expect_identical(args[[name]], expected[[name]], info = paste0(method, "$", name))
  }
}

test_that("cmdstan_defaults() errors for invalid method", {
  expect_error(
    mod$cmdstan_defaults("bogus"),
    "'arg' should be one of",
    fixed = TRUE
  )
})

test_that("cmdstan_defaults() returns expected names and values", {
  for (method in names(expected_cmdstan_defaults)) {
    expect_cmdstan_defaults(method, expected_cmdstan_defaults[[method]])
  }
})
