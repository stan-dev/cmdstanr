local({
  if (!testthat::is_parallel()) {
    stan_files <- dir(test_path("resources", "stan"), pattern = "\\.stan$", full.names = TRUE)
    exe_files <- cmdstanr:::cmdstan_ext(cmdstanr:::strip_ext(stan_files))
    existing_exe_files <- exe_files[file.exists(exe_files)]
    if (length(existing_exe_files) > 0) {
      unlink(existing_exe_files, force = TRUE)
    }
  }
})

testing_data <- function(name) {
  if (file.exists(test_path("resources", "data", paste0(name, ".data.rds")))) {
    readRDS(test_path("resources", "data", paste0(name, ".data.rds")))
  } else {
    test_path("resources", "data", paste0(name, ".data.json"))
  }
}
testing_stan_file <- function(name) {
  test_path("resources", "stan", paste0(name, ".stan"))
}

testing_stan_program <- "
data {
  int<lower=0> N;
  array[N] int<lower=0,upper=1> y;
}
parameters {
  real<lower=0,upper=1> theta;
}
model {
  y ~ bernoulli(theta);
}
"

cmdstan_example_file <- function() {
  # stan program in different directory from the others
  file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
}

testing_model_stan_file <- function(name) {
  stan_file <- testing_stan_file(name)
  if (!testthat::is_parallel()) {
    return(stan_file)
  }

  model_dir <- tempfile(pattern = paste0(name, "-"))
  dir.create(model_dir)
  model_file <- file.path(model_dir, basename(stan_file))
  file.copy(stan_file, model_file, overwrite = TRUE)
  model_file
}

with_testing_model_compile_lock <- function(code) {
  if (!testthat::is_parallel()) {
    return(code)
  }

  lock_dir <- file.path(cmdstan_path(), ".cmdstanr-test-compile-lock")
  while (!dir.create(lock_dir, showWarnings = FALSE)) {
    Sys.sleep(0.1)
  }
  on.exit(unlink(lock_dir, recursive = TRUE), add = TRUE)

  code
}

testing_model <- function(name) {
  with_testing_model_compile_lock(
    cmdstan_model(stan_file = testing_model_stan_file(name))
  )
}

testing_fit <-
  function(name,
           method = c("sample",
                      "optimize",
                      "laplace",
                      "variational",
                      "pathfinder",
                      "generate_quantities"),
           seed = 123,
           ...) {
    method <- match.arg(method)
    mod <- testing_model(name)
    utils::capture.output(
      fit <- mod[[method]](data = testing_data(name), seed = seed, ...)
    )
    fit
  }
