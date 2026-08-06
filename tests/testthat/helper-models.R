testing_data <- function(name) {
  if (file.exists(test_path("resources", "data", paste0(name, ".data.rds")))) {
    readRDS(test_path("resources", "data", paste0(name, ".data.rds")))
  } else {
    test_path("resources", "data", paste0(name, ".data.json"))
  }
}
testing_stan_file <- function(name) {
  stan_dir <- .cmdstanr$TESTING_STAN_DIR %||% test_path("resources", "stan")
  file.path(stan_dir, paste0(name, ".stan"))
}
testing_stan_dir <- function() {
  .cmdstanr$TESTING_STAN_DIR %||% test_path("resources", "stan")
}

local_include_model_with_spaces <- function(.local_envir = parent.frame()) {
  model_dir <- withr::local_tempdir(
    pattern = "include path",
    .local_envir = .local_envir
  )
  source_files <- c(
    testing_stan_file("bernoulli_include"),
    testing_stan_file("divide_real_by_two")
  )
  if (!all(file.copy(source_files, model_dir))) {
    stop("Failed to copy Stan include test fixtures.", call. = FALSE)
  }
  list(
    stan_file = file.path(model_dir, "bernoulli_include.stan"),
    include_paths = model_dir
  )
}

cmdstan_example_file <- function() {
  # stan program in different directory from the others
  file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
}

testing_model <- function(name) {
  cmdstan_model(stan_file = testing_stan_file(name))
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
