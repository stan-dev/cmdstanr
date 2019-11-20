testing_data <- function(name) {
  readRDS(test_path("resources", "data", paste0(name, ".data.rds")))
}
testing_stan_file <- function(name) {
  test_path("resources", "stan", paste0(name, ".stan"))
}

cmdstan_example_file <- function() {
  # stan program in different directory from the others
  file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
}

testing_model <- function(name) {
  cmdstan_model(stan_file = testing_stan_file(name))
}

testing_fit <- function(name, method = c("sample", "optimize", "variational"), ...) {
  method <- match.arg(method)
  mod <- testing_model(name)
  utils::capture.output(
    fit <- mod[[method]](data = testing_data(name), ...)
  )
  fit
}
