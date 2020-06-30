#' Fit models for use in examples
#'
#' @export
#' @param example Name of the example. The currently available examples are:
#'   * `"logistic"`: logistic regression with intercept and 3 predictors.
#'   * `"schools"`: the so-called "eight schools" model, a hierarchical
#'   meta-analysis. Fitting this model will result in warnings about
#'   divergences.
#'   * `"schools_ncp"`: non-centered parameterization eight schools model that
#'   fixes the problem with divergences.
#'
#' To print the Stan code for a given example use `print_example_program()`.
#'
#' @param method Which fitting method should be used? The default is the
#'   `"sample"` method (MCMC).
#' @param ... Arguments passed to the chosen `method`.
#' @param quiet If `TRUE` (the default) then fitting the model is wrapped in
#'   [utils::capture.output()].
#'
#' @return
#' The fitted model object returned by the selected `method`.
#'
#' @examples
#' \dontrun{
#' print_example_program("logistic")
#' fit_logistic_mcmc <- cmdstanr_example("logistic", chains = 2)
#' fit_logistic_mcmc$summary()
#'
#' fit_logistic_optim <- cmdstanr_example("logistic", method = "optimize")
#' fit_logistic_optim$summary()
#'
#' fit_logistic_vb <- cmdstanr_example("logistic", method = "variational")
#' fit_logistic_vb$summary()
#'
#' print_example_program("schools")
#' fit_schools_mcmc <- cmdstanr_example("schools")
#' fit_schools_mcmc$summary()
#'
#' print_example_program("schools_ncp")
#' fit_schools_ncp_mcmc <- cmdstanr_example("schools_ncp")
#' fit_schools_ncp_mcmc$summary()
#'
#' # optimization fails for hierarchical model
#' cmdstanr_example("schools", "optimize", quiet = FALSE)
#' }
#'
cmdstanr_example <-
  function(example = c("logistic", "schools", "schools_ncp"),
           method = c("sample", "optimize", "variational"),
           ...,
           quiet = TRUE) {

    example <- match.arg(example)
    method <- match.arg(method)
    example_program <- paste0(example, ".stan")
    example_data <- paste0(example, ".data.json")

    # create executable in temporary directory
    tmp <- file.path(tempdir(), example_program)
    if (!file.exists(tmp)) {
      file.copy(system.file(example_program, package = "cmdstanr"), tmp)
    }
    mod <- cmdstan_model(tmp)
    data_file <- system.file(example_data, package = "cmdstanr")

    if (quiet) {
      out <- utils::capture.output(fit <- mod[[method]](data = data_file, ...))
    } else {
      fit <- mod[[method]](data = data_file, ...)
    }
    fit
  }

#' @rdname cmdstanr_example
#' @export
print_example_program <-
  function(example = c("logistic", "schools", "schools_ncp")) {
    example <- match.arg(example)
    code <- readLines(system.file(paste0(example, ".stan"), package = "cmdstanr"))
    cat(code, sep = "\n")
  }



# including write_stan_tempfile in example.R since it will be mostly used
# in examples

#' Write Stan code to a temporary file
#'
#' @export
#' @param code A single string containing a Stan program or a character vector
#'   containing the individual lines of a Stan program. See **Examples**.
#' @return The path to the temporary file.
#'
#' @examples
#' # stan program as a single string
#' stan_program <- "
#' data {
#'   int<lower=0> N;
#'   int<lower=0,upper=1> y[N];
#' }
#' parameters {
#'   real<lower=0,upper=1> theta;
#' }
#' model {
#'   y ~ bernoulli(theta);
#' }
#' "
#'
#' f <- write_stan_tempfile(stan_program)
#' print(f)
#'
#' lines <- readLines(f)
#' print(lines)
#' cat(lines, sep = "\n")
#'
#' # stan program as character vector of lines
#' f2 <- write_stan_tempfile(lines)
#' identical(readLines(f), readLines(f2))
#'
write_stan_tempfile <- function(code) {
  tmp <- tempfile(fileext = ".stan")
  cat(code, file = tmp, sep = "\n")
  tmp
}

