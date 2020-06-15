#' Fit models for use in examples
#'
#' @export
#' @param example Name of the example. Currently only `"logistic"` is available.
#' * `logistic`: logistic regression with parameters `alpha` (intercept) and
#' `beta` (vector of regression coefficients).
#' @param method Which fitting method should be used? The default is the
#'   `"sample"` method (MCMC).
#' @param ... Arguments passed to the chosen `method`.
#'
#' @return The fitted model object returned by the selected `method`.
#'
#' @examples
#' \dontrun{
#' fit_mcmc <- cmdstanr_example(chains = 2, save_warmup = TRUE)
#' fit_mcmc$summary()
#'
#' fit_optim <- cmdstanr_example(method = "optimize")
#' fit_optim$summary()
#' }
#'
cmdstanr_example <-
  function(example = "logistic",
           method = c("sample", "optimize", "variational"),
           ...) {

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

    out <- utils::capture.output(
      fit <- mod[[method]](
        data = system.file(example_data, package = "cmdstanr"),
        ...
      )
    )
    fit
  }

