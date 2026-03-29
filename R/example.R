#' Fit models for use in examples
#'
#' @export
#' @param example (string) The name of the example. The currently available
#'   examples are
#'   * `"logistic"`: logistic regression with intercept and 3 predictors.
#'   * `"schools"`: the so-called "eight schools" model, a hierarchical
#'   meta-analysis. Fitting this model will result in warnings about
#'   divergences.
#'   * `"schools_ncp"`: non-centered parameterization of the "eight schools"
#'   model that fixes the problem with divergences.
#'
#'   To print the Stan code for a given `example` use
#'   `print_example_program(example)`.
#'
#' @param method (string) Which fitting method should be used? The default is
#'   the `"sample"` method (MCMC).
#' @param ... Arguments passed to the chosen `method`. See the help pages for
#'   the individual methods for details.
#' @param quiet (logical) If `TRUE` (the default) then fitting the model is
#'   wrapped in [utils::capture.output()].
#' @param force_recompile Passed to the [$compile()][model-method-compile] method.
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
           method = c("sample", "optimize", "laplace", "variational", "pathfinder", "diagnose"),
           ...,
           quiet = TRUE,
           force_recompile = getOption("cmdstanr_force_recompile", default = FALSE)) {

    example <- match.arg(example)
    method <- match.arg(method)
    example_program <- paste0(example, ".stan")
    example_data <- paste0(example, ".data.json")

    # create executable in temporary directory
    tmp <- file.path(tempdir(), example_program)
    if (!file.exists(tmp)) {
      file.copy(system.file(example_program, package = "cmdstanr"), tmp)
    }
    mod <- cmdstan_model(tmp, force_recompile = force_recompile)
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
