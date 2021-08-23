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
           method = c("sample", "optimize", "variational", "diagnose"),
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



# including write_stan_file in example.R since it will be mostly used
# in examples

#' Write Stan code to a file
#'
#' Convenience function for writing Stan code to a (possibly
#' [temporary][base::tempfile]) file with a `.stan` extension. By default, the
#' file name is chosen deterministically based on a [hash][rlang::hash()]
#' of the Stan code, and the file is not overwritten if it already has correct
#' contents. This means that calling this function multiple times with the same
#' Stan code will reuse the compiled model. This also however means that the
#' function is potentially not thread-safe. Using `hash_salt = Sys.getpid()`
#' should ensure thread-safety in the rare cases when it is needed.
#'
#' @export
#' @param code (character vector) The Stan code to write to the file. This can
#'   be a character vector of length one (a string) containing the entire Stan
#'   program or a character vector with each element containing one line of the
#'   Stan program.
#' @param dir (string) An optional path to the directory where the file will be
#'   written. If omitted, a global option `cmdstanr_write_stan_file_dir` is
#'   used. If the global options is not set, [temporary directory][base::tempdir]
#'   is used.
#' @param basename (string) If `dir` is specified, optionally the basename to
#'   use for the file created. If not specified a file name is generated
#'   from [hashing][rlang::hash()] the code.
#' @param force_overwrite (logical) If set to `TRUE` the file will always be
#'   overwritten and thus the resulting model will always be recompiled.
#' @param hash_salt (string) Text to add to the model code prior to hashing to
#'   determine the file name if `basename` is not set.
#' @return The path to the file.
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
#' f <- write_stan_file(stan_program)
#' print(f)
#'
#' lines <- readLines(f)
#' print(lines)
#' cat(lines, sep = "\n")
#'
#' # stan program as character vector of lines
#' f2 <- write_stan_file(lines)
#' identical(readLines(f), readLines(f2))
#'
write_stan_file <- function(code,
                            dir = getOption("cmdstanr_write_stan_file_dir", tempdir()),
                            basename = NULL,
                            force_overwrite = FALSE,
                            hash_salt = "") {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  collapsed_code <- paste0(code, collapse = "\n")

  if (!is.null(basename)) {
    if (!endsWith(basename, ".stan")) {
      basename <- paste0(basename, ".stan")
    }
    file <- file.path(dir, basename)
  } else {
    require_suggested_package("rlang")
    hash <- rlang::hash(paste0(hash_salt, collapsed_code))
    file <- file.path(dir, paste0("model_", hash, ".stan"))
  }
  overwrite <- TRUE
  # Do not overwrite file if it has the correct contents (to avoid recompilation)
  if (!force_overwrite && file.exists(file)) {
    tryCatch({
      file_contents <- paste0(readLines(file), collapse = "\n")
      if (gsub("\r|\n", "\n", file_contents) == gsub("\r|\n", "\n", collapsed_code)) {
        overwrite <- FALSE
      }
    },
    error = function(e) {
      warning("Error when checking old file contents", e)
    })
  }

  if (overwrite) {
    cat(code, file = file, sep = "\n")
  }
  file
}


#' Write Stan code to a temporary file
#'
#' This function is deprecated. Please use [write_stan_file()] instead.
#'
#' @export
#' @inheritParams write_stan_file
write_stan_tempfile <- function(code, dir = tempdir()) {
  warning("write_stan_tempfile() is deprecated. Please use write_stan_file() instead.",
          call. = FALSE)
  write_stan_file(code, dir)
}
