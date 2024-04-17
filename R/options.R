#' CmdStanR global options
#'
#' These options can be set via [options()] for an entire \R session.
#'
#' @name cmdstanr_global_options
#'
#' @details
#'
#' * `cmdstanr_draws_format`: Which format provided by the \pkg{posterior}
#' package should be used when returning the posterior or approximate posterior
#' draws? The default depends on the model fitting method. See
#' [draws][fit-method-draws] for more details.
#'
#' * `cmdstanr_force_recompile`: Should the default be to recompile models
#' even if there were no Stan code changes since last compiled?  See
#' [compile][model-method-compile] for more details. The default is `FALSE`.
#'
#' * `cmdstanr_max_rows`: The maximum number of rows of output to print when
#' using the [`$print()`][fit-method-summary] method. The default is 10.
#'
#' * `cmdstanr_no_ver_check`: Should the check for a more recent version of
#' CmdStan be disabled? The default is `FALSE`.
#'
#' * `cmdstanr_output_dir`: The directory where CmdStan should write its output
#' CSV files when fitting models. The default is a temporary directory.
#'
#' * `cmdstanr_verbose`: Should more information be printed
#' when compiling or running models, including showing how CmdStan was called
#' internally? The default is `FALSE`.
#'
#' * `cmdstanr_warn_inits`: Should a warning be thrown if initial values are
#' only provided for a subset of parameters? The default is `TRUE`.
#'
#' * `cmdstanr_write_stan_file_dir`: The directory where [write_stan_file()]
#' should write Stan files. The default is a temporary directory.
#'
#' * `mc.cores`: The number of cores to use for various parallelization tasks
#' (e.g. running MCMC chains, installing CmdStan). The default depends on the
#' use case and is documented with the methods that make use of `mc.cores`.
#'
#'
NULL
