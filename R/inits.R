#' Generate initial values for Stan Models
#'
#' The `generate_inits()` methods generate a list of lists of initial values for
#' each chain to be used in initializing a model fit with Stan
#'
#' @name generate_inits
#' @param object An object from which to generate initial values
#' @param ... Additional arguments to be passed to the specific methods
#' @details The `generate_inits()` method is generic function to which specific
#'   methods for different classes of objects can be written. In the `cmdstanr`
#'   package, the following objects are supported:
#'
#'  * A `CmdStanMCMC` object, which is the result of sampling from a Stan model with cmdstanr
#'  * A vector of file paths to the CSV files containing the draws from a Stan model
#'  * A draws object from the `posterior` package
#'
#'   For these objects, the function specified in \code{FUN} is applied to the
#'   draws to generate the inits. This can be very flexible - any function works
#'   as long as it returns a scalar and can be applied to a vector.
#' @return A list of lists of initial values for each chain
#' @export
#' @examples
#' \dontrun{
#' # inits from a CmdStanMCMC object
#' stanfit <- cmdstanr::cmdstanr_example("logistic")
#' generate_inits(stanfit)
#' generate_inits(stanfit, FUN = mean)
#' generate_inits(stanfit, FUN = quantile, probs = 0.5)
#' generate_inits(stanfit, draws = "last")
#'
#' # inits from a vector of file paths
#' files <- stanfit$output_files()
#' generate_inits(files)
#'
#' # inits from a draws object
#' draws <- stanfit$draws()
#' generate_inits(draws)
#'
#' # warmup and then use the final draws for the inits of a separate sampling stage
#' warmup <- cmdstanr_example("logistic", parallel_chains = 4, iter_sampling = 0, save_warmup = T)
#' inits <- generate_inits(warmup, draws = "last")
#' mod <- cmdstan_model(exe_file = warmup$runset$exe_file())
#' fit <- mod$sample(warmup$data_file(),
#'                   parallel_chains = 4,
#'                   init = inits,
#'                   iter_warmup = 0,
#'                   inv_metric = warmup$inv_metric(matrix = FALSE),
#'                   step_size = warmup$metadata()$step_size_adaptation,
#'                   adapt_engaged = FALSE)
#'
#' # compare with standard fitting with combined warmup and sampling
#' fit_standard <- mod$sample(warmup$data_file(),
#'                            parallel_chains = 4)
#' }
generate_inits <- function(object, ...) {
  UseMethod("generate_inits")
}

#' @rdname generate_inits
#' @param FUN A function to apply to the draws to generate the inits. Only used
#'   if draws = "all" or "sampling". It should be a function name that takes a
#'   vector as input and returns a scalar, such as mean or median. The function
#'   will be applied to each parameter's draws to generate the inits. The
#'   default is to sample 1 random draw from the posterior draws
#' @param variables A character vector of parameter names for which to generate
#'   inits
#' @export
generate_inits.draws <- function(object, variables = NULL, FUN = sample1, ...) {
  checkmate::assert_function(FUN)
  checkmate::assert_character(variables, null.ok = TRUE)
  draws <- posterior::as_draws_array(object)
  checkmate::assert_scalar(
    FUN(c(draws[,1,1]), ...),
    .var.name = paste0('the return value of ', as.character(quote(FUN)))
  )

  # extract parameter information from draws
  nchains <- length(dimnames(draws)$chain)
  all_pars <- dimnames(draws)$variable
  par_dims <- variable_dims(all_pars)
  if (is.null(variables)) {
    variables <- names(par_dims)
  } else {
    variables <- intersect(variables, names(par_dims))
  }

  # apply the function to the draws to select the inits
  draws <- apply(draws, 2:3, FUN, ...)

  # prepare init list
  out <- vector('list', nchains)

  for (i in 1:nchains) {
    out[[i]] <- vector('list', length(variables))
    names(out[[i]]) <- variables

    # extract the draw for each parameter and store it in the proper format
    for (par in variables) {
      pattern <- paste0("^", par, "(\\[|$)")
      idx <- grep(pattern, all_pars)
      values <- draws[i,idx]
      dims <- par_dims[[par]]
      if (any(dims > 1)) {
        out[[i]][[par]] <- array(values, dims)
      } else {
        out[[i]][[par]] <- as.numeric(values)
      }
    }
  }

  out
}

#' @param draws A character string. Either "last", "sampling" or "all". If
#'   "last", only the last draw is used. If "sampling", all the draws from the
#'   sampling phase are used. If "all", all the draws, including warmup, are
#'   used.
#' @export
#' @rdname generate_inits
generate_inits.CmdStanMCMC <- function(object, variables = NULL, FUN = sample1,
                                       draws = "sampling", ...) {
  draws <- match.arg(draws, c("last", "sampling", "all"))
  pars <- names(object$runset$args$model_variables$parameters)
  if (!is.null(variables)) {
    pars <- intersect(variables, pars)
  }

  # get the draws array
  if (draws == "last") {
    draws <- read_last_draws(object$output_files())
    dimnames(draws)$variable <- object$metadata()$model_params
  } else {
    draws <- object$draws(variables = pars, inc_warmup = draws == "all",
                          format = "draws_array")
  }

  generate_inits(draws, FUN = FUN, variables = pars, ...)
}

#' @export
#' @rdname generate_inits
generate_inits.character <- function(object, variables = NULL, FUN = sample1,
                                     draws = "sampling", ...) {
  checkmate::assert_file_exists(object)
  checkmate::assert_function(FUN)
  draws <- match.arg(draws, c("sampling", "all"))
  stanfit <- as_cmdstan_fit(object, format = "draws_array")
  generate_inits(stanfit, variables = variables, FUN = FUN, draws = draws, ...)
}



sample1 <- function(x) {
  if (length(x) == 1) {
    return(x)
  }
  base::sample(x, size = 1)
}


# efficiently extract the last complete draw recorded in a cmdstan csv file
# @param csv_file A character string of the file path
# @param par_names A logical. If TRUE, the parameter names are included.
# @return A draws array
read_last_draws <- function(csv_files, par_names = FALSE) {
  checkmate::assert_file_exists(csv_files)
  checkmate::assert_logical(par_names)

  out <- vector("list", length(csv_files))
  for (i in seq_along(csv_files)) {
    file <- csv_files[i]
    tmpfile <- tempfile()
    cmd <- glue::glue("tail -12 {file} | grep \"^[0-9-]\" | tail -2 > {tmpfile}")
    switch(.Platform$OS.type,
           windows = shell(cmd),
           unix = system(cmd))
    lines <- readLines(tmpfile)

    if (length(lines) == 0) {
      stop("No draws found in ", csv_files[i], call. = FALSE)
    }

    lines <- strsplit(lines, ",")
    if (length(lines[[2]]) < length(lines[[1]])) {
      message("The last draw is incomplete. The last complete draw will be used.")
      res <- lines[[1]]
    } else {
      res <- lines[[2]]
    }
    out[[i]] <- as.data.frame(t(as.numeric(unlist(res))))
  }

  out <- do.call(posterior::as_draws_array, list(out))

  if (par_names) {
    tmpfile <- tempfile()
    cmd <- glue::glue('grep \"^[a-zA-Z]\" {csv_files[i]} > {tmpfile}')
    switch(.Platform$OS.type,
           windows = shell(cmd),
           unix = system(cmd))
    pars <- readLines(tmpfile)
    if (length(pars) == 0) {
      message("No parameter names found in ", csv_files[i])
    } else if (length(pars) > 1) {
      stop("Could not identify the parameter names in ", csv_files[i], call. = FALSE)
    } else {
      pars <- strsplit(pars, ",")[[1]]
      dimnames(out)$variable <- repair_variable_names(pars)
    }
  }

  # remove diagnostics
  out <- out[,,-c(2:7)]

  out
}
