#' Write data to a JSON file readable by CmdStan
#'
#' @export
#' @param data (list) A named list of \R objects.
#' @param file (string) The path to where the data file should be written.
#' @param always_decimal (logical) Force generate non-integers with decimal
#' points to better distinguish between integers and floating point values.
#' If `TRUE` all \R objects in `data` intended for integers must be of integer
#' type. 
#'
#' @details
#' `write_stan_json()` performs several conversions before writing the JSON
#' file:
#'
#' * `logical` -> `integer` (`TRUE` -> `1`, `FALSE` -> `0`)
#' * `data.frame` -> `matrix` (via [data.matrix()])
#' * `list` -> `array`
#' * `table` -> `vector`, `matrix`, or `array` (depending on dimensions of table)
#'
#' The `list` to `array` conversion is intended to make it easier to prepare
#' the data for certain Stan declarations involving arrays:
#'
#' * `vector[J] v[K]` (or equivalently `array[K] vector[J] v ` as of Stan 2.27)
#' can be constructed in \R as a list with `K` elements where each element a
#' vector of length `J`
#' * `matrix[I,J] v[K]` (or equivalently `array[K] matrix[I,J] m ` as of Stan
#' 2.27 ) can be constructed in \R as a list with `K` elements where each element
#' an `IxJ` matrix
#'
#' These can also be passed in from \R as arrays instead of lists but the list
#' option is provided for convenience. Unfortunately for arrays with more than
#' one dimension, e.g., `vector[J] v[K,L]` (or equivalently
#' `array[K,L] vector[J] v ` as of Stan 2.27) it is not possible to use an \R
#' list and an array must be used instead. For this example the array in \R
#' should have dimensions `KxLxJ`.
#'
#' @examples
#' x <- matrix(rnorm(10), 5, 2)
#' y <- rpois(nrow(x), lambda = 10)
#' z <- c(TRUE, FALSE)
#' data <- list(N = nrow(x), K = ncol(x), x = x, y = y, z = z)
#'
#' # write data to json file
#' file <- tempfile(fileext = ".json")
#' write_stan_json(data, file)
#'
#' # check the contents of the file
#' cat(readLines(file), sep = "\n")
#'
#'
#' # demonstrating list to array conversion
#' # suppose x is declared as `vector[3] x[2]` (or equivalently `array[2] vector[3] x`)
#' # we can use a list of length 2 where each element is a vector of length 3
#' data <- list(x = list(1:3, 4:6))
#' file <- tempfile(fileext = ".json")
#' write_stan_json(data, file)
#' cat(readLines(file), sep = "\n")
#'
write_stan_json <- function(data, file, always_decimal = FALSE) {
  if (!is.list(data)) {
    stop("'data' must be a list.", call. = FALSE)
  }
  if (!is.character(file) || !nzchar(file)) {
    stop("The supplied filename is invalid!", call. = FALSE)
  }

  data_names <- names(data)
  if (length(data) > 0 &&
      (length(data_names) == 0 ||
       length(data_names) != sum(nzchar(data_names)))) {
    stop("All elements in 'data' list must have names.", call. = FALSE)

  }
  if (anyDuplicated(data_names) != 0) {
    stop("Duplicate names not allowed in 'data'.", call. = FALSE)
  }

  for (var_name in data_names) {
    var <- data[[var_name]]
    if (!(is.numeric(var) || is.factor(var) || is.logical(var) ||
          is.data.frame(var) || is.list(var))) {
      stop("Variable '", var_name, "' is of invalid type.", call. = FALSE)
    }
    if (anyNA(var)) {
      stop("Variable '", var_name, "' has NA values.", call. = FALSE)
    }

    if (is.table(var)) {
      var <- unclass(var)
    } else if (is.logical(var)) {
      mode(var) <- "integer"
    } else if (is.data.frame(var)) {
      var <- data.matrix(var)
    } else if (is.list(var)) {
      var <- list_to_array(var, var_name)
    }
    data[[var_name]] <- var
  }

  # unboxing variables (N = 10 is stored as N : 10, not N: [10])
  jsonlite::write_json(
    data,
    path = file,
    auto_unbox = TRUE,
    factor = "integer",
    always_decimal = always_decimal,
    digits = NA,
    pretty = TRUE
  )
}


list_to_array <- function(x, name = NULL) {
  list_length <- length(x)
  if (list_length == 0) {
    return(NULL)
  }
  all_dims <- lapply(x, function(z) dim(z) %||% length(z)) # dim is null if vector
  all_equal_dim <- all(sapply(all_dims, function(d) {
    isTRUE(all.equal(d, all_dims[[1]]))
  }))
  if (!all_equal_dim) {
    stop("All matrices/vectors in list '", name, "' must be the same size!", call. = FALSE)
  }
  all_numeric <- all(sapply(x, function(a) is.numeric(a)))
  if (!all_numeric) {
    stop("All elements in list '", name, "' must be numeric!", call. = FALSE)
  }
  element_num_of_dim <- length(all_dims[[1]])
  x <- unlist(x)
  dim(x) <- c(all_dims[[1]], list_length)
  aperm(x, c(element_num_of_dim + 1L, seq_len(element_num_of_dim)))
}


#' Process data for CmdStanModel methods
#'
#' @noRd
#' @param data If not `NULL`, then either a path to a data file compatible with
#'   CmdStan, or a named list of \R objects to pass to [write_stan_json()].
#' @param stan_file If not `NULL`, the path to the Stan model for which to
#'   process the named list suppiled to the `data` argument. The Stan model
#'   is used for checking whether the supplied named list has all the
#'   required elements/Stan variables and to help differentiate between a
#'   vector of length 1 and a scalar when genereting the JSON file. This
#'   argument is ignored when a path to a data file is supplied for `data`.
#' @return Path to data file.
process_data <- function(data, stan_file = NULL) {
  if (length(data) == 0) {
    data <- NULL
  }
  if (is.null(data)) {
    path <- data
  } else if (is.character(data)) {
    path <- absolute_path(data)
  } else if (is.list(data) && !is.data.frame(data)) {
    if (cmdstan_version() < "2.22" && any_zero_dims(data)) {
      stop(
        "Data includes 0-dimensional data structures. To use this data please ",
        "either update your CmdStan installation with install_cmdstan() ",
        "or specify data as a file created by rstan::stan_rdump().",
        call. = FALSE
      )
    }
    if (cmdstan_version() > "2.26" && !is.null(stan_file)) {
      stan_file <- absolute_path(stan_file)
      if (file.exists(stan_file)) {
        data_variables <- model_variables(stan_file)$data
        is_data_supplied <- names(data_variables) %in%  names(data)
        if (!all(is_data_supplied)) {
          missing <- names(data_variables[!is_data_supplied])
          stop(
            "Missing input data for the following data variables: ",
            paste0(missing, collapse = ", "),
            ".",
            call. = FALSE
          )
        }          
        for(var_name in names(data_variables)) {
          # distinguish between scalars and arrays/vectors of length 1
          if (length(data[[var_name]]) == 1 
              && data_variables[[var_name]]$dimensions == 1) {
              data[[var_name]] <- array(data[[var_name]], dim = 1)
          }
          # Make sure integer inputs are of integer type to avoid
          # generating a decimal point in write_stan_json
          if (data_variables[[var_name]]$type == "int"
              && !is.integer(data[[var_name]])) {
                data[[var_name]] <- as.integer(data[[var_name]])
          }
        }
      }
    }
    path <- tempfile(pattern = "standata-", fileext = ".json")
    write_stan_json(data = data, file = path, always_decimal = (cmdstan_version() > "2.26"))
  } else {
    stop("'data' should be a path or a named list.", call. = FALSE)
  }
  path
}

# check if any objects in the data list have zero as one of their dimensions
any_zero_dims <- function(data) {
  has_zero_dims <- sapply(data, function(x) any(dim(x) == 0))
  any(has_zero_dims)
}

#' Write posterior draws objects to csv files
#' @noRd
#' @param draws A `draws_array` from posterior pkg
#' @param sampler_diagnostics Either `NULL` or a `draws_array` of sampler diagnostics
#' @return Paths to CSV files (one per chain).
#'
draws_to_csv <- function(draws, sampler_diagnostics = NULL) {
  n <- posterior::niterations(draws)
  n_chains <- posterior::nchains(draws)
  zeros <- rep(0, n * n_chains) # filler for creating dummy sampler diagnostics and lp__ if necessary
  if (is.null(sampler_diagnostics)) {
    # create dummy sampler diagnostics due to CmdStan requirement for all columns in GQ
    sampler_diagnostics <- posterior::draws_array(
      accept_stat__ = zeros,
      stepsize__ = zeros,
      treedepth__ = zeros,
      n_leapfrog__ = zeros,
      divergent__ = zeros,
      energy__ = zeros,
      .nchains = n_chains
    )
  }

  # the columns must be in order "lp__, sampler_diagnostics, parameters"
  draws_variables <- posterior::variables(draws)
  if ("lp__" %in% draws_variables) {
    lp__ <- NULL
  } else { # create a dummy lp__ if it does not exist
    lp__ <- posterior::draws_array(lp__ = zeros, .nchains = n_chains)
  }
  all_variables <- c(
    "lp__",
    posterior::variables(sampler_diagnostics),
    draws_variables[!(draws_variables %in% c("lp__", "lp_approx__"))]
  )
  draws <- posterior::subset_draws(
    posterior::bind_draws(draws, sampler_diagnostics, lp__, along = "variable"),
    variable = all_variables
  )

  chains <- posterior::chain_ids(draws)
  paths <- generate_file_names(basename = "fittedParams", ids = chains)
  paths <- file.path(tempdir(), paths)
  chain <- 1
  for (path in paths) {
    write(
      paste0("# num_samples = ", n, "\n", paste0(unrepair_variable_names(all_variables), collapse = ",")),
      file = path,
      append = FALSE
    )
    utils::write.table(
      posterior::subset_draws(draws, chain = chain),
      sep = ",",
      file = path,
      col.names = FALSE,
      row.names = FALSE,
      append = TRUE
    )
    chain <- chain + 1
  }
  paths
}

#' Process fitted params for the generate quantities method
#'
#' @noRd
#' @param fitted_params Paths to CSV files produced by CmdStan sampling,
#'  a CmdStanMCMC or CmdStanVB object, a draws_array or draws_matrix.
#' @return Paths to CSV files containing parameter values.
#'
process_fitted_params <- function(fitted_params) {
  if (is.character(fitted_params)) {
    paths <- absolute_path(fitted_params)
  } else if (checkmate::test_r6(fitted_params, "CmdStanMCMC") &&
             all(file.exists(fitted_params$output_files()))) {
      paths <- absolute_path(fitted_params$output_files())
  } else if (checkmate::test_r6(fitted_params, "CmdStanMCMC")) {
    draws <- tryCatch(
      fitted_params$draws(),
      error = function(cond) {
        stop("Unable to obtain draws from the fit object.", call. = FALSE)
      }
    )
    sampler_diagnostics <- tryCatch(
      fitted_params$sampler_diagnostics()
    )
    paths <- draws_to_csv(draws, sampler_diagnostics)
  } else if (checkmate::test_r6(fitted_params, "CmdStanVB")) {
    draws <- tryCatch(
      fitted_params$draws(),
      error = function(cond) {
        stop("Unable to obtain draws from the fit object.", call. = FALSE)
      }
    )
    paths <- draws_to_csv(posterior::as_draws_array(draws))
  } else if (any(class(fitted_params) == "draws_array")) {
    paths <- draws_to_csv(fitted_params)
  } else if (any(class(fitted_params) == "draws_matrix")) {
    paths <- draws_to_csv(posterior::as_draws_array(fitted_params))
  } else {
    stop(
      "'fitted_params' must be a list of paths to CSV files, ",
      "a CmdStanMCMC/CmdStanVB object, ",
      "a posterior::draws_array or a posterior::draws_matrix.", call. = FALSE)
  }
  paths
}
