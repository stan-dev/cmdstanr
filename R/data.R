#' Write data to a JSON file readable by CmdStan
#'
#' @export
#' @param data A named list of \R objects.
#' @param file A string specifying the path to where the data file should be
#'   written.
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
write_stan_json <- function(data, file) {
  if (!is.character(file) || !nzchar(file)) {
    stop("The supplied filename is invalid!", call. = FALSE)
  }

  for (var_name in names(data)) {
    var <- data[[var_name]]
    if (!(is.numeric(var) || is.factor(var) || is.logical(var) ||
          is.data.frame(var) || is.list(var))) {
      stop("Variable '", var_name, "' is of invalid type.", call. = FALSE)
    }

    if (is.logical(var)) {
      mode(var) <- "integer" # convert TRUE/FALSE to 1/0
    } else if (is.data.frame(var)) {
      var <- data.matrix(var)
    } else if (is.list(var)) {
      var <- list_to_array(var, var_name)
    }
    data[[var_name]] <- var
  }

  # call to write JSON with
  # unboxing variables (N = 10 is stored as N : 10, not N: [10])
  # handling factors as integers
  jsonlite::write_json(
    data,
    path = file,
    auto_unbox = TRUE,
    factor = "integer",
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
    stop("All elements in list '", name,"' must be numeric!", call. = FALSE)
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
#' @return Path to data file.
process_data <- function(data) {
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
    if (any_na_elements(data)) {
      stop("Data includes NA values.", call. = FALSE)
    }
    path <- tempfile(pattern = "standata-", fileext = ".json")
    write_stan_json(data = data, file = path)
  } else {
    stop("'data' should be a path or a named list.", call. = FALSE)
  }
  path
}

# check if any objects in the data list have zero as one of their dimensions
any_zero_dims <- function(data) {
  has_zero_dims <- sapply(data, function(x) {
    any(dim(x) == 0)
  })
  any(has_zero_dims)
}

# check if any objects in the data list contain NAs
any_na_elements <- function(data) {
  has_na_elements <- sapply(data, anyNA)
  any(has_na_elements)
}


#' Process fitted params for the generate quantities method
#'
#' @noRd
#' @param fitted_params Paths to CSV files compatible with CmdStan or a CmdStanMCMC object.
#' @return Paths to CSV files containing parameter values.
#'
process_fitted_params <- function(fitted_params) {
  if (is.character(fitted_params)) {
    paths <- absolute_path(fitted_params)
  } else if (checkmate::test_r6(fitted_params, classes = ("CmdStanMCMC"))) {
    if (all(file.exists(fitted_params$output_files()))) {
      paths <- absolute_path(fitted_params$output_files())
    } else {
      draws <- tryCatch(posterior::as_draws_array(fitted_params$draws()),
        error=function(cond) {
            stop("Unable to obtain draws from the fit (CmdStanMCMC) object.", call. = FALSE)
        }
      )
      sampler_diagnostics <- tryCatch(posterior::as_draws_array(fitted_params$sampler_diagnostics()),
        error=function(cond) {
            stop("Unable to obtain sampler diagnostics from the fit (CmdStanMCMC) object.", call. = FALSE)
        }
      )
      if (!is.null(draws)) {
        variables <- posterior::variables(draws)
        non_lp_variables <- variables[variables != "lp__"]
        draws <- posterior::bind_draws(
          posterior::subset_draws(draws, variable = "lp__"),
          sampler_diagnostics,
          posterior::subset_draws(draws, variable = non_lp_variables),
          along = "variable"
        )
        variables <- posterior::variables(draws)
        chains <- posterior::chain_ids(draws)
        iterations <- posterior::niterations(draws)
        paths <- generate_file_names(basename = "fittedParams", ids = chains)
        paths <- file.path(tempdir(), paths)
        chain <- 1
        for (path in paths) {
          chain_draws <- posterior::subset_draws(draws, chain = chain)
          unname(chain_draws)         
          write(
            paste0("# num_samples = ", iterations),
            file = path
          )
          write(
            paste0(unrepair_variable_names(variables), collapse = ","),
            file = path,
            append = TRUE
          )
          utils::write.table(
            chain_draws,
            file = path,
            sep = ",",
            col.names = FALSE,
            append = TRUE
          )
          chain <- chain + 1
        }
      }
    }
  } else {
    stop("'fitted_params' should be a vector of paths or a CmdStanMCMC object.", call. = FALSE)
  }
  paths
}
