#' Write data to a JSON file readable by CmdStan
#'
#' @export
#' @param data (list) A named list of \R objects.
#' @param file (string) The path to where the data file should be written.
#' @param always_decimal (logical) Force generate non-integers with decimal
#'   points to better distinguish between integers and floating point values. If
#'   `TRUE` all \R objects in `data` intended for integers must be of integer
#'   type.
#' @param variables (list) Optionally, the Stan declarations of the variables
#'   in `data`, so that they are written the way the Stan program expects.
#'   Use `mod$variables()$data` (see [`$variables()`][model-method-variables])
#'   or `variables_stan_file(stan_file)$data`. Given the declarations, an
#'   unnamed list is written as a tuple when the variable is a tuple (see
#'   **Tuples** below), a length-1 value is written as an array when the
#'   variable is declared with a dimension (see **Scalar vs. length-1 vector**
#'   below), and a factor is only accepted for an `int` variable. The fitting
#'   methods of a model compiled from a Stan file pass the declarations
#'   themselves. Without them every unnamed list is converted to an array, a
#'   length-1 value is written as a scalar, and every factor is converted to
#'   its level indices.
#'
#' @return `NULL`, invisibly.
#'
#' @details
#' `write_stan_json()` performs several conversions before writing the JSON
#' file:
#'
#' * `logical` -> `integer` (`TRUE` -> `1`, `FALSE` -> `0`)
#' * `factor` -> `integer` (the index of each value's level)
#' * `data.frame` -> `matrix` (via [data.matrix()]); every column must be
#' numeric, integer, logical, or factor
#' * `list` -> `array`
#' * `complex` -> the pair `[re, im]`; a complex vector or matrix -> an array
#' of pairs
#' * `table` -> `vector`, `matrix`, or `array` (depending on dimensions of table)
#'
#' ### Factor conversion
#' Factors are written as their level indices, i.e., the position of each value
#' in `levels(x)` rather than the value itself. The default levels are the
#' sorted unique values, e.g., `factor(c(10, 9, 8))` has levels `8`, `9`, `10`
#' and is written as `[3, 2, 1]`. An unused level shifts the indices of the
#' levels after it. With `variables`, which the fitting methods of a model
#' compiled from a Stan file always pass, a factor for a variable not declared
#' as `int` is an error. Without them `write_stan_json()` has no declarations
#' to check and always does the conversion.
#'
#' ### List to array conversion
#' The `list` to `array` conversion is intended to make it easier to prepare
#' the data for certain Stan declarations involving arrays:
#'
#' * `array[K] vector[J] v ` can be constructed in \R as a list with `K`
#' elements where each element is a vector of length `J`
#' * `array[K] matrix[I,J] m ` can be constructed in \R as a list with `K`
#' elements where each element is an `IxJ` matrix
#' * `array[K,I,J] int n ` can be constructed in \R as a list with `K`
#' elements where each element is an `IxJ` matrix of integers
#'
#' These can also be passed in from \R as arrays instead of lists but the list
#' option is provided for convenience. A list always contributes exactly one
#' leading dimension, so `array[K,L] vector[J] v ` can be supplied either as a
#' list of `K` matrices each with dimensions `LxJ` or as a single \R array with
#' dimensions `KxLxJ`. Nested lists are not supported: every element of the list
#' must be a vector, matrix, or array.
#'
#' ### Tuples
#' A tuple is an unnamed list with one element per tuple element, so
#' `tuple(int, vector[2]) t` is `list(3, c(1.5, 2.5))`, and a nested tuple is
#' a nested list. An array of tuples is a list of such lists:
#' `array[2] tuple(real, real) ts` is `list(list(1, 2), list(3, 4))`. For an
#' array with more than one dimension give the list a `dim` attribute, so
#' `array[2, 3] tuple(real, real)` is `array(cells, dim = c(2, 3))` with
#' `cells` a list of the six tuples in the order [array()] fills them, the
#' first index changing fastest. Since an unnamed list is otherwise converted
#' to an array, a tuple is only written as one when `variables` declares it as
#' a tuple. The fitting methods pass the declarations for you.
#'
#' ### Scalar vs. length-1 vector
#' Because \R does not distinguish between a scalar and a vector of length 1, a
#' length-1 vector like `c(42)` is written to JSON as a scalar (`42`) rather
#' than an array (`[42]`). If a Stan variable is declared as a vector or array
#' that may have length 1, wrap the value in [array()] to force array output.
#' Because `array()` uses the length of its input as the default dimension, this
#' works regardless of length:
#'
#' * `write_stan_json(list(x = array(42)), file)` writes `"x": [42]`
#' * `write_stan_json(list(x = array(c(42, 43))), file)` writes `"x": [42, 43]`
#'
#' This is only necessary when calling `write_stan_json()` directly without
#' `variables`. With them, and in the fitting methods of a model compiled from
#' a Stan file (e.g., `$sample()`), CmdStanR makes this correction from the
#' declarations.
#'
#' @seealso [`$variables()`][model-method-variables] for inspecting the input
#'   and output variables of a Stan program.
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
#' # suppose x is declared as `array[2] vector[3] x`
#' # we can use a list of length 2 where each element is a vector of length 3
#' data <- list(x = list(1:3, 4:6))
#' file <- tempfile(fileext = ".json")
#' write_stan_json(data, file)
#' cat(readLines(file), sep = "\n")
#'
#'
#' # complex numbers are written as [re, im] pairs
#' data <- list(z = 1 + 2i, zv = c(1 + 2i, 3 + 4i))
#' write_stan_json(data, file)
#' cat(readLines(file), sep = "\n")
#'
#'
#' # tuples need the declarations from the Stan program, see 'variables'
#' \dontrun{
#' stan_file <- write_stan_file("
#' data {
#'   tuple(int, vector[2]) t;
#'   array[2] tuple(real, real) ts;
#' }
#' ")
#' data <- list(t = list(3, c(1.5, 2.5)), ts = list(list(1, 2), list(3, 4)))
#' write_stan_json(data, file, variables = variables_stan_file(stan_file)$data)
#' cat(readLines(file), sep = "\n")
#' }
#'
write_stan_json <- function(data, file, always_decimal = FALSE,
                            variables = NULL) {
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
    data[[var_name]] <- convert_variable(data[[var_name]], var_name,
                                         variables[[var_name]])
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


# Types accepted for a data variable and for each column of a data frame
is_valid_data_type <- function(x) {
  is.numeric(x) || is.factor(x) || is.logical(x)
}


# TRUE for a factor, or a data frame with any factor column
has_factor <- function(x) {
  is.factor(x) || (is.data.frame(x) && any(vapply(x, is.factor, logical(1))))
}


# Error if a variable is not one of the types accepted in a data list. Data
# frames and lists are accepted here and converted by convert_to_array().
validate_data_type <- function(var, var_name) {
  if (!is_valid_data_type(var) && !is.complex(var) && !is.data.frame(var) &&
      !is.list(var)) {
    stop("Variable '", var_name, "' is of invalid type.", call. = FALSE)
  }
  invisible(NULL)
}


# Convert the R container types accepted in a data list to the atomic arrays
# CmdStan's JSON reader expects. Used by both write_stan_json() and
# process_data() so that the two paths agree.
convert_to_array <- function(var, var_name = NULL) {
  if (is.table(var)) {
    var <- unclass(var)
  } else if (is.data.frame(var)) {
    # first check all columns are valid types, so data.matrix() doesn't silently
    # coerce character columns to factor codes and date/time columns to numeric
    invalid <- !vapply(var, is_valid_data_type, logical(1))
    if (any(invalid)) {
      stop("Variable '", var_name, "' has columns of invalid type: ",
           paste(names(var)[invalid], collapse = ", "), ".", call. = FALSE)
    }
    var <- data.matrix(var)
  } else if (is.list(var)) {
    var <- list_to_array(var, var_name)
  }
  # after the conversions above so we also convert lists of logicals
  if (is.logical(var)) {
    mode(var) <- "integer"
  }
  var
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
  all_numeric <- all(sapply(x, function(a) is.numeric(a) || is.logical(a)))
  if (!all_numeric) {
    stop("All elements in list '", name, "' must be numeric or logical!", call. = FALSE)
  }
  element_num_of_dim <- length(all_dims[[1]])
  x <- unlist(x)
  dim(x) <- c(all_dims[[1]], list_length)
  aperm(x, c(element_num_of_dim + 1L, seq_len(element_num_of_dim)))
}


#' Convert one variable to what jsonlite writes as CmdStan reads it
#'
#' Used for data and for initial values. The variable's declaration, an
#' entry of `$variables()`, enables the conversions that need one: a tuple
#' from a list, factors and rounding for an `int` variable, and a length-1
#' array kept an array.
#' @noRd
convert_variable <- function(var, var_name, declaration = NULL) {
  if (is.null(var)) {
    stop("Variable '", var_name, "' is NULL.", call. = FALSE)
  }
  if (is.list(declaration$type)) {
    return(convert_tuple(var, var_name, declaration))
  }
  validate_data_type(var, var_name)
  # Factors are written as level indices, which are only meaningful for
  # variables declared as int. Handle them before the conversions below,
  # which replace factors with their codes and drop the factor class.
  if (identical(declaration$type, "int")) {
    if (is.factor(var)) {
      var <- as.integer(var)
    }
  } else if (!is.null(declaration) && has_factor(var)) {
    stop("A factor was supplied for '", var_name, "', which is declared as '",
         declaration$type, "'.", call. = FALSE)
  }
  var <- convert_to_array(var, var_name)
  if (anyNA(var)) {
    stop("Variable '", var_name, "' has NA values.", call. = FALSE)
  }
  # distinguish between scalars and arrays/vectors of length 1
  if (isTRUE(declaration$dimensions == 1) && length(var) == 1) {
    var <- array(var, dim = 1)
  }
  if (is.complex(var)) {
    var <- complex_to_array(var)
  }
  # Make sure integer inputs are of integer type to avoid
  # generating a decimal point in write_stan_json
  if (identical(declaration$type, "int") && !is.integer(var)) {
    if (!isTRUE(all(is_wholenumber(var)))) {
      warning("A non-integer value was supplied for '", var_name, "'!",
              " It will be truncated to an integer.", call. = FALSE)
    } else {
      # Round before setting mode to integer to avoid floating point errors
      var <- round(var)
    }
    mode(var) <- "integer"
  }
  var
}

# A tuple is an unnamed list of its elements, written as a JSON object
# with keys "1", "2", ...; an array of tuples is a list of tuples, with a
# dim for more than one array dimension, written as nested JSON arrays.
convert_tuple <- function(var, var_name, declaration) {
  if (!is.list(var) || is.data.frame(var)) {
    stop("Variable '", var_name, "' is declared as a tuple and must be a list.",
         call. = FALSE)
  }
  if (declaration$dimensions > 0) {
    element <- list(type = declaration$type, dimensions = 0L)
    cells <- lapply(var, convert_tuple, var_name = var_name,
                    declaration = element)
    return(nest_cells(cells, dim(var) %||% length(var)))
  }
  if (length(var) != length(declaration$type)) {
    stop("Variable '", var_name, "' is a tuple with ", length(declaration$type),
         " elements, but ", length(var), " were supplied.", call. = FALSE)
  }
  elements <- lapply(seq_along(var), function(k) {
    convert_variable(var[[k]], paste0(var_name, ":", k), declaration$type[[k]])
  })
  names(elements) <- seq_along(elements)
  elements
}

# Nest cells stored in column-major order the way jsonlite nests an
# array, first index outermost
nest_cells <- function(cells, dims) {
  if (length(dims) == 1) {
    return(unname(cells))
  }
  lapply(seq_len(dims[1]), function(i) {
    nest_cells(cells[seq(i, length(cells), by = dims[1])], dims[-1])
  })
}

# CmdStan reads a complex number as the pair [re, im], and complex
# containers as arrays of pairs
complex_to_array <- function(x) {
  parts <- c(Re(x), Im(x))
  if (is.null(dim(x)) && length(x) == 1) {
    return(parts)
  }
  array(parts, dim = c(dim(x) %||% length(x), 2))
}


#' Process data for CmdStanModel methods
#'
#' @noRd
#' @param data If not `NULL`, then either a path to a data file compatible with
#'   CmdStan, or a named list of \R objects to pass to [write_stan_json()].
#' @param model_variables A list of all parameters with their types and
#'   number of dimensions. Typically the output of model$variables().
#' @return Path to data file.
process_data <- function(data, model_variables = NULL) {
  if (length(data) == 0) {
    data <- NULL
  }
  if (is.null(data)) {
    path <- data
  } else if (is.character(data)) {
    path <- absolute_path(data)
  } else if (is.list(data) && !is.data.frame(data)) {
    if (!is.null(model_variables)) {
      data_variables <- model_variables$data
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
    }
    path <- tempfile(pattern = "standata-", fileext = ".json")
    write_stan_json(data = data, file = path,
                    always_decimal = !is.null(model_variables),
                    variables = model_variables$data)
  } else {
    stop("'data' should be a path or a named list.", call. = FALSE)
  }
  path
}


#' Write posterior draws objects to CSV files suitable for running standalone generated
#' quantities with CmdStan.
#'
#' @export
#' @param draws A `posterior::draws_*` object.
#' @param sampler_diagnostics Either `NULL` or a `posterior::draws_*` object
#'  of sampler diagnostics.
#' @param dir (string) An optional path to the directory where the CSV files
#'   will be written. If not set, [temporary directory][base::tempdir] is used.
#' @param basename (string) The base name for the output CSV files. The default
#'   is `"fittedParams"`. A timestamp, chain ID, and six-character random
#'   hexadecimal suffix are appended to the base name.
#'
#' @return Paths to CSV files (one per chain).
#'
#' @details
#' `draws_to_csv()` generates a CSV suitable for running standalone generated
#' quantities with CmdStan. The CSV file contains a single comment
#' `# num_samples = <n>`, where `<n>` is the number of iterations in the
#' supplied draws object.
#'
#' The comment is followed by the column names. The first column is the `lp__`
#' value, followed by sampler diagnostics and finally other variables of the
#' draws object. If the draws object does not contain the `lp__` or sampler
#' diagnostics variables, columns with zeros are created in order to conform
#' with the requirements of the standalone generated quantities method of
#' CmdStan.
#'
#' The column names line is finally followed by the values of the draws in the same
#' order as the column names.
#'
#' @seealso [`$generate_quantities()`][model-method-generate-quantities] for
#'   using the generated CSV files
#'
#' @examples
#' \dontrun{
#' draws <- posterior::example_draws()
#'
#' draws_csv_files <- draws_to_csv(draws)
#' print(draws_csv_files)
#'
#' # draws_csv_files <- draws_to_csv(draws,
#' #                                 sampler_diagnostics = sampler_diagnostics,
#' #                                 dir = "~/my_folder",
#' #                                 basename = "my-samples")
#' }
#'
draws_to_csv <- function(draws,
                         sampler_diagnostics = NULL,
                         dir = tempdir(),
                         basename = "fittedParams") {
  sampler_diagnostics_names <- c(
    "accept_stat__", "stepsize__", "treedepth__",
    "n_leapfrog__", "divergent__", "energy__"
  )
  n <- posterior::niterations(draws)
  n_chains <- posterior::nchains(draws)
  draws_variables <- posterior::variables(draws)
  sampler_diagnostics_variables <- posterior::variables(sampler_diagnostics)

  # create dummy sampler diagnostics due to CmdStan requirement for all columns in GQ if needed
  zeros <- rep(0, n * n_chains) # filler for creating dummy sampler diagnostics and lp__ if necessary
  if (is.null(sampler_diagnostics)) {
    missing_sampler_diagnostics <- sampler_diagnostics_names[!(sampler_diagnostics_names %in% draws_variables)]

  } else {
    missing_sampler_diagnostics <- sampler_diagnostics_names[!(sampler_diagnostics_names %in% draws_variables)]
    missing_sampler_diagnostics <- missing_sampler_diagnostics[!(missing_sampler_diagnostics %in% sampler_diagnostics_variables)]
  }
  if (length(missing_sampler_diagnostics) > 0) {
    additional_sampler_diagnostics <- list()
    for (name in missing_sampler_diagnostics) {
      additional_sampler_diagnostics[[name]] <- zeros
    }
    additional_sampler_diagnostics[[".nchains"]] <- n_chains
    additional_sampler_diagnostics <- do.call(posterior::draws_array, additional_sampler_diagnostics)
    sampler_diagnostics <- posterior::bind_draws(sampler_diagnostics, additional_sampler_diagnostics)
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
    sampler_diagnostics_names,
    draws_variables[!(draws_variables %in% c("lp__", "lp_approx__", sampler_diagnostics_names))]
  )
  draws <- posterior::subset_draws(
    posterior::bind_draws(draws, sampler_diagnostics, lp__, along = "variable"),
    variable = all_variables
  )

  chains <- posterior::chain_ids(draws)
  paths <- generate_file_names(basename = basename, ids = chains)
  paths <- file.path(dir, paths)
  chain <- 1
  for (path in paths) {
    write(
      paste0("# num_samples = ", n, "\n", paste0(unrepair_variable_names(all_variables), collapse = ",")),
      file = path,
      append = FALSE
    )
    data <- posterior::as_draws_df(posterior::subset_draws(draws, chain = chain))
    class(data) <- "data.frame"
    data$.chain <- NULL
    data$.iteration <- NULL
    data$.draw <- NULL
    data.table::fwrite(
      data,
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
#'  a CmdStanMCMC, CmdStanMLE, CmdStanLaplace, CmdStanVB, or CmdStanPathfinder
#'  object, a draws_array or draws_matrix.
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
  } else if (checkmate::test_r6(fitted_params, "CmdStanMLE") ||
             checkmate::test_r6(fitted_params, "CmdStanLaplace") ||
             checkmate::test_r6(fitted_params, "CmdStanVB") ||
             checkmate::test_r6(fitted_params, "CmdStanPathfinder")) {
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
      "a CmdStanMCMC, CmdStanMLE, CmdStanLaplace, CmdStanVB, or ",
      "CmdStanPathfinder object, ",
      "a posterior::draws_array or a posterior::draws_matrix.", call. = FALSE)
  }
  paths
}
