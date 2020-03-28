# misc --------------------------------------------------------------------

#' Check for Windows
#' @return `TRUE` if OS is Windows, `FALSE` if not.
#' @noRd
os_is_windows <- function() {
  isTRUE(.Platform$OS.type == "windows")
}

#' Famous helper for switching on `NULL`
#' @param x,y Any \R objects.
#' @return `x` if not `NULL`, otherwise `y` regardless of whether `y` is `NULL`.
#' @noRd
`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}

#' Returns the type of make command to use to compile
#' @return Returns "mingw32-make" if using cmdstan 2.21+ and running Windows
#' @noRd
make_cmd <- function() {
  # Cmdstan 2.21 introduced TBB that requires mingw32-make on Windows
  ver <- .cmdstanr$VERSION
  if (os_is_windows() && (is.null(ver) || ver >= "2.21")) {
    "mingw32-make.exe"
  } else {
    "make"
  }
}

check_target_exe <- function(exe) {
  exe_path <- file.path(cmdstan_path(), exe)
  if (!file.exists(exe_path)) {
    run_log <- processx::run(
      command = make_cmd(),
      args = exe,
      wd = cmdstan_path(),
      echo_cmd = TRUE,
      echo = TRUE,
      error_on_status = TRUE
    )
  }
}
# paths and extensions ----------------------------------------------------

# Replace `\\` with `/` in a path
# Needed for windows if CmdStan version is < 2.21:
# https://github.com/stan-dev/cmdstanr/issues/1#issuecomment-539118598
repair_path <- function(path) {
  if (!length(path) || !is.character(path)) {
    return(path)
  }
  path <- path.expand(path)
  path <- gsub("\\\\", "/", path)
  path <- gsub("//", "/", path)
  if (endsWith(path, "/")) {
    # remove trailing "/" (is this necessary?)
    path <- substr(path, 1, nchar(path) - 1)
  }
  path
}

#' Get extension for executable depending on OS
#' @noRd
#' @param path If not `NULL` then a path to add the extension to.
#' @return If `path` is `NULL` then `".exe"` on Windows and `""` otherwise. If
#'   `path` is not `NULL` then `.exe` is added as the extension on Windows.
cmdstan_ext <- function(path = NULL) {
  ext <- if (os_is_windows()) ".exe" else ""
  if (is.null(path)) {
    return(ext)
  }
  path <- repair_path(path)
  paste0(path, ext)
}

# Strip extension from a file path
strip_ext <- function(file) {
  tools::file_path_sans_ext(file)
}

# If a file/dir exists return its absolute path
# doesn't error if not found
.absolute_path <- function(path) {
  if (file.exists(path)) {
    new_path <- repair_path(path)
  } else {
    new_path <- path
  }

  if (grepl("^~", path) || grepl("^(/+|[A-Za-z]:)", new_path)) {
    return(new_path)
  }
  repair_path(file.path(getwd(), new_path))
}
absolute_path <- Vectorize(.absolute_path, USE.NAMES = FALSE)



# read, write, and copy files --------------------------------------------

#' Copy temporary files (e.g., output, data) to a different location
#'
#' Copies to specified directory using specified basename,
#' appending suffix `-id.ext` to each. If files with the specified
#' names already exist they are overwritten.
#'
#' @noRd
#' @param current_paths Paths to current temporary files.
#' @param new_dir Path to directory where the files should be saved.
#' @param new_basename Base filename to use.
#' @param ids Unique identifiers (e.g., `chain_ids`).
#' @param timestamp Add a timestamp to the file names?
#' @param ext Extension to use for all saved files (default is `ext=".csv"`).
#' @return The paths to the new files or `NA` for any that couldn't be
#'   copied.
copy_temp_files <-
  function(current_paths,
           new_dir,
           new_basename,
           ids = NULL,
           timestamp = TRUE,
           random = TRUE,
           ext = ".csv") {
    checkmate::assert_directory_exists(new_dir, access = "w")
    destinations <- generate_file_names(
      basename = new_basename,
      ext = ext,
      ids = ids,
      timestamp = timestamp,
      random = random
    )
    if (new_dir != ".") {
      destinations <- file.path(new_dir, destinations)
    }

    copied <- file.copy(
      from = current_paths,
      to = destinations,
      overwrite = TRUE
    )
    if (!all(copied)) {
      destinations[!copied] <- NA_character_
    }
    absolute_path(destinations)
  }

# generate new file names
# see doc above for copy_temp_files
generate_file_names <-
  function(basename,
           ext = ".csv",
           ids = NULL,
           timestamp = TRUE,
           random = TRUE) {
    new_names <- basename
    if (timestamp) {
      stamp <- format(Sys.time(), "%Y%m%d%H%M")
      new_names <- paste0(new_names, "-", stamp)
    }
    if (!is.null(ids)) {
      new_names <- paste0(new_names, "-", ids)
    }

    if (random) {
      tf <- tempfile()
      rand <- substr(tf, nchar(tf) - 5, nchar(tf))
      new_names <- paste0(new_names, "-", rand)
    }

    if (length(ext)) {
      ext <- if (startsWith(ext, ".")) ext else paste0(".", ext)
      new_names <- paste0(new_names, ext)
    }
    new_names
  }


list_to_array <- function(x) {
  list_length <- length(x)
  if (list_length == 0 ) return(NULL)
  element_dim <- length(x[[1]])
  check_equal_dim <- function(x, target_dim) { !is.null(element_dim) && length(x) == target_dim }
  all_same_size <- all(sapply(x, check_equal_dim, target_dim = element_dim))
  if (!all_same_size) {
    stop("All matrices/vectors in the list must be the same size!", call. = FALSE)
  }
  all_numeric <- all(sapply(x, function(a) is.numeric(a)))
  if (!all_numeric) {
    stop("All elements of the list must be numeric!", call. = FALSE)
  }
  element_num_of_dim <- length(element_dim)
  x <- unlist(x)
  dim(x) <- c(element_dim, list_length)
  aperm(x, c(element_num_of_dim + 1L, seq_len(element_num_of_dim)))
}

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
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Please install the 'jsonlite' package.", call. = FALSE)
  }
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
      var <- list_to_array(var)
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

#' Set the Cmdstan make options or flags used when compiling the
#' generated C++ code.
#'
#' @param cpp_options a list of make options to use when compiling the generated C++ code
#' @param quiet If TRUE, will suppress the output of compilation
#' @return TRUE if cpp_options were changed, FALSE otherwise
#' @export
#'
set_cmdstan_cpp_options <- function(cpp_options, quiet = TRUE) {
  if (is.null(.cmdstanr$CPP_OPTIONS) ||
      any(length(cpp_options) != length(.cmdstanr$CPP_OPTIONS)) ||
      any(names(cpp_options) != names(.cmdstanr$CPP_OPTIONS)) ||
      any(unlist(cpp_options) != unlist(.cmdstanr$CPP_OPTIONS))) {
    message("The cpp options were changed, recompiling pre-built binaries...")
    .cmdstanr$CPP_OPTIONS <- cpp_options
    main_path <- file.path(cmdstan_path(), "src", "cmdstan", "main")
    model_header_path <- file.path(cmdstan_path(), "stan", "src", "stan", "model", "model_header")
    files_to_remove <- c(
      paste0(main_path, c(".d", ".o")),
      paste0(model_header_path, c(".d", ".hpp.gch"))
    )
    for (file in files_to_remove) if (file.exists(file)) {
      file.remove(file)
    }

    run_log <- processx::run(
      command = make_cmd(),
      args = c(
                paste0(main_path, ".o"),
                cpp_options_to_compile_flags(cpp_options)
              ),
      wd = cmdstan_path(),
      echo_cmd = !quiet,
      echo = !quiet,
      spinner = quiet,
      stderr_line_callback = function(x,p) { if(!quiet) message(x) },
      error_on_status = TRUE
    )
    TRUE
  } else {
    FALSE
  }
}

cpp_options_to_compile_flags <- function(cpp_options) {
  if (length(cpp_options) == 0) {
    return(NULL)
  }
  stanc_built_options = c()
  for (i in seq_len(length(cpp_options))) {
    option_name <- names(cpp_options)[i]
    stanc_built_options = c(stanc_built_options, paste0(toupper(option_name), "=", cpp_options[[i]]))
  }
  paste0(stanc_built_options, collapse = " ")
}

#' Set or get the number of threads used to execute Stan models
#'
#' @name stan_threads
#' @description These functions set or get the `STAN_NUM_THREADS` environment
#'   variable, which will be read by CmdStan at run-time if threading support
#'   was enabled when [compiled][model-method-compile]. For details on how this
#'   is used by CmdStan see the
#'   [Threading Support](https://github.com/stan-dev/math/wiki/Threading-Support)
#'   wiki on GitHub.
#'
NULL

#' @rdname stan_threads
#' @export
#' @return The value of the environment variable `STAN_NUM_THREADS`.
num_threads <- function() {
  num_threads <- Sys.getenv("STAN_NUM_THREADS")
  as.integer(num_threads)
}

#' @rdname stan_threads
#' @export
#' @param num_threads (positive integer) The number of threads to set.
set_num_threads <- function(num_threads) {
  if (is.numeric(num_threads) && num_threads%%1==0 && num_threads > 0) {
    Sys.setenv("STAN_NUM_THREADS" = num_threads)
  } else {
    stop("Please set a valid number of threads. Valid values are integers > 0.",
         call. = FALSE)
  }
}

check_divergences <- function(data_csv) {
  if(!is.null(data_csv$post_warmup_sampler_diagnostics)) {
    divergences <- posterior::extract_variable_matrix(data_csv$post_warmup_sampler_diagnostics, "divergent__")
    num_of_draws <- length(divergences)
    num_of_divergences <- sum(divergences)
    if (num_of_divergences > 0) {
      percentage_divergences <- (num_of_divergences)/num_of_draws*100
      message(num_of_divergences, " of ", num_of_draws, " (", (format(round(percentage_divergences, 0), nsmall = 1)), "%)",
              " transitions ended with a divergence.\n",
              "These divergent transitions indicate that HMC is not fully able to explore the posterior distribution.\n",
              "Try increasing adapt delta closer to 1.\n",
              "If this doesn't remove all divergences, try to reparameterize the model.\n")
    }
  }
}

check_sampler_transitions_treedepth <- function(data_csv) {
  if(!is.null(data_csv$post_warmup_sampler_diagnostics)) {
    treedepth <- posterior::extract_variable_matrix(data_csv$post_warmup_sampler_diagnostics, "treedepth__")
    num_of_draws <- length(treedepth)
    max_treedepth <- data_csv$sampling_info$max_depth
    max_treedepth_hit <- sum(treedepth >= max_treedepth)
    if (max_treedepth_hit > 0) {
      percentage_max_treedepth <- (max_treedepth_hit)/num_of_draws*100
      message(max_treedepth_hit, " of ", num_of_draws, " (", (format(round(percentage_max_treedepth, 0), nsmall = 1)), "%)",
              " transitions hit the maximum treedepth limit of ", max_treedepth,
              " or 2^", max_treedepth, "-1 leapfrog steps.\n",
              "Trajectories that are prematurely terminated due to this limit will result in slow exploration.\n",
              "Increasing the max_depth limit can avoid this at the expense of more computation.\n",
              "If increasing max_depth does not remove warnings, try to reparameterize the model.\n")
    }
  }
}
