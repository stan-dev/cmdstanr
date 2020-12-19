# misc --------------------------------------------------------------------


is_verbose_mode <- function() {
  getOption("cmdstanr_verbose", default = FALSE)
}

# Famous helper for switching on `NULL` or zero length
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}


# checks for OS and hardware ----------------------------------------------

os_is_windows <- function() {
  isTRUE(.Platform$OS.type == "windows")
}

os_is_macos <- function() {
  isTRUE(Sys.info()[["sysname"]] == "Darwin")
}

# Check if running R in Rosetta 2 translation environment, which is an
# Intel-to-ARM translation layer.
is_rosetta2 <- function() {
  rosetta2 <- FALSE
  if (os_is_macos()) {
    rosetta2_check <- processx::run("/usr/sbin/sysctl",
                                    args = c("-n", "sysctl.proc_translated"),
                                    error_on_status = FALSE)
    rosetta2 <- rosetta2_check$stdout == "1\n"
  }
  rosetta2
}

# Returns the type of make command to use to compile depending on the OS
make_cmd <- function() {
  # Cmdstan 2.21 introduced TBB that requires mingw32-make on Windows
  ver <- .cmdstanr$VERSION
  if (os_is_windows() && (is.null(ver) || ver >= "2.21")) {
    "mingw32-make.exe"
  } else {
    "make"
  }
}

# Returns the stanc exe path depending on the OS
stanc_cmd <- function() {
  if (os_is_windows()) {
    "bin/stanc.exe"
  } else {
    "bin/stanc"
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
    # remove trailing "/"
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
      rand_num_pid <- as.integer(stats::runif(1, min = 0, max = 1E7)) + Sys.getpid()
      rand <- format(as.hexmode(rand_num_pid) , width = 6)
      new_names <- paste0(new_names, "-", rand)
    }

    if (length(ext)) {
      ext <- if (startsWith(ext, ".")) ext else paste0(".", ext)
      new_names <- paste0(new_names, ext)
    }
    new_names
  }


cpp_options_to_compile_flags <- function(cpp_options) {
  if (length(cpp_options) == 0) {
    return(NULL)
  }
  cpp_built_options = c()
  for (i in seq_len(length(cpp_options))) {
    option_name <- names(cpp_options)[i]
    if (is.null(option_name) || !nzchar(option_name)) {
      cpp_built_options = c(cpp_built_options, toupper(cpp_options[[i]]))
    } else {
      cpp_built_options = c(cpp_built_options, paste0(toupper(option_name), "=", cpp_options[[i]]))
    }
  }
  cpp_built_options
}

check_stanc_options <- function(stanc_options) {
  i <- 1
  names <- names(stanc_options)
  for (s in stanc_options){
    if (!is.null(names[i]) && nzchar(names[i])) {
      name <- names[i]
    } else {
      name <- s
    }
    if (startsWith(name, "--")) {
      stop("No leading hyphens allowed in stanc options (", name, "). ",
           "Use options without leading hyphens, like for example ",
           "`stanc_options = list('allow-undefined')`",
           call. = FALSE)
    }
    i <- i + 1
  }
}

#' Set or get the number of threads used to execute Stan models
#'
#' DEPRECATED. Please use the `threads_per_chain` argument when fitting the model.
#'
#' @name stan_threads
NULL

#' @rdname stan_threads
#' @export
#' @return The value of the environment variable `STAN_NUM_THREADS`.
num_threads <- function() {
  warning("'num_threads()' is deprecated. Please use the 'metadata()' method of the fit object to obtain the number of threads used.")
  as.integer(Sys.getenv("STAN_NUM_THREADS"))
}

#' @rdname stan_threads
#' @export
#' @param num_threads (positive integer) The number of threads to set.
set_num_threads <- function(num_threads) {
  stop("Please use the 'threads_per_chain' argument in the $sample() method instead of set_num_threads().",
       call. = FALSE)
}

check_divergences <- function(data_csv) {
  if (!is.null(data_csv$post_warmup_sampler_diagnostics)) {
    divergences <- posterior::extract_variable_matrix(data_csv$post_warmup_sampler_diagnostics, "divergent__")
    num_of_draws <- length(divergences)
    num_of_divergences <- sum(divergences)
    if (!is.na(num_of_divergences) && num_of_divergences > 0) {
      percentage_divergences <- (num_of_divergences)/num_of_draws*100
      message(
        "\nWarning: ", num_of_divergences, " of ", num_of_draws,
        " (", (format(round(percentage_divergences, 0), nsmall = 1)), "%)",
        " transitions ended with a divergence.\n",
        "This may indicate insufficient exploration of the posterior distribution.\n",
        "Possible remedies include: \n",
        "  * Increasing adapt_delta closer to 1 (default is 0.8) \n",
        "  * Reparameterizing the model (e.g. using a non-centered parameterization)\n",
        "  * Using informative or weakly informative prior distributions \n"
      )
    }
  }
}

check_sampler_transitions_treedepth <- function(data_csv) {
  if (!is.null(data_csv$post_warmup_sampler_diagnostics)) {
    treedepth <- posterior::extract_variable_matrix(data_csv$post_warmup_sampler_diagnostics, "treedepth__")
    num_of_draws <- length(treedepth)
    max_treedepth <- data_csv$metadata$max_treedepth
    max_treedepth_hit <- sum(treedepth >= max_treedepth)
    if (!is.na(max_treedepth_hit) && max_treedepth_hit > 0) {
      percentage_max_treedepth <- (max_treedepth_hit)/num_of_draws*100
      message(max_treedepth_hit, " of ", num_of_draws, " (", (format(round(percentage_max_treedepth, 0), nsmall = 1)), "%)",
              " transitions hit the maximum treedepth limit of ", max_treedepth,
              " or 2^", max_treedepth, "-1 leapfrog steps.\n",
              "Trajectories that are prematurely terminated due to this limit will result in slow exploration.\n",
              "Increasing the max_treedepth limit can avoid this at the expense of more computation.\n",
              "If increasing max_treedepth does not remove warnings, try to reparameterize the model.\n")
    }
  }
}

matching_variables <- function(variable_filters, variables) {
  not_found <- c()
  selected_variables <- c()
  for(v in variable_filters) {
    selected <- variables == v | startsWith(variables, paste0(v, "["))
    selected_variables <- c(selected_variables, variables[selected])
    variables <- variables[!selected]
    if (!any(selected)) {
      not_found <- c(not_found, v)
    }
  }
  list(
    matching = selected_variables,
    not_found = not_found
  )
}

#' Returns a list of dimensions for the input variables.
#'
#' @noRd
#' @param variable_names A character vector of variable names including all
#'   individual elements (e.g., `c("beta[1]", "beta[2]")`, not just `"beta"`).
#' @return A list giving the dimensions of the variables. The equivalent of the
#'   `par_dims` slot of RStan's stanfit objects, except that scalars have
#'   dimension `1` instead of `0`.
#' @note For this function to return the correct dimensions the input must be
#'   already sorted in ascending order. Since CmdStan always has the variables
#'   sorted correctly we avoid a sort by not sorting again here.
#'
variable_dims <- function(variable_names = NULL) {
  if (is.null(variable_names)) {
    return(NULL)
  }
  dims <- list()
  uniq_variable_names <- unique(gsub("\\[.*\\]", "", variable_names))
  var_names <- gsub("\\]", "", variable_names)
  for (var in uniq_variable_names) {
    pattern <- paste0("^", var, "\\[")
    var_indices <- var_names[grep(pattern, var_names)]
    var_indices <- gsub(pattern, "", var_indices)
    if (length(var_indices)) {
      var_indices <- strsplit(var_indices[length(var_indices)], ",")[[1]]
      dims[[var]] <- as.numeric(var_indices)
    } else {
      dims[[var]] <- 1
    }
  }
  dims
}

