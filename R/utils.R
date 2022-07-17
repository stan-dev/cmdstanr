# misc --------------------------------------------------------------------

require_suggested_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Please install the '", pkg, "' package to use this function.",
         call. = FALSE)
  }
}

is_verbose_mode <- function() {
  getOption("cmdstanr_verbose", default = FALSE)
}

# Famous helper for switching on `NULL` or zero length
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

# used in both fit.R and csv.R for variable filtering
matching_variables <- function(variable_filters, variables) {
  not_found <- c()
  selected_variables <- c()
  for (v in variable_filters) {
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


# checks for OS and hardware ----------------------------------------------

os_is_windows <- function() {
  isTRUE(.Platform$OS.type == "windows")
}

os_is_wsl <- function() {
  os_is_windows() && (isTRUE(.cmdstanr$WSL) || Sys.getenv("CMDSTANR_USE_WSL") == 1)
}

os_is_macos <- function() {
  isTRUE(Sys.info()[["sysname"]] == "Darwin")
}

is_rtools42_toolchain <- function() {
  os_is_windows() && R.version$major == "4" && R.version$minor >= "2.0"
}

is_rtools40_toolchain <- function() {
  os_is_windows() && R.version$major == "4" && R.version$minor < "2.0"
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
  if (os_is_windows() && !os_is_wsl()) {
    "mingw32-make.exe"
  } else {
    "make"
  }
}

# Returns the stanc exe path depending on the OS
stanc_cmd <- function() {
  if (os_is_windows() && !os_is_wsl()) {
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
  ext <- if (os_is_windows() && !os_is_wsl()) ".exe" else ""
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

# When providing the model path to WSL, it needs to be in reference to the
# to Windows mount point (/mnt/drive-letter) within the WSL install:
# e.g., C:/Users/... -> /mnt/c/Users/...
wsl_safe_path <- function(path = NULL, revert = FALSE) {
  if (!is.character(path) || is.null(path) || !os_is_wsl()) {
    return(path)
  }
  if (revert) {
    if (!grepl("^/mnt/", path)) {
      return(path)
    }
    strip_mnt <- gsub("^/mnt/", "", path)
    drive_letter <- strtrim(strip_mnt, 1)
    path <- gsub(paste0("^/mnt/", drive_letter),
                  paste0(toupper(drive_letter), ":"),
                  path)
  } else {
    path_already_safe <- grepl("^/mnt/", path)
    if (os_is_wsl() && !isTRUE(path_already_safe) && !is.na(path)) {
      base_file <- basename(path)
      path <- dirname(path)
      abs_path <- repair_path(utils::shortPathName(path))
      drive_letter <- tolower(strtrim(abs_path, 1))
      path <- gsub(paste0(drive_letter, ":"),
                  paste0("/mnt/", drive_letter),
                  abs_path,
                  ignore.case = TRUE)
      path <- paste0(path, "/", base_file)
    }
  }
  path
}

# Running commands through WSL requires using 'wsl' as the command with the
# intended command (e.g., stanc) as the first argument. This function acts as
# a wrapper around processx::run() to apply this change where necessary, and
# forward all other arguments
wsl_compatible_run <- function(...) {
  run_args <- list(...)
  if (os_is_wsl()) {
    command <- run_args$command
    run_args$command <- "wsl"
    run_args$args <- c(command, run_args$args)
  }
  do.call(processx::run, run_args)
}

wsl_compatible_process_new <- function(...) {
  run_args <- list(...)
  if (os_is_wsl()) {
    command <- run_args$command
    run_args$command <- "wsl"
    run_args$args <- c(command, run_args$args)
  }
  do.call(processx::process$new, run_args)
}

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
      stamp <- base::format(Sys.time(), "%Y%m%d%H%M")
      new_names <- paste0(new_names, "-", stamp)
    }
    if (!is.null(ids)) {
      new_names <- paste0(new_names, "-", ids)
    }
    if (random) {
      rand_num_pid <- as.integer(stats::runif(1, min = 0, max = 1E7)) + Sys.getpid()
      rand <- base::format(as.hexmode(rand_num_pid), width = 6)
      new_names <- paste0(new_names, "-", rand)
    }

    if (length(ext)) {
      ext <- if (startsWith(ext, ".")) ext else paste0(".", ext)
      new_names <- paste0(new_names, ext)
    }
    new_names
  }

# threading helpers (deprecated) ------------------------------------------


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


# hmc diagnostics ------------------------------------------------------
check_divergences <- function(post_warmup_sampler_diagnostics) {
  num_divergences_per_chain <- NULL
  if (!is.null(post_warmup_sampler_diagnostics)) {
    divergences <- posterior::extract_variable_matrix(post_warmup_sampler_diagnostics, "divergent__")
    num_divergences_per_chain <- colSums(divergences)
    num_divergences <- sum(num_divergences_per_chain)
    num_draws <- length(divergences)
    if (!is.na(num_divergences) && num_divergences > 0) {
      percentage_divergences <- 100 * num_divergences / num_draws
      message(
        "Warning: ", num_divergences, " of ", num_draws,
        " (", (base::format(round(percentage_divergences, 0), nsmall = 1)), "%)",
        " transitions ended with a divergence.\n",
        "See https://mc-stan.org/misc/warnings for details.\n"
      )
    }
  }
  invisible(unname(num_divergences_per_chain))
}

check_max_treedepth <- function(post_warmup_sampler_diagnostics, metadata) {
  num_max_treedepths_per_chain <- NULL
  if (!is.null(post_warmup_sampler_diagnostics)) {
    treedepths <- posterior::extract_variable_matrix(post_warmup_sampler_diagnostics, "treedepth__")
    num_max_treedepths_per_chain <- apply(treedepths, 2, function(x) sum(x >= metadata$max_treedepth))
    num_max_treedepths <- sum(num_max_treedepths_per_chain)
    num_draws <- length(treedepths)
    if (!is.na(num_max_treedepths) && num_max_treedepths > 0) {
      percentage_max_treedepths <- 100 * num_max_treedepths / num_draws
      message(
        "Warning: ", num_max_treedepths, " of ", num_draws, " (", (base::format(round(percentage_max_treedepths, 0), nsmall = 1)), "%)",
        " transitions hit the maximum treedepth limit of ", metadata$max_treedepth,".\n",
        "See https://mc-stan.org/misc/warnings for details.\n"
      )
    }
  }
  invisible(unname(num_max_treedepths_per_chain))
}

ebfmi <- function(post_warmup_sampler_diagnostics) {
  efbmi_per_chain <- NULL
  if (!is.null(post_warmup_sampler_diagnostics)) {
    if (!("energy__" %in% posterior::variables(post_warmup_sampler_diagnostics))) {
      warning("E-BFMI not computed because the 'energy__' diagnostic could not be located.", call. = FALSE)
    } else if (posterior::niterations(post_warmup_sampler_diagnostics) < 3) {
      warning("E-BFMI not computed because it is undefined for posterior chains of length less than 3.", call. = FALSE)
    } else {
      energy <- posterior::extract_variable_matrix(post_warmup_sampler_diagnostics, "energy__")
      if (any(is.na(energy))) {
        warning("E-BFMI not computed because 'energy__' contains NAs.", call. = FALSE)
      } else {
        efbmi_per_chain <- apply(energy, 2, function(x) {
          (sum(diff(x)^2) / length(x)) / stats::var(x)
        })
      }
    }
  }
  efbmi_per_chain
}

check_ebfmi <- function(post_warmup_sampler_diagnostics, threshold = 0.2) {
  efbmi_per_chain <- ebfmi(post_warmup_sampler_diagnostics)
  nan_efbmi_count <- sum(is.nan(efbmi_per_chain))
  efbmi_below_threshold <- sum(efbmi_per_chain < threshold)
  if (nan_efbmi_count > 0) {
    message(
      "Warning: ", nan_efbmi_count, " of ", length(efbmi_per_chain),
      " chains have a NaN E-BFMI.\n",
      "See https://mc-stan.org/misc/warnings for details.\n"
    )
  } else if (efbmi_below_threshold > 0) {
    message(
      "Warning: ", efbmi_below_threshold, " of ", length(efbmi_per_chain),
      " chains had an E-BFMI less than ", threshold, ".\n",
      "See https://mc-stan.org/misc/warnings for details.\n"
    )
  }
  invisible(unname(efbmi_per_chain))
}

# used in various places (e.g., fit$diagnostic_summary() and validate_sample_args())
# to validate the selected diagnostics
available_hmc_diagnostics <- function() {
  c("divergences", "treedepth", "ebfmi")
}

# in some places we need to convert user friendly names
# to the names used in the sampler diagnostics files:
#   * ebfmi --> energy__
#   * divergences --> divergent__
#   * treedepth --> treedepth__
convert_hmc_diagnostic_names <- function(diagnostics) {
  diagnostic_names <- c()
  if ("divergences" %in% diagnostics) {
    diagnostic_names <- c(diagnostic_names, "divergent__")
  }
  if ("treedepth" %in% diagnostics) {
    diagnostic_names <- c(diagnostic_names, "treedepth__")
  }
  if ("ebfmi" %in% diagnostics) {
    diagnostic_names <- c(diagnostic_names, "energy__")
  }
  if (length(diagnostic_names) == 0) {
    diagnostic_names <- ""
  }
  diagnostic_names
}

# draws formatting --------------------------------------------------------

as_draws_format_fun <- function(draws_format) {
  if (draws_format %in% c("draws_array", "array")) {
    f <- posterior::as_draws_array
  } else if (draws_format %in% c("draws_df", "df", "data.frame")) {
    f <- posterior::as_draws_df
  } else if (draws_format %in% c("draws_matrix", "matrix")) {
    f <- posterior::as_draws_matrix
  } else if (draws_format %in% c("draws_list", "list")) {
    f <- posterior::as_draws_list
  } else if (draws_format %in% c("draws_rvars", "rvars")) {
    f <- posterior::as_draws_rvars
  }
  f
}

assert_valid_draws_format <- function(format) {
  if (!is.null(format)) {
    if (!format %in% valid_draws_formats()) {
      stop(
        "The supplied draws format is not valid. ",
        call. = FALSE
      )
    }
    if (format %in% c("rvars", "draws_rvars")) {
      stop(
        "\nWe are fixing a bug in fit$draws(format = 'draws_rvars').",
        "\nFor now please use posterior::as_draws_rvars(fit$draws()) instead."
      )
    }
  }
  invisible(format)
}

valid_draws_formats <- function() {
  c("draws_array", "array", "draws_matrix", "matrix",
    "draws_list", "list", "draws_df", "df", "data.frame",
    "draws_rvars", "rvars")
}

maybe_convert_draws_format <- function(draws, format) {
  if (is.null(draws)) {
    return(draws)
  }
  format <- sub("^draws_", "", format)
  switch(
    format,
    "array" = posterior::as_draws_array(draws),
    "df" = posterior::as_draws_df(draws),
    "data.frame" = posterior::as_draws_df(draws),
    "list" = posterior::as_draws_list(draws),
    "matrix" = posterior::as_draws_matrix(draws),
    "rvars" = posterior::as_draws_rvars(draws),
    stop("Invalid draws format.", call. = FALSE)
  )
}


# convert draws for external packages ------------------------------------------

#' Convert `CmdStanMCMC` to `mcmc.list`
#'
#' This function converts a `CmdStanMCMC` object to an `mcmc.list` object
#' compatible with the \pkg{coda} package. This is primarily intended for users
#' of Stan coming from BUGS/JAGS who are used to \pkg{coda} for plotting and
#' diagnostics. In general we recommend the more recent MCMC diagnostics in
#' \pkg{posterior} and the \pkg{ggplot2}-based plotting functions in
#' \pkg{bayesplot}, but for users who prefer \pkg{coda} this function provides
#' compatibility.
#'
#' @export
#' @param x A [CmdStanMCMC] object.
#' @return An `mcmc.list` object compatible with the \pkg{coda} package.
#' @examples
#' \dontrun{
#' fit <- cmdstanr_example()
#' x <- as_mcmc.list(fit)
#' }
#'
as_mcmc.list <- function(x) {
  if (!inherits(x, "CmdStanMCMC")) {
    stop("Currently only CmdStanMCMC objects can be converted to mcmc.list.",
         call. = FALSE)
  }
  sample_array <- x$draws(format = "array")
  n_chain <- posterior::nchains(sample_array)
  n_iteration <- posterior::niterations(sample_array)
  class(sample_array) <- 'array'
  mcmc_list <- lapply(seq_len(n_chain), function(chain) {
    x <- sample_array[, chain, ]
    dimnames(x) <- list(iteration = dimnames(sample_array)$iteration,
                        variable  = dimnames(sample_array)$variable)
    attr(x, 'mcpar') <- c(1, n_iteration, 1)
    class(x) <- 'mcmc'
    x
  })
  class(mcmc_list) <- 'mcmc.list'
  return(mcmc_list)
}
