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
  # identify exact matches
  matched <- as.list(match(variable_filters, variables))
  # loop over filters not exactly matched
  for (id in which(is.na(matched))) {
    # assign all variable names that match the filter as an array
    matched[[id]] <-
      which(startsWith(variables, paste0(variable_filters[id], "[")))
  }
  # collect all selected variables
  selected_variables <- variables[unlist(matched)]
  # collect all filters not found
  not_found <- variable_filters[vapply(matched, length, 0L) == 0]
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

is_rtools43_toolchain <- function() {
  os_is_windows() && R.version$major == "4" && R.version$minor >= "3.0"
}

is_rtools42_toolchain <- function() {
  os_is_windows() && R.version$major == "4" && R.version$minor >= "2.0" && R.version$minor < "3.0"
}

is_rtools40_toolchain <- function() {
  os_is_windows() && R.version$major == "4" && R.version$minor < "2.0"
}

is_ucrt_toolchain <- function() {
  os_is_windows() && R.version$major == "4" && R.version$minor >= "2.0"
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
  # WSL cmdstan path is a network path and needs the leading //
  path <- gsub("//(?!wsl)", "/", path, perl = TRUE)
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
    assert_dir_exists(new_dir, access = "w")
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
#' @keywords internal
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

# WSL-related helper functions ------------------------------------------

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
  } else if (grepl("^//wsl", path)) {
    path <- gsub(wsl_dir_prefix(), "", path, fixed = TRUE)
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
    if (!is.null(run_args$wd)) {
      wd <- wsl_safe_path(run_args$wd)
      run_args$wd <- NULL
      run_args$args <- c(c("cd", wd, "&&"), command, run_args$args)
    } else {
      run_args$args <- c(command, run_args$args)
    }
  }
  do.call(processx::run, run_args)
}

wsl_compatible_process_new <- function(...) {
  run_args <- list(...)
  if (os_is_wsl()) {
    command <- run_args$command
    run_args$command <- "wsl"
    if (!is.null(run_args$wd)) {
      wd <- wsl_safe_path(run_args$wd)
      run_args$wd <- NULL
      run_args$args <- c(c("cd", wd, "&&"), command, run_args$args)
    } else {
      run_args$args <- c(command, run_args$args)
    }
  }
  do.call(processx::process$new, run_args)
}

wsl_installed <- function() {
  tryCatch({
    # Call can hang indefinitely on Github actions, so explicitly kill
    p <- processx::process$new("wsl", "uname")
    Sys.sleep(1)
    if (p$is_alive()) {
      p$kill()
      FALSE
    } else {
      status <- p$get_exit_status()
      if (is.null(status)) {
        FALSE
      }
      isTRUE(status == 0)
    }
  }, error = function(e) { FALSE }, finally = function(ret) { ret })
}

wsl_distro_name <- function() {
  name <- processx::run(
    command = "wsl",
    args = c("echo", "$WSL_DISTRO_NAME")
  )$stdout
  gsub("\n", "", name, fixed = TRUE)
}

wsl_home_dir <- function() {
  dir <- processx::run(
        command = "wsl",
        args = c("echo", "$HOME")
  )$stdout
  gsub("\n", "", dir, fixed = TRUE)
}

wsl_dir_prefix <- function(wsl = FALSE) {
  if (os_is_wsl() || wsl) {
    paste0("//wsl$/", wsl_distro_name())
  } else {
    ""
  }
}

wsl_tempdir <- function() {
  dir <- processx::run(command = "wsl",
                        args = c("mktemp", "-d"))$stdout
  gsub("\n", "", dir, fixed = TRUE)
}

# The checkmate file and directory assertion functions don't register the WSL
# network path as legitimate, and will always error. To avoid this we create a
# new checking functions with WSL handling, and then pass these to
# checkmate::makeAssertionFunction to replicate the existing assertion functionality
check_dir_exists <- function(dir, access = NULL) {
  if (os_is_wsl()) {
    if (!checkmate::qtest(dir, "S+")) {
      return("No directory provided.")
    }
    checks <- sapply(dir, .wsl_check_exists, is_dir = TRUE, access = access)
    if (any(as.character(checks) != "TRUE")) {
      grep("TRUE", checks, value = TRUE, invert = TRUE)[1]
    } else {
      TRUE
    }
  } else {
    checkmate::checkDirectoryExists(dir, access = access)
  }
}

check_file_exists <- function(files, access = NULL, ...) {
  if (os_is_wsl()) {
    if (!checkmate::qtest(files, "S+")) {
      return("No file provided.")
    }
    checks <- sapply(files, .wsl_check_exists, is_dir = FALSE, access = access)
    if (any(as.character(checks) != "TRUE")) {
      grep("TRUE", checks, value = TRUE, invert = TRUE)[1]
    } else {
      TRUE
    }
  } else {
    checkmate::checkFileExists(files, access = access, ...)
  }
}

.wsl_check_exists <- function(path, is_dir = TRUE, access = NULL) {
  path_check <- processx::run(
    command = "wsl",
    args = c("ls", "-la", wsl_safe_path(path)),
    error_on_status = FALSE
  )

  if (path_check$status != 0) {
    path <- gsub("^./", "", path)
    err <- ifelse(is_dir,
                  paste0("Directory '", path, "' does not exist"),
                  paste0("File does not exist: '", path, "'"))
    return(err)
  }

  path_metadata <- strsplit(path_check$stdout, split = "\n",
                            fixed = TRUE)[[1]]

  wsl_user <- processx::run(
    command = "wsl",
    args = c("echo", "$USER"),
    error_on_status = FALSE
  )$stdout
  wsl_user <- gsub("\n", "", wsl_user, fixed = TRUE)

  path_metadata <- grep(wsl_user, path_metadata, value = TRUE)

  if (!is.null(access)) {
    path_permissions <- strsplit(path_metadata, " ", fixed = TRUE)[[1]][1]
    if (!any(grepl(access, path_permissions))) {
      name <- ifelse(is_dir, "directory", "file")
      return(paste0("Specified ", name, ": ", path,
                    " does not have access permission ", access))
    }
  }
  TRUE
}

assert_dir_exists <- checkmate::makeAssertionFunction(check_dir_exists)
assert_file_exists <- checkmate::makeAssertionFunction(check_file_exists)

# Model methods & expose_functions helpers ------------------------------------------------------
get_cmdstan_flags <- function(flag_name) {
  cmdstan_path <- cmdstanr::cmdstan_path()
  flags <- wsl_compatible_run(
    command = "make",
    args = c(paste0("print-", flag_name)),
    wd = cmdstan_path
  )$stdout

  flags <- gsub("\n", "", flags, fixed = TRUE)

  flags <- gsub(
    pattern = paste0(flag_name, "\\s(=|\\+=)(\\s|$)"),
    replacement = "", x = flags
  )

  if (flags == "") {
    return(flags)
  }

  if (flag_name == "STANCFLAGS") {
    # StanC flags need to be returned as a character vector
    flags_vec <- strsplit(x = flags, split = " ", fixed = TRUE)[[1]]
    return(flags_vec)
  }

  if (flag_name %in% c("LDLIBS", "LDFLAGS_TBB")) {
    # shQuote -L paths and rpaths
    # The LDLIBS flags change paths to /c/ instead of C:/, need to revert to
    # format consistent with path on windows
    if (.Platform$OS.type == "windows") {
      flags <- gsub("(-L|-rpath),/([a-zA-Z])/", "\\1,\\2:/", flags, perl = TRUE)
    }
    flags <- gsub(cmdstan_path, "", flags, ignore.case = TRUE)
    flags <- gsub("(-L,|-rpath,)/stan/lib/stan_math/lib/tbb",
                  paste0("\\1", shQuote(paste0(cmdstan_path, "/stan/lib/stan_math/lib/tbb"))),
                  flags)
    return(flags)
  }

  # shQuote include paths
  flags <- gsub("-I ", "-I", flags, fixed = TRUE)
  flags <- strsplit(flags, " ", fixed = TRUE)[[1]]
  include_flags <- grep("^-I", flags)
  flags <- gsub("^-I", paste0(cmdstan_path, "/"), flags)
  flags[include_flags] <- paste0("-I", shQuote(flags[include_flags]))
  flags <- paste(flags, collapse = " ")

  # shQuote Remaining " stan/" paths
  flags <- strsplit(flags, split = " ", fixed = TRUE)[[1]]
  oth_stan_flags <- grep("^stan/", flags)
  flags[oth_stan_flags] <- shQuote(paste0(cmdstan_path, "/", flags[oth_stan_flags]))
  paste(flags, collapse = " ")
}

rcpp_source_stan <- function(code, env, verbose = FALSE) {
  cxxflags <- get_cmdstan_flags("CXXFLAGS")
  libs <- c("LDLIBS", "LIBSUNDIALS", "TBB_TARGETS", "LDFLAGS_TBB")
  libs <- paste(sapply(libs, get_cmdstan_flags), collapse = " ")
  if (.Platform$OS.type == "windows") {
    libs <- paste(libs, "-fopenmp")
  }
  lib_paths <- c("/stan/lib/stan_math/lib/tbb/",
                 "/stan/lib/stan_math/lib/sundials_6.1.1/lib/")
  withr::with_path(paste0(cmdstan_path(), lib_paths),
    withr::with_makevars(
      c(
        USE_CXX14 = 1,
        PKG_CPPFLAGS = ifelse(cmdstan_version() <= "2.30.1", "-DCMDSTAN_JSON", ""),
        PKG_CXXFLAGS = cxxflags,
        PKG_LIBS = libs
      ),
      Rcpp::sourceCpp(code = code, env = env, verbose = verbose)
    )
  )
  invisible(NULL)
}

expose_model_methods <- function(env, verbose = FALSE, hessian = FALSE) {
  code <- c(env$hpp_code_,
            readLines(system.file("include", "model_methods.cpp",
                                  package = "cmdstanr", mustWork = TRUE)))

  if (hessian) {
    code <- c("#include <stan/math/mix.hpp>",
            code,
            readLines(system.file("include", "hessian.cpp",
                                  package = "cmdstanr", mustWork = TRUE)))
  }

  code <- paste(code, collapse = "\n")
  rcpp_source_stan(code, env, verbose)
  invisible(NULL)
}

initialize_model_pointer <- function(env, datafile_path, seed = 0) {
  ptr_and_rng <- env$model_ptr(ifelse(is.null(datafile_path), "", datafile_path), seed)
  env$model_ptr_ <- ptr_and_rng$model_ptr
  env$model_rng_ <- ptr_and_rng$base_rng
  env$num_upars_ <- env$get_num_upars(env$model_ptr_)
  env$param_metadata_ <- env$get_param_metadata(env$model_ptr_)
  invisible(NULL)
}

create_skeleton <- function(param_metadata, model_variables,
                            transformed_parameters, generated_quantities) {
  target_params <- names(model_variables$parameters)
  if (transformed_parameters) {
    target_params <- c(target_params,
                       names(model_variables$transformed_parameters))
  }
  if (generated_quantities) {
    target_params <- c(target_params,
                       names(model_variables$generated_quantities))
  }
  lapply(param_metadata[target_params], function(par_dims) {
    if ((length(par_dims) == 0)) {
      array(0, dim = 1)
    } else {
      array(0, dim = par_dims)
    }
  })
}

get_standalone_hpp <- function(stan_file, stancflags) {
  status <- withr::with_path(
      c(
        toolchain_PATH_env_var(),
        tbb_path()
      ),
      wsl_compatible_run(
        command = stanc_cmd(),
        args = c(stan_file,
                stancflags),
        wd = cmdstan_path(),
        error_on_status = FALSE
      )
    )
  if (status$status == 0) {
    name <- strip_ext(basename(stan_file))
    path <- dirname(stan_file)
    hpp_path <- file.path(path, paste0(name, ".hpp"))
    hpp <- suppressWarnings(readLines(hpp_path, warn = FALSE))
    unlink(hpp_path)
    hpp
  } else {
    invisible(NULL)
  }
}

get_function_name <- function(fun_start, fun_end, model_lines) {
  fun_string <- paste(model_lines[(fun_start+1):fun_end], collapse = " ")
  fun_name <- gsub("auto ", "", fun_string, fixed = TRUE)
  sub("\\(.*", "", fun_name, perl = TRUE)
}

# Construct the plain return type for a standalone function by
# looking up the return type of the functor declaration and replacing
# the template types (i.e., T0__) with double
get_plain_rtn <- function(fun_start, fun_end, model_lines) {
  fun_name <- get_function_name(fun_start, fun_end, model_lines)

  # Depending on the version of stanc3, the standalone functions
  # with a plain return type can either be wrapped in a struct as a functor,
  # or as a separate forward declaration
  struct_name <- paste0("struct ", fun_name, "_functor")

  if (any(grepl(struct_name, model_lines))) {
    struct_start <- grep(struct_name, model_lines)
    struct_op_start <- grep("operator()", model_lines[-(1:struct_start)])[1] + struct_start
    rtn_type <- paste0(model_lines[struct_start:struct_op_start], collapse = " ")
    rm_operator <- gsub("operator().*", "", rtn_type)
    rm_prev <- gsub(".*\\{", "", rm_operator)
  } else {
    # Find first declaration of function (will be the forward declaration)
    first_decl <- grep(paste0(fun_name,"\\("), model_lines)[1]

    # The return type will be between the function name and the semicolon terminating
    # the previous line
    last_scolon <- grep(";", model_lines[1:first_decl])
    last_scolon <- ifelse(last_scolon[length(last_scolon)] == first_decl,
                          last_scolon[length(last_scolon) - 1],
                          last_scolon[length(last_scolon)])
    rtn_type_full <- paste0(model_lines[last_scolon:first_decl], collapse = " ")
    rm_fun_name <- gsub(paste0(fun_name, ".*"), "", rtn_type_full)
    rm_prev <- gsub(".*;", "", rm_fun_name)
  }
  rm_template <- gsub("template <typename(.*?)> ", "", rm_prev)
  gsub("T([0-9])*__", "double", rm_template)
}


# Prepare the c++ code for a standalone function so that it can be exported to R:
# - Replace the auto return type with the plain type
# - Add Rcpp::export attribute
# - Remove the pstream__ argument and pass Rcpp::Rcout by default
# - Replace the boost::ecuyer1988& base_rng__ argument with an integer seed argument
#     that instantiates an RNG
prep_fun_cpp <- function(fun_start, fun_end, model_lines) {
  fun_body <- paste(model_lines[fun_start:fun_end], collapse = " ")
  fun_body <- gsub("auto", get_plain_rtn(fun_start, fun_end, model_lines), fun_body)
  fun_body <- gsub("// [[stan::function]]", "// [[Rcpp::export]]\n", fun_body, fixed = TRUE)
  fun_body <- gsub("std::ostream\\*\\s*pstream__\\s*=\\s*nullptr", "", fun_body)
  fun_body <- gsub("boost::ecuyer1988&\\s*base_rng__", "SEXP base_rng_ptr", fun_body)
  fun_body <- gsub("base_rng__,", "*(Rcpp::XPtr<boost::ecuyer1988>(base_rng_ptr).get()),", fun_body, fixed = TRUE)
  fun_body <- gsub("pstream__", "&Rcpp::Rcout", fun_body, fixed = TRUE)
  fun_body <- paste(fun_body, collapse = "\n")
  gsub(pattern = ",\\s*)", replacement = ")", fun_body)
}

compile_functions <- function(env, verbose = FALSE, global = FALSE) {
  funs <- grep("// [[stan::function]]", env$hpp_code, fixed = TRUE)
  funs <- c(funs, length(env$hpp_code))

  stan_funs <- sapply(seq_len(length(funs) - 1), function(ind) {
    fun_end <- funs[ind + 1]
    fun_end <- ifelse(env$hpp_code[fun_end] == "}", fun_end, fun_end - 1)
    prep_fun_cpp(funs[ind], fun_end, env$hpp_code)
  })

  env$fun_names <- sapply(seq_len(length(funs) - 1), function(ind) {
    get_function_name(funs[ind], funs[ind + 1], env$hpp_code)
  })

  dups <- env$fun_names[duplicated(env$fun_names)]

  if (length(dups) > 0) {
    stop("Overloaded functions are currently not able to be exposed to R!",
          " The following overloaded functions were found: ",
          paste(dups, collapse=", "),
          call. = FALSE)
  }

  mod_stan_funs <- paste(c(
    env$hpp_code[1:(funs[1] - 1)],
    "#include <RcppEigen.h>",
    "// [[Rcpp::depends(RcppEigen)]]",
    stan_funs),
  collapse = "\n")
  if (global) {
    rcpp_source_stan(mod_stan_funs, globalenv(), verbose)
  } else {
    rcpp_source_stan(mod_stan_funs, env, verbose)
  }

  # If an RNG function is exposed, initialise a Boost RNG object stored in the
  # environment
  rng_funs <- grep("rng\\b", env$fun_names, value = TRUE)
  if (length(rng_funs) > 0) {
    rng_cpp <- system.file("include", "base_rng.cpp", package = "cmdstanr", mustWork = TRUE)
    rcpp_source_stan(paste0(readLines(rng_cpp), collapse="\n"), env, verbose)
    env$rng_ptr <- env$base_rng(seed=0)
  }

  # For all RNG functions, pass the initialised Boost RNG by default
  for (fun in rng_funs) {
    if (global) {
      fun_env <- globalenv()
    } else {
      fun_env <- env
    }
    fundef <- get(fun, envir = fun_env)
    funargs <- formals(fundef)
    funargs$base_rng_ptr <- env$rng_ptr
    formals(fundef) <- funargs
    assign(fun, fundef, envir = fun_env)
  }

  env$compiled <- TRUE
  invisible(NULL)
}

expose_stan_functions <- function(function_env, global = FALSE, verbose = FALSE) {
  if (os_is_wsl()) {
    stop("Standalone functions are not currently available with ",
          "WSL CmdStan and will not be compiled",
          call. = FALSE)
  }
  if (function_env$existing_exe) {
    stop("Exporting standalone functions is not possible with a pre-compiled Stan model!",
          call. = FALSE)
  }
  if (function_env$external && cmdstan_version() < "2.32") {
    stop("Exporting standalone functions with external C++ is not available before CmdStan 2.32",
         call. = FALSE)
  }
  require_suggested_package("Rcpp")
  require_suggested_package("RcppEigen")
  if (function_env$compiled) {
    if (!global) {
      message("Functions already compiled, nothing to do!")
    } else {
      message("Functions already compiled, copying to global environment")
      # Create reference to global environment, avoids NOTE about assigning to global
      pos <- 1
      envir <- as.environment(pos)
      lapply(function_env$fun_names, function(fun_name) {
        assign(fun_name, get(fun_name, function_env), envir)
      })
    }
  } else {
    message("Compiling standalone functions...")
    compile_functions(function_env, verbose, global)
  }
  invisible(NULL)
}
