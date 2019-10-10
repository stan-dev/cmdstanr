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


# paths and extensions ----------------------------------------------------

# Replace `\\` with `/` in a path
# Needed for windows if CmdStan version is < 2.21:
# https://github.com/stan-dev/cmdstanr/issues/1#issuecomment-539118598
repair_path <- function(path) {
  if (!length(path) || !is.character(path)) {
    return(path)
  }
  gsub("\\\\", "/", path)
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
# unlike tools::file_path_as_absolute() it doesn't error if can't be found
absolute_path <- function(x) {
  if (!file.exists(x)) {
    return(x)
  }
  tools::file_path_as_absolute(x)
}

# Change extension from a file path
change_ext <- function(file, ext) {
  out <- strip_ext(file)
  paste0(out, ext)
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
#' @param ext Extension to use for all saved files.
#' @return The output from `base::file.copy()`, which is a logical vector
#'   indicating if the operation succeeded for each of the files.
copy_temp_files <-
  function(current_paths,
           new_dir,
           new_basename,
           ids = NULL,
           ext = ".csv",
           overwrite = TRUE) {
    checkmate::assert_directory_exists(new_dir, access = "w")

    new_names <- new_basename
    if (!is.null(ids)) {
      new_names <- paste0(new_basename, "-", ids)
    }
    new_names <- paste0(new_names, ext)
    destinations <- file.path(new_dir, new_names)
    file.copy(from = current_paths,
              to = destinations,
              overwrite = overwrite)
  }


#' Create default output csv file basename
#' @noRd
#' @param model_name,method Strings giving the model name (e.g., `"my_model"`)
#'   and CmdStan method (e.g. `"sample"`).
#' @return String default file basename (e.g., `"my_model-stan-sample"`).
output_csv_basename <- function(model_name, method) {
  paste0(model_name, "-stan-", method)
}

# FIXME: also parse the csv header
read_optim_csv <- function(csv_file) {
  full_csv <- readLines(csv_file)
  mark <- grep("#   refresh", full_csv)
  col_names <- strsplit(full_csv[mark + 1], split = ",")[[1]]

  header <- full_csv[1:mark]
  x <- scan(csv_file, skip = mark + 1, sep = ",", quiet = TRUE)
  list(mle = stats::setNames(x, col_names))
}


#' Dump data to temporary file in format readable by CmdStan
#'
#' Currently calls `rstan::stan_rdump()` to create the `.data.R` file.
#' FIXME:
#'
#' @param data A named list of \R objects.
#' @return Path to temporary file containing the data.
#' @noRd
write_rdump <- function(data) {
  # FIXME don't use rstan
  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("Please install the 'rstan' package.\n",
         "This is required for writing the data file for CmdStan ",
         "until CmdStanR has its own implementation.",
          call. = FALSE)
  }
  checkmate::assert_names(names(data), type = "unique")
  temp_file <- tempfile(pattern = "standata-", fileext = ".data.R")
  rstan::stan_rdump(
    list = names(data),
    file = temp_file,
    envir = as.environment(data)
  )
  temp_file
}
