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
  path <- path.expand(path)
  path <- gsub("\\\\", "/", path)
  path <- gsub("//", "/", path)
  if (substr(path, nchar(path), nchar(path)) == "/") {
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
absolute_path <- function(path) {
  if (!file.exists(path)) {
    return(path)
  }
  new_path <- repair_path(path)
  if (grepl("^~", path) || grepl("^(/+|[A-Za-z]:)", new_path)) {
    return(new_path)
  }
  repair_path(file.path(getwd(), new_path))
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
           ext = ".csv") {
    checkmate::assert_directory_exists(new_dir, access = "w")

    new_names <- new_basename
    if (!is.null(ids)) {
      new_names <- paste0(new_basename, "-", ids)
    }
    if (timestamp) {
      stamp <- format(Sys.time(), "%Y%m%d-%H%M")
      new_names <- paste0(new_names, "_", stamp)
    }
    new_names <- paste0(new_names, ext)
    destinations <- file.path(new_dir, new_names)

    copied <- file.copy(
      from = current_paths,
      to = destinations,
      overwrite = TRUE
    )
    if (!all(copied)) {
      destinations[!copied] <- NA_character_
    }
    destinations
  }


# FIXME: also parse the csv header
read_optim_csv <- function(csv_file) {
  csv_no_comments <- utils::read.csv(
    csv_file,
    comment.char = "#",
    colClasses = "numeric"
  )
  mat <- as.matrix(csv_no_comments)
  colnames(mat) <- repair_variable_names(colnames(mat))
  list(
    mle = mat[1, colnames(mat) != "lp__"],
    lp = mat[1, colnames(mat) == "lp__"]
  )
}

# FIXME: also parse the csv header
read_vb_csv <- function(csv_file) {
  csv_no_comments <- utils::read.csv(
    csv_file,
    comment.char = "#",
    colClasses = "numeric"
  )
  # drop first row since according to CmdStan manual it's just the mean
  mat <- as.matrix(csv_no_comments)[-1,, drop=FALSE]
  colnames(mat) <- repair_variable_names(colnames(mat))
  drop_cols <- c("lp__", "log_p__", "log_g__")
  keep_cols <- setdiff(colnames(mat), drop_cols)
  list(
    log_p = mat[, "log_p__"],
    log_g = mat[, "log_g__"],
    draws = mat[, keep_cols, drop=FALSE]
  )
}

# convert names like beta.1.1 to beta[1,1]
repair_variable_names <- function(names) {
  names <- sub("\\.", "[", names)
  names <- gsub("\\.", ",", names)
  names[grep("\\[", names)] <-
    paste0(names[grep("\\[", names)], "]")
  names
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
    stop("Please install the 'rstan' package. This is temporarily required ",
         "for writing the data file for CmdStan.", call. = FALSE)
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



# compilation, build files, threading -------------------------------------

#' Cleanup build files of a Stan model
#'
#' deletes the model_name.o, model_name.hpp and the executable.
#'
#' @param model_path (string) The absolute path to the model
#' @param remove_main (logical) Set TRUE to also remove the cmdstan main.o
#' @noRd
build_cleanup <- function(model_path,
                          remove_main = FALSE) {
  model_hpp_file <- paste(model_path, ".hpp", sep = "")
  model_o_file <- paste(model_path, ".o", sep = "")
  if(file.exists(model_hpp_file)) {
    file.remove(model_hpp_file)
  }
  if(file.exists(model_o_file)) {
    file.remove(model_o_file)
  }
  if(file.exists(model_path)) {
    file.remove(model_path)
  }
  if(remove_main) {
    main_o_file <- file.path(cmdstan_path(), "src", "cmdstan", "main.o")
    if(file.exists(main_o_file)) {
      file.remove(main_o_file)
    }
  }
}

set_make_local <- function(threads = FALSE,
                           opencl = FALSE,
                           opencl_platform_id = 0,
                           opencl_device_id = 0,
                           compiler_flags = NULL) {
  make_local_path <- file.path(cmdstan_path(), "make", "local")
  old_make_local_content <- ""
  if(file.exists(make_local_path)) {
    # read the contents of make/local to compare with the new contents
    old_make_local_content <- paste(readLines(make_local_path), collapse = "\n")
  }
  if(opencl) {
    stan_opencl <- "STAN_OPENCL = true"
    platform_id <- paste("OPENCL_PLATFORM_ID", opencl_platform_id, sep = " = ")
    device_id <- paste("OPENCL_DEVICE_ID", opencl_device_id, sep = " = ")
    compiler_flags <- c(compiler_flags, stan_opencl, platform_id, device_id)
  }
  if(threads) {
    stan_threads <- "CXXFLAGS += -DSTAN_THREADS"
    compiler_flags <- c(compiler_flags, stan_threads)
  }
  new_make_local_content <- paste(compiler_flags, collapse = "\n")
  # dont rewrite make/local if there are no changes
  if(new_make_local_content != old_make_local_content) {
    write(new_make_local_content, file = make_local_path, append = FALSE)
    return(TRUE)
  }
  return(FALSE)
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
