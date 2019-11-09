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
  if ((.cmdstanr$VERSION >= "2.21") && os_is_windows()) {
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

list_to_array <- function(x) {
  list_length <- length(x)
  if (list_length == 0 ) return(NULL)
  element_dim <- length(x[[1]])
  check_equal_dim <- function(x, target_dim) { !is.null(element_dim) && length(x) == target_dim }
  all_same_size <- all(sapply(x, check_equal_dim, target_dim = element_dim))
  if(!all_same_size) {
    stop("All matrices/vectors in the list must be the same size!")
  }
  all_numeric <- all(sapply(x, function(a) is.numeric(a)))
  if(!all_numeric) {
    stop("All elements of the list must be numeric!")
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
  # check filename is valid(vector of characters) and of nonzero length
  if (!is.character(file) || !nzchar(file)) {
    stop("The supplied filename is invalid!")
  }
  for (var_name in names(data)) {
    var <- data[[var_name]]
    if(!(is.numeric(var) || is.factor(var) || is.logical(var) || is.data.frame(var)  || is.list(var))) {
      stop(paste("Variable ", var_name, " is of invalid type."))
    }
    # convert TRUE/FALSE to 1/0
    if(is.logical(var)) {
      mode(var) <- "integer"
    } else if(is.data.frame(var)) {
      var <- data.matrix(var)
    }else if(is.list(var)) {
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
  cmdstanr_generated_flags_comment <- "# cmdstanr generated make/local flags (add user flags above this line)"
  make_local_path <- file.path(cmdstan_path(), "make", "local")
  user_flags <- c()
  old_make_local_cmdstanr <- c()
  if (file.exists(make_local_path)) {
    old_make_local_all <- readLines(make_local_path)
    is_user_flag <- TRUE
    for (x in old_make_local_all) {
      if (startsWith(x, cmdstanr_generated_flags_comment)) {
        is_user_flag <- FALSE
      }
      if (!is_user_flag) {
        old_make_local_cmdstanr <- c(old_make_local_cmdstanr, x)
      } else {
        user_flags <- c(user_flags, x)
      }
    }
  }
  old_make_local_cmdstanr <- paste(old_make_local_cmdstanr, collapse = "\n")
  if (opencl) {
    stan_opencl <- "STAN_OPENCL = true"
    platform_id <- paste("OPENCL_PLATFORM_ID =", opencl_platform_id)
    device_id <- paste("OPENCL_DEVICE_ID =", opencl_device_id)
    compiler_flags <- c(compiler_flags, stan_opencl, platform_id, device_id)
  }
  if (threads) {
    stan_threads <- "CXXFLAGS += -DSTAN_THREADS"
    compiler_flags <- c(compiler_flags, stan_threads)
  }
  if (length(compiler_flags) > 0) {
    compiler_flags <- c(cmdstanr_generated_flags_comment, compiler_flags)
  }
  new_make_local_cmdstanr <- paste(compiler_flags, collapse = "\n")
  if (new_make_local_cmdstanr != old_make_local_cmdstanr) {
    # only rewrite make/local if there are changes
    make_local_content <- c(user_flags, new_make_local_cmdstanr)
    writeLines(make_local_content, make_local_path)
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
