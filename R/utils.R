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
  paste0(path, ext)
}

# Strip extension from a file path
strip_ext <- function(file) {
  tools::file_path_sans_ext(file)
}

# Prepend cmdstan_path() to a relative path
add_cmdstan_path <- function(relative_path) {
  if (!nzchar(cmdstan_path())) {
    stop("Please set the path to CmdStan. See ?set_cmdstan_path.",
         call. = FALSE)
  }
  file.path(cmdstan_path(), relative_path)
}

# Strip the cmdstan_path() from a full path
strip_cmdstan_path <- function(full_path) {
  if (!nzchar(cmdstan_path())) {
    stop("Please set the path to CmdStan. See ?set_cmdstan_path.",
         call. = FALSE)
  }
  sub(cmdstan_path(), "", full_path)
}

#' Check for .stan file extension
#' @noRd
#' @param stan_file Path to Stan program.
#' @return T/F
has_stan_ext <- function(stan_file) {
  stopifnot(is.character(stan_file))
  isTRUE(tools::file_ext(stan_file) == "stan")
}



# read csv output ---------------------------------------------------------

# FIXME: also parse the csv header
read_optim_csv <- function(csv_file) {
  full_csv <- readLines(csv_file)
  mark <- grep("#   refresh", full_csv)
  col_names <- strsplit(full_csv[mark + 1], split = ",")[[1]]

  header <- full_csv[1:mark]
  x <- scan(csv_file, skip = mark + 1, sep = ",")
  list(mle = setNames(x, col_names))
}



# write data  -------------------------------------------------------------

#' Dump data to temporary file with `.data.R` extension
#' @param standata A named list of \R objects.
#' @return Path to temporary file containing the data.
#' @noRd
write_rdump <- function(standata) {
  temp_file <- tempfile(fileext = ".data.R", tmpdir = cmdstan_tempdir())
  file <- file(temp_file, open = "w")
  on.exit(close(file), add = TRUE)

  standata_names <- names(standata)
  if (is.null(standata_names) || !all(nzchar(standata_names))) {
    stop("If data is a list all elements must have names.", call. = FALSE)
  }

  for (data_name in standata_names) {
    data_object <- preprocess_data(standata[[data_name]])

    if (!is.numeric(data_object))  {
      warning("Problem writing variable '", data_name,
              "' to data file for CmdStan.", call. = FALSE)
    }

    width_pattern <- paste0("(.{1,", getOption("width"), "})(\\s|$)")
    if (is.vector(data_object)) {
      write_vector(data_object, data_name, file, width_pattern)
    } else if (is.matrix(data_object) || is.array(data_object)) {
      write_array(data_object, data_name, file, width_pattern)
    } else {
      warning("Problem writing variable '",
              data_name,
              "' to data file for CmdStan.",
              call. = FALSE)
    }
  }

  # return path to new temporary file
  temp_file
}


#' Do necessary type conversions before data can be dumped
#' @noRd
#' @param x A single object from the `standata` list.
#' @return Either `x` or `x` converted to a different type.
preprocess_data <- function(x) {
  if (is.data.frame(x)) {
    x <- data.matrix(x)
  } else if (is.list(x)) {
    x <- list_to_array(x)
  } else if (is.logical(x)) {
    mode(x) <- "integer"
  } else if (is.factor(x)) {
    x <- as.integer(x)
  }

  if (!is.integer(x) &&
      max(abs(x)) < .Machine$integer.max &&
      real_is_int(x)) {
    storage.mode(x) <- "integer"
  }
  x
}

# use if is.vector(x) is TRUE
write_vector <- function(x, x_name, file, width_pattern) {
  if (length(x) == 0) {
    cat(x_name, " <- integer(0)\n", file = file, sep = '')
  } else if (length(x) == 1) {
    cat(x_name, " <- ", as.character(x), "\n", file = file, sep = '')
  } else {
    str <- paste0(x_name, " <- \nc(", paste(x, collapse = ', '), ")")
    str <-  gsub(width_pattern, '\\1\n', str)
    cat(str, file = file)
  }
}

# use if is.matrix(x) or is.array(x) is TRUE
write_array <- function(x, x_name, file, width_pattern) {
  xdim <- dim(x)
  if (length(x) == 0) {
    str <- paste0("structure(integer(0), ")
  } else {
    str <- paste0("structure(c(", paste(as.vector(x), collapse = ', '), "),")
  }
  str <- gsub(width_pattern, '\\1\n', str)
  cat(x_name, " <- \n",
      file = file, sep = '')
  cat(str, ".Dim = c(", paste(xdim, collapse = ', '), "))\n",
      file = file, sep = '')
}

# based on rstan::data_list2array
list_to_array <- function(x, x_name) {
  if (!length(x))  {
    return(NULL)
  }
  if (!all(sapply(x, is.numeric))) {
    stop("All elements in list '", x_name, "' must be numeric.",
         call. = FALSE)
  }

  x_dims <- lapply(x, function(a) dim(a) %||% length(a))
  x_ndims <- sapply(x_dims, length)
  if (!all(x_ndims == x_ndims[1]) ||
      !all(sapply(x_dims, function(d) all(d == x_dims[[1]])))
  ) {
    stop("All elements in list '", x_name, "' must have the same dimensions.",
         call. = FALSE)
  }
  x_len <- length(x)
  x <- do.call(c, x)
  dim(x) <- c(x_dims[[1]], x_len)
  aperm(x, c(x_ndims[1] + 1L, seq_len(x_ndims[1])))
}

real_is_int <- function(x) {
  if (length(x) < 1L) return(TRUE)
  if (any(is.infinite(x)) || any(is.nan(x))) return(FALSE)
  all(floor(x) == x)
}
