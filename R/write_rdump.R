#' Dump data to temporary file with `.data.R` extension
#'
#' @noRd
#' @param standata A named list of \R objects.
#' @param width The width for maximum characters on a line. The output is broken
#'   into lines with width equal to `width`.
#' @return Path to temporary file containing the data.
#'
write_rdump <- function(standata, width = options("width")$width) {
  temp_data_file <- tempfile(fileext = ".data.R")
  file <- file(temp_data_file, open = "w")
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

    width_pattern <- paste0("(.{1,", width, "})(\\s|$)")
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
  temp_data_file
}


# internals ---------------------------------------------------------------

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
