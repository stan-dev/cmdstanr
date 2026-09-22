# Internal functions for handling cpp options

#' Normalize the flags sent to make
#'
#' The last value for a name wins. Goes through the emitted flags rather than
#' the list because a vector value expands into one assignment per element.
#'
#' @param cpp_options The user's `cpp_options`, as
#'   `assert_valid_cpp_options()` returned them.
#' @return A named list with one value per Make variable.
#' @noRd
parsed_cpp_options <- function(cpp_options) {
  assignments <- structure(list(), names = character())
  for (flag in cpp_options_to_compile_flags(cpp_options)) {
    option_name <- sub("=.*$", "", flag)
    assignments[[option_name]] <- sub("^[^=]*=", "", flag)
  }
  assignments
}

# convert to compile flags --------------------
# from list(FLAG1 = TRUE, FLAG2 = FALSE) to c("FLAG1=TRUE", "FLAG2=")
cpp_options_to_compile_flags <- function(cpp_options) {
  if (length(cpp_options) == 0) {
    return(NULL)
  }
  cpp_built_options <- c()
  for (i in seq_along(cpp_options)) {
    value <- cpp_options[[i]]
    # FALSE asks for the option off, which make spells as an empty assignment.
    if (is.logical(value)) {
      value <- as.character(value)
      value[value %in% "FALSE"] <- ""
    }
    cpp_built_options <- c(
      cpp_built_options,
      paste0(names(cpp_options)[i], "=", value)
    )
  }
  cpp_built_options
}


# check options overall for validity ---------------------------------
make_variable_name_pattern <- "[A-Za-z_][A-Za-z0-9_]*"

#' Check the `cpp_options` the user supplied and return them
#'
#' Every entry must be named and every name must be a Make variable name. The
#' names are uppercased here so that one spelling reaches everything downstream.
#' The user header and the stanc flags have their own arguments, so setting them
#' here is an error. A TBB directory has to be a literal path, since the build
#' record keeps it as written and never asks make what it expands to.
#'
#' @param cpp_options What the user passed, or `NULL`.
#' @return The options with their names uppercased, or an empty list for
#'   `NULL`.
#' @noRd
assert_valid_cpp_options <- function(cpp_options) {
  if (is.null(cpp_options)) {
    return(list())
  }
  checkmate::assert_list(cpp_options, .var.name = "cpp_options")
  option_names <- names(cpp_options)
  for (i in seq_along(cpp_options)) {
    if (is.null(option_names) || !nzchar(option_names[[i]])) {
      stop(unnamed_cpp_option_message(cpp_options[[i]]), call. = FALSE)
    }
    if (!grepl(paste0("^", make_variable_name_pattern, "$"), option_names[[i]])) {
      stop(
        "`cpp_options` names must be Make variable names, made of letters, ",
        "digits and underscores and not starting with a digit. `",
        option_names[[i]], "` is not one.",
        call. = FALSE
      )
    }
  }
  if (!is.null(option_names)) {
    names(cpp_options) <- toupper(option_names)
  }
  header_at <- which(names(cpp_options) == "USER_HEADER")
  if (length(header_at) > 0) {
    stop(
      user_header_cpp_option_message(cpp_options[[header_at[[1]]]]),
      call. = FALSE
    )
  }
  if ("STANCFLAGS" %in% names(cpp_options)) {
    stop(stancflags_cpp_option_message(), call. = FALSE)
  }
  for (tbb_at in which(names(cpp_options) %in% c("TBB_LIB", "TBB_BIN"))) {
    flags <- cpp_options_to_compile_flags(cpp_options[tbb_at])
    bad <- grep("$", flags, fixed = TRUE, value = TRUE)
    if (length(bad) > 0) {
      stop(
        "`", names(cpp_options)[[tbb_at]], "` must be a literal directory. ",
        "cmdstanr records it to launch the model with the right TBB and ",
        "doesn't expand make expressions like `", sub("^[^=]*=", "", bad[[1]]),
        "`.",
        call. = FALSE
      )
    }
  }
  cpp_options
}

#' Explain why an unnamed `cpp_options` entry cannot be used
#'
#' Users reach for makefile syntax here, so name the route that accepts the
#' entry they wrote rather than repeating the rule.
#'
#' @param value The unnamed entry.
#' @return The message.
#' @noRd
unnamed_cpp_option_message <- function(value) {
  entry <- if (checkmate::test_string(value)) trimws(value) else ""
  assignment <- paste0("^(", make_variable_name_pattern, ")[ \t]*=(.*)$")
  operator <- paste0(
    "^(", make_variable_name_pattern, ")[ \t]*(\\+=|\\?=|::=|:=|!=).*$"
  )
  is_assignment <- grepl(assignment, entry)
  is_operator <- !is_assignment && grepl(operator, entry)
  option_name <- ""
  if (is_assignment) {
    option_name <- toupper(sub(assignment, "\\1", entry))
  } else if (is_operator) {
    option_name <- toupper(sub(operator, "\\1", entry))
  }
  if (option_name == "USER_HEADER") {
    header <- if (is_assignment) trimws(sub(assignment, "\\2", entry)) else NULL
    return(user_header_cpp_option_message(header))
  }
  if (option_name == "STANCFLAGS") {
    return(stancflags_cpp_option_message())
  }
  if (is_assignment) {
    value <- trimws(sub(assignment, "\\2", entry))
    return(sprintf(
      paste0(
        "`cpp_options` entries must be named. ",
        "Write `list(%s = %s)` instead of `%s`."
      ),
      option_name, encodeString(value, quote = '"'), encodeString(entry, quote = '"')
    ))
  }
  if (is_operator) {
    return(sprintf(
      paste0(
        "`%s` is makefile syntax and cannot be passed through `cpp_options`. ",
        "To set it in `make/local` use `cmdstan_make_local(cpp_options = list(%s))`."
      ),
      encodeString(entry, quote = '"'), encodeString(entry, quote = '"')
    ))
  }
  if (grepl("^(-B|--always-make)$", entry)) {
    return(sprintf(
      paste0(
        "Make flags cannot be passed through `cpp_options`. ",
        "`%s` rebuilds everything; pass `force_recompile = TRUE` instead."
      ),
      entry
    ))
  }
  makefile_flag_pattern <- "^(?:-f[ \t]*|--(?:file|makefile)=)(.+)$"
  if (grepl(makefile_flag_pattern, entry)) {
    path <- sub(makefile_flag_pattern, "\\1", entry)
    return(sprintf(
      paste0(
        "Make flags cannot be passed through `cpp_options`. ",
        "To read another makefile add `include %s` to `make/local`, for example ",
        "`cmdstan_make_local(cpp_options = list(%s))`."
      ),
      path, encodeString(paste0("include ", path), quote = '"')
    ))
  }
  if (startsWith(entry, "-")) {
    return(paste0(
      "Make flags cannot be passed through `cpp_options`. ",
      "Set them in `make/local` with `cmdstan_make_local()`, ",
      "for example `MAKEFLAGS += -j4`."
    ))
  }
  "`cpp_options` entries must be named: `list(NAME = value)`."
}

#' The error for a user header supplied through `cpp_options`
#'
#' @param value What was given for `USER_HEADER`, or `NULL`.
#' @return The message.
#' @noRd
user_header_cpp_option_message <- function(value) {
  is_empty_string <- checkmate::test_string(value) && !nzchar(trimws(value))
  if (is.null(value) || isFALSE(value) || is_empty_string) {
    example <- ": `user_header = NULL`"
  } else if (checkmate::test_string(value)) {
    example <- paste0(": `user_header = ", encodeString(value, quote = '"'), "`")
  } else {
    example <- ""
  }
  paste0(
    "The user header cannot be set through `cpp_options`. ",
    "Pass it with the `user_header` argument", example, "."
  )
}

# The error for STANCFLAGS supplied through cpp_options.
stancflags_cpp_option_message <- function() {
  paste0(
    "`STANCFLAGS` cannot be set through `cpp_options`. ",
    "Pass stanc flags with the `stanc_options` argument."
  )
}

# check specific options for validity ---------------------------------
cpp_option_value <- function(cpp_options, option) {
  # The last match wins, as it does for make.
  matches <- which(tolower(names(cpp_options)) == tolower(option))
  if (length(matches) == 0) {
    return(NULL)
  }
  cpp_options[[matches[[length(matches)]]]]
}

# check runtime requests against what the executable reports ------------
#' Check a thread request against the features the executable reports
#'
#' More than one thread needs threading known on. One thread, or no request,
#' needs nothing.
#'
#' @param threads The request, or `NULL`.
#' @param features What the executable reports about its own build: each
#'   feature is known on, known off, or absent when unknown.
#' @param multiple_chains Whether the request came as `threads_per_chain`.
#' @return `threads`, invisibly.
#' @noRd
assert_valid_threads <- function(threads, features, multiple_chains = FALSE) {
  threads_arg <- if (multiple_chains) "threads_per_chain" else "threads"
  checkmate::assert_integerish(threads, .var.name = threads_arg,
                               null.ok = TRUE, lower = 1, len = 1)
  threaded <- isTRUE(features[["stan_threads"]])
  if (!is.null(threads) && threads > 1 && !threaded) {
    stop(
      "'", threads_arg, "' is set but the executable does not report ",
      "threading as enabled.\nRecompile the model with ",
      "'cpp_options = list(stan_threads = TRUE)'.",
      call. = FALSE
    )
  }
  invisible(threads)
}

#' Check an OpenCL device request against the features the executable reports
#'
#' @param opencl_ids The request, or `NULL`.
#' @param features As for `assert_valid_threads()`.
#' @return `opencl_ids`, invisibly.
#' @noRd
assert_valid_opencl <- function(opencl_ids, features) {
  if (!is.null(opencl_ids) && !isTRUE(features[["stan_opencl"]])) {
    stop(
      "'opencl_ids' is set but the executable does not report OpenCL as ",
      "enabled.\nRecompile the model with ",
      "'cpp_options = list(stan_opencl = TRUE)'.",
      call. = FALSE
    )
  }
  invisible(opencl_ids)
}
