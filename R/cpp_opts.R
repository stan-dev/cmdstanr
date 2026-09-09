# Internal functions for handling cpp options

# running and parsing exe info --------------------------------
# run <model> info command
#' @noRd
#' @example `.cmdstan/bin`
run_info_cli <- function(exe_file) {
  withr::with_path(
    c(
      toolchain_PATH_env_var(),
      tbb_path()
    ),
    wsl_compatible_run(
      command = wsl_safe_path(exe_file),
      args = "info",
      echo = is_verbose_mode(),
      error_on_status = FALSE
    )
  )
}

# new (future) parser
# Parse the string output of <model> `info` into an R object (list)
parse_exe_info_string <- function(ret_stdout) {
  info <- list()
  info_raw <- strsplit(strsplit(ret_stdout, "\n")[[1]], "=")
  for (key_val in info_raw) {
    if (length(key_val) > 1) {
      key_val <- trimws(key_val)
      val <- key_val[2]
      if (!is.na(as.logical(val))) {
        val <- as.logical(val)
      }
      info[[tolower(key_val[1])]] <- val
    }
  }

  info[["stan_version"]] <- paste0(
    info[["stan_version_major"]],
    ".",
    info[["stan_version_minor"]],
    ".", info[["stan_version_patch"]]
  )
  info[["stan_version_major"]] <- NULL
  info[["stan_version_minor"]] <- NULL
  info[["stan_version_patch"]] <- NULL

  info
}

# old (current) parser
model_compile_info <- function(exe_file, version) {
  info <- NULL
  ret <- run_info_cli(exe_file)
  if (ret$status == 0) {
    info <- list()
    info_raw <- strsplit(strsplit(ret$stdout, "\n")[[1]], "=")
    for (key_val in info_raw) {
      if (length(key_val) > 1) {
        key_val <- trimws(key_val)
        val <- key_val[2]
        if (!is.na(as.logical(val))) {
          val <- as.logical(val)
        }
        info[[toupper(key_val[1])]] <- val
      }
    }
    info[["STAN_VERSION"]] <- paste0(info[["STAN_VERSION_MAJOR"]], ".", info[["STAN_VERSION_MINOR"]], ".", info[["STAN_VERSION_PATCH"]])
    info[["STAN_VERSION_MAJOR"]] <- NULL
    info[["STAN_VERSION_MINOR"]] <- NULL
    info[["STAN_VERSION_PATCH"]] <- NULL
  }
  info
}

# Merge build options reported by the executable. Skip STAN_VERSION and the
# flags reported off, so only the options the build turned on are recorded.
merge_exe_info_cpp_options <- function(cpp_options, exe_info) {
  for (option_name in names(exe_info)) {
    value <- exe_info[[option_name]]
    if (option_name != "STAN_VERSION" &&
        (!is.logical(value) || isTRUE(value))) {
      cpp_options[[option_name]] <- value
    }
  }
  cpp_options
}

# Normalize the flags sent to make. The last value for a name wins. Go through
# the emitted flags rather than the list because a vector value expands into one
# assignment per element.
parsed_cpp_options <- function(cpp_options) {
  assignments <- list()
  for (flag in cpp_options_to_compile_flags(cpp_options)) {
    option_name <- sub("=.*$", "", flag)
    assignments[[option_name]] <- sub("^[^=]*=", "", flag)
  }
  assignments
}

normalized_cpp_options <- function(cpp_options) {
  assignments <- parsed_cpp_options(cpp_options)
  if (length(assignments) == 0) {
    return(character())
  }
  sort(paste0(
    names(assignments), "=",
    unlist(assignments, use.names = FALSE)
  ))
}

# Omitted recorded options count as changes because cpp_options are one-shot.
cpp_options_disagree <- function(requested, recorded) {
  !identical(
    normalized_cpp_options(requested),
    normalized_cpp_options(recorded)
  )
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

#' Check the `cpp_options` a caller supplied and return them
#'
#' Every entry must be named and every name must be a Make variable name. The
#' names are uppercased here so that one spelling reaches everything downstream.
#' The user header and the stanc flags have their own arguments, so setting them
#' here is an error.
#'
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
  cpp_options
}

#' Explain why an unnamed `cpp_options` entry cannot be used
#'
#' Callers reach for makefile syntax here, so name the route that accepts the
#' entry they wrote rather than repeating the rule.
#'
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

#' The error for STANCFLAGS supplied through `cpp_options`
#'
#' @noRd
stancflags_cpp_option_message <- function() {
  paste0(
    "`STANCFLAGS` cannot be set through `cpp_options`. ",
    "Pass stanc flags with the `stanc_options` argument."
  )
}

# check specific options for validity ---------------------------------
cpp_option_value <- function(cpp_options, option) {
  # CmdStanR input and executable metadata can use different casing. Prefer
  # the final match, even when it is NULL, because later executable metadata
  # best describes the binary.
  matches <- which(tolower(names(cpp_options)) == tolower(option))
  if (length(matches) == 0) {
    return(NULL)
  }
  cpp_options[[matches[[length(matches)]]]]
}

# no type checking for opencl_ids
# cpp_options must be a list
# opencl_ids returned unchanged
assert_valid_opencl <- function(opencl_ids, cpp_options) {
  if (is.null(cpp_option_value(cpp_options, "stan_opencl"))
      && !is.null(opencl_ids)) {
    stop("'opencl_ids' is set but the model was not compiled for use with OpenCL.",
         "\nRecompile the model with 'cpp_options = list(stan_opencl = TRUE)'",
         call. = FALSE)
  }
  invisible(opencl_ids)
}

# cpp_options must be a list
assert_valid_threads <- function(threads, cpp_options, multiple_chains = FALSE) {
  threads_arg <- if (multiple_chains) "threads_per_chain" else "threads"
  checkmate::assert_integerish(threads, .var.name = threads_arg,
                               null.ok = TRUE, lower = 1, len = 1)
  stan_threads <- cpp_option_value(cpp_options, "stan_threads")
  if (is.null(stan_threads) || !isTRUE(stan_threads)) {
    if (!is.null(threads)) {
      warning(
        "'", threads_arg, "' is set but the model was not compiled with ",
        "'cpp_options = list(stan_threads = TRUE)' ",
        "so '", threads_arg, "' will have no effect!",
        call. = FALSE
      )
      threads <- NULL
    }
  } else if (isTRUE(stan_threads) && is.null(threads)) {
    stop(
      "The model executable was built with threading enabled but '",
      threads_arg, "' was not set!",
      call. = FALSE
    )
  }
  invisible(threads)
}

# For two functions below
# cpp_options style means is NULL or empty string
# exe_info style means off is FALSE

exe_info_style_cpp_options <- function(cpp_options) {
  if (is.null(cpp_options)) cpp_options <- list()
  names(cpp_options) <- toupper(names(cpp_options))
  flags_reported_in_exe_info <- c(
    "STAN_THREADS", "STAN_MPI", "STAN_OPENCL",
    "STAN_NO_RANGE_CHECKS", "STAN_CPP_OPTIMS"
  )
  for (flag in flags_reported_in_exe_info) {
    cpp_options[[flag]] <- !(
      is.null(cpp_options[[flag]]) || cpp_options[[flag]] == ""
    )
  }
  cpp_options
}

exe_info_reflects_cpp_options <- function(exe_info, cpp_options) {
  if (length(exe_info) == 0) {
    warning("Recompiling is recommended due to missing exe_info.")
    return(TRUE)
  }
  if (is.null(cpp_options)) return(TRUE)

  # Compare only options reported by the executable. Other options are unknown.
  # Parse the emitted flags so duplicates and vector values match make.
  assignments <- parsed_cpp_options(cpp_options)
  reported <- intersect(names(assignments), names(exe_info))

  for (option_name in reported) {
    # CmdStan treats any nonempty make value as enabled.
    requested <- nzchar(assignments[[option_name]])
    if (requested != isTRUE(cpp_option_value(exe_info, option_name))) {
      return(FALSE)
    }
  }
  TRUE
}
