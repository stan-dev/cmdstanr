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

# Merge build options reported by the executable. Ignore STAN_VERSION and false
# flags (passing FLAG=FALSE back to CmdStan can enable the flag).
merge_exe_info_cpp_options <- function(cpp_options, exe_info) {
  for (option_name in names(exe_info)) {
    value <- exe_info[[option_name]]
    if (tolower(option_name) != "stan_version" &&
        (!is.logical(value) || isTRUE(value))) {
      cpp_options[[option_name]] <- value
    }
  }
  cpp_options
}

# Normalize the flags sent to make. Assignment names are case-insensitive and
# the last value wins. Nonassignments keep their order. Headers are handled
# separately.
parsed_cpp_options <- function(cpp_options) {
  assignments <- list()
  opaque <- character()
  for (flag in cpp_options_to_compile_flags(cpp_options)) {
    if (!grepl("^[A-Za-z_][A-Za-z0-9_]*=", flag)) {
      opaque <- c(opaque, flag)
      next
    }
    option_name <- tolower(sub("=.*$", "", flag))
    if (option_name %in% c("user_header", "stan_version")) {
      next
    }
    assignments[[option_name]] <- sub("^[^=]*=", "", flag)
  }
  list(assignments = assignments, opaque = opaque)
}

normalized_cpp_options <- function(cpp_options) {
  parsed <- parsed_cpp_options(cpp_options)
  reduced <- character()
  if (length(parsed$assignments) > 0) {
    reduced <- paste0(
      names(parsed$assignments), "=",
      unlist(parsed$assignments, use.names = FALSE)
    )
  }
  c(sort(reduced), parsed$opaque)
}

# Omitted recorded options count as changes because cpp_options are one-shot.
cpp_options_disagree <- function(requested, recorded) {
  !identical(
    normalized_cpp_options(requested),
    normalized_cpp_options(recorded)
  )
}

# convert to compile flags --------------------
# from list(flag1=TRUE, flag2=FALSE) to "FLAG1=TRUE\nFLAG2=FALSE"
cpp_options_to_compile_flags <- function(cpp_options) {
  if (length(cpp_options) == 0) {
    return(NULL)
  }
  cpp_built_options <- c()
  for (i in seq_along(cpp_options)) {
    option_name <- names(cpp_options)[i]
    if (is.null(option_name) || !nzchar(option_name)) {
      cpp_built_options <- c(cpp_built_options, cpp_options[[i]])
    } else {
      cpp_built_options <- c(cpp_built_options, paste0(toupper(option_name), "=", cpp_options[[i]]))
    }
  }
  cpp_built_options
}


# check options overall for validity ---------------------------------
# takes list of options as input and returns list of options
# returns list with names standardized to lowercase
validate_cpp_options <- function(cpp_options) {
  if (is.null(cpp_options) || length(cpp_options) == 0) return(list())

  if (
    !is.null(cpp_options[["user_header"]]) &&
      !is.null(cpp_options[["USER_HEADER"]])
  ) {
    warning(
      "User header specified both via cpp_options[[\"USER_HEADER\"]] ",
      "and cpp_options[[\"user_header\"]]. Please only specify your user header in one location",
      call. = FALSE
    )
  }

  names(cpp_options) <- tolower(names(cpp_options))
  flags_set_if_defined <- c(
    # cmdstan
    "stan_threads", "stan_mpi", "stan_opencl",
    "stan_no_range_checks", "stan_cpp_optims",
    # stan math
    "integrated_opencl", "tbb_lib", "tbb_inc", "tbb_interface_new"
  )
  for (flag in flags_set_if_defined)   {
    if (isFALSE(cpp_options[[flag]])) warning(
      toupper(flag), " set to ", cpp_options[flag],
      " Since this is a non-empty value, ",
      "it will result in the corresponding ccp option being turned ON. To turn this",
      " option off, use cpp_options = list(", flag, " = NULL)."
    )
  }
  cpp_options
}

# user headers ---------------------------------------------------------
# Resolve one header and remove both header spellings from cpp_options.
# Precedence is explicit user_header (including NULL), USER_HEADER,
# user_header, then previous. `supplied` distinguishes NULL from omission.
# `cpp_options_supplied` limits conflict warnings to this call.
resolve_user_header <- function(user_header,
                                supplied,
                                cpp_options,
                                cpp_options_supplied = TRUE,
                                previous = NULL) {
  # Use positions so duplicate options follow make's last-value-wins behavior.
  upper_at <- which(names(cpp_options) == "USER_HEADER")
  lower_at <- which(names(cpp_options) == "user_header")
  last_of <- function(positions) {
    if (length(positions) == 0) {
      NULL
    } else {
      cpp_options[[positions[[length(positions)]]]]
    }
  }
  # NULL is still present here because it emits an empty USER_HEADER= assignment.
  has_upper <- length(upper_at) > 0
  has_lower <- length(lower_at) > 0
  from_upper <- last_of(upper_at)
  from_lower <- last_of(lower_at)
  conflict <- NULL
  spelling <- "USER_HEADER"

  if (supplied) {
    if (cpp_options_supplied && (has_upper || has_lower)) {
      conflict <- "argument"
    }
    header <- user_header
  } else if (has_upper) {
    if (has_lower) {
      conflict <- "cpp_options"
    }
    header <- from_upper
  } else if (has_lower) {
    header <- from_lower
    spelling <- "user_header"
  } else {
    header <- previous
  }

  # Validate the value now and check file existence when compiling.
  if (!is.null(header)) {
    checkmate::assert_string(header, .var.name = "user_header")
  }
  # Guarded because x[-integer(0)] is empty.
  header_at <- c(upper_at, lower_at)
  if (length(header_at) > 0) {
    cpp_options <- cpp_options[-header_at]
  }

  list(
    user_header = header,
    spelling = spelling,
    cpp_options = cpp_options,
    conflict = conflict
  )
}

warn_user_header_conflict <- function(conflict) {
  if (identical(conflict, "argument")) {
    warning("User header specified both via user_header argument and via cpp_options arguments")
  } else if (identical(conflict, "cpp_options")) {
    warning('User header specified both via cpp_options[["USER_HEADER"]] and cpp_options[["user_header"]].', call. = FALSE)
  }
  invisible(NULL)
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
# both styles are lists which should have flag names in lower case as names of the list
# cpp_options style means is NULL or empty string
# exe_info style means off is FALSE

exe_info_style_cpp_options <- function(cpp_options) {
  if (is.null(cpp_options)) cpp_options <- list()
  names(cpp_options) <- tolower(names(cpp_options))
  flags_reported_in_exe_info <- c(
    "stan_threads", "stan_mpi", "stan_opencl",
    "stan_no_range_checks", "stan_cpp_optims"
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
  # Parse the emitted flags so duplicates and unnamed assignments match make.
  assignments <- parsed_cpp_options(cpp_options)$assignments
  reported <- intersect(names(assignments), tolower(names(exe_info)))

  for (option_name in reported) {
    # CmdStan treats any nonempty make value as enabled.
    requested <- nzchar(assignments[[option_name]])
    if (requested != isTRUE(cpp_option_value(exe_info, option_name))) {
      return(FALSE)
    }
  }
  TRUE
}
