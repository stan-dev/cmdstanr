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

# Merge the options an executable reports about itself into the options already
# recorded for it. STAN_VERSION describes the toolchain rather than a make
# option, and a flag the executable reports as FALSE was never set at all, so
# recording it would pass "FLAG=FALSE" to make, which CmdStan reads as enabling
# the flag.
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

# The options a compilation would actually be run with, normalized so that a
# request can be compared against what an executable was built with. This has to
# follow cpp_options_to_compile_flags(), because what make is given is what
# decides whether two builds differ:
#
#   - names are lower-cased, since CmdStanR input and executable metadata
#     disagree on case, and values compared as strings so TRUE and "TRUE" are
#     not read as two different requests;
#   - an unnamed entry is a raw make argument and is compared as written;
#   - a later entry with the same name wins, because every duplicate reaches
#     make and a makefile takes the last;
#   - only NULL is omission. FALSE is *not*: it reaches make as
#     STAN_THREADS=FALSE, and CmdStan enables some options whenever their make
#     variable is non-empty, so requesting FALSE can change the executable;
#   - header entries are dropped, header identity being tracked separately and
#     forcing a rebuild on its own.
normalized_cpp_options <- function(cpp_options) {
  named <- list()
  raw <- character()
  for (i in seq_along(cpp_options)) {
    option_name <- names(cpp_options)[i]
    value <- cpp_options[[i]]
    if (is.null(option_name) || is.na(option_name) || !nzchar(option_name)) {
      raw <- c(raw, as.character(value))
      next
    }
    if (tolower(option_name) %in% c("user_header", "stan_version")) {
      next
    }
    if (is.null(value)) {
      next
    }
    named[[tolower(option_name)]] <- paste(as.character(value), collapse = ",")
  }
  entries <- character()
  if (length(named) > 0) {
    entries <- paste0(names(named), "=", unlist(named, use.names = FALSE))
  }
  sort(c(raw, entries))
}

# Whether an executable built with `recorded` would differ from one built with
# `requested`. Symmetric, because cpp_options are one-shot: a recompilation
# carrying `requested` would drop anything `recorded` holds that it does not.
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
# Decide which user header a compilation should use and reduce cpp_options to a
# single, unambiguous source for it.
#
# Precedence:
#   1. an explicit non-NULL `user_header` argument;
#   2. an explicit `user_header = NULL`, which clears any header carried in
#      cpp_options as well;
#   3. only when the argument is omitted, cpp_options -- USER_HEADER ahead of
#      user_header whichever order they appear in -- and then `previous`, the
#      header the model already holds.
#
# `supplied` is what makes (2) expressible at all: `user_header = NULL` is also
# the default, so the value alone cannot separate "cleared" from "not
# mentioned". `cpp_options_supplied` separates a header passed in the same call,
# a conflict worth warning about, from one inherited from an earlier call.
#
# Both spellings are always dropped from cpp_options; callers reinsert the
# selected header under `spelling`, in whatever form they store. The header is
# returned as supplied, neither made absolute nor WSL-safe, because callers
# differ on which they need.
resolve_user_header <- function(user_header,
                                supplied,
                                cpp_options,
                                cpp_options_supplied = TRUE,
                                previous = NULL) {
  from_upper <- cpp_options[["USER_HEADER"]]
  from_lower <- cpp_options[["user_header"]]
  conflict <- NULL
  spelling <- "USER_HEADER"

  if (supplied) {
    if (cpp_options_supplied && (!is.null(from_upper) || !is.null(from_lower))) {
      conflict <- "argument"
    }
    header <- user_header
  } else if (!is.null(from_upper)) {
    if (!is.null(from_lower)) {
      conflict <- "cpp_options"
    }
    header <- from_upper
  } else if (!is.null(from_lower)) {
    header <- from_lower
    spelling <- "user_header"
  } else {
    header <- previous
  }

  # Shape is checked wherever a header is accepted; whether it exists is checked
  # only when compiling, so that a header created between construction and
  # $compile() still works.
  if (!is.null(header)) {
    checkmate::assert_string(header, .var.name = "user_header")
  }
  cpp_options[["USER_HEADER"]] <- NULL
  cpp_options[["user_header"]] <- NULL

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

  cpp_options <- exe_info_style_cpp_options(cpp_options)[tolower(names(cpp_options))]
  overlap <- names(cpp_options)[names(cpp_options) %in% names(exe_info)]

  if (length(overlap) == 0) TRUE else all.equal(
    exe_info[overlap],
    cpp_options[overlap]
  )
}
