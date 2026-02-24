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
    ret <- wsl_compatible_run(
      command = wsl_safe_path(exe_file),
      args = "info",
      echo = is_verbose_mode(),
      error_on_status = FALSE
    )
  )
  ret
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
  if (version > "2.26.1") {

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
  }
  info
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

# check specific options for validity ---------------------------------
# no type checking for opencl_ids
# cpp_options must be a list
# opencl_ids returned unchanged
assert_valid_opencl <- function(opencl_ids, cpp_options) {
  if (is.null(cpp_options[["stan_opencl"]])
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
  if (is.null(cpp_options[["stan_threads"]]) || !isTRUE(cpp_options[["stan_threads"]])) {
    if (!is.null(threads)) {
      warning(
        "'", threads_arg, "' is set but the model was not compiled with ",
        "'cpp_options = list(stan_threads = TRUE)' ",
        "so '", threads_arg, "' will have no effect!",
        call. = FALSE
      )
      threads <- NULL
    }
  } else if (isTRUE(cpp_options[["stan_threads"]]) && is.null(threads)) {
    stop(
      "The model was compiled with 'cpp_options = list(stan_threads = TRUE)' ",
      "but '", threads_arg, "' was not set!",
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
  if(is.null(cpp_options)) cpp_options <- list()
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
