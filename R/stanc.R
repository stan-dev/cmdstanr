# Talking to stanc: checking and shaping the options it is given, running
# it, and reading what it printed. The build and the model methods both come
# through here.

# Returns the stanc exe path depending on the OS
stanc_cmd <- function() {
  if (os_is_windows() && !os_is_wsl()) {
    "bin/stanc.exe"
  } else {
    "bin/stanc"
  }
}

#' Check the user's `stanc_options` for what stanc or CmdStanR would reject
#'
#' Each element is a flag, named or not. A leading `--`, a flag CmdStanR sets
#' from one of its own arguments, and a `=` inside a name are errors.
#'
#' @param stanc_options The options the user passed to `cmdstan_model()` or
#'   another function that runs stanc.
#' @return `stanc_options`, invisibly.
#' @noRd
assert_valid_stanc_options <- function(stanc_options) {
  i <- 1
  names <- names(stanc_options)
  for (s in stanc_options) {
    named <- !is.null(names[i]) && nzchar(names[i])
    if (named) {
      name <- names[i]
    } else {
      name <- s
    }
    if (startsWith(name, "--")) {
      stop("No leading hyphens allowed in stanc options (", name, "). ",
           "Use options without leading hyphens, for example ",
           "`stanc_options = list(\"warn-uninitialized\")`",
           call. = FALSE)
    }
    # The flag is the part before the first `=`, wherever the name occurs.
    flag <- sub("=.*$", "", name)
    derived <- derived_stanc_option_message(flag)
    if (!is.null(derived)) {
      stop(derived, call. = FALSE)
    }
    if (named && grepl("=", name, fixed = TRUE)) {
      stop(
        sprintf(
          paste0(
            "`stanc_options` names cannot contain `=`. ",
            "Write the value after the name: `list(\"%s\" = \"%s\")` ",
            "instead of `list(\"%s\" = ...)`."
          ),
          flag, sub("^[^=]*=", "", name), name
        ),
        call. = FALSE
      )
    }
    i <- i + 1
  }
  invisible(stanc_options)
}

#' The error for a stanc flag CmdStanR sets from one of its own arguments
#'
#' The five flag names live here so the check and the messages cannot drift
#' apart.
#'
#' @param flag The flag name, without its value.
#' @return The message, or `NULL` for any other flag.
#' @noRd
derived_stanc_option_message <- function(flag) {
  messages <- c(
    "include-paths" = paste0(
      "`include-paths` cannot be set through `stanc_options`. ",
      "Pass the directories with the `include_paths` argument."
    ),
    "warn-pedantic" = paste0(
      "`warn-pedantic` cannot be set through `stanc_options`. ",
      "Use `pedantic = TRUE`."
    ),
    "allow-undefined" = paste0(
      "`allow-undefined` cannot be set through `stanc_options`. ",
      "It is on whenever a `user_header` is supplied, and ",
      "`$check_syntax()`, `$format()` and `$variables()` always use it."
    ),
    "use-opencl" = paste0(
      "`use-opencl` cannot be set through `stanc_options`. ",
      "Use `cpp_options = list(stan_opencl = TRUE)`, which turns it on."
    ),
    "name" = paste0(
      "`name` cannot be set through `stanc_options`. ",
      "The model name comes from the name of the Stan file."
    )
  )
  if (flag %in% names(messages)) {
    messages[[flag]]
  } else {
    NULL
  }
}

#' Whether `stanc_options` sets a flag, whichever way it was written
#'
#' A flag arrives named, `list("filename-in-msg" = "x.stan")`, or unnamed with
#' its value attached, `list("filename-in-msg=x.stan")`. As in
#' assert_valid_stanc_options(), the flag is the text before the first `=`.
#'
#' @param stanc_options The user's `stanc_options`.
#' @param flag The flag name, without its value.
#' @return `TRUE` or `FALSE`.
#' @noRd
stanc_option_supplied <- function(stanc_options, flag) {
  names <- names(stanc_options)
  for (i in seq_along(stanc_options)) {
    name <- names[i]
    if (is.null(name) || !nzchar(name)) {
      name <- stanc_options[[i]]
    }
    if (sub("=.*$", "", name) == flag) {
      return(TRUE)
    }
  }
  FALSE
}

#' Turn a `stanc_options` list into stanc command line arguments
#'
#' @param stanc_options (list) Named or unnamed stanc options. Logical values
#'   mark boolean flags and any other value is passed as `--name=value`.
#' @param quote_values (logical) Quote the arguments for the `STANCFLAGS`
#'   string handed to make, which expands it through a shell? Arguments for
#'   direct stanc calls are passed to processx as separate elements and must
#'   be left unquoted (#1227).
#' @return A character vector of arguments, one per element.
#' @noRd
stanc_options_to_args <- function(stanc_options, quote_values = FALSE) {
  args <- c()
  for (i in seq_len(length(stanc_options))) {
    option_name <- names(stanc_options)[i]
    option_value <- stanc_options[[i]]
    if (is.null(option_name) || !nzchar(option_name)) {
      # Unnamed options are already flag names, e.g. list("O1")
      args <- c(args, paste0("--", option_value))
    } else if (is.logical(option_value)) {
      # TRUE emits a bare flag, FALSE leaves the flag out entirely
      if (isTRUE(option_value)) {
        args <- c(args, paste0("--", option_name))
      }
    } else {
      args <- c(args, paste0("--", option_name, "=", option_value))
    }
  }
  if (isTRUE(quote_values)) {
    args <- make_shell_quote(args)
  }
  args
}

#' Drop the `make/local` stanc flags that the call sets itself
#'
#' A flag is the text before the first `=`. An element of `local_flags` whose
#' flag is one the call emits is dropped. When that element is a bare flag and
#' the next element does not start with a hyphen, the next element is the value
#' given separately and goes with it.
#'
#' @param local_flags (character) The `STANCFLAGS` words from `make/local`, one
#'   argument per element.
#' @param call_args (character) The arguments the call emits, one per element,
#'   each starting with `--`.
#' @return `local_flags` without the overridden elements, the rest in order.
#' @noRd
drop_overridden_stancflags <- function(local_flags, call_args) {
  call_flags <- sub("=.*$", "", call_args)
  keep <- rep(TRUE, length(local_flags))
  i <- 1
  while (i <= length(local_flags)) {
    if (sub("=.*$", "", local_flags[i]) %in% call_flags) {
      keep[i] <- FALSE
      if (!grepl("=", local_flags[i], fixed = TRUE) &&
          i < length(local_flags) &&
          !startsWith(local_flags[i + 1], "-")) {
        keep[i + 1] <- FALSE
        i <- i + 1
      }
    }
    i <- i + 1
  }
  local_flags[keep]
}

#' Build stanc include-path arguments
#'
#' Include paths go to make through `STANCFLAGS`, which make expands and hands
#' to the shell, so `make_shell_quote()` quotes each path for both (#1230)
#' inside a single `--include-paths=` flag. Direct calls through processx
#' instead need the flag and comma-separated paths as separate, unquoted
#' arguments.
#'
#' @param include_paths A character vector of directories containing files used
#'   in Stan `#include` directives, or `NULL`.
#' @param direct_call A logical indicating whether the arguments will be passed
#'   directly to stanc through processx instead of through make.
#'
#' @return `NULL` if `include_paths` is `NULL`; otherwise, a single
#'   `--include-paths=` argument for make or two arguments for a direct call.
#' @noRd
include_paths_stanc3_args <- function(include_paths = NULL, direct_call = FALSE) {
  stancflags <- NULL
  if (!is.null(include_paths)) {
    assert_dir_exists(include_paths, access = "r")
    include_paths <- sapply(absolute_path(include_paths), wsl_safe_path)
    # Calling stanc3 directly through processx::run does not need quoting
    if (!isTRUE(direct_call)) {
      include_paths <- make_shell_quote(include_paths)
    }
    include_paths <- paste0(include_paths, collapse = ",")
    include_paths_flag <- "--include-paths="
    if (isTRUE(direct_call)) {
      stancflags <- c(stancflags, "--include-paths", include_paths)
    } else {
      stancflags <- paste0(stancflags, include_paths_flag, include_paths)
    }
  }
  stancflags
}

#' Ask make for `STANCFLAGS`, one argument per line
#'
#' CmdStan's `print-%` rule echoes a variable through the shell, which strips
#' the quotes, so `make print-STANCFLAGS` returns `--filename-in-msg='/my dir'`
#' as two words (#1232). This rule hands `$(STANCFLAGS)` to the shell the way
#' the stanc recipe does and prints what the shell delivers, so the result holds
#' exactly the arguments stanc gets from make. Each line carries a prefix that
#' tells it apart from other make output. The rule lives in a temporary makefile
#' rather than an `--eval` argument because users may have a make too old for
#' `--eval`; the one Apple ships with macOS is. The fragment's first line
#' removes the fragment from `MAKEFILE_LIST` so a value that reads the list sees
#' the same makefiles the real build does.
#'
#' @param cmdstan_path (string) The CmdStan directory.
#' @param make_args (character) Command-line variable assignments (`NAME=value`)
#'   for the make call.
#' @return A character vector, one element per argument, `character(0)` when the
#'   variable is empty. An empty argument (`''`) is dropped.
#' @noRd
stancflags_from_make <- function(cmdstan_path, make_args = character()) {
  rule_file <- withr::local_tempfile(pattern = "cmdstanr-stancflags-", fileext = ".mk")
  # Binary mode keeps the line endings LF; under WSL a Linux make reads a file
  # written on Windows.
  con <- file(rule_file, open = "wb")
  writeLines(
    c(
      "MAKEFILE_LIST := $(filter-out $(lastword $(MAKEFILE_LIST)),$(MAKEFILE_LIST))",
      ".PHONY: cmdstanr-print-stancflags",
      "cmdstanr-print-stancflags: ; @printf 'cmdstanr-stancflag=%s\\n' $(STANCFLAGS)"
    ),
    con,
    sep = "\n"
  )
  close(con)
  # The recipe needs sh, which on Windows comes from the toolchain
  withr::with_path(
    toolchain_PATH_env_var(),
    withr::with_envvar(
      c("HOME" = short_path(Sys.getenv("HOME"))),
      stdout <- wsl_compatible_run(
        command = "make",
        args = c(
          "-s", make_args, "-f", "makefile", "-f", wsl_safe_path(rule_file),
          "cmdstanr-print-stancflags"
        ),
        wd = cmdstan_path
      )$stdout
    )
  )
  lines <- strsplit(stdout, "\r?\n")[[1]]
  flags <- grep("^cmdstanr-stancflag=", lines, value = TRUE)
  flags <- sub("^cmdstanr-stancflag=", "", flags)
  flags[nzchar(flags)]
}

#' Run stanc on a Stan program and return what it printed
#'
#' What stanc writes to stderr, its warnings and its errors, is relayed as
#' it arrives. When stanc rejects the program, the error follows that output.
#'
#' @param stan_file Path to the program.
#' @param args Arguments after the file, one per element.
#' @param spinner Whether to show a spinner while stanc runs.
#' @return What stanc wrote to stdout, one string.
#' @noRd
run_stanc <- function(stan_file, args, spinner = FALSE) {
  withr::with_path(
    c(
      toolchain_PATH_env_var(),
      tbb_path()
    ),
    run_log <- wsl_compatible_run(
      command = stanc_cmd(),
      args = c(wsl_safe_path(stan_file), args),
      wd = checked_cmdstan_path(),
      echo = is_verbose_mode(),
      echo_cmd = is_verbose_mode(),
      spinner = spinner,
      stderr_callback = function(x, p) {
        message(x)
      },
      error_on_status = FALSE
    )
  )
  if (is.na(run_log$status) || run_log$status != 0) {
    stop("Syntax error found! See the message above for more information.",
         call. = FALSE)
  }
  run_log$stdout
}

#' What stanc reports about a Stan program
#'
#' The parsed `stanc --info` output, holding the program's variables and the
#' files it included. A program stanc rejects is an error carrying stanc's
#' own message, which names the file and the line.
#'
#' @param stan_file Path to the program.
#' @param include_paths Directories for its `#include` lines, or `NULL`.
#' @return The parsed JSON, a list.
#' @noRd
stanc_info <- function(stan_file, include_paths = NULL) {
  out_file <- tempfile(fileext = ".json")
  withr::defer(unlink(out_file))
  run_log <- wsl_compatible_run(
    command = stanc_cmd(),
    args = c(wsl_safe_path(stan_file),
              "--info",
              include_paths_stanc3_args(
                include_paths,
                direct_call = TRUE
              ),
              "--allow-undefined"),
    wd = checked_cmdstan_path(),
    echo = FALSE,
    echo_cmd = FALSE,
    stdout = out_file,
    error_on_status = FALSE
  )
  if (is.na(run_log$status) || run_log$status != 0) {
    stop(trimws(run_log$stderr), call. = FALSE)
  }
  jsonlite::read_json(out_file, na = "null")
}

#' The `$variables()` result, from what `stanc --info` reported
#'
#' @param info What `stanc_info()` returned.
#' @return The list `$variables()` documents.
#' @noRd
variables_from_info <- function(info) {
  variables <- info
  variables$data <- variables$inputs
  variables$inputs <- NULL
  variables$transformed_parameters <- variables[["transformed parameters"]]
  variables[["transformed parameters"]] <- NULL
  variables$generated_quantities <- variables[["generated quantities"]]
  variables[["generated quantities"]] <- NULL
  variables$functions <- NULL
  variables$distributions <- NULL
  variables
}

#' Run stanc on a Stan program and return the C++ it generated
#'
#' @param stan_file Path to the program.
#' @param stancflags Arguments for stanc, one per element.
#' @param show_warnings Whether to show the warnings stanc prints on a
#'   successful run, pedantic ones included. A build shows them from its own
#'   stanc run, so it passes `FALSE`.
#' @return The C++, one element per line.
#' @noRd
get_standalone_hpp <- function(stan_file, stancflags, show_warnings = FALSE) {
  hpp_path <- tempfile(pattern = "model-", fileext = ".hpp")
  withr::defer(unlink(hpp_path))

  status <- withr::with_path(
      c(
        toolchain_PATH_env_var(),
        tbb_path()
      ),
      wsl_compatible_run(
        command = stanc_cmd(),
        args = c(paste0("--o=", wsl_safe_path(hpp_path)), stancflags, wsl_safe_path(stan_file)),
        wd = checked_cmdstan_path(),
        error_on_status = FALSE
      )
    )
  if (is.na(status$status) || status$status != 0) {
    if (length(status$stderr) > 0 && nzchar(status$stderr)) {
      message(status$stderr)
    }
    err_msg <- paste0(
      "An error occurred during compilation! See the message above for more ",
      "information. (stanc exited with status ", status$status, ")"
    )
    if (length(status$stderr) > 0 &&
        grepl("auto-format flag to stanc", status$stderr)) {
      err_msg <- paste0(
        err_msg,
        "\nTo fix deprecated or removed syntax please see ",
        "?cmdstanr::format for an example."
      )
    }
    stop(err_msg, call. = FALSE)
  }
  if (show_warnings && length(status$stderr) > 0 && nzchar(status$stderr)) {
    message(status$stderr)
  }
  suppressWarnings(readLines(hpp_path, warn = FALSE))
}
