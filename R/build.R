# Building the executable for a Stan program or adopting one as it is

#' Build or verify the executable for a Stan program
#'
#' The executable beside the program, or in `dir`, is reused when
#' `assess_build()` finds nothing changed and rebuilt otherwise, or when
#' `force_recompile` asks for a rebuild. Both paths generate the model's C++ and
#' `stanc --info` from the source just verified or built, so a model object
#' has the same `info` and `hpp_code` whether its executable was reused or
#' built.
#'
#' `NULL` for any build argument means it was omitted. An omitted `pedantic`
#' is `FALSE`. An omitted `force_recompile` lets the `cmdstanr_force_recompile`
#' option decide, and the rebuild reason names whichever of the two asked.
#'
#' @param stan_file,dir,include_paths,user_header As `cmdstan_model()`
#'   documents them.
#' @param cpp_options,stanc_options,pedantic,force_recompile As
#'   `cmdstan_model()` documents them.
#' @param quiet Whether to hide make's output.
#' @return A list: `exe_file`, `record`, `include_paths` (the effective ones),
#'   `info` (what `stanc --info` reported) and `hpp_code` (the model's C++).
#' @noRd
build_executable <- function(stan_file,
                             dir = NULL,
                             include_paths = NULL,
                             user_header = NULL,
                             cpp_options = NULL,
                             stanc_options = NULL,
                             pedantic = FALSE,
                             force_recompile = NULL,
                             quiet = TRUE) {
  cpp_options <- assert_valid_cpp_options(cpp_options)
  stanc_options <- assert_valid_stanc_options(stanc_options) %||% list()
  checkmate::assert_string(user_header, null.ok = TRUE)
  pedantic <- isTRUE(checkmate::assert_flag(pedantic, null.ok = TRUE))
  checkmate::assert_flag(force_recompile, null.ok = TRUE)
  checkmate::assert_flag(quiet)
  include_paths <- effective_include_paths(stan_file, include_paths)
  if (!is.null(user_header)) {
    # Kept as a host path. make gets the WSL spelling below.
    user_header <- resolve_path(user_header)
    if (!file.exists(user_header)) {
      stop(
        "User header file '", user_header, "' does not exist.", call. = FALSE
      )
    }
  }
  exe <- executable_path(stan_file, dir)

  # Options cmdstanr adds stay apart from the user's so the record can hold
  # each as it was. They're merged only when they become arguments.
  added <- list()
  if (pedantic) {
    added[["warn-pedantic"]] <- TRUE
  }
  if (isTRUE(cpp_option_value(cpp_options, "stan_opencl"))) {
    added[["use-opencl"]] <- TRUE
  }
  if (!is.null(user_header)) {
    added[["allow-undefined"]] <- TRUE
  }
  added[["name"]] <- paste0(model_name_from_path(stan_file), "_model")
  if (!stanc_option_supplied(stanc_options, "filename-in-msg")) {
    added[["filename-in-msg"]] <- wsl_safe_path(stan_file)
  }
  configuration <- list(
    cpp_options = parsed_cpp_options(cpp_options),
    stanc_options = as.list(stanc_options_to_args(stanc_options)),
    stanc_options_added = as.list(stanc_options_to_args(added)),
    stanc_name = added[["name"]],
    include_paths = as.list(include_paths)
  )

  forced <- NULL
  if (isTRUE(force_recompile)) {
    forced <- "force_recompile"
  } else if (is.null(force_recompile) &&
             isTRUE(getOption("cmdstanr_force_recompile"))) {
    forced <- "force_recompile_option"
  }
  current <- read_current_build(stan_file, include_paths, user_header, exe)
  reasons <- c(
    assess_build(
      list(configuration = configuration, executable_hash = NULL), current
    ),
    forced
  )
  rebuild <- length(reasons) > 0
  if (rlang::is_interactive()) {
    message(build_message(reasons, current))
  }

  # The flags stanc receives: the user's and the added ones, then what make
  # adds for this build.
  # On a reuse the record says what make added since make/local is unchanged.
  stancflags_call <- stanc_options_to_args(c(stanc_options, added))
  make_vars <- cpp_options_to_compile_flags(cpp_options)
  if (!is.null(user_header)) {
    make_vars <- c(
      make_vars, paste0("USER_HEADER=", wsl_safe_path(user_header))
    )
  }
  if (rebuild) {
    if (is.null(current$info)) {
      current <- c(
        current, resolve_dependencies(stan_file, include_paths, user_header)
      )
    }
    from_make <- stancflags_added_by_make(make_vars)
  } else {
    from_make <-
      unlist(current$record$record$configuration$stanc_options_from_make)
  }
  from_make <- drop_overridden_stancflags(from_make, stancflags_call)
  stancflags_direct <- c(stancflags_call, from_make)
  stanc_inc_paths <- include_paths_stanc3_args(
    include_paths, direct_call = TRUE
  )

  # On a rebuild make compiles a copy of the Stan file, and the C++ below is
  # generated from the same copy, so an edit made during the build reaches
  # neither. Stanc's warnings are left to the build's own stanc run. When the
  # executable is reused there is no such run, so a pedantic check shows them
  # here.
  source <- stan_file
  if (rebuild) {
    source <- tempfile(
      pattern = "model-", fileext = paste0(".", tools::file_ext(stan_file))
    )
    file.copy(stan_file, source, overwrite = TRUE)
    withr::defer(unlink(c(
      source, paste0(strip_ext(source), ".hpp"), cmdstan_ext(strip_ext(source))
    )))
  }
  hpp_code <- get_standalone_hpp(
    source, c(stanc_inc_paths, stancflags_direct),
    show_warnings = !rebuild && pedantic
  )

  if (!rebuild) {
    record <- current$record$record
  } else {
    tmp_exe <- cmdstan_ext(strip_ext(source))
    if (os_is_windows() && !os_is_wsl()) {
      tmp_exe <- utils::shortPathName(tmp_exe)
    }
    # get_cmdstan_flags() split the flags from make into words. Requote them
    # for the STANCFLAGS value handed back to make.
    stancflags_quoted <- stanc_options_to_args(
      c(stanc_options, added), quote_values = TRUE
    )
    stancflags_make <- paste0(
      "STANCFLAGS += ", include_paths_stanc3_args(include_paths),
      paste0(
        " ", c(stancflags_quoted, make_shell_quote(from_make)), collapse = ""
      )
    )
    run_make(
      c(wsl_safe_path(repair_path(tmp_exe)), make_vars, stancflags_make), quiet
    )
    record <- new_build_record(
      configuration = append(
        configuration, list(stanc_options_from_make = as.list(from_make)),
        after = 3
      ),
      reported_features = reported_features_from_exe(tmp_exe),
      dependencies = current$dependencies,
      executable_hash = hash_file(tmp_exe),
      cmdstan = current$cmdstan,
      tbb_dir = tbb_dir_from_options(cpp_options),
      untracked_dependencies = untracked_dependencies(
        current$dependencies$make_local$built_from, user_header
      )
    )
    leftover_backup <- install_executable(tmp_exe, exe, record)
    # Said once, when the record is written, and never on a no-op.
    if (length(record$untracked_dependencies) > 0) {
      message(untracked_dependencies_note(record$untracked_dependencies))
    }
    if (!is.null(leftover_backup)) {
      warning(
        "Files left over from the previous build could not be removed: '",
        paste(leftover_backup, collapse = "', '"), "'.",
        call. = FALSE
      )
    }
  }
  list(
    exe_file = exe,
    record = record,
    include_paths = include_paths,
    info = current$info,
    hpp_code = hpp_code
  )
}

#' Install a model executable and its build record as a pair
#'
#' `file.copy()` and `file.rename()` warnings are suppressed so that
#' `options(warn = 2)` cannot interrupt the undo. A crash between two renames
#' can leave only the backup.
#'
#' @param from Path to the newly compiled executable.
#' @param to Path the executable should be installed at.
#' @param record The build record to install beside the executable.
#' @return `NULL` after a clean install, or the backup paths that cleanup could
#'   not remove. The new executable and its record are installed in either case.
#' @noRd
install_executable <- function(from, to, record) {
  if (dir.exists(to)) {
    stop(
      "Cannot install the compiled executable at '", to,
      "' because that path is a directory. Nothing was modified.",
      call. = FALSE
    )
  }
  # Normalize mixed Windows separators before converting the path for WSL.
  stage <- function(pattern) {
    repair_path(tempfile(pattern = pattern, tmpdir = dirname(to)))
  }
  rename <- function(from, to) {
    isTRUE(suppressWarnings(file.rename(from, to)))
  }
  # The paths that are still there after trying to remove them.
  remove <- function(paths) {
    paths <- Filter(file.exists, paths)
    paths[vapply(paths, unlink, integer(1), expand = FALSE) != 0L]
  }
  left_behind <- function(paths) {
    if (length(paths) == 0) {
      ""
    } else {
      paste0(" Files left behind: '", paste(paths, collapse = "', '"), "'.")
    }
  }

  # Nothing at the destination changes until both files are staged beside it.
  candidate <- stage("exe-new-")
  staged_record <- build_record_path(candidate)
  staging <- tryCatch({
    if (!isTRUE(suppressWarnings(file.copy(from, candidate)))) {
      stop(
        "Could not stage the compiled executable at '", candidate, "'.",
        call. = FALSE
      )
    }
    if (os_is_wsl()) {
      chmod <- processx::run(
        command = "wsl",
        args = c("chmod", "+x", wsl_safe_path(candidate)),
        error_on_status = FALSE
      )
      if (is.na(chmod$status) || chmod$status != 0) {
        stop("Could not make the compiled executable executable.", call. = FALSE)
      }
    }
    write_build_record(record, candidate)
    NULL
  }, error = function(e) e)
  if (!is.null(staging)) {
    stop(
      conditionMessage(staging),
      " The model executable at '", to, "' was not modified.",
      left_behind(remove(c(candidate, staged_record))),
      call. = FALSE
    )
  }

  # The old pair aside, then the new pair in. A move with nothing to back up
  # drops out.
  record_path <- build_record_path(to)
  exe_backup <- if (file.exists(to)) stage("exe-old-")
  record_backup <- if (file.exists(record_path)) stage("record-old-")
  moves <- list(
    c(to, exe_backup),
    c(record_path, record_backup),
    c(candidate, to),
    c(staged_record, record_path)
  )
  moves <- moves[lengths(moves) == 2]
  done <- list()
  failure <- tryCatch({
    for (move in moves) {
      if (!rename(move[1], move[2])) {
        stop("Could not move '", move[1], "' to '", move[2], "'.", call. = FALSE)
      }
      done <- c(done, list(move))
    }
    verify_build_record(to)
    NULL
  }, error = function(e) e)
  if (!is.null(failure)) {
    # A later undo can put the old file back over one that would not move.
    stuck <- character()
    for (move in rev(done)) {
      if (rename(move[2], move[1])) {
        stuck <- setdiff(stuck, move[1])
      } else {
        stuck <- c(stuck, move[2])
      }
    }
    stop(
      "Could not install the compiled executable at '", to, "': ",
      conditionMessage(failure),
      if (length(stuck) == 0) {
        " The executable and build record there are as they were."
      } else {
        " The previous executable and build record could not all be put back."
      },
      left_behind(c(stuck, remove(c(candidate, staged_record)))),
      call. = FALSE
    )
  }

  leftover <- remove(c(exe_backup, record_backup))
  if (length(leftover) == 0) NULL else leftover
}

#' Take an executable as it is
#'
#' @param exe_file Path to the executable.
#' @return A list: `record` (`NULL` when there is no record),
#'   `reported_features`, `version` and `executable_hash`.
#' @noRd
adopt_executable <- function(exe_file) {
  found <- inspect_executable(exe_file)
  if (found$status == "available") {
    return(facts_from_record(found$record))
  }
  list(
    record = NULL,
    reported_features = found$reported_features,
    version = found$reported_features[["stan_version"]],
    executable_hash = hash_file(exe_file)
  )
}

#' Read the build record beside an executable, or query the executable
#'
#' With a usable record the features come from it and the executable is not
#' run. Without one the executable is run with `info` and the features come
#' from its output.
#'
#' @param exe_file Path to the executable.
#' @return What `read_build_record()` returns, plus `reported_features`.
#' @noRd
inspect_executable <- function(exe_file) {
  found <- read_build_record(exe_file)
  if (found$status == "available") {
    found$reported_features <- found$record$reported_features
    return(found)
  }
  features <- reported_features_from_exe(exe_file)
  if (is.null(features[["stan_version"]])) {
    stop(
      "Running '", exe_file, "' with the argument 'info' did not report a ",
      "Stan version, so it is either not a CmdStan executable or cannot be ",
      "run.",
      call. = FALSE
    )
  }
  found$reported_features <- features
  found
}

facts_from_record <- function(record) {
  list(
    record = record,
    reported_features = record$reported_features,
    version = record$cmdstan$version,
    executable_hash = record$executable_hash
  )
}

#' What is on disk for an executable built from a Stan program
#'
#' The `current` argument of `assess_build()`. `build_executable()` and
#' `assert_current()` both come here, so they cannot assemble it differently.
#' The sources are hashed only when the selected CmdStan is the one in the
#' record, because the selected stanc resolves them and a different CmdStan
#' already forces a rebuild. `info`, the `stanc --info` output they were
#' resolved from, comes along for the model's `$variables()`.
#'
#' @param stan_file,include_paths,user_header The Stan program, include paths
#'   and user header of the model being checked, with `include_paths` from
#'   `effective_include_paths()`.
#' @param exe_file Where the executable is or would be.
#' @return A list: `exe_file`; `record`, what `read_build_record()` returned,
#'   or the reason `no_executable`; `cmdstan`, the selected installation's
#'   `path` and `version`; and, when the sources were hashed, `info` and
#'   `dependencies` from `resolve_dependencies()`.
#' @noRd
read_current_build <- function(stan_file, include_paths, user_header,
                               exe_file) {
  current <- list(
    exe_file = exe_file,
    record = list(status = "unavailable", reason = "no_executable"),
    cmdstan = list(path = cmdstan_path(), version = current_cmdstan_version())
  )
  if (file.exists(exe_file)) {
    current$record <- read_build_record(exe_file)
  }
  same_cmdstan <- function() {
    recorded <- current$record$record
    differs <- compare_build_records(recorded, current, "cmdstan")
    length(differs) == 0
  }
  if (current$record$status == "available" && same_cmdstan()) {
    current <- c(
      current, resolve_dependencies(stan_file, include_paths, user_header)
    )
  }
  current
}

#' Hash what a build of this program consumes
#'
#' The Stan program, the files stanc resolves its includes to under these
#' paths, the user header and the installation's make/local. Under WSL stanc
#' reports the includes in its own spelling.
#'
#' @param stan_file,include_paths,user_header As for `read_current_build()`.
#' @return A list: `info`, what `stanc --info` reported, and `dependencies`,
#'   the record's `dependencies` field for these files.
#' @noRd
resolve_dependencies <- function(stan_file, include_paths, user_header) {
  info <- stanc_info(stan_file, include_paths)
  dependency <- function(path) {
    list(hash = hash_file(path), built_from = resolve_path(path))
  }
  dependencies <- list(
    stan_file = dependency(stan_file),
    included_files = lapply(
      wsl_safe_path(unlist(info$included_files), revert = TRUE), dependency
    )
  )
  if (!is.null(user_header)) {
    dependencies$user_header <- dependency(user_header)
  }
  make_local <- file.path(cmdstan_path(), "make", "local")
  if (file.exists(make_local)) {
    dependencies$make_local <- dependency(make_local)
  }
  list(info = info, dependencies = dependencies)
}

#' The stanc flags make adds for this build
#'
#' What make resolves `STANCFLAGS` to with this build's variables applied,
#' which is how make/local and CmdStan's own makefiles reach stanc. Setting
#' an include path there is an error. Include paths have to come through
#' `include_paths`, because that's the only place `stanc --info` looks when
#' the dependencies are resolved.
#'
#' @param make_vars The variable assignments this build passes to make.
#' @return The flags, one per element.
#' @noRd
stancflags_added_by_make <- function(make_vars) {
  flags <- get_cmdstan_flags("STANCFLAGS", make_vars)
  is_include_path <- grepl("--include-paths", flags, fixed = TRUE) |
    startsWith(flags, "-I")
  if (any(is_include_path)) {
    stop(
      "`make/local` sets an include path in `STANCFLAGS` (`",
      flags[is_include_path][1],
      "`). Include paths cannot be set there. Remove it from `make/local` ",
      "and pass the directories with the `include_paths` argument.",
      call. = FALSE
    )
  }
  flags
}

#' Run make on a model target inside the selected installation
#'
#' @param args Arguments to make: the target, then variable assignments.
#' @param quiet Whether to hide make's output.
#' @return The `processx::run()` result, invisibly. A failed build is an
#'   error.
#' @noRd
run_make <- function(args, quiet) {
  withr::with_envvar(
    c("HOME" = short_path(Sys.getenv("HOME"))),
    withr::with_path(
      c(
        toolchain_PATH_env_var(),
        tbb_path()
      ),
      run_log <- wsl_compatible_run(
        command = make_cmd(),
        args = args,
        wd = checked_cmdstan_path(),
        echo = !quiet || is_verbose_mode(),
        echo_cmd = is_verbose_mode(),
        spinner = quiet && use_spinner(),
        stderr_callback = function(x, p) {
          if (!startsWith(x, paste0(make_cmd(), ": *** No rule to make target"))) {
            message(x)
          }
          if (grepl("PCH file", x) || grepl("precompiled header", x) || grepl(".hpp.gch", x) ) {
            warning(
              "CmdStan's precompiled header (PCH) files may need to be rebuilt.\n",
              "If your model failed to compile please run rebuild_cmdstan().\n",
              "If the issue persists please open a bug report.",
              call. = FALSE
            )
          }
          if (grepl("No space left on device", x) || grepl("error in backend: IO failure on output stream", x)) {
            warning(
              "The C++ compiler ran out of disk space and was unable to build the executables for your model!\n",
              "See the above error for more details.",
              call. = FALSE
            )
          }
          if (os_is_macos()) {
            if (R.version$arch == "aarch64"
                && grepl("but the current translation unit is being compiled for target", x)) {
              warning(
                "The C++ compiler has errored due to incompatibility between the x86 and ",
                "Apple Silicon architectures.\n",
                "If you are running R inside an IDE (RStudio, VSCode, ...), ",
                "make sure the IDE is a native Apple Silicon app.\n",
                call. = FALSE
              )
            }
          }
        },
        error_on_status = FALSE
      )
    )
  )
  if (is.na(run_log$status) || run_log$status != 0) {
    stop("An error occurred during compilation! See the message above for more information.",
         call. = FALSE)
  }
  invisible(run_log)
}

#' The include paths a Stan program is resolved with
#'
#' A program with `#include` lines and no paths given looks in its own
#' directory. stanc does not do this itself.
#'
#' @param stan_file Path to the program.
#' @param include_paths What the user gave, or `NULL`.
#' @return The paths, resolved, or `NULL` when there are none.
#' @noRd
effective_include_paths <- function(stan_file, include_paths = NULL) {
  code <- readLines(stan_file, warn = FALSE)
  if (is.null(include_paths) && any(grepl("#include", code, fixed = TRUE))) {
    include_paths <- dirname(stan_file)
  }
  resolve_path(include_paths)
}

#' Where the executable for a Stan program goes
#'
#' Beside the program, or in `dir`, under the program's name.
#'
#' @param stan_file Path to the program.
#' @param dir A directory to put it in instead, or `NULL`.
#' @return The path. A directory already there under that name is an error.
#' @noRd
executable_path <- function(stan_file, dir = NULL) {
  exe_base <- stan_file
  if (!is.null(dir)) {
    dir <- repair_path(absolute_path(dir))
    assert_dir_exists(dir, access = "rw")
    exe_base <- file.path(dir, basename(stan_file))
  }
  exe <- cmdstan_ext(strip_ext(exe_base))
  if (dir.exists(exe)) {
    stop(
      "There is a subfolder matching the model name ",
      "in the same folder as the model! ",
      "Please remove or rename the subfolder and try again.",
      call. = FALSE
    )
  }
  exe
}

model_name_from_path <- function(path) {
  gsub(" ", "_", strip_ext(basename(path)))
}


# what build_executable() and assert_current() say --------------------------

#' What `build_executable()` says before it builds or reuses
#'
#' @param reasons What `assess_build()` returned, plus the forced reason if
#'   any.
#' @param current What `read_current_build()` returned.
#' @return The message, one string.
#' @noRd
build_message <- function(reasons, current) {
  if (length(reasons) == 0) {
    return("Model executable is up to date!")
  }
  if ("no_executable" %in% reasons) {
    return("Compiling Stan program...")
  }
  paste(
    c("Recompiling:", paste0("  - ", rebuild_reasons(reasons, current))),
    collapse = "\n"
  )
}

#' Word the reasons assess_build() returns, one line each
#'
#' @param reasons As for `build_message()`.
#' @param current What `read_current_build()` returned.
#' @return One line per reason, unnamed.
#' @noRd
rebuild_reasons <- function(reasons, current) {
  recorded <- current$record$record
  deps <- current$dependencies
  included_files <- function() {
    old <- recorded$dependencies$included_files
    new <- deps$included_files
    if (length(old) != length(new)) {
      return("the set of included files changed")
    }
    changed <- vapply(
      seq_along(old),
      function(i) !identical(old[[i]]$hash, new[[i]]$hash),
      logical(1)
    )
    paths <- vapply(new[changed], `[[`, character(1), "built_from")
    paste0("included files changed (", paste(paths, collapse = ", "), ")")
  }
  format_version <- function() {
    written <- current$record$format_version
    sprintf(
      paste0(
        "the build record was written by %s version of cmdstanr (format %s; ",
        "this version understands %s), so how the executable was built ",
        "cannot be verified"
      ),
      if (written > build_record_format_version) "a newer" else "an older",
      written, build_record_format_version
    )
  }
  line <- function(reason) {
    switch(
      reason,
      no_executable = paste0(
        "there is no executable at '", current$exe_file, "'"
      ),
      missing = paste0(
        "the executable has no build record, so what it was built with ",
        "cannot be determined"
      ),
      unreadable = paste0(
        "the build record beside the executable could not be read, so what ",
        "it was built with cannot be verified"
      ),
      unsupported_format = format_version(),
      executable_mismatch = paste0(
        "the executable does not match its build record, so it was replaced ",
        "or altered after it was built"
      ),
      executable = "the executable changed after this model was created",
      cpp_options = "`cpp_options` changed",
      stanc_options = "`stanc_options` changed",
      stanc_name = "the model name changed",
      stan_file = "the Stan program changed",
      included_files = included_files(),
      user_header = paste0(
        "the user header changed (",
        (deps$user_header %||% recorded$dependencies$user_header)$built_from,
        ")"
      ),
      make_local = paste0(
        "make/local changed (",
        (deps$make_local %||% recorded$dependencies$make_local)$built_from,
        ")"
      ),
      cmdstan = sprintf(
        paste0(
          "the selected CmdStan changed (built with %s at '%s'; ",
          "%s at '%s' is selected now)"
        ),
        recorded$cmdstan$version, recorded$cmdstan$path,
        current$cmdstan$version, current$cmdstan$path
      ),
      force_recompile = "`force_recompile = TRUE` was supplied",
      force_recompile_option = "the `cmdstanr_force_recompile` option is set"
    )
  }
  vapply(reasons, line, character(1), USE.NAMES = FALSE)
}

#' The error a checked method raises on a stale executable
#'
#' Carries the class `cmdstanr_stale_executable` so callers can catch it by
#' what it means rather than by its text.
#'
#' @param lines The message, one element per line.
#' @return Does not return.
#' @noRd
stop_stale_executable <- function(lines) {
  rlang::abort(
    paste(lines, collapse = "\n"),
    class = "cmdstanr_stale_executable",
    call = NULL
  )
}
