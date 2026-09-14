# Building the executable for a Stan program, and adopting one as it is. The
# constructor is the one caller of both.

#' Build or verify the executable for a Stan program
#'
#' The one build path. The call is resolved into the request the record
#' compares, assess_build() says whether the executable beside the program (or
#' in `dir`) was built from it, and a rebuild follows when it was not or when
#' `force_recompile` asks for one. Both ways out generate the model's C++ from
#' the source just verified or built, so a model constructed on a reused
#' executable holds the same snapshot as one that built it.
#'
#' `force_recompile = NULL` means the caller did not say and the
#' `cmdstanr_force_recompile` option decides. The rebuild reason names
#' whichever of the two asked.
#'
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
  # NULL means omitted for every build argument (the adoption path relies on
  # it), and an omitted pedantic is FALSE.
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

  # Options cmdstanr adds stay apart from the caller's, so the record can hold
  # each as it was, and are merged only when they become arguments.
  injected <- list()
  if (pedantic) {
    injected[["warn-pedantic"]] <- TRUE
  }
  if (isTRUE(cpp_option_value(cpp_options, "stan_opencl"))) {
    injected[["use-opencl"]] <- TRUE
  }
  if (!is.null(user_header)) {
    injected[["allow-undefined"]] <- TRUE
  }
  injected[["name"]] <- paste0(model_name_from_path(stan_file), "_model")
  if (is.null(stanc_options[["filename-in-msg"]])) {
    injected[["filename-in-msg"]] <- stan_file
  }
  request <- list(
    cpp_options_supplied = parsed_cpp_options(cpp_options),
    stanc_options_supplied = as.list(stanc_options_to_args(stanc_options)),
    stanc_options_injected = as.list(stanc_options_to_args(injected)),
    stanc_name = injected[["name"]],
    include_paths = as.list(include_paths)
  )

  forced <- NULL
  if (isTRUE(force_recompile)) {
    forced <- "force_recompile"
  } else if (is.null(force_recompile) &&
             isTRUE(getOption("cmdstanr_force_recompile"))) {
    forced <- "force_recompile_option"
  }
  observed <- observe_build(stan_file, include_paths, user_header, exe)
  reasons <- c(
    assess_build(list(request = request, artifact = NULL), observed),
    forced
  )
  rebuild <- length(reasons) > 0
  if (rlang::is_interactive()) {
    message(constructor_message(reasons, observed))
  }

  # The flags stanc receives: the call's, then what make adds for this build.
  # On a reuse the record says what make added, since make/local is unchanged.
  stancflags_call <- stanc_options_to_args(c(stanc_options, injected))
  make_vars <- cpp_options_to_compile_flags(cpp_options)
  if (!is.null(user_header)) {
    make_vars <- c(
      make_vars, paste0("USER_HEADER=", wsl_safe_path(user_header))
    )
  }
  if (rebuild) {
    if (is.null(observed$info)) {
      observed <- c(
        observed, resolve_dependencies(stan_file, include_paths, user_header)
      )
    }
    inherited <- inherited_stancflags(make_vars)
  } else {
    inherited <- unlist(observed$record$record$request$stanc_options_inherited)
  }
  inherited <- drop_overridden_stancflags(inherited, stancflags_call)
  stancflags_direct <- c(stancflags_call, inherited)
  stanc_inc_paths <- include_paths_stanc3_args(
    include_paths, direct_call = TRUE
  )

  # make compiles a copy, so an edit during the build reaches neither the
  # executable nor the C++ generated beside it. Pedantic and other stanc
  # warnings are shown from the build's own stanc run, or here when nothing
  # builds.
  source <- stan_file
  if (rebuild) {
    source <- tempfile(
      pattern = "model-", fileext = paste0(".", tools::file_ext(stan_file))
    )
    file.copy(stan_file, source, overwrite = TRUE)
  }
  hpp_code <- get_standalone_hpp(
    source, c(stanc_inc_paths, stancflags_direct), show_warnings = !rebuild
  )

  if (!rebuild) {
    record <- observed$record$record
  } else {
    tmp_exe <- cmdstan_ext(strip_ext(source))
    if (os_is_windows() && !os_is_wsl()) {
      tmp_exe <- utils::shortPathName(tmp_exe)
    }
    # get_cmdstan_flags() split the inherited flags into words. Requote them
    # for the STANCFLAGS value handed back to make.
    stancflags_quoted <- stanc_options_to_args(
      c(stanc_options, injected), quote_values = TRUE
    )
    stancflags_make <- paste0(
      "STANCFLAGS += ", include_paths_stanc3_args(include_paths),
      paste0(
        " ", c(stancflags_quoted, make_shell_quote(inherited)), collapse = ""
      )
    )
    run_make(
      c(wsl_safe_path(repair_path(tmp_exe)), make_vars, stancflags_make), quiet
    )
    record <- new_build_record(
      request = append(
        request, list(stanc_options_inherited = as.list(inherited)), after = 3
      ),
      reported_features = reported_features_from_exe(tmp_exe),
      dependencies = observed$dependencies,
      artifact = hash_file(tmp_exe),
      builder = observed$builder,
      tbb_dir = tbb_dir_from_options(cpp_options),
      known_untracked_dependencies = untracked_dependencies(
        observed$dependencies$make_local$built_from, user_header
      )
    )
    leftover_backup <- install_executable(tmp_exe, exe, record)
    # Said once, when the record is written, and never on a no-op.
    if (length(record$known_untracked_dependencies) > 0) {
      message(untracked_dependencies_note(record$known_untracked_dependencies))
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
    info = observed$info,
    hpp_code = hpp_code
  )
}

#' Take an executable as it is
#'
#' Three outcomes. With a usable record beside it nothing is launched: the
#' hash the reader checked proves the binary is the one the record describes.
#' Without one the executable is asked to identify itself with `info`, once. A
#' version it reports admits it, unprovenanced. No version refuses it, since an
#' executable that cannot say what built it is not a CmdStan executable.
#'
#' @return A list: `record` (`NULL` when unprovenanced), `reported_features`,
#'   `version` and `artifact`, the executable's hash.
#' @noRd
adopt_executable <- function(exe_file) {
  found <- read_build_record(exe_file)
  if (found$status == "available") {
    return(snapshot_from_record(found$record))
  }
  features <- reported_features_from_exe(exe_file)
  if (is.null(features[["stan_version"]])) {
    stop(
      "'", exe_file, "' did not identify itself as a CmdStan executable. ",
      "Running it with the argument 'info' did not report a Stan version.",
      call. = FALSE
    )
  }
  list(
    record = NULL,
    reported_features = features,
    version = features[["stan_version"]],
    artifact = hash_file(exe_file)
  )
}

snapshot_from_record <- function(record) {
  list(
    record = record,
    reported_features = record$reported_features,
    version = record$builder$version,
    artifact = record$artifact
  )
}

#' What is on disk for an executable built from a Stan program
#'
#' The `observed` argument of assess_build(): the record beside the executable,
#' the installation selected now, and the sources hashed the way the writer
#' hashes them, with `info`, the `stanc --info` output they were resolved
#' from, kept for the snapshot. The constructor and the guard both come here,
#' so they cannot assemble it differently. The sources are resolved through
#' the selected stanc, so they are left unresolved when the selection is not
#' the recorded builder. That difference is a reason on its own.
#'
#' @noRd
observe_build <- function(stan_file, include_paths, user_header, exe_file) {
  observed <- list(
    exe_file = exe_file,
    record = list(status = "unavailable", reason = "no_executable"),
    builder = list(path = cmdstan_path(), version = cmdstan_version())
  )
  if (file.exists(exe_file)) {
    observed$record <- read_build_record(exe_file)
  }
  same_builder <- function() {
    recorded <- observed$record$record
    differs <- compare_build_records(recorded, observed, "builder")
    length(differs) == 0
  }
  if (observed$record$status == "available" && same_builder()) {
    observed <- c(
      observed, resolve_dependencies(stan_file, include_paths, user_header)
    )
  }
  observed
}

#' Hash what a build of this program consumes
#'
#' The Stan program, the files stanc resolves its includes to under these
#' paths, the user header and the installation's make/local, each by content
#' and by where it is. Under WSL stanc reports the includes in its own
#' spelling.
#'
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
#' What Make resolves `STANCFLAGS` to with this build's variables applied,
#' which is how make/local and CmdStan's own makefiles reach stanc. An include
#' path there is refused: `include_paths` is the one channel, so that
#' re-resolution sees every path the build saw.
#'
#' @noRd
inherited_stancflags <- function(make_vars) {
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


# what the constructor and the guard say ------------------------------------

#' What the constructor says before it builds or reuses
#'
#' @noRd
constructor_message <- function(reasons, observed) {
  if (length(reasons) == 0) {
    return("Model executable is up to date!")
  }
  if ("no_executable" %in% reasons) {
    return("Compiling Stan program...")
  }
  paste(
    c("Recompiling:", paste0("  - ", rebuild_reasons(reasons, observed))),
    collapse = "\n"
  )
}

#' Word the reasons assess_build() returns, one line each
#'
#' @noRd
rebuild_reasons <- function(reasons, observed) {
  recorded <- observed$record$record
  current <- observed$dependencies
  included_files <- function() {
    old <- recorded$dependencies$included_files
    new <- current$included_files
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
    written <- observed$record$format_version
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
        "there is no executable at '", observed$exe_file, "'"
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
      artifact_mismatch = paste0(
        "the executable does not match its build record, so it was replaced ",
        "or altered after it was built"
      ),
      artifact = "the executable was replaced after this model was created",
      cpp_options = "`cpp_options` changed",
      stanc_options = "`stanc_options` changed",
      stanc_name = "the model name changed",
      stan_file = "the Stan program changed",
      included_files = included_files(),
      user_header = paste0(
        "the user header changed (",
        (current$user_header %||% recorded$dependencies$user_header)$built_from,
        ")"
      ),
      make_local = paste0(
        "make/local changed (",
        (current$make_local %||% recorded$dependencies$make_local)$built_from,
        ")"
      ),
      builder = sprintf(
        paste0(
          "the selected CmdStan changed (built with %s at '%s'; ",
          "%s at '%s' is selected now)"
        ),
        recorded$builder$version, recorded$builder$path,
        observed$builder$version, observed$builder$path
      ),
      force_recompile = "`force_recompile = TRUE` was supplied",
      force_recompile_option = "the `cmdstanr_force_recompile` option is set"
    )
  }
  vapply(reasons, line, character(1), USE.NAMES = FALSE)
}

#' The error a guarded member raises on a stale executable
#'
#' Carries the class `cmdstanr_stale_executable` so callers can catch it by
#' what it means rather than by its text.
#'
#' @noRd
stop_stale_executable <- function(lines) {
  rlang::abort(
    paste(lines, collapse = "\n"),
    class = "cmdstanr_stale_executable",
    call = NULL
  )
}
