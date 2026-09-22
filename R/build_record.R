# The build record: a JSON file beside an executable describing how it was
# built. The schema, the writer, the reader and the public view of a record,
# stan_build_info(), all live here.

# The only format version this CmdStanR reads or writes.
build_record_format_version <- 1L

#' Where an executable's build record lives
#'
#' Beside the executable and named from its file name, never from
#' `$model_name()`, which substitutes underscores for spaces while the
#' executable path does not. Two executables in one directory therefore cannot
#' share a record.
#'
#' @param exe_file Path to the executable.
#' @return The record's path.
#' @noRd
build_record_path <- function(exe_file) {
  checkmate::assert_string(exe_file)
  file.path(
    dirname(exe_file),
    paste0(".", basename(exe_file), ".cmdstanr.json")
  )
}

#' Hash a file's contents
#'
#' Every hash in a record comes from here, so the algorithm can change in one
#' place.
#'
#' @param path Path to the file.
#' @return The hash, a string.
#' @noRd
hash_file <- function(path) {
  unname(rlang::hash_file(path))
}


# schema ------------------------------------------------------------------

# Shapes are as jsonlite::fromJSON(simplifyVector = FALSE) returns them. An
# object is a named list with no name repeated, an array is an unnamed list, and
# a scalar is an atomic vector of length one that is not NA.
is_json_object <- function(x) {
  is.list(x) && !is.null(names(x)) && all(nzchar(names(x))) &&
    !anyDuplicated(names(x))
}

is_json_array <- function(x) {
  is.list(x) && is.null(names(x))
}

record_shapes <- list(
  string = list(
    test = checkmate::test_string, requirement = "must be a string"
  ),
  flag = list(
    test = checkmate::test_flag, requirement = "must be true or false"
  ),
  object = list(test = is_json_object, requirement = "must be a JSON object"),
  array = list(test = is_json_array, requirement = "must be a JSON array")
)

cmdstan_version_pattern <- "^[0-9]+\\.[0-9]+\\.[0-9]+(-rc[0-9]+)?$"

#' Reject a build record, naming the field that failed
#'
#' @param field The field's path in the record, such as
#'   `"dependencies.stan_file.hash"`.
#' @param requirement What the field had to be, completing "build record
#'   field `x` ...".
#' @return Does not return.
#' @noRd
stop_build_record_field <- function(field, requirement) {
  stop("build record field `", field, "` ", requirement, ".", call. = FALSE)
}

#' Check one value in a build record
#'
#' @param value The value. A missing member and an explicit JSON null both
#'   arrive as `NULL`.
#' @param shape A name in `record_shapes`.
#' @param field The path the error names, such as
#'   `"dependencies.stan_file.hash"`.
#' @return `value`, invisibly.
#' @noRd
assert_record_shape <- function(value, shape, field) {
  if (is.null(value)) {
    stop_build_record_field(field, "is missing")
  }
  if (!record_shapes[[shape]]$test(value)) {
    stop_build_record_field(field, record_shapes[[shape]]$requirement)
  }
  invisible(value)
}

assert_record_member <- function(x, name, shape, field = name) {
  assert_record_shape(x[[name]], shape, field)
}

assert_record_string_array <- function(x, name, field) {
  value <- assert_record_member(x, name, "array", field)
  for (i in seq_along(value)) {
    assert_record_shape(value[[i]], "string", paste0(field, "[[", i, "]]"))
  }
  invisible(value)
}

#' Check a dependency entry in a build record
#'
#' An entry is an object with the file's `hash` and `built_from`, the path
#' it had when the build ran.
#'
#' @param value The entry.
#' @param field The entry's path in the record, such as
#'   `"dependencies.stan_file"`, for the error.
#' @return `value`, invisibly.
#' @noRd
assert_record_dependency_entry <- function(value, field) {
  assert_record_shape(value, "object", field)
  assert_record_member(value, "hash", "string", paste0(field, ".hash"))
  assert_record_member(
    value, "built_from", "string", paste0(field, ".built_from")
  )
  invisible(value)
}

#' Check a build record against the format version 1 schema
#'
#' Every check of a record's fields is here. `new_build_record()` runs it
#' before a record is written and `read_build_record()` after one is read, so
#' a record that passes when written passes when read. Fields the schema does
#' not name are ignored. No feature in `reported_features` is required, since
#' an executable that does not report one leaves it unknown; the ones present
#' must be true or false, except `stan_version`, a string.
#'
#' @param record The record, as `jsonlite::fromJSON(simplifyVector = FALSE)`
#'   returns it or as `new_build_record()` assembled it.
#' @return `record`, invisibly. A failed check is an error naming the field.
#' @noRd
validate_build_record <- function(record) {
  assert_record_shape(record, "object", "record")

  format_version <- record[["format_version"]]
  if (!checkmate::test_int(format_version, tol = 0) ||
      format_version != build_record_format_version) {
    stop_build_record_field(
      "format_version", paste0("must be ", build_record_format_version)
    )
  }

  configuration <- assert_record_member(record, "configuration", "object")
  cpp_options <- assert_record_member(
    configuration, "cpp_options", "object", "configuration.cpp_options"
  )
  for (i in seq_along(cpp_options)) {
    option_name <- names(cpp_options)[[i]]
    field <- paste0("configuration.cpp_options.", option_name)
    if (!grepl(paste0("^", make_variable_name_pattern, "$"), option_name)) {
      stop_build_record_field(field, "must be named for a Make variable")
    }
    assert_record_shape(cpp_options[[i]], "string", field)
  }
  assert_record_string_array(
    configuration, "stanc_options", "configuration.stanc_options"
  )
  assert_record_string_array(
    configuration, "stanc_options_added",
    "configuration.stanc_options_added"
  )
  assert_record_string_array(
    configuration, "stanc_options_from_make",
    "configuration.stanc_options_from_make"
  )
  stanc_name <- assert_record_member(
    configuration, "stanc_name", "string", "configuration.stanc_name"
  )
  if (!nzchar(stanc_name)) {
    stop_build_record_field("configuration.stanc_name", "must not be empty")
  }
  assert_record_string_array(
    configuration, "include_paths", "configuration.include_paths"
  )

  reported_features <- assert_record_member(
    record, "reported_features", "object"
  )
  for (i in seq_along(reported_features)) {
    feature_name <- names(reported_features)[[i]]
    shape <- if (feature_name == "stan_version") "string" else "flag"
    assert_record_shape(
      reported_features[[i]], shape, paste0("reported_features.", feature_name)
    )
  }

  dependencies <- assert_record_member(record, "dependencies", "object")
  # An absent user header or make/local means there was none.
  optional <- intersect(c("user_header", "make_local"), names(dependencies))
  for (name in c("stan_file", optional)) {
    assert_record_dependency_entry(
      dependencies[[name]], paste0("dependencies.", name)
    )
  }
  included_files <- assert_record_member(
    dependencies, "included_files", "array", "dependencies.included_files"
  )
  for (i in seq_along(included_files)) {
    assert_record_dependency_entry(
      included_files[[i]], paste0("dependencies.included_files[[", i, "]]")
    )
  }

  assert_record_member(record, "executable_hash", "string")

  cmdstan <- assert_record_member(record, "cmdstan", "object")
  assert_record_member(cmdstan, "path", "string", "cmdstan.path")
  version <- assert_record_member(
    cmdstan, "version", "string", "cmdstan.version"
  )
  # A string that is not a CmdStan version is the wrong shape, not an odd value.
  if (!grepl(cmdstan_version_pattern, version)) {
    stop_build_record_field(
      "cmdstan.version", "must be a CmdStan version such as \"2.39.0\""
    )
  }

  assert_record_member(record, "tbb_dir", "string")

  untracked <- assert_record_member(record, "untracked_dependencies", "array")
  for (i in seq_along(untracked)) {
    field <- paste0("untracked_dependencies[[", i, "]]")
    entry <- assert_record_shape(untracked[[i]], "object", field)
    kind <- assert_record_member(
      entry, "kind", "string", paste0(field, ".kind")
    )
    if (!kind %in% c("make_local_include", "user_header_include")) {
      stop_build_record_field(
        paste0(field, ".kind"),
        "must be \"make_local_include\" or \"user_header_include\""
      )
    }
    assert_record_member(
      entry, "detected_in", "string", paste0(field, ".detected_in")
    )
  }

  invisible(record)
}

#' Assemble a build record
#'
#' The one place a record is built. The fields go in the schema's order so the
#' written JSON reads that way too.
#'
#' @param configuration The options the build used: `cpp_options` as the Make
#'   assignments `parsed_cpp_options()` returns, one per name; `stanc_options`,
#'   `stanc_options_added` and `stanc_options_from_make` as the argument
#'   vectors stanc receives; `stanc_name`; and `include_paths` as searched.
#' @param reported_features What `reported_features_from_exe()` returned.
#' @param dependencies The hashed sources, as `resolve_dependencies()` returns
#'   them.
#' @param executable_hash The hash of the executable the record describes.
#' @param cmdstan A list with the installation's `path` and `version`.
#' @param tbb_dir What `tbb_dir_from_options()` returned.
#' @param untracked_dependencies What `untracked_dependencies()` returned.
#' @return The record, validated.
#' @noRd
new_build_record <- function(configuration, reported_features, dependencies,
                             executable_hash, cmdstan, tbb_dir,
                             untracked_dependencies = list()) {
  record <- list(
    format_version = build_record_format_version,
    configuration = configuration,
    reported_features = reported_features,
    dependencies = dependencies,
    executable_hash = executable_hash,
    cmdstan = cmdstan,
    tbb_dir = tbb_dir,
    untracked_dependencies = untracked_dependencies
  )
  validate_build_record(record)
  record
}


# building a record at compile time ---------------------------------------

#' Run an executable's `info` command
#'
#' @param exe_file Path to the executable.
#' @param tbb_dir The TBB directory the build resolved, or `NULL` when there
#'   is no usable record.
#' @return The `processx::run()` result. A non-zero exit is not an error.
#' @noRd
run_exe_info <- function(exe_file, tbb_dir = NULL) {
  withr::with_path(
    c(
      toolchain_PATH_env_var(),
      tbb_launch_path(tbb_dir)
    ),
    wsl_compatible_run(
      command = wsl_safe_path(exe_file),
      args = "info",
      echo = is_verbose_mode(),
      error_on_status = FALSE
    )
  )
}

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

#' The build features the executable reports
#'
#' Runs `<exe> info` and keeps the flags that parsed as true or false, plus a
#' `stan_version` of three dotted integers. Anything else the executable
#' prints is dropped, since the schema would reject it. A failed run leaves
#' every feature unknown rather than failing the build.
#'
#' @param exe_file Path to the executable.
#' @param tbb_dir The TBB directory the build resolved, or `NULL` when there
#'   is no usable record.
#' @return A named list of what was kept, empty when nothing was.
#' @noRd
reported_features_from_exe <- function(exe_file, tbb_dir = NULL) {
  unknown <- structure(list(), names = character())
  tryCatch({
    result <- run_exe_info(exe_file, tbb_dir)
    if (result$status != 0) {
      unknown
    } else {
      info <- parse_exe_info_string(result$stdout)
      version <- grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", info[["stan_version"]])
      keep <- nzchar(names(info)) & (
        vapply(info, checkmate::test_flag, logical(1)) |
          (names(info) == "stan_version" & version)
      )
      info[keep]
    }
  }, error = function(e) unknown)
}

#' Where the record says the TBB is
#'
#' We use `TBB_LIB` if the call set it, otherwise `TBB_BIN`, otherwise the
#' installation's own copy, which is the order the makefile uses when it
#' links. The options are read the way make reads them, so the last
#' assignment wins and `FALSE` counts as empty. A relative directory is
#' relative to the installation, since that's where make runs. We take the
#' value as written; `assert_valid_cpp_options()` has already rejected a
#' make expression, because nothing here could expand it.
#'
#' The two variables can also reach make from `make/local`,
#' `~/.config/stan/make.local` or the environment, and we don't look there.
#' So a build set up that way links against a different TBB than the record
#' names, which is the installation's. On Windows that means the launch puts the
#' installation's TBB on PATH, which is what happened before too. We decided
#' not to ask make for the real answer for now and can revisit if anyone
#' needs it.
#'
#' @param cpp_options The call's `cpp_options`, as given.
#' @return The directory. Under WSL it is the Windows path.
#' @noRd
tbb_dir_from_options <- function(cpp_options) {
  assigned <- parsed_cpp_options(cpp_options)
  candidates <- c(
    assigned[["TBB_LIB"]], assigned[["TBB_BIN"]], "stan/lib/stan_math/lib/tbb"
  )
  tbb <- candidates[nzchar(candidates)][1]
  if (!grepl("^(/|[A-Za-z]:)", tbb)) {
    tbb <- file.path(cmdstan_path(), tbb)
  }
  repair_path(wsl_safe_path(tbb, revert = TRUE))
}

#' Dependencies the build can see exist but cannot resolve
#'
#' A `make/local` that includes another makefile, and a user header that
#' includes another header, both pull in files nothing here can enumerate. An
#' empty list means nothing was detected, never that the record is complete.
#'
#' @param make_local Path to the installation's `make/local`, or `NULL` when
#'   there is none.
#' @param user_header Path to the user header, or `NULL`.
#' @return A list with one entry per detection, each with `kind` and
#'   `detected_in`.
#' @noRd
untracked_dependencies <- function(make_local = NULL, user_header = NULL) {
  detectors <- list(
    make_local_include = list(
      path = make_local, pattern = "^\\s*(?:-?include|sinclude)\\b"
    ),
    user_header_include = list(
      path = user_header, pattern = "^\\s*#\\s*include\\s*\""
    )
  )
  detected <- list()
  for (kind in names(detectors)) {
    path <- detectors[[kind]]$path
    if (is.null(path)) {
      next
    }
    lines <- readLines(path, warn = FALSE)
    if (any(grepl(detectors[[kind]]$pattern, lines, perl = TRUE))) {
      detected[[length(detected) + 1]] <- list(kind = kind, detected_in = path)
    }
  }
  detected
}

untracked_dependency_descriptions <- c(
  make_local_include = "make/local includes another makefile",
  user_header_include = "the user header includes other headers"
)

#' The one line a build prints when it has dependencies we cannot track
#'
#' @param untracked What `untracked_dependencies()` returned, non-empty.
#' @return The line, a string.
#' @noRd
untracked_dependencies_note <- function(untracked) {
  kinds <- vapply(untracked, `[[`, character(1), "kind")
  paste0(
    "Note: this model has dependencies CmdStanR does not track: ",
    paste(untracked_dependency_descriptions[kinds], collapse = ", "),
    ". If those files change, rebuild with force_recompile = TRUE."
  )
}


# writing and reading -----------------------------------------------------

#' Write a build record beside its executable
#'
#' Written to a temporary file and renamed into place, so a reader never
#' sees a half-written record. Because `auto_unbox = TRUE` writes a
#' length-one vector as a JSON scalar, the schema stores arrays as lists and
#' scalars as length-one vectors, so a single included file is still written
#' as an array.
#'
#' @param record A record `new_build_record()` assembled.
#' @param exe_file Path to the executable it describes.
#' @return The record's path, invisibly.
#' @noRd
write_build_record <- function(record, exe_file) {
  validate_build_record(record)
  path <- build_record_path(exe_file)
  staged <- tempfile(pattern = basename(path), tmpdir = dirname(path))
  withr::defer(unlink(staged, expand = FALSE))
  jsonlite::write_json(
    record, staged, auto_unbox = TRUE, pretty = TRUE, digits = NA
  )
  if (!isTRUE(suppressWarnings(file.rename(staged, path)))) {
    stop("Could not write the build record to ", path, ".", call. = FALSE)
  }
  invisible(path)
}

#' Read the build record beside an executable
#'
#' A record that cannot be used is not an error, the result says why
#' instead. The format version is checked before anything else, so a record
#' in a format this version of CmdStanR does not read is never checked
#' against the current schema.
#'
#' @param exe_file Path to the executable, which must exist.
#' @return A list with `status` `"available"` and the `record`, or `status`
#'   `"unavailable"` and a `reason`: `"missing"` when there is no record,
#'   `"unreadable"` when the file is not JSON or fails a field check,
#'   `"unsupported_format"` with the `format_version` found, or
#'   `"executable_mismatch"` when the record's hash is not the executable's.
#'   Only an available record's contents are returned.
#' @noRd
read_build_record <- function(exe_file) {
  checkmate::assert_file_exists(exe_file)
  path <- build_record_path(exe_file)
  if (!file.exists(path)) {
    return(list(status = "unavailable", reason = "missing"))
  }
  unreadable <- list(status = "unavailable", reason = "unreadable")

  record <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (!is.list(record) || !"format_version" %in% names(record)) {
    return(unreadable)
  }
  format_version <- record[["format_version"]]
  if (!checkmate::test_int(format_version, tol = 0)) {
    return(unreadable)
  }
  if (format_version != build_record_format_version) {
    return(list(
      status = "unavailable",
      reason = "unsupported_format",
      format_version = format_version
    ))
  }

  accepted <- tryCatch({
    validate_build_record(record)
    TRUE
  }, error = function(e) FALSE)
  if (!accepted) {
    return(unreadable)
  }
  if (!identical(hash_file(exe_file), record[["executable_hash"]])) {
    return(list(status = "unavailable", reason = "executable_mismatch"))
  }

  list(status = "available", record = record)
}

#' Check that the record beside an executable describes it
#'
#' `install_executable()` calls this after its last rename. The test for
#' interleaved writes calls it directly.
#'
#' @param exe_file Path to the executable.
#' @return The record, invisibly. A record that cannot be used is an error
#'   naming the reason.
#' @noRd
verify_build_record <- function(exe_file) {
  result <- read_build_record(exe_file)
  if (result$status != "available") {
    stop(
      "The build record beside '", exe_file, "' does not describe it (",
      result$reason, ").",
      call. = FALSE
    )
  }
  invisible(result$record)
}

# What the rebuild check compares: one function per compared field, in the
# order the reasons are reported. Each takes a record, or the list
# assess_build() builds for the current state, and returns the value to
# compare.
build_record_comparisons <- list(
  cpp_options = function(x) {
    supplied <- x[["configuration"]][["cpp_options"]]
    supplied[order(names(supplied))]
  },
  stanc_options = function(x) x[["configuration"]][["stanc_options"]],
  stanc_name = function(x) x[["configuration"]][["stanc_name"]],
  stan_file = function(x) x[["dependencies"]][["stan_file"]][["hash"]],
  included_files = function(x) {
    lapply(x[["dependencies"]][["included_files"]], `[[`, "hash")
  },
  user_header = function(x) {
    x[["dependencies"]][["user_header"]][c("hash", "built_from")]
  },
  make_local = function(x) x[["dependencies"]][["make_local"]]["hash"],
  executable = function(x) x[["executable_hash"]],
  cmdstan = function(x) x[["cmdstan"]][c("path", "version")]
)

#' Compare a recorded build against the current one
#'
#' @param recorded The record read from disk.
#' @param current The list `assess_build()` builds for the current state.
#' @param rows Which of `build_record_comparisons` to apply.
#' @return The names in `rows` whose values differ, in that order.
#' @noRd
compare_build_records <- function(recorded, current,
                                  rows = names(build_record_comparisons)) {
  differs <- vapply(
    build_record_comparisons[rows],
    function(value) !identical(value(recorded), value(current)),
    logical(1)
  )
  rows[differs]
}

#' Decide whether an executable is current
#'
#' @param expected What the executable must match: `configuration`, the
#'   options it should have been built with, which `build_executable()`
#'   resolves from its arguments and `assert_current()` takes from the model
#'   object's record; and `executable_hash`, the executable the model object
#'   was created with, or `NULL` from `build_executable()`, where there is no
#'   model object yet.
#' @param current What is on disk now: `record`, as `read_build_record()`
#'   returned it; `dependencies`, the current files hashed as the writer
#'   hashes them, or `NULL` when they were not resolved; and `cmdstan`, the
#'   installation in use.
#' @return The reasons to rebuild as a character vector, empty when the
#'   executable is current. If the record cannot be used, that is the only
#'   reason, since there is nothing to compare against. Otherwise every
#'   compared field that differs is a reason. Comparing `executable_hash` is
#'   what catches an executable that another process rebuilt: the record
#'   beside it matches the new executable, and only the model object
#'   remembers the old one.
#' @noRd
assess_build <- function(expected, current) {
  if (current$record$status != "available") {
    return(current$record$reason)
  }
  recorded <- current$record$record
  now <- list(
    configuration = expected$configuration,
    dependencies = current$dependencies,
    executable_hash = expected$executable_hash %||% recorded$executable_hash,
    cmdstan = current$cmdstan
  )
  rows <- names(build_record_comparisons)
  if (is.null(current$dependencies)) {
    rows <- setdiff(
      rows, c("stan_file", "included_files", "user_header", "make_local")
    )
  }
  compare_build_records(recorded, now, rows)
}


# the public view --------------------------------------------------------

#' What is known about how a CmdStan executable was built
#'
#' @export
#' @description When CmdStanR builds a model it writes a build record next to
#'   the executable containing the options the build was asked for, the
#'   files it read, the CmdStan installation it used, and what the executable
#'   reports about itself. `stan_build_info()` reads that record back into \R.
#'   When there is no usable record it says why, and reports only the
#'   information we can obtain by querying the executable itself (using
#'   CmdStan's `<exe> info`).
#'
#'   The [`$build_info()`][model-method-build_info] method of a [`CmdStanModel`]
#'   object runs `stan_build_info()` internally for the model's executable.
#'
#' @param exe_file (string) Path to the executable.
#' @param x (`stan_build_info`) The object to print.
#' @param ... Not used.
#'
#' @return A list of class `"stan_build_info"`. Two fields are always there:
#'
#' * `record`: A list containing `status`, either `"available"` or
#' `"unavailable"`, and `reason`, which is `NULL` when the record is available
#' and otherwise one of `"missing"` (no record beside the executable),
#' `"unreadable"` (a record that could not be read), `"executable_mismatch"`
#' (the record describes a different executable, so the one at this path has
#' changed since the record was written) or `"unsupported_format"` (written by
#' a CmdStanR that stores records differently, in which case the result also
#' has a `format_version` field).
#'
#' * `reported_features`: A list containing what the executable reports about
#' its own build. `stan_threads`, `stan_mpi`, `stan_opencl` and
#' `stan_no_range_checks` are each `TRUE`, `FALSE`, or `NA` when the executable
#' did not report the feature, and `stan_version` is the Stan version the
#' executable reports being compiled with, or `NA` when it did not report one.
#' These come from the record when it is available and otherwise from querying
#' the executable.
#'
#' When the build record is available, there are four more fields:
#'
#' * `configuration`: A list of the options the model was created with.
#'   * `cpp_options`: the list of options in their Make spelling, as
#'   `$cpp_options()` reports them. For example, `list(stan_threads = TRUE)`
#'   comes back as `list(STAN_THREADS = "true")`.
#'   * `stanc_options`: the flags as given to stanc, in order.
#'   For example, `list(O1 = TRUE)` and `list("O1")` both come back as
#'   `list("--O1")`.
#'   * `stanc_options_from_make`: the flags `make/local` added to the stanc
#'   call through `STANCFLAGS`. A flag that `stanc_options` also sets is not
#'   repeated here since `stanc_options` takes precedence.
#'   * `include_paths`: a character vector of the directories searched for
#'   included files. When none were provided but the Stan program has includes
#'   this is set to the program's own directory.
#'
#' * `dependencies`: A list describing the files the build read. Contains sublists
#' `stan_file`, `included_files`, `user_header` and `make_local`. `user_header`
#' and `make_local` are `NULL` when the build had none. `included_files` holds
#' one entry per file. Each entry has two fields: `built_from`, the path the
#' file had when the build ran, and `exists`, whether that path exists now. A
#' path that no longer exists is not necessarily a problem. For example, an \R
#' package may build its models at install time in a temporary directory that
#' is gone by the time the model is used.
#'
#' * `cmdstan`: A list containing the `path` and `version` of the CmdStan
#' installation that built the executable, and whether that path still `exists`.
#' This version and `reported_features$stan_version` will typically agree except
#' when using a release candidate (`cmdstan$version` will have a
#' release-candidate suffix whereas `reported_features$stan_version` comes from
#' the Stan library headers the executable was compiled against and will not).
#'
#' * `untracked_dependencies`: A list of files the build depended on that CmdStanR
#' cannot follow, so a change to them does not automatically trigger a rebuild.
#' An empty list means nothing of the kind was found. Each file is reported as
#' a sublist with two fields: `kind`, which is `"make_local_include"` when
#' `make/local` includes another makefile or `"user_header_include"` when the
#' user header includes other headers, and `detected_in`, the file the include
#' was found in.
#'
#' The result leaves out some of what the record holds: the file hashes the
#' rebuild check compares, the stanc flags CmdStanR added itself, the model
#' name given to stanc, and the TBB directory.
#'
#' Absent items and empty items have different interpretations. A field missing
#' from the result means there was no usable record to read it from. An empty
#' list is a recorded empty value, such as no untracked dependencies.
#'
#' @seealso [cmdstan_model()], [model-method-build_info]
#' @examples
#' \dontrun{
#' exe <- compile_stan_file(
#'   file.path(cmdstan_path(), "examples/bernoulli/bernoulli.stan"),
#'   cpp_options = list(stan_threads = TRUE),
#'   stanc_options = list("O1")
#' )
#' info <- stan_build_info(exe)
#' info
#' info$configuration
#' info$reported_features$stan_threads
#' info$dependencies$stan_file
#' }
#'
stan_build_info <- function(exe_file) {
  exe_file <- resolve_path(exe_file)
  found <- inspect_executable(exe_file)
  info <- list(
    record = list(status = found$status, reason = found$reason),
    reported_features = public_reported_features(found$reported_features)
  )
  if (found$status == "available") {
    record <- found$record
    info$configuration <- list(
      cpp_options = record$configuration$cpp_options,
      stanc_options = record$configuration$stanc_options,
      stanc_options_from_make = record$configuration$stanc_options_from_make,
      include_paths = as.character(unlist(record$configuration$include_paths))
    )
    info$dependencies <- public_dependencies(record$dependencies)
    info$cmdstan <- list(
      path = record$cmdstan$path,
      version = record$cmdstan$version,
      exists = dir.exists(record$cmdstan$path)
    )
    info$untracked_dependencies <- public_untracked_dependencies(
      record$untracked_dependencies
    )
  } else if (found$reason == "unsupported_format") {
    info$format_version <- found$format_version
  }
  structure(info, class = "stan_build_info")
}

# The four flags `<exe> info` prints, in its order.
reported_feature_flags <- c(
  "stan_threads", "stan_mpi", "stan_opencl", "stan_no_range_checks"
)

#' The reported features with every name present
#'
#' The record omits a feature the executable did not report. Here the same
#' feature is `NA`, so the result always has the same five names and the
#' user can check `is.na()`.
#'
#' @param reported The record's `reported_features`, or what
#'   `reported_features_from_exe()` returned.
#' @return The four flags and `stan_version`, `NA` where unknown.
#' @noRd
public_reported_features <- function(reported) {
  features <- lapply(reported_feature_flags, function(name) {
    reported[[name]] %||% NA
  })
  names(features) <- reported_feature_flags
  features$stan_version <- reported[["stan_version"]] %||% NA_character_
  features
}

#' Each dependency's recorded path and whether it exists now
#'
#' The file hashes are not copied over, since they are only meaningful to
#' the rebuild check. `user_header` and `make_local` are `NULL` when the
#' build had none, so the result always has the same four names.
#'
#' @param dependencies The record's `dependencies`.
#' @return The four entries, each with `built_from` and `exists`.
#' @noRd
public_dependencies <- function(dependencies) {
  entry <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    list(built_from = x$built_from, exists = file.exists(x$built_from))
  }
  list(
    stan_file = entry(dependencies[["stan_file"]]),
    included_files = lapply(dependencies[["included_files"]], entry),
    user_header = entry(dependencies[["user_header"]]),
    make_local = entry(dependencies[["make_local"]])
  )
}

#' One entry per distinct pair, ordered by kind then path
#'
#' @param untracked The record's `untracked_dependencies`.
#' @return A list of `kind` and `detected_in` pairs.
#' @noRd
public_untracked_dependencies <- function(untracked) {
  entries <- lapply(untracked, function(x) {
    list(kind = x[["kind"]], detected_in = x[["detected_in"]])
  })
  kind <- vapply(entries, `[[`, character(1), "kind")
  detected_in <- vapply(entries, `[[`, character(1), "detected_in")
  ordered <- entries[order(kind, detected_in)]
  ordered[!duplicated(ordered)]
}

#' @rdname stan_build_info
#' @export
print.stan_build_info <- function(x, ...) {
  cat(build_record_status_line(x), "\n", sep = "")
  cat("Reported features:\n")
  for (name in names(x$reported_features)) {
    value <- x$reported_features[[name]]
    cat("  ", name, ": ", if (is.na(value)) "unknown" else value, "\n", sep = "")
  }
  if (x$record$status != "available") {
    return(invisible(x))
  }

  cat("Configuration:\n")
  cpp_options <- x$configuration$cpp_options
  cat("  cpp_options: ", if (length(cpp_options) == 0) "none" else
    paste(names(cpp_options), unlist(cpp_options), sep = "=", collapse = " "),
    "\n", sep = "")
  stanc_options <- unlist(x$configuration$stanc_options)
  cat("  stanc_options: ", if (length(stanc_options) == 0) "none" else
    paste(stanc_options, collapse = " "), "\n", sep = "")
  from_make <- unlist(x$configuration$stanc_options_from_make)
  cat("  stanc_options_from_make: ", if (length(from_make) == 0) "none" else
    paste(from_make, collapse = " "), "\n", sep = "")
  include_paths <- x$configuration$include_paths
  cat("  include_paths: ", if (length(include_paths) == 0) "none" else
    paste(include_paths, collapse = ", "), "\n", sep = "")

  cat("Dependencies:\n")
  path_line <- function(label, entry) {
    if (is.null(entry)) {
      return(invisible())
    }
    gone <- if (entry$exists) "" else " (no longer exists)"
    cat("  ", label, ": ", entry$built_from, gone, "\n", sep = "")
  }
  path_line("stan_file", x$dependencies$stan_file)
  for (included in x$dependencies$included_files) {
    path_line("included_file", included)
  }
  path_line("user_header", x$dependencies$user_header)
  path_line("make_local", x$dependencies$make_local)

  cmdstan <- x$cmdstan
  cat("CmdStan ", cmdstan$version, " at ", cmdstan$path,
    if (cmdstan$exists) "" else " (no longer exists)", "\n", sep = "")

  if (length(x$untracked_dependencies) > 0) {
    cat("Dependencies CmdStanR does not track:\n")
    for (entry in x$untracked_dependencies) {
      cat("  ", untracked_dependency_descriptions[[entry$kind]], " (",
        entry$detected_in, ")\n", sep = "")
    }
  }
  invisible(x)
}

#' The first printed line, from the record's status and reason
#'
#' @param x A `stan_build_info` object.
#' @return The line, a string.
#' @noRd
build_record_status_line <- function(x) {
  if (x$record$status == "available") {
    return("Build record: available")
  }
  switch(x$record$reason,
    missing = paste0(
      "Build record: not found. Only what the executable says about itself ",
      "is known."
    ),
    unreadable = paste0(
      "Build record: could not be read. Rebuilding the executable writes a ",
      "new one."
    ),
    executable_mismatch =
      "Build record: does not match. Executable changed after the build.",
    unsupported_format = if (x$format_version > build_record_format_version) {
      paste0(
        "Build record: format ", x$format_version, " (newer CmdStanR). ",
        "Upgrade CmdStanR to read it."
      )
    } else {
      paste0(
        "Build record: format ", x$format_version, " (older CmdStanR). ",
        "Rebuild the executable to replace it."
      )
    }
  )
}
