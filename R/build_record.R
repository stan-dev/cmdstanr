# The build record: a JSON file beside an executable describing how it was
# built. The schema, the writer and the reader all live here.

# The only format version this cmdstanr reads or writes.
build_record_format_version <- 1L

#' Where an executable's build record lives
#'
#' Beside the executable and named from its file name, never from
#' `$model_name()`, which substitutes underscores for spaces while the
#' executable path does not. Two executables in one directory therefore cannot
#' share a record.
#'
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
#' @noRd
stop_build_record_field <- function(field, requirement) {
  stop("build record field `", field, "` ", requirement, ".", call. = FALSE)
}

# `field` is the path the error names, such as "dependencies.stan_file.hash".
# A missing member and an explicit JSON null both arrive as NULL.
record_shape <- function(value, shape, field) {
  if (is.null(value)) {
    stop_build_record_field(field, "is missing")
  }
  if (!record_shapes[[shape]]$test(value)) {
    stop_build_record_field(field, record_shapes[[shape]]$requirement)
  }
  invisible(value)
}

record_member <- function(x, name, shape, field = name) {
  record_shape(x[[name]], shape, field)
}

record_string_array <- function(x, name, field) {
  value <- record_member(x, name, "array", field)
  for (i in seq_along(value)) {
    record_shape(value[[i]], "string", paste0(field, "[[", i, "]]"))
  }
  invisible(value)
}

#' A file the build consumed, identified by content and by where it then was
#'
#' @noRd
record_dependency_entry <- function(value, field) {
  record_shape(value, "object", field)
  record_member(value, "hash", "string", paste0(field, ".hash"))
  record_member(value, "built_from", "string", paste0(field, ".built_from"))
}

#' Check a build record against the format version 1 schema
#'
#' This function is the schema. The constructor and the reader both call it and
#' nothing else checks a record's fields, so a record one caller can use is a
#' record every caller can use. Fields are checked in the order the record
#' holds them and the first failure names its field. Members the schema
#' does not name are ignored rather than rejected. `reported_features` is
#' checked for shape and never for membership, because an absent feature means
#' unknown and the set CmdStan reports is the binary's to decide.
#'
#' @noRd
validate_build_record <- function(record) {
  record_shape(record, "object", "record")

  format_version <- record[["format_version"]]
  if (!checkmate::test_int(format_version, tol = 0) ||
      format_version != build_record_format_version) {
    stop_build_record_field(
      "format_version", paste0("must be ", build_record_format_version)
    )
  }

  request <- record_member(record, "request", "object")
  cpp_options <- record_member(
    request, "cpp_options_supplied", "object", "request.cpp_options_supplied"
  )
  for (i in seq_along(cpp_options)) {
    option_name <- names(cpp_options)[[i]]
    field <- paste0("request.cpp_options_supplied.", option_name)
    if (!grepl(paste0("^", make_variable_name_pattern, "$"), option_name)) {
      stop_build_record_field(field, "must be named for a Make variable")
    }
    record_shape(cpp_options[[i]], "string", field)
  }
  record_string_array(
    request, "stanc_options_supplied", "request.stanc_options_supplied"
  )
  record_string_array(
    request, "stanc_options_injected", "request.stanc_options_injected"
  )
  stanc_name <- record_member(
    request, "stanc_name", "string", "request.stanc_name"
  )
  if (!nzchar(stanc_name)) {
    stop_build_record_field("request.stanc_name", "must not be empty")
  }
  record_string_array(request, "include_paths", "request.include_paths")

  reported_features <- record_member(record, "reported_features", "object")
  for (i in seq_along(reported_features)) {
    feature_name <- names(reported_features)[[i]]
    shape <- if (feature_name == "stan_version") "string" else "flag"
    record_shape(
      reported_features[[i]], shape, paste0("reported_features.", feature_name)
    )
  }

  dependencies <- record_member(record, "dependencies", "object")
  # An absent user header or make/local means there was none.
  optional <- intersect(c("user_header", "make_local"), names(dependencies))
  for (name in c("stan_file", optional)) {
    record_dependency_entry(dependencies[[name]], paste0("dependencies.", name))
  }
  included_files <- record_member(
    dependencies, "included_files", "array", "dependencies.included_files"
  )
  for (i in seq_along(included_files)) {
    record_dependency_entry(
      included_files[[i]], paste0("dependencies.included_files[[", i, "]]")
    )
  }

  record_member(record, "artifact", "string")

  builder <- record_member(record, "builder", "object")
  record_member(builder, "path", "string", "builder.path")
  version <- record_member(builder, "version", "string", "builder.version")
  # A string that is not a CmdStan version is the wrong shape, not an odd value.
  if (!grepl(cmdstan_version_pattern, version)) {
    stop_build_record_field(
      "builder.version", "must be a CmdStan version such as \"2.39.0\""
    )
  }

  record_member(record, "tbb_dir", "string")

  untracked <- record_member(record, "known_untracked_dependencies", "array")
  for (i in seq_along(untracked)) {
    field <- paste0("known_untracked_dependencies[[", i, "]]")
    entry <- record_shape(untracked[[i]], "object", field)
    kind <- record_member(entry, "kind", "string", paste0(field, ".kind"))
    if (!kind %in% c("make_local_include", "user_header_include")) {
      stop_build_record_field(
        paste0(field, ".kind"),
        "must be \"make_local_include\" or \"user_header_include\""
      )
    }
    record_member(
      entry, "detected_in", "string", paste0(field, ".detected_in")
    )
  }

  invisible(record)
}

#' Assemble a build record
#'
#' The one place a record is built. `format_version` comes first and the rest
#' follow the schema's order, so the written JSON reads in that order too.
#' `request` arrives in the forms the record compares: `cpp_options_supplied`
#' as canonical Make assignments, one per name, and the two stanc option lists
#' as the argument vectors stanc receives.
#'
#' @noRd
new_build_record <- function(request, reported_features, dependencies, artifact,
                             builder, tbb_dir,
                             known_untracked_dependencies = list()) {
  record <- list(
    format_version = build_record_format_version,
    request = request,
    reported_features = reported_features,
    dependencies = dependencies,
    artifact = artifact,
    builder = builder,
    tbb_dir = tbb_dir,
    known_untracked_dependencies = known_untracked_dependencies
  )
  validate_build_record(record)
  record
}


# building a record at compile time ---------------------------------------

#' The build features the executable reports
#'
#' Keeps the entries reported as a single logical under a name, plus a
#' `stan_version` of three dotted integers, so output the record cannot hold
#' cannot fail the writer. Any failure leaves every feature unknown rather than
#' failing the build.
#'
#' @noRd
reported_features_from_exe <- function(exe_file) {
  unknown <- structure(list(), names = character())
  tryCatch({
    result <- run_info_cli(exe_file)
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

#' The TBB directory this build resolved
#'
#' Asked of Make with the build's own variables, so a `TBB_LIB` supplied on the
#' call is seen, and for both variables in one call. Make returns `TBB_LIB` as it
#' was written, so a relative one is resolved against the directory Make ran in,
#' and as the shell prints it, so repeated spaces and glob characters in it are
#' not preserved.
#'
#' @noRd
tbb_dir_from_make <- function(make_vars) {
  withr::with_envvar(
    c(HOME = short_path(Sys.getenv("HOME"))),
    stdout <- wsl_compatible_run(
      command = "make",
      args = c("-s", make_vars, "print-TBB_BIN_ABSOLUTE_PATH", "print-TBB_LIB"),
      wd = cmdstan_path()
    )$stdout
  )
  tbb_lib <- parse_make_print_flag("TBB_LIB", stdout)
  if (!nzchar(tbb_lib)) {
    parse_make_print_flag("TBB_BIN_ABSOLUTE_PATH", stdout)
  } else if (grepl("^(/|[A-Za-z]:)", tbb_lib)) {
    tbb_lib
  } else {
    repair_path(file.path(cmdstan_path(), tbb_lib))
  }
}

#' Dependencies the build can see exist but cannot resolve
#'
#' A `make/local` that includes another makefile, and a user header that
#' includes another header, both pull in files nothing here can enumerate. An
#' empty list means nothing was detected, never that the record is complete.
#'
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
#' @noRd
untracked_dependencies_note <- function(untracked) {
  kinds <- vapply(untracked, `[[`, character(1), "kind")
  paste0(
    "Note: this model has dependencies cmdstanr does not track: ",
    paste(untracked_dependency_descriptions[kinds], collapse = ", "),
    ". If those files change, rebuild with force_recompile = TRUE."
  )
}


# writing and reading -----------------------------------------------------

#' Write a build record beside its executable
#'
#' Staged in the same directory and renamed into place so a reader never meets
#' a half-written record, and the staging file is removed on the way out whether
#' or not it got that far. A failed rename warns, and the warning is suppressed
#' so that `warn = 2` cannot pre-empt the error below. `auto_unbox` writes a
#' length-one vector as a JSON scalar, which is why the schema holds every
#' array as a list and every scalar as a length-one vector: a one-element
#' `included_files` still writes as an array. No field the schema names is ever
#' `NULL` or `NA`, since an unknown state is an absent key.
#'
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
#' Returns the reason a record cannot be used instead of signalling it, because
#' every one of those reasons is an ordinary outcome. The version is checked
#' first and on its own, so a record written in a format we do not read is
#' never measured against the current schema. Anything failing a field check is
#' unreadable whole and comes back with no `format_version`, and a record whose
#' hash does not match the executable comes back with nothing it contains.
#'
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
  if (!identical(hash_file(exe_file), record[["artifact"]])) {
    return(list(status = "unavailable", reason = "artifact_mismatch"))
  }

  list(status = "available", record = record)
}

#' Check that the record beside an executable describes it
#'
#' The check the install transaction ends with. The test for interleaved writes
#' runs it on its own.
#'
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

#' Compare a recorded build against the current one
#'
#' Returns the name of every compared field whose value differs between the
#' two records, in the order the design's table lists them. Every field is
#' checked, and nothing stops at the first difference, so a caller who changed
#' more than one thing is told about all of them. Each entry below extracts
#' the value a row compares, so the list is the table.
#'
#' @noRd
compare_build_records <- function(recorded, current) {
  sorted <- function(x) x[order(names(x))]
  optional_dependency <- function(record, name, fields) {
    record$dependencies[[name]][fields]
  }
  compared <- list(
    cpp_options = function(x) sorted(x$request$cpp_options_supplied),
    stanc_options = function(x) x$request$stanc_options_supplied,
    stanc_name = function(x) x$request$stanc_name,
    stan_file = function(x) x$dependencies$stan_file$hash,
    included_files = function(x) {
      lapply(x$dependencies$included_files, `[[`, "hash")
    },
    user_header = function(x) {
      optional_dependency(x, "user_header", c("hash", "built_from"))
    },
    make_local = function(x) optional_dependency(x, "make_local", "hash"),
    artifact = function(x) x$artifact,
    builder = function(x) x$builder[c("path", "version")]
  )
  differs <- vapply(
    compared,
    function(value) !identical(value(recorded), value(current)),
    logical(1)
  )
  names(compared)[differs]
}
