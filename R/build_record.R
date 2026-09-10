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
# object is a named list, an array is an unnamed list, and a scalar is an atomic
# vector of length one that is not NA.

#' Reject a build record, naming the field that failed
#'
#' @noRd
stop_build_record_field <- function(field, requirement) {
  stop("build record field `", field, "` ", requirement, ".", call. = FALSE)
}

#' Fetch a member the schema requires
#'
#' @noRd
record_member <- function(x, name, field) {
  if (!name %in% names(x)) {
    stop_build_record_field(field, "is missing")
  }
  x[[name]]
}

#' @noRd
require_record_object <- function(value, field) {
  if (!is.list(value) || is.null(names(value)) || !all(nzchar(names(value)))) {
    stop_build_record_field(field, "must be a JSON object")
  }
}

#' @noRd
require_record_array <- function(value, field) {
  if (!is.list(value) || !is.null(names(value))) {
    stop_build_record_field(field, "must be a JSON array")
  }
}

#' @noRd
require_record_string <- function(value, field) {
  if (!checkmate::test_string(value)) {
    stop_build_record_field(field, "must be a string")
  }
}

#' @noRd
require_record_flag <- function(value, field) {
  if (!checkmate::test_flag(value)) {
    stop_build_record_field(field, "must be true or false")
  }
}

#' @noRd
require_record_string_array <- function(value, field) {
  require_record_array(value, field)
  for (i in seq_along(value)) {
    require_record_string(value[[i]], paste0(field, "[[", i, "]]"))
  }
}

#' A file the build consumed, identified by content and by where it then was
#'
#' @noRd
require_dependency_entry <- function(value, field) {
  require_record_object(value, field)
  require_record_string(
    record_member(value, "hash", paste0(field, ".hash")),
    paste0(field, ".hash")
  )
  require_record_string(
    record_member(value, "built_from", paste0(field, ".built_from")),
    paste0(field, ".built_from")
  )
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
  checkmate::assert_list(record, .var.name = "record")

  format_version <- record_member(record, "format_version", "format_version")
  if (!checkmate::test_int(format_version) ||
      format_version != build_record_format_version) {
    stop_build_record_field(
      "format_version",
      paste0("must be ", build_record_format_version)
    )
  }

  request <- record_member(record, "request", "request")
  require_record_object(request, "request")

  cpp_options <- record_member(
    request, "cpp_options_supplied", "request.cpp_options_supplied"
  )
  require_record_object(cpp_options, "request.cpp_options_supplied")
  for (i in seq_along(cpp_options)) {
    option_name <- names(cpp_options)[[i]]
    field <- paste0("request.cpp_options_supplied.", option_name)
    if (!grepl(paste0("^", make_variable_name_pattern, "$"), option_name)) {
      stop_build_record_field(field, "must be named for a Make variable")
    }
    require_record_string(cpp_options[[i]], field)
  }

  require_record_string_array(
    record_member(
      request, "stanc_options_supplied", "request.stanc_options_supplied"
    ),
    "request.stanc_options_supplied"
  )
  require_record_string_array(
    record_member(
      request, "stanc_options_injected", "request.stanc_options_injected"
    ),
    "request.stanc_options_injected"
  )

  stanc_name <- record_member(request, "stanc_name", "request.stanc_name")
  require_record_string(stanc_name, "request.stanc_name")
  if (!nzchar(stanc_name)) {
    stop_build_record_field("request.stanc_name", "must not be empty")
  }

  require_record_string_array(
    record_member(request, "include_paths", "request.include_paths"),
    "request.include_paths"
  )

  reported_features <- record_member(
    record, "reported_features", "reported_features"
  )
  require_record_object(reported_features, "reported_features")
  for (i in seq_along(reported_features)) {
    feature_name <- names(reported_features)[[i]]
    field <- paste0("reported_features.", feature_name)
    if (feature_name == "stan_version") {
      require_record_string(reported_features[[i]], field)
    } else {
      require_record_flag(reported_features[[i]], field)
    }
  }

  dependencies <- record_member(record, "dependencies", "dependencies")
  require_record_object(dependencies, "dependencies")
  require_dependency_entry(
    record_member(dependencies, "stan_file", "dependencies.stan_file"),
    "dependencies.stan_file"
  )
  # An absent user header or make/local means there was none.
  for (optional in c("user_header", "make_local")) {
    if (optional %in% names(dependencies)) {
      require_dependency_entry(
        dependencies[[optional]], paste0("dependencies.", optional)
      )
    }
  }
  included_files <- record_member(
    dependencies, "included_files", "dependencies.included_files"
  )
  require_record_array(included_files, "dependencies.included_files")
  for (i in seq_along(included_files)) {
    require_dependency_entry(
      included_files[[i]], paste0("dependencies.included_files[[", i, "]]")
    )
  }

  require_record_string(
    record_member(record, "artifact", "artifact"), "artifact"
  )

  builder <- record_member(record, "builder", "builder")
  require_record_object(builder, "builder")
  require_record_string(
    record_member(builder, "path", "builder.path"), "builder.path"
  )
  builder_version <- record_member(builder, "version", "builder.version")
  require_record_string(builder_version, "builder.version")
  # A string that is not a CmdStan version is the wrong shape, not an odd value.
  if (!grepl("^[0-9]+\\.[0-9]+\\.[0-9]+(-rc[0-9]+)?$", builder_version)) {
    stop_build_record_field(
      "builder.version", "must be a CmdStan version such as \"2.39.0\""
    )
  }

  require_record_string(record_member(record, "tbb_dir", "tbb_dir"), "tbb_dir")

  untracked <- record_member(
    record, "known_untracked_dependencies", "known_untracked_dependencies"
  )
  require_record_array(untracked, "known_untracked_dependencies")
  for (i in seq_along(untracked)) {
    field <- paste0("known_untracked_dependencies[[", i, "]]")
    require_record_object(untracked[[i]], field)
    kind <- record_member(untracked[[i]], "kind", paste0(field, ".kind"))
    require_record_string(kind, paste0(field, ".kind"))
    if (!kind %in% c("make_local_include", "user_header_include")) {
      stop_build_record_field(
        paste0(field, ".kind"),
        "must be \"make_local_include\" or \"user_header_include\""
      )
    }
    require_record_string(
      record_member(
        untracked[[i]], "detected_in", paste0(field, ".detected_in")
      ),
      paste0(field, ".detected_in")
    )
  }

  invisible(record)
}

#' Assemble a build record
#'
#' The one place a record is built. `format_version` comes first and the rest
#' follow the schema's order, so the written JSON reads in that order too.
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


# writing and reading -----------------------------------------------------

#' Write a build record beside its executable
#'
#' Staged in the same directory and renamed into place so a reader never meets
#' a half-written record. A failed rename warns, and the warning is suppressed
#' so that `warn = 2` cannot pre-empt the error below. `auto_unbox` writes a length-one vector as a JSON
#' scalar, which is why the schema holds every array as a list and every scalar
#' as a length-one vector: a one-element `included_files` still writes as an
#' array. Nothing is ever `NULL` or `NA`, since an unknown state is an absent
#' key.
#'
#' @noRd
write_build_record <- function(record, exe_file) {
  validate_build_record(record)
  path <- build_record_path(exe_file)
  staged <- tempfile(pattern = basename(path), tmpdir = dirname(path))
  jsonlite::write_json(
    record, staged, auto_unbox = TRUE, pretty = TRUE, digits = NA
  )
  if (!isTRUE(suppressWarnings(file.rename(staged, path)))) {
    unlink(staged)
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
  if (!checkmate::test_int(format_version)) {
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
