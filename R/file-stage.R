# Exact CmdStan file lists ------------------------------------------------

#' Plan an exact CmdStan file-list mapping
#'
#' This function only describes a mapping. File copies and cleanup belong to
#' the `CmdStanRun` that consumes the resulting run plan.
#'
#' @noRd
new_cmdstan_file_stage <- function(paths,
                                   direction = c("input", "output"),
                                   staging_dir,
                                   cmdstan_paths = NULL) {
  checkmate::assert_character(
    paths,
    min.len = 1,
    any.missing = FALSE,
    names = NULL
  )
  direction <- match.arg(direction)
  assert_dir_exists(staging_dir, access = "rw")
  staging_dir <- absolute_path(staging_dir)
  if (grepl(",", staging_dir, fixed = TRUE)) {
    stop("The CmdStan staging directory must not contain a comma.", call. = FALSE)
  }

  public_paths <- absolute_path(paths)
  if (is.null(cmdstan_paths)) {
    cmdstan_paths <- public_paths
    for (i in which(grepl(",", public_paths, fixed = TRUE))) {
      extension <- tools::file_ext(public_paths[[i]])
      if (nzchar(extension)) {
        extension <- paste0(".", extension)
      }
      cmdstan_paths[[i]] <- tempfile(
        pattern = "cmdstan-file-",
        tmpdir = staging_dir,
        fileext = extension
      )
    }
  } else {
    checkmate::assert_character(
      cmdstan_paths,
      len = length(paths),
      any.missing = FALSE,
      names = NULL
    )
    cmdstan_paths <- absolute_path(cmdstan_paths)
  }

  stage <- list(
    public_paths = public_paths,
    cmdstan_paths = cmdstan_paths,
    direction = direction
  )
  validate_cmdstan_file_stage(stage)
  stage
}

#' Validate a planned CmdStan file-list mapping
#'
#' @noRd
validate_cmdstan_file_stage <- function(stage) {
  checkmate::assert_list(stage)
  if (!identical(
    names(stage),
    c("public_paths", "cmdstan_paths", "direction")
  )) {
    stop(
      "A CmdStan file stage must contain public paths, CmdStan paths, and a direction.",
      call. = FALSE
    )
  }
  checkmate::assert_character(
    stage$public_paths,
    min.len = 1,
    any.missing = FALSE,
    names = NULL
  )
  checkmate::assert_character(
    stage$cmdstan_paths,
    len = length(stage$public_paths),
    any.missing = FALSE,
    names = NULL
  )
  checkmate::assert_choice(stage$direction, c("input", "output"))
  if (any(grepl(",", stage$cmdstan_paths, fixed = TRUE))) {
    stop("A staged CmdStan path must not contain a comma.", call. = FALSE)
  }
  invisible(stage)
}

#' Prepare the input files in a planned CmdStan file-list mapping
#'
#' @noRd
prepare_cmdstan_file_stage <- function(stage) {
  validate_cmdstan_file_stage(stage)
  if (stage$direction != "input") {
    return(FALSE)
  }
  staged <- stage$public_paths != stage$cmdstan_paths
  if (!any(staged)) {
    return(FALSE)
  }
  assert_file_exists(stage$public_paths[staged], access = "r")
  copied <- file.copy(
    stage$public_paths[staged],
    stage$cmdstan_paths[staged],
    overwrite = TRUE
  )
  if (!all(copied)) {
    stop("Failed to stage one or more CmdStan input files.", call. = FALSE)
  }
  TRUE
}

#' Restore the output files in a planned CmdStan file-list mapping
#'
#' @noRd
restore_cmdstan_file_stage <- function(stage) {
  validate_cmdstan_file_stage(stage)
  if (stage$direction != "output") {
    return(FALSE)
  }
  staged <- stage$public_paths != stage$cmdstan_paths
  changed <- FALSE
  for (i in which(staged)) {
    staged_path <- stage$cmdstan_paths[[i]]
    if (file.exists(staged_path)) {
      copied <- file.copy(
        staged_path,
        stage$public_paths[[i]],
        overwrite = TRUE
      )
      if (!isTRUE(copied)) {
        stop(
          "Failed to restore staged CmdStan output to '",
          stage$public_paths[[i]], "'.",
          call. = FALSE
        )
      }
      unlink(staged_path)
      changed <- TRUE
    }
  }
  changed
}

#' Remove temporary files in a planned CmdStan file-list mapping
#'
#' @noRd
cleanup_cmdstan_file_stage <- function(stage) {
  validate_cmdstan_file_stage(stage)
  staged <- stage$public_paths != stage$cmdstan_paths
  unlink(stage$cmdstan_paths[staged])
  invisible(NULL)
}

#' Convert and join an exact CmdStan file list
#'
#' @noRd
compose_cmdstan_file_list <- function(paths) {
  checkmate::assert_character(paths, min.len = 1, any.missing = FALSE)
  converted <- vapply(paths, wsl_safe_path, character(1))
  if (any(grepl(",", converted, fixed = TRUE))) {
    stop(
      "A CmdStan file-list element contains a comma after path conversion; ",
      "stage it before composing the command.",
      call. = FALSE
    )
  }
  paste(converted, collapse = ",")
}
