# Exact CmdStan file lists ------------------------------------------------

#' Staging record for an exact CmdStan file list
#'
#' @noRd
CmdStanFileListStage <- R6::R6Class(
  classname = "CmdStanFileListStage",
  public = list(
    initialize = function(paths, direction, staging_dir, cmdstan_paths = NULL) {
      checkmate::assert_character(
        paths,
        min.len = 1,
        any.missing = FALSE,
        names = NULL
      )
      direction <- match.arg(direction, c("input", "output"))
      assert_dir_exists(staging_dir, access = "rw")
      staging_dir <- absolute_path(staging_dir)
      if (grepl(",", staging_dir, fixed = TRUE)) {
        stop("The CmdStan staging directory must not contain a comma.", call. = FALSE)
      }

      private$public_paths_ <- absolute_path(paths)
      private$direction_ <- direction
      private$finalized_ <- rep(FALSE, length(paths))

      if (is.null(cmdstan_paths)) {
        private$cmdstan_paths_ <- private$public_paths_
        private$staged_ <- grepl(",", private$public_paths_, fixed = TRUE)
        for (i in which(private$staged_)) {
          extension <- tools::file_ext(private$public_paths_[[i]])
          if (nzchar(extension)) {
            extension <- paste0(".", extension)
          }
          private$cmdstan_paths_[[i]] <- tempfile(
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
        private$cmdstan_paths_ <- absolute_path(cmdstan_paths)
        if (any(grepl(",", private$cmdstan_paths_, fixed = TRUE))) {
          stop("A staged CmdStan path must not contain a comma.", call. = FALSE)
        }
        private$staged_ <- private$public_paths_ != private$cmdstan_paths_
      }

      if (direction == "input" && any(private$staged_)) {
        assert_file_exists(private$public_paths_[private$staged_], access = "r")
        copied <- file.copy(
          private$public_paths_[private$staged_],
          private$cmdstan_paths_[private$staged_],
          overwrite = TRUE
        )
        if (!all(copied)) {
          stop("Failed to stage one or more CmdStan input files.", call. = FALSE)
        }
      }
      invisible(self)
    },

    public_paths = function() {
      private$public_paths_
    },

    cmdstan_paths = function() {
      private$cmdstan_paths_
    },

    direction = function() {
      private$direction_
    },

    restore_outputs = function() {
      if (private$direction_ != "output") {
        return(FALSE)
      }
      changed <- FALSE
      for (i in which(private$staged_ & !private$finalized_)) {
        staged_path <- private$cmdstan_paths_[[i]]
        if (file.exists(staged_path)) {
          copied <- file.copy(
            staged_path,
            private$public_paths_[[i]],
            overwrite = TRUE
          )
          if (!isTRUE(copied)) {
            stop(
              "Failed to restore staged CmdStan output to '",
              private$public_paths_[[i]], "'.",
              call. = FALSE
            )
          }
          unlink(staged_path)
          private$finalized_[[i]] <- TRUE
          changed <- TRUE
        }
      }
      changed
    },

    cleanup = function() {
      unlink(private$cmdstan_paths_[private$staged_])
      invisible(self)
    }
  ),
  private = list(
    public_paths_ = character(),
    cmdstan_paths_ = character(),
    direction_ = character(),
    staged_ = logical(),
    finalized_ = logical()
  )
)

#' Stage comma-containing elements of an exact CmdStan file list
#'
#' @noRd
stage_cmdstan_file_list <- function(paths,
                                    direction = c("input", "output"),
                                    staging_dir) {
  CmdStanFileListStage$new(
    paths = paths,
    direction = match.arg(direction),
    staging_dir = staging_dir
  )
}

#' Record an explicit public-to-CmdStan staging mapping
#'
#' @noRd
new_cmdstan_file_stage_mapping <- function(paths,
                                           cmdstan_paths,
                                           direction = c("input", "output"),
                                           staging_dir) {
  CmdStanFileListStage$new(
    paths = paths,
    direction = match.arg(direction),
    staging_dir = staging_dir,
    cmdstan_paths = cmdstan_paths
  )
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
