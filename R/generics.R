
#' Coercion methods for CmdStan objects
#'
#' These methods are used to coerce objects into `cmdstanr` objects.
#' Primarily intended for other packages to use when interfacing
#' with `cmdstanr`.
#'
#' @param object to be coerced
#' @param ... additional arguments
#'
#' @name cmdstan_coercion
NULL

#' @rdname cmdstan_coercion
#' @export
as.CmdStanMCMC <- function(object, ...) {
  UseMethod("as.CmdStanMCMC")
}

#' @rdname cmdstan_coercion
#' @export
as.CmdStanMLE <- function(object, ...) {
  UseMethod("as.CmdStanMLE")
}

#' @rdname cmdstan_coercion
#' @export
as.CmdStanLaplace <- function(object, ...) {
  UseMethod("as.CmdStanLaplace")
}

#' @rdname cmdstan_coercion
#' @export
as.CmdStanVB <- function(object, ...) {
  UseMethod("as.CmdStanVB")
}

#' @rdname cmdstan_coercion
#' @export
as.CmdStanPathfinder <- function(object, ...) {
  UseMethod("as.CmdStanPathfinder")
}

#' @rdname cmdstan_coercion
#' @export
as.CmdStanGQ <- function(object, ...) {
  UseMethod("as.CmdStanGQ")
}

#' @rdname cmdstan_coercion
#' @export
as.CmdStanDiagnose <- function(object, ...) {
  UseMethod("as.CmdStanDiagnose")
}
