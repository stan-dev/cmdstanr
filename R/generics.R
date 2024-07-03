#' Coercion methods for CmdStan objects
#'
#' These are generic functions intended to primarily be used by developers of
#' packages that interface with on CmdStanR. Developers can define methods on
#' top of these generics to coerce objects into CmdStanR's fitted model objects.
#'
#' @param object The object to be coerced.
#' @param ... Additional arguments to pass to methods.
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
