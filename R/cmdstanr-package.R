#' CmdStanR: the R interface to CmdStan
#'
#' @docType package
#' @name cmdstanr-package
#' @aliases cmdstanr CmdStanR
#'
#' @description
#' \if{html}{
#'    \figure{logo.png}{options: width="50" alt="https://mc-stan.org/about/logo/"}
#'    \emph{Stan Development Team}
#' }
#'
#' \pkg{CmdStanR}: the \R interface to CmdStan.
#'
#' @details CmdStanR (\pkg{cmdstanr} package) is an interface to Stan
#'   ([mc-stan.org](https://mc-stan.org)) for \R users. It provides the
#'   necessary objects and functions to compile a Stan program and run Stan's
#'   algorithms from \R via CmdStan, the shell interface to Stan
#'   ([mc-stan.org/users/interfaces/cmdstan](https://mc-stan.org/users/interfaces/cmdstan)).
#'
#' @includeRmd vignettes/children/comparison-with-rstan.md
#'
#' @section Getting started: CmdStanR requires a working version of CmdStan. If
#'   you already have CmdStan installed see [cmdstan_model()] to get started,
#'   otherwise see [install_cmdstan()] to install CmdStan. The vignette
#'   [_Getting started with CmdStanR_](https://mc-stan.org/cmdstanr/articles/cmdstanr.html)
#'   demonstrates the basic functionality of the package.
#'
#' @template seealso-docs
#' @inherit cmdstan_model examples
#' @import R6
#'
NULL

if (getRversion() >= "2.15.1")  utils::globalVariables(c("self", "private", "super"))
