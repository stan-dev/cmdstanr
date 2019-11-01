#' CmdStanR: the \R interface to CmdStan
#'
#' @docType package
#' @name cmdstanr-package
#' @aliases cmdstanr CmdStanR
#'
#' @description
#' \if{html}{
#'    \figure{logo.png}{options: width="50px" alt="https://mc-stan.org/about/logo/"}
#'    \emph{Stan Development Team}
#' }
#'
#' \pkg{CmdStanR}: the \R interface to CmdStan.
#'
#' @details CmdStanR (\pkg{cmdstanr} package) is a lightweight interface to Stan
#'   ([mc-stan.org](https://mc-stan.org)) for \R users. It provides the
#'   necessary objects and functions to compile a Stan program and run Stan's
#'   algorithms from \R via CmdStan, the shell interface to Stan
#'   ([mc-stan.org/users/interfaces/cmdstan](https://mc-stan.org/users/interfaces/cmdstan)).
#'
#' @section Getting started: CmdStanR requires a working version of CmdStan. If
#'   you already have CmdStan installed see [cmdstan_model()] to get
#'   started, otherwise see [install_cmdstan()] for help with installation.
#'
#' @section Comparison with RStan: The RStan interface (\pkg{rstan} package) is
#'   an in-memory interface to Stan and relies on \R packages like \pkg{Rcpp}
#'   and \pkg{inline} to call C++ code from R. On the other hand, the \R code in
#'   this package does not directly call any C++ code, instead relying on
#'   CmdStan for compilation, running algorithms, and writing results to output
#'   files. Both forms of interfacing with Stan have advantages and
#'   disadvantages. An in-memory interface like RStan is able to offer more
#'   advanced features than CmdStanR (for example RStan's `log_prob` and
#'   `grad_log_prob` methods) but keeping up with Stan releases is more
#'   complicated for RStan, often requiring non-trivial changes to the
#'   \pkg{rstan} package and always requiring new CRAN releases of \pkg{rstan}
#'   and \pkg{StanHeaders}. With CmdStanR, the latest features in Stan will be
#'   available from \R immediately after updating CmdStan, which can be done
#'   with the [install_cmdstan()] function.
#'
#'   The licenses of CmdStanR (BSD-3) and RStan (GPL-3) are also different.
#'
#' @template seealso-docs
#' @inherit cmdstan_model examples
#'
NULL

if (getRversion() >= "2.15.1")  utils::globalVariables(c("self"))
