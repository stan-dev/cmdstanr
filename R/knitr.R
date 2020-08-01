#' Register the knitr engine for Stan
#'
#' Registers CmdStanR's knitr engine [eng_stan()] for processing Stan chunks.
#'
#' @param override Override knitr's built-in, RStan-based engine for `stan`.
#'   See below for details.
#' @details
#' If `override = TRUE` (default), this registers CmdStanR's knitr engine as the
#' engine for `stan` chunks, replacing knitr's built-in, RStan-based engine. If
#' `override = FALSE`, this registers a `cmdstan` engine so that both engines
#' may be used in the same R Markdown document. If there is syntax highlighting,
#' the `cmdstan` chunks will have `stan` syntax highlighting applied to them.
#' @references
#' - [Register a custom language engine](https://bookdown.org/yihui/rmarkdown-cookbook/custom-engine.html)
#' - [Stan language engine](https://bookdown.org/yihui/rmarkdown/language-engines.html#stan)
#' @export
register_knitr_engine <- function(override = TRUE) {
  if (requireNamespace("knitr", quietly = TRUE)) {
    if (override) {
      knitr::knit_engines$set(stan = eng_stan)
    } else {
      knitr::knit_engines$set(cmdstan = eng_stan)
    }
  }
}

#' Stan knitr engine
#'
#' This provides a `stan` engine for `knitr`, suitable for usage when attempting
#' to render Stan chunks and compile the model code within to an executable with
#' CmdStan.
#'
#' @param options Chunk options, as provided by `knitr` during chunk execution
#' @export
eng_stan <- function(options) {
  output_var <- options$output.var
  if (!is.character(output_var) || length(output_var) != 1L)
    stop(
      "the chunk option output.var must be a character string ",
      "providing a name for the returned `CmdStanModel` object."
    )
  if (options$eval) {
    if (options$cache)
      dir <- knitr:::valid_path(options[["cache.path"]], options$label)
    else
      dir <- tempdir()
    model_file <- write_stan_tempfile(options$code, dir)
    csm <- cmdstan_model(model_file)
    assign(output_var, csm, envir = knitr::knit_global())
  }
  options$engine <- "stan"
  code <- knitr:::one_string(options$code)
  knitr::engine_output(options, code, '')
}
