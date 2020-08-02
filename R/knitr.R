#' Register the knitr engine for Stan
#'
#' Registers CmdStanR's knitr engine [eng_stan()] for processing Stan chunks.
#' Refer to the vignette "R Markdown Stan Engine" for a demonstration.
#'
#' @param override Override knitr's built-in, RStan-based engine for `stan`.
#'   See below for details.
#' @details
#' If `override = TRUE` (default), this registers CmdStanR's knitr engine as the
#' engine for `stan` chunks, replacing knitr's built-in, RStan-based engine. If
#' `override = FALSE`, this registers a `cmdstan` engine so that both engines
#' may be used in the same R Markdown document. If there is syntax highlighting,
#' the `cmdstan` chunks will have `stan` syntax highlighting applied to them.
#'
#' See the vignette "R Markdown Stan Engine" for an example.
#' @references
#' - [Register a custom language engine](https://bookdown.org/yihui/rmarkdown-cookbook/custom-engine.html)
#' - [Stan language engine](https://bookdown.org/yihui/rmarkdown/language-engines.html#stan)
#' @export
register_knitr_engine <- function(override = TRUE) {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Please install the knitr package.", call. = FALSE)
  }
  if (override) {
    knitr::knit_engines$set(stan = eng_stan)
  } else {
    knitr::knit_engines$set(cmdstan = eng_stan)
  }
}

#' Stan knitr engine
#'
#' This provides a `stan` engine for `knitr`, suitable for usage when attempting
#' to render Stan chunks and compile the model code within to an executable with
#' CmdStan. Use [register_knitr_engine()] to make this the default engine for
#' `stan` chunks.
#'
#' @param options Chunk options, as provided by `knitr` during chunk execution.
#' @examples \dontrun{
#' knitr::knit_engines$set(stan = cmdstanr::eng_stan)
#' }
#' @export
eng_stan <- function(options) {
  output_var <- options$output.var
  if (!is.character(output_var) || length(output_var) != 1L) {
    stop(
      "The chunk option output.var must be a character string ",
      "providing a name for the returned `CmdStanModel` object.",
      call. = FALSE
    )
  }
  if (options$eval) {
    if (options$cache) {
      cache_path <- options$cache.path
      if (length(cache_path) == 0L || is.na(cache_path) || cache_path == "NA") {
        cache_path <- ""
      }
      dir <- paste0(cache_path, options$label)
    } else {
      dir <- tempdir()
    }
    file <- write_stan_tempfile(options$code, dir)
    mod <- cmdstan_model(file)
    assign(output_var, mod, envir = knitr::knit_global())
  }
  options$engine <- "stan" # for syntax highlighting
  code <- paste(options$code, collapse = "\n")
  knitr::engine_output(options, code, '')
}

