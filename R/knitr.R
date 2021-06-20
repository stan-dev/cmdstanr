#' Register CmdStanR's knitr engine for Stan
#'
#' Registers CmdStanR's knitr engine [eng_cmdstan()] for processing Stan chunks.
#' Refer to the vignette
#' [R Markdown CmdStan Engine](https://mc-stan.org/cmdstanr/articles/r-markdown.html)
#' for a demonstration.
#'
#' @export
#'
#' @param override (logical) Override knitr's built-in, RStan-based engine for
#'   Stan? The default is `TRUE`. See **Details**.
#'
#' @details
#' If `override = TRUE` (default), this registers CmdStanR's knitr engine as the
#' engine for `stan` chunks, replacing knitr's built-in, RStan-based engine. If
#' `override = FALSE`, this registers a `cmdstan` engine so that both engines
#' may be used in the same R Markdown document. If the template supports syntax
#' highlighting for the Stan language, the `cmdstan` chunks will have `stan`
#' syntax highlighting applied to them.
#'
#' See the vignette
#' [R Markdown CmdStan Engine](https://mc-stan.org/cmdstanr/articles/r-markdown.html)
#' for an example.
#'
#' **Note:** When running chunks interactively in RStudio (e.g. when using
#' [R Notebooks](https://bookdown.org/yihui/rmarkdown/notebook.html)), it has
#' been observed that the built-in, RStan-based engine is used for `stan`
#' chunks even when CmdStanR's engine has been registered in the session. When
#' the R Markdown document is knit/rendered, the correct engine is used. As a
#' workaround, when running chunks interactively, it is recommended to use the
#' `override = FALSE` option and change `stan` chunks to be `cmdstan` chunks.
#'
#' If you would like to keep `stan` chunks as `stan` chunks, it is possible to
#' specify `engine = "cmdstan"` in the chunk options after registering the
#' `cmdstan` engine with `override = FALSE`.
#'
#' @references
#' * [Register a custom language engine for knitr](https://bookdown.org/yihui/rmarkdown-cookbook/custom-engine.html)
#' * [knitr's built-in Stan language engine](https://bookdown.org/yihui/rmarkdown/language-engines.html#stan)
#'
register_knitr_engine <- function(override = TRUE) {
  require_suggested_package("knitr")
  if (override) {
    knitr::knit_engines$set(stan = eng_cmdstan)
  } else {
    knitr::knit_engines$set(cmdstan = eng_cmdstan)
  }
}

#' CmdStan knitr engine for Stan
#'
#' This provides a knitr engine for Stan, suitable for usage when attempting
#' to render Stan chunks and compile the model code within to an executable with
#' CmdStan. Use [register_knitr_engine()] to make this the default engine for
#' `stan` chunks. See the vignette
#' [R Markdown CmdStan Engine](https://mc-stan.org/cmdstanr/articles/r-markdown.html)
#' for an example.
#'
#' @param options (named list) Chunk options, as provided by `knitr` during
#'   chunk execution.
#' @examples \dontrun{
#' knitr::knit_engines$set(stan = cmdstanr::eng_cmdstan)
#' }
#' @export
eng_cmdstan <- function(options) {
  require_suggested_package("knitr")
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
    file <- write_stan_file(options$code, dir = dir)
    mod <- cmdstan_model(file)
    assign(output_var, mod, envir = cmdstanr_knitr_env())
  }
  options$engine <- "stan" # for syntax highlighting
  code <- paste(options$code, collapse = "\n")
  knitr::engine_output(options, code, '')
}
