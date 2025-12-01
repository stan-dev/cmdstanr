# Register CmdStanR's knitr engine for Stan

Registers CmdStanR's knitr engine
[`eng_cmdstan()`](https://mc-stan.org/cmdstanr/dev/reference/eng_cmdstan.md)
for processing Stan chunks. Refer to the vignette [R Markdown CmdStan
Engine](https://mc-stan.org/cmdstanr/articles/r-markdown.html) for a
demonstration.

## Usage

``` r
register_knitr_engine(override = TRUE)
```

## Arguments

- override:

  (logical) Override knitr's built-in, RStan-based engine for Stan? The
  default is `TRUE`. See **Details**.

## Details

If `override = TRUE` (default), this registers CmdStanR's knitr engine
as the engine for `stan` chunks, replacing knitr's built-in, RStan-based
engine. If `override = FALSE`, this registers a `cmdstan` engine so that
both engines may be used in the same R Markdown document. If the
template supports syntax highlighting for the Stan language, the
`cmdstan` chunks will have `stan` syntax highlighting applied to them.

See the vignette [R Markdown CmdStan
Engine](https://mc-stan.org/cmdstanr/articles/r-markdown.html) for an
example.

**Note:** When running chunks interactively in RStudio (e.g. when using
[R Notebooks](https://bookdown.org/yihui/rmarkdown/notebook.html)), it
has been observed that the built-in, RStan-based engine is used for
`stan` chunks even when CmdStanR's engine has been registered in the
session. When the R Markdown document is knit/rendered, the correct
engine is used. As a workaround, when running chunks interactively, it
is recommended to use the `override = FALSE` option and change `stan`
chunks to be `cmdstan` chunks.

If you would like to keep `stan` chunks as `stan` chunks, it is possible
to specify `engine = "cmdstan"` in the chunk options after registering
the `cmdstan` engine with `override = FALSE`.

## References

- [Register a custom language engine for
  knitr](https://bookdown.org/yihui/rmarkdown-cookbook/custom-engine.html)

- [knitr's built-in Stan language
  engine](https://bookdown.org/yihui/rmarkdown/language-engines.html#stan)
