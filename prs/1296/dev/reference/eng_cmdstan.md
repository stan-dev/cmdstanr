# CmdStan knitr engine for Stan

This provides a knitr engine for Stan, suitable for usage when
attempting to render Stan chunks and compile the model code within to an
executable with CmdStan. Use
[`register_knitr_engine()`](https://mc-stan.org/cmdstanr/dev/reference/register_knitr_engine.md)
to make this the default engine for `stan` chunks. See the vignette [R
Markdown CmdStan
Engine](https://mc-stan.org/cmdstanr/articles/r-markdown.html) for an
example.

## Usage

``` r
eng_cmdstan(options)
```

## Arguments

- options:

  (named list) Chunk options supplied by `knitr`. The `output.var`
  element is required and must be a single character string naming the
  `CmdStanModel` object created by the chunk.

## Value

A character vector containing the formatted chunk output produced by
[`knitr::engine_output()`](https://rdrr.io/pkg/knitr/man/engine_output.html).

## See also

[`register_knitr_engine()`](https://mc-stan.org/cmdstanr/dev/reference/register_knitr_engine.md)

## Examples

``` r
# \dontrun{
knitr::knit_engines$set(stan = cmdstanr::eng_cmdstan)
# }
```
