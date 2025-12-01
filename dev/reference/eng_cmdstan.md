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

  (named list) Chunk options, as provided by `knitr` during chunk
  execution.

## Examples

``` r
# \dontrun{
knitr::knit_engines$set(stan = cmdstanr::eng_cmdstan)
# }
```
