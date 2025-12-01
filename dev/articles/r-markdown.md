# R Markdown CmdStan Engine

R Markdown supports a variety of languages through the use of knitr
language engines. Where users wish to write Stan programs as chunks
directly in R Markdown documents there are three options:

1.  the user wishes all the Stan chunks in the R Markdown document to be
    processed using RStan;  
2.  all Stan chunks are to be processed using CmdStanR; and.  
3.  some chunks are to be processed by RStan and some by CmdStanR.

Behind the scenes in each option, the engine compiles the model code in
each chunk and creates an object that provides methods to run the model:
a `stanmodel` if Rstan is being used, or a `CmdStanModel` in the
CmdStanR case. This model object is assigned to a variable with the name
given by the `output.var` chunk option.

## Option 1: Using RStan for all chunks

This is the default option. In that case we can write, for example:

```` default
```{stan, output.var="model"}
// Stan model code
```

```{r}
rstan::sampling(model)
```
````

## Option 2: Using CmdStanR for all chunks

If CmdStanR is being used a replacement engine needs to be registered
along the following lines:

``` r
library(cmdstanr)
register_knitr_engine(override = TRUE)
```

This overrides knitr’s built-in `stan` engine so that all `stan` chunks
are processed with CmdStanR, not RStan. Of course, this also means that
the variable specified by `output.var` will no longer be a `stanmodel`
object, but instead a `CmdStanModel` object, so the example code above
would look like this:

```` default
```{stan, output.var="model"}
// Stan model code
```

```{r}
model$sample()
```
````

## Example

``` stan
// This stan chunk results in a CmdStanModel object called "ex1"
parameters {
  array[2] real y;
}
model {
  y[1] ~ normal(0, 1);
  y[2] ~ double_exponential(0, 2);
}
```

``` r
ex1$print()
#> // This stan chunk results in a CmdStanModel object called "ex1"
#> parameters {
#>   array[2] real y;
#> }
#> model {
#>   y[1] ~ normal(0, 1);
#>   y[2] ~ double_exponential(0, 2);
#> }
```

``` r
fit <- ex1$sample(
  refresh = 0,
  seed = 42L
)
#> Running MCMC with 4 sequential chains...
#> 
#> Chain 1 finished in 0.0 seconds.
#> Chain 2 finished in 0.0 seconds.
#> Chain 3 finished in 0.0 seconds.
#> Chain 4 finished in 0.0 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 0.0 seconds.
#> Total execution time: 0.6 seconds.

print(fit)
#>  variable  mean median   sd  mad    q5   q95 rhat ess_bulk ess_tail
#>      lp__ -1.56  -1.23 1.26 1.04 -4.05 -0.16 1.00     1395     1598
#>      y[1]  0.03   0.03 1.04 1.04 -1.68  1.70 1.00     1803     1921
#>      y[2]  0.06   0.04 2.89 2.05 -4.71  5.10 1.00     2320     1304
```

## Option 3: Using both RStan and CmdStanR in the same R Markdown document

While the default behavior is to override the built-in `stan` engine
because the assumption is that the user is probably not using both RStan
and CmdStanR in the same document or project, the option to use both
exists. When registering CmdStanR’s knitr engine, set `override = FALSE`
to register the engine as a `cmdstan` engine:

``` r
register_knitr_engine(override = FALSE)
```

This will cause `stan` chunks to be processed by knitr’s built-in,
RStan-based engine and only use CmdStanR’s knitr engine for `cmdstan`
chunks:

```` default
```{stan, output.var="model_obj1"}
// Results in a stanmodel object from RStan
```

```{r}
rstan::sampling(model_obj1)
```

```{cmdstan, output.var="model_obj2"}
// Results in a CmdStanModel object from CmdStanR
```

```{r}
model_obj2$sample()
```
````

## Caching chunks

Use `cache=TRUE` chunk option to avoid re-compiling the Stan model code
every time the R Markdown is knit/rendered.

You can find the Stan model file and the compiled executable in the
document’s cache directory.

## Running interactively

When running chunks interactively in RStudio (e.g. when using [R
Notebooks](https://bookdown.org/yihui/rmarkdown/notebook.html)), it has
been observed that the built-in, RStan-based engine is used for `stan`
chunks even when CmdStanR’s engine has been registered in the session as
the engine for `stan`. As a workaround, when running chunks
*interactively*, it is recommended to use the `override = FALSE` option
and change `stan` chunks to be `cmdstan` chunks.

Do not worry: if the template you use supports syntax highlighting for
the Stan language, that syntax highlighting will be applied to `cmdstan`
chunks when the document is knit/rendered.
