# Print a Stan file with syntax highlighting in Quarto and R Markdown

Prints the contents of a Stan file, optionally with syntax highlighting
when used in a Quarto or R Markdown document. When called inside a
[knitr](https://rdrr.io/pkg/knitr/man/knitr-package.html) code chunk
with the chunk option `output: asis` (or `results: asis` in R Markdown),
the output is a fenced Stan code block that Quarto renders with syntax
highlighting. When called interactively or without `output: asis`, the
code is printed as plain text via
[`writeLines()`](https://rdrr.io/r/base/writeLines.html).

## Usage

``` r
print_stan_file(file, fold = FALSE, summary = "Stan model code")
```

## Arguments

- file:

  (string) Path to a `.stan` file.

- fold:

  (logical) Whether to wrap the output in an HTML `<details>` block so
  that the code is collapsed (folded) by default. Only has an effect
  when rendering with `output: asis` and when outputting HTML. Defaults
  to `FALSE`.

- summary:

  (string) The summary text shown in the fold toggle when `fold = TRUE`.
  Defaults to `"Stan model code"`.

## Value

The file path (invisibly).

## Quarto usage

Use in a Quarto code chunk with `output: asis` to get syntax
highlighting:

    ```{r}
    #| output: asis
    print_stan_file("path/to/model.stan")
    ```

To make the code block collapsible:

    ```{r}
    #| output: asis
    print_stan_file("path/to/model.stan", fold = TRUE)
    ```

## Examples

``` r
stan_file <- write_stan_file("
parameters {
  real y;
}
model {
  y ~ std_normal();
}
")

# Prints plain code at the console
print_stan_file(stan_file)
#> 
#> parameters {
#>   real y;
#> }
#> model {
#>   y ~ std_normal();
#> }
#> 
```
