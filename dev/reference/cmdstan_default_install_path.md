# cmdstan_default_install_path

Path to where
[`install_cmdstan()`](https://mc-stan.org/cmdstanr/dev/reference/install_cmdstan.md)
with default settings installs CmdStan.

## Usage

``` r
cmdstan_default_install_path(old = FALSE, wsl = FALSE)
```

## Arguments

- old:

  Should the old default path (.cmdstanr) be used instead of the new one
  (.cmdstan)? Defaults to `FALSE` and may be removed in a future
  release.

- wsl:

  Return the directory for WSL installations?

## Value

The installation path.
