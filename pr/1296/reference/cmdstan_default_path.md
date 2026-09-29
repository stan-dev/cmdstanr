# Path to the installation of CmdStan with the most recent release version

For Windows systems with WSL CmdStan installs, if there are side-by-side
WSL and native installs with the same version then the WSL is preferred.
Otherwise, the most recent release is chosen, regardless of whether it
is native or WSL.

## Usage

``` r
cmdstan_default_path(dir = NULL)
```

## Arguments

- dir:

  Path to a custom install folder with CmdStan installations.

## Value

Path to the CmdStan installation with the most recent release version,
or `NULL` if no installation found.

## See also

[`install_cmdstan()`](https://mc-stan.org/cmdstanr/dev/reference/install_cmdstan.md),
[`set_cmdstan_path()`](https://mc-stan.org/cmdstanr/dev/reference/set_cmdstan_path.md),
and
[`cmdstan_default_install_path()`](https://mc-stan.org/cmdstanr/dev/reference/cmdstan_default_install_path.md)
