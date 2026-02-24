# cmdstan_default_path

Returns the path to the installation of CmdStan with the most recent
release version.

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

## Details

For Windows systems with WSL CmdStan installs, if there are side-by-side
WSL and native installs with the same version then the WSL is preferred.
Otherwise, the most recent release is chosen, regardless of whether it
is native or WSL.
