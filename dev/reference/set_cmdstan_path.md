# Get or set the file path to the CmdStan installation

Use the `set_cmdstan_path()` function to tell CmdStanR where the CmdStan
installation in located. Once the path has been set, `cmdstan_path()`
will return the full path to the CmdStan installation and
`cmdstan_version()` will return the CmdStan version number. See
**Details** for how to avoid manually setting the path in each R
session.

## Usage

``` r
set_cmdstan_path(path = NULL)

cmdstan_path()

cmdstan_version(error_on_NA = TRUE)
```

## Arguments

- path:

  (string) The full file path to the CmdStan installation. If `NULL`
  (the default) then the path is set to the default path used by
  [`install_cmdstan()`](https://mc-stan.org/cmdstanr/dev/reference/install_cmdstan.md)
  if it exists.

- error_on_NA:

  (logical) Should an error be thrown if CmdStan is not found. The
  default is `TRUE`. If `FALSE`, `cmdstan_version()` returns `NULL`.

## Value

A string. Either the file path to the CmdStan installation or the
CmdStan version number.

CmdStan version string if available. If CmdStan is not found and
`error_on_NA` is `FALSE`, `cmdstan_version()` returns `NULL`.

## Details

Before the package can be used it needs to know where the CmdStan
installation is located. When the package is loaded it tries to help
automate this to avoid having to manually set the path every session:

- If the [environment variable](https://rdrr.io/r/base/Sys.setenv.html)
  `"CMDSTAN"` exists at load time then its value will be automatically
  set as the default path to CmdStan for the R session. If the
  environment variable `"CMDSTAN"` is set, but a valid CmdStan is not
  found in the supplied path, the path is treated as a top folder that
  contains CmdStan installations. In that case, the CmdStan installation
  with the largest version number will be set as the path to CmdStan for
  the R session.

- If no environment variable is found when loaded but any directory in
  the form `".cmdstan/cmdstan-[version]"` (e.g.,
  `".cmdstan/cmdstan-2.23.0"`), exists in the user's home directory
  (`Sys.getenv("HOME")`, *not* the current working directory) then the
  path to the cmdstan with the largest version number will be set as the
  path to CmdStan for the R session. This is the same as the default
  directory that
  [`install_cmdstan()`](https://mc-stan.org/cmdstanr/dev/reference/install_cmdstan.md)
  would use to install the latest version of CmdStan.

It is always possible to change the path after loading the package using
`set_cmdstan_path(path)`.
