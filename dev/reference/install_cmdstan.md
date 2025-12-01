# Install CmdStan or clean and rebuild an existing installation

The `install_cmdstan()` function attempts to download and install the
latest release of
[CmdStan](https://github.com/stan-dev/cmdstan/releases/latest).
Installing a previous release or a new release candidate is also
possible by specifying the `version` or `release_url` argument. See the
first few sections of the CmdStan [installation
guide](https://mc-stan.org/docs/cmdstan-guide/cmdstan-installation.html)
for details on the C++ toolchain required for installing CmdStan.

The `rebuild_cmdstan()` function cleans and rebuilds the CmdStan
installation. Use this function in case of any issues when compiling
models.

The `cmdstan_make_local()` function is used to read/write makefile flags
and variables from/to the `make/local` file of a CmdStan installation.
Writing to the `make/local` file can be used to permanently add makefile
flags/variables to an installation. For example adding specific compiler
switches, changing the C++ compiler, etc. A change to the `make/local`
file should typically be followed by calling `rebuild_cmdstan()`.

The `check_cmdstan_toolchain()` function attempts to check for the
required C++ toolchain. It is called internally by `install_cmdstan()`
but can also be called directly by the user. On Windows only, calling
the function with the `fix = TRUE` argument will attempt to install the
necessary toolchain components if they are not found. For Windows users
with RTools and CmdStan versions \>= 2.35 no additional toolchain
configuration is required.

NOTE: When installing CmdStan on Windows with RTools and CmdStan
versions prior to 2.35.0, the above additional toolchain configuration
is still required. To enable this configuration, set the environment
variable `CMDSTANR_USE_MSYS_TOOLCHAIN` to 'true' and call
`check_cmdstan_toolchain(fix = TRUE)`.

## Usage

``` r
install_cmdstan(
  dir = NULL,
  cores = getOption("mc.cores", 2),
  quiet = FALSE,
  overwrite = FALSE,
  timeout = 1200,
  version = NULL,
  release_url = NULL,
  release_file = NULL,
  cpp_options = list(),
  check_toolchain = TRUE,
  wsl = FALSE
)

rebuild_cmdstan(
  dir = cmdstan_path(),
  cores = getOption("mc.cores", 2),
  quiet = FALSE,
  timeout = 600
)

cmdstan_make_local(dir = cmdstan_path(), cpp_options = NULL, append = TRUE)

check_cmdstan_toolchain(fix = FALSE, quiet = FALSE)
```

## Arguments

- dir:

  (string) The path to the directory in which to install CmdStan. The
  default is to install it in a directory called `.cmdstan` within the
  user's home directory (i.e,
  `file.path(Sys.getenv("HOME"), ".cmdstan")`).

- cores:

  (integer) The number of CPU cores to use to parallelize building
  CmdStan and speed up installation. If `cores` is not specified then
  the default is to look for the option `"mc.cores"`, which can be set
  for an entire R session by `options(mc.cores=value)`. If the
  `"mc.cores"` option has not been set then the default is `2`.

- quiet:

  (logical) For `install_cmdstan()`, should the verbose output from the
  system processes be suppressed when building the CmdStan binaries? The
  default is `FALSE`. For `check_cmdstan_toolchain()`, should the
  function suppress printing informational messages? The default is
  `FALSE`. If `TRUE` only errors will be printed.

- overwrite:

  (logical) Should CmdStan still be downloaded and installed even if an
  installation of the same version is found in `dir`? The default is
  `FALSE`, in which case an informative error is thrown instead of
  overwriting the user's installation.

- timeout:

  (positive real) Timeout (in seconds) for the build stage of the
  installation.

- version:

  (string) The CmdStan release version to install. The default is
  `NULL`, which downloads the latest stable release from
  <https://github.com/stan-dev/cmdstan/releases>.

- release_url:

  (string) The URL for the specific CmdStan release or release candidate
  to install. See <https://github.com/stan-dev/cmdstan/releases>. The
  URL should point to the tarball (`.tar.gz.` file) itself, e.g.,
  `release_url="https://github.com/stan-dev/cmdstan/releases/download/v2.25.0/cmdstan-2.25.0.tar.gz"`.
  If both `version` and `release_url` are specified then `version` will
  be used.

- release_file:

  (string) A file path to a CmdStan release tar.gz file downloaded from
  the releases page: <https://github.com/stan-dev/cmdstan/releases>. For
  example: `release_file=""./cmdstan-2.33.1.tar.gz"`. If `release_file`
  is specified then both `release_url` and `version` will be ignored.

- cpp_options:

  (list) Any makefile flags/variables to be written to the `make/local`
  file. For example, `list("CXX" = "clang++")` will force the use of
  clang for compilation.

- check_toolchain:

  (logical) Should `install_cmdstan()` attempt to check that the
  required toolchain is installed and properly configured. The default
  is `TRUE`.

- wsl:

  (logical) Should CmdStan be installed and run through the Windows
  Subsystem for Linux (WSL). The default is `FALSE`.

- append:

  (logical) For `cmdstan_make_local()`, should the listed makefile flags
  be appended to the end of the existing `make/local` file? The default
  is `TRUE`. If `FALSE` the file is overwritten.

- fix:

  For `check_cmdstan_toolchain()`, should CmdStanR attempt to fix any
  detected toolchain problems? Currently this option is only available
  on Windows. The default is `FALSE`, in which case problems are only
  reported along with suggested fixes.

## Value

For `cmdstan_make_local()`, if `cpp_options=NULL` then the existing
contents of `make/local` are returned without writing anything,
otherwise the updated contents are returned.

## Examples

``` r
# \dontrun{
check_cmdstan_toolchain()
#> The C++ toolchain required for CmdStan is setup properly!

# install_cmdstan(cores = 4)

cpp_options <- list(
  "CXX" = "clang++",
  "CXXFLAGS+= -march=native",
  PRECOMPILED_HEADERS = TRUE
)
# cmdstan_make_local(cpp_options = cpp_options)
# rebuild_cmdstan()
# }
```
