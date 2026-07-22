# Access information from a `CmdStanModel` object

These methods access information stored in a
[`CmdStanModel`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanModel.md)
object, print its Stan program, and manage paths to its executable and
generated C++ file.

    stan_file()
    has_stan_file()
    code()
    print(line_numbers = getOption("cmdstanr_print_line_numbers", FALSE))
    model_name()
    exe_file(path = NULL)
    include_paths()
    cmdstan_version()
    cpp_options()
    hpp_file()
    save_hpp_file(dir = NULL)

## Arguments

- line_numbers:

  (logical) Should line numbers be printed? The default is
  `getOption("cmdstanr_print_line_numbers", FALSE)`.

- path:

  (string) The path to a model executable. If `NULL` (the default),
  `$exe_file()` returns the current path. Otherwise, the stored path is
  updated before being returned.

- dir:

  (string) The directory in which to save the `.hpp` file. The default
  is the directory containing the Stan program.

## Value

- `$stan_file()` returns a path as a string, or `character(0)` if the
  model was created without a Stan file.

- `$has_stan_file()` returns `TRUE` if the model was created with a Stan
  file and `FALSE` otherwise.

- `$code()` returns a character vector with one element per line of Stan
  code, or `NULL` if the model was created without a Stan file.

- `$print()` returns the
  [`CmdStanModel`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanModel.md)
  object invisibly.

- `$model_name()` returns the model name as a string.

- `$exe_file()` returns a path as a string, or `character(0)` if no
  executable path is set.

- `$include_paths()` returns a character vector of paths or `NULL`.

- `$cmdstan_version()` returns a CmdStan version as a string.

- `$cpp_options()` returns a named list of C++ options.

- `$hpp_file()` returns the path to the `.hpp` file as a string when C++
  code was generated while compiling this model object. It errors if no
  `.hpp` path is available, such as when an up-to-date executable was
  reused.

- `$save_hpp_file()` requires an available `.hpp` file. It moves the
  file to `dir`, updates the stored path, and returns the new path
  invisibly.

## See also

[`$compile()`](https://mc-stan.org/cmdstanr/dev/reference/model-method-compile.md)
and
[`cmdstan_model()`](https://mc-stan.org/cmdstanr/dev/reference/cmdstan_model.md)

The CmdStanR website
([mc-stan.org/cmdstanr](https://mc-stan.org/cmdstanr/)) for online
documentation and tutorials.

The Stan and CmdStan documentation:

- Stan documentation:
  [mc-stan.org/users/documentation](https://mc-stan.org/users/documentation/)

- CmdStan User’s Guide:
  [mc-stan.org/docs/cmdstan-guide](https://mc-stan.org/docs/cmdstan-guide/)

Other CmdStanModel methods:
[`model-method-check_syntax`](https://mc-stan.org/cmdstanr/dev/reference/model-method-check_syntax.md),
[`model-method-cmdstan_defaults`](https://mc-stan.org/cmdstanr/dev/reference/model-method-cmdstan_defaults.md),
[`model-method-compile`](https://mc-stan.org/cmdstanr/dev/reference/model-method-compile.md),
[`model-method-diagnose`](https://mc-stan.org/cmdstanr/dev/reference/model-method-diagnose.md),
[`model-method-expose_functions`](https://mc-stan.org/cmdstanr/dev/reference/model-method-expose_functions.md),
[`model-method-format`](https://mc-stan.org/cmdstanr/dev/reference/model-method-format.md),
[`model-method-generate-quantities`](https://mc-stan.org/cmdstanr/dev/reference/model-method-generate-quantities.md),
[`model-method-laplace`](https://mc-stan.org/cmdstanr/dev/reference/model-method-laplace.md),
[`model-method-optimize`](https://mc-stan.org/cmdstanr/dev/reference/model-method-optimize.md),
[`model-method-pathfinder`](https://mc-stan.org/cmdstanr/dev/reference/model-method-pathfinder.md),
[`model-method-sample`](https://mc-stan.org/cmdstanr/dev/reference/model-method-sample.md),
[`model-method-sample_mpi`](https://mc-stan.org/cmdstanr/dev/reference/model-method-sample_mpi.md),
[`model-method-variables`](https://mc-stan.org/cmdstanr/dev/reference/model-method-variables.md),
[`model-method-variational`](https://mc-stan.org/cmdstanr/dev/reference/model-method-variational.md)
