# Access information from a `CmdStanModel` object

These methods access information stored in a
[`CmdStanModel`](https://mc-stan.org/cmdstanr/pr/1287/reference/CmdStanModel.md)
object, print its Stan program, and manage paths to its executable and
generated C++ file. For how the executable was built, see the
[`$build_info()`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-build_info.md)
method, which has its own page.

    stan_file()
    has_stan_file()
    code()
    print(line_numbers = getOption("cmdstanr_print_line_numbers", FALSE))
    model_name()
    exe_file()
    include_paths()
    cmdstan_version()
    cpp_options()
    user_header()
    hpp_file()
    save_hpp_file(dir = NULL)

## Arguments

- line_numbers:

  (logical) Should line numbers be printed? The default is
  `getOption("cmdstanr_print_line_numbers", FALSE)`.

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
  [`CmdStanModel`](https://mc-stan.org/cmdstanr/pr/1287/reference/CmdStanModel.md)
  object invisibly.

- `$model_name()` returns the model name as a string.

- `$exe_file()` returns a path as a string, or `character(0)` if no
  executable path is set.

- `$include_paths()` returns a character vector of absolute paths or
  `NULL`.

- `$cmdstan_version()` returns the version of CmdStan that built the
  executable, as a string.

- `$cpp_options()` returns a named list of C++ options, with names in
  their make spelling and values as the strings make received: `TRUE`
  comes back as `"TRUE"` and `FALSE` as `""`. To ask whether the
  executable was built with a feature, use `$build_info()`, which
  reports logicals.

- `$user_header()` returns the absolute path to the user header as a
  string, or `NULL` if the model has no user header.

- `$hpp_file()` returns the path to the `.hpp` file holding the C++ code
  generated for the Stan program when the model object was created. It
  errors if the model was created without a Stan file.

- `$save_hpp_file()` moves the `.hpp` file to `dir`, updates the stored
  path, and returns the new path invisibly.

## See also

[`cmdstan_model()`](https://mc-stan.org/cmdstanr/pr/1287/reference/cmdstan_model.md)

The CmdStanR website
([mc-stan.org/cmdstanr](https://mc-stan.org/cmdstanr/)) for online
documentation and tutorials.

The Stan and CmdStan documentation:

- Stan documentation:
  [mc-stan.org/users/documentation](https://mc-stan.org/users/documentation/)

- CmdStan User’s Guide:
  [mc-stan.org/docs/cmdstan-guide](https://mc-stan.org/docs/cmdstan-guide/)

Other CmdStanModel methods:
[`model-method-build_info`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-build_info.md),
[`model-method-check_syntax`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-check_syntax.md),
[`model-method-cmdstan_defaults`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-cmdstan_defaults.md),
[`model-method-diagnose`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-diagnose.md),
[`model-method-expose_functions`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-expose_functions.md),
[`model-method-format`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-format.md),
[`model-method-generate-quantities`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-generate-quantities.md),
[`model-method-is_current`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-is_current.md),
[`model-method-laplace`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-laplace.md),
[`model-method-optimize`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-optimize.md),
[`model-method-pathfinder`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-pathfinder.md),
[`model-method-sample`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-sample.md),
[`model-method-sample_mpi`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-sample_mpi.md),
[`model-method-variables`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-variables.md),
[`model-method-variational`](https://mc-stan.org/cmdstanr/pr/1287/reference/model-method-variational.md)
