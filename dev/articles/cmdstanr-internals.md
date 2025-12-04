# How does CmdStanR work?

## Introduction

This vignette is intended to be read after the [*Getting started with
CmdStanR*](http://mc-stan.org/cmdstanr/articles/cmdstanr.md) vignette.
Please read that first for important background. In this document we
provide additional details about compiling models, passing in data, and
how CmdStan output is saved and read back into R.

We will only use the `$sample()` method in examples, but all model
fitting methods work in a similar way under the hood.

``` r
library(cmdstanr)
check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)
```

## Compilation

### Immediate compilation

The
[`cmdstan_model()`](https://mc-stan.org/cmdstanr/dev/reference/cmdstan_model.md)
function creates a new `CmdStanModel` object. The `CmdStanModel` object
stores the path to a Stan program as well as the path to a compiled
executable.

``` r
stan_file <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
mod <- cmdstan_model(stan_file)
mod$print()
```

    data {
      int<lower=0> N;
      array[N] int<lower=0, upper=1> y;
    }
    parameters {
      real<lower=0, upper=1> theta;
    }
    model {
      theta ~ beta(1, 1); // uniform prior on interval 0,1
      y ~ bernoulli(theta);
    }

``` r
mod$stan_file()
```

    [1] "/home/runner/.cmdstan/cmdstan-2.37.0/examples/bernoulli/bernoulli.stan"

``` r
mod$exe_file()
```

    [1] "/home/runner/.cmdstan/cmdstan-2.37.0/examples/bernoulli/bernoulli"

Subsequently, if you create a `CmdStanModel` object from the same Stan
file then compilation will be skipped (assuming the file hasn’t
changed).

``` r
mod <- cmdstan_model(stan_file)
```

Internally,
[`cmdstan_model()`](https://mc-stan.org/cmdstanr/dev/reference/cmdstan_model.md)
first creates the `CmdStanModel` object from just the Stan file and then
calls its
[`$compile()`](http://mc-stan.org/cmdstanr/reference/model-method-compile.md)
method. Optional arguments to the `$compile()` method can be passed via
`...`.

``` r
mod <- cmdstan_model(
  stan_file,
  force_recompile = TRUE,
  include_paths = "paths/to/directories/with/included/files",
  cpp_options = list(stan_threads = TRUE, STANC2 = TRUE)
)
```

### Delayed compilation

It is also possible to delay compilation when creating the
`CmdStanModel` object by specifying `compile=FALSE` and then later
calling the `$compile()` method directly.

``` r
unlink(mod$exe_file())
mod <- cmdstan_model(stan_file, compile = FALSE)
mod$exe_file() # not yet created
```

    character(0)

``` r
mod$compile()
mod$exe_file()
```

    [1] "/home/runner/.cmdstan/cmdstan-2.37.0/examples/bernoulli/bernoulli"

### Pedantic check

If you are using CmdStan version 2.24 or later and CmdStanR version
0.2.1 or later, you can run a pedantic check for your model. CmdStanR
will always check that your Stan program does not contain any invalid
syntax but with pedantic mode enabled the check will also warn you about
other potential issues in your model, for example:

- Distribution usages issues: distribution arguments do not match the
  distribution specification, or some specific distribution is used in
  an inadvisable way.
- Unused parameter: a parameter is defined but does not contribute to
  target.
- Large or small constant in a distribution: very large or very small
  constants are used as distribution arguments.
- Control flow depends on a parameter: branching control flow (like
  if/else) depends on a parameter value.
- Parameter has multiple twiddles: a parameter is on the left-hand side
  of multiple twiddles (i.e., multiple `~` symbols).
- Parameter has zero or multiple priors: a parameter has zero or more
  than one prior distribution.
- Variable is used before assignment: a variable is used before being
  assigned a value.
- Strict or nonsensical parameter bounds: a parameter is given
  questionable bounds.

For the latest information on the checks performed in pedantic mode see
the [Pedantic mode
chapter](https://mc-stan.org/docs/stan-users-guide/pedantic-mode.html)
in the Stan Reference Manual.

Pedantic mode is available when compiling the model or when using the
separate `$check_syntax()` method of a `CmdStanModel` object. Internally
this corresponds to setting the `stanc` (Stan transpiler) option
`warn-pedantic`. Here we demonstrate pedantic mode with a Stan program
that is syntactically correct but is missing a lower bound and a prior
for a parameter.

``` r
stan_file_pedantic <- write_stan_file("
data {
  int N;
  array[N] int y;
}
parameters {
  // should have <lower=0> but omitting to demonstrate pedantic mode
  real lambda;
}
model {
  y ~ poisson(lambda);
}
")
```

To turn on pedantic mode at compile time you can set `pedantic=TRUE` in
the call to
[`cmdstan_model()`](https://mc-stan.org/cmdstanr/dev/reference/cmdstan_model.md)
(or when calling the `$compile()` method directly if using the delayed
compilation approach described above).

``` r
mod_pedantic <- cmdstan_model(stan_file_pedantic, pedantic = TRUE)
Warning in '/tmp/Rtmp6PSBgr/model-370941ae8cd.stan', line 11, column 14: A
    poisson distribution is given parameter lambda as a rate parameter
    (argument 1), but lambda was not constrained to be strictly positive.
Warning: The parameter lambda has no priors. This means either no prior is
    provided, or the prior(s) depend on data variables. In the later case,
    this may be a false positive.
```

To turn on pedantic mode separately from compilation use the `pedantic`
argument to the `$check_syntax()` method.

``` r
mod_pedantic$check_syntax(pedantic = TRUE)
Warning in '/tmp/Rtmp6PSBgr/model_febb1e69c7387a0e64cf13583e078104.stan', line 11, column 14: A
    poisson distribution is given parameter lambda as a rate parameter
    (argument 1), but lambda was not constrained to be strictly positive.
Warning: The parameter lambda has no priors. This means either no prior is
    provided, or the prior(s) depend on data variables. In the later case,
    this may be a false positive.
Stan program is syntactically correct
```

Using `pedantic=TRUE` via the `$check_syntax()` method also has the
advantage that it can be used even if the model hasn’t been compiled
yet. This can be helpful because the pedantic and syntax checks
themselves are much faster than compilation.

``` r
file.remove(mod_pedantic$exe_file()) # delete compiled executable
[1] TRUE
rm(mod_pedantic)

mod_pedantic <- cmdstan_model(stan_file_pedantic, compile = FALSE)
mod_pedantic$check_syntax(pedantic = TRUE)
Warning in '/tmp/Rtmp6PSBgr/model_febb1e69c7387a0e64cf13583e078104.stan', line 11, column 14: A
    poisson distribution is given parameter lambda as a rate parameter
    (argument 1), but lambda was not constrained to be strictly positive.
Warning: The parameter lambda has no priors. This means either no prior is
    provided, or the prior(s) depend on data variables. In the later case,
    this may be a false positive.
Stan program is syntactically correct
```

### Stan model variables

If using CmdStan 2.27 or newer, you can obtain the names, types and
dimensions of the data, parameters, transformed parameters and generated
quantities variables of a Stan model using the `$variables()` method of
the `CmdStanModel` object.

``` r
stan_file_variables <- write_stan_file("
data {
  int<lower=1> J;
  vector<lower=0>[J] sigma;
  vector[J] y;
}
parameters {
  real mu;
  real<lower=0> tau;
  vector[J] theta_raw;
}
transformed parameters {
  vector[J] theta = mu + tau * theta_raw;
}
model {
  target += normal_lpdf(tau | 0, 10);
  target += normal_lpdf(mu | 0, 10);
  target += normal_lpdf(theta_raw | 0, 1);
  target += normal_lpdf(y | theta, sigma);
}
")
mod_v <- cmdstan_model(stan_file_variables)
variables <- mod_v$variables()
```

The `$variables()` method returns a list with `data`, `parameters`,
`transformed_parameters` and `generated_quantities` elements, each
corresponding to variables in their respective block of the program.
Transformed data variables are not listed as they are not used in the
model’s input or output.

``` r
names(variables)
```

    [1] "parameters"             "included_files"         "data"                  
    [4] "transformed_parameters" "generated_quantities"  

``` r
names(variables$data)
```

    [1] "J"     "sigma" "y"    

``` r
names(variables$parameters)
```

    [1] "mu"        "tau"       "theta_raw"

``` r
names(variables$transformed_parameters)
```

    [1] "theta"

``` r
names(variables$generated_quantities)
```

    character(0)

Each variable is represented as a list containing the type information
(currently limited to `real` or `int`) and the number of dimensions.

``` r
variables$data$J
```

    $type
    [1] "int"

    $dimensions
    [1] 0

``` r
variables$data$sigma
```

    $type
    [1] "real"

    $dimensions
    [1] 1

``` r
variables$parameters$tau
```

    $type
    [1] "real"

    $dimensions
    [1] 0

``` r
variables$transformed_parameters$theta
```

    $type
    [1] "real"

    $dimensions
    [1] 1

### Executable location

By default, the executable is created in the same directory as the file
containing the Stan program. You can also specify a different location
with the `dir` argument.

``` r
mod <- cmdstan_model(stan_file, dir = "path/to/directory/for/executable")
```

## Processing data

There are three data formats that CmdStanR allows when fitting a model:

- named list of R objects
- JSON file
- R dump file

### Named list of R objects

Like the RStan interface, CmdStanR accepts a named list of R objects
where the names correspond to variables declared in the data block of
the Stan program. In the Bernoulli model the data is `N`, the number of
data points, and `y` an integer array of observations.

``` r
mod$print()
```

    data {
      int<lower=0> N;
      array[N] int<lower=0, upper=1> y;
    }
    parameters {
      real<lower=0, upper=1> theta;
    }
    model {
      theta ~ beta(1, 1); // uniform prior on interval 0,1
      y ~ bernoulli(theta);
    }

``` r
# data block has 'N' and 'y'
data_list <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))
fit <- mod$sample(data = data_list)
```

Because CmdStan doesn’t accept lists of R objects, CmdStanR will first
write the data to a temporary JSON file using
[`write_stan_json()`](https://mc-stan.org/cmdstanr/dev/reference/write_stan_json.md).
This happens internally, but it is also possible to call
[`write_stan_json()`](https://mc-stan.org/cmdstanr/dev/reference/write_stan_json.md)
directly.

``` r
data_list <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))
json_file <- tempfile(fileext = ".json")
write_stan_json(data_list, json_file)
cat(readLines(json_file), sep = "\n")
```

    {
      "N": 10,
      "y": [0, 1, 0, 0, 0, 0, 0, 0, 0, 1]
    }

### JSON file

If you already have your data in a JSON file you can just pass that file
directly to CmdStanR instead of using a list of R objects. For example,
we could pass in the JSON file we created above using
[`write_stan_json()`](https://mc-stan.org/cmdstanr/dev/reference/write_stan_json.md):

``` r
fit <- mod$sample(data = json_file)
```

### R dump file

Finally, it is also possible to use the R dump file format. This is
*not* recommended because CmdStan can process JSON faster than R dump,
but CmdStanR allows it because CmdStan will accept files created by
`rstan::stan_rdump()`:

``` r
rdump_file <- tempfile(fileext = ".data.R")
rstan::stan_rdump(names(data_list), file = rdump_file, envir = list2env(data_list))
cat(readLines(rdump_file), sep = "\n")
fit <- mod$sample(data = rdump_file)
```

## Writing CmdStan output to CSV

### Default temporary files

``` r
data_list <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))
fit <- mod$sample(data = data_list)
```

When fitting a model, the default behavior is to write the output from
CmdStan to CSV files in a temporary directory.

``` r
fit$output_files()
```

    [1] "/tmp/Rtmp6PSBgr/bernoulli-202512040232-1-5be241.csv"
    [2] "/tmp/Rtmp6PSBgr/bernoulli-202512040232-2-5be241.csv"
    [3] "/tmp/Rtmp6PSBgr/bernoulli-202512040232-3-5be241.csv"
    [4] "/tmp/Rtmp6PSBgr/bernoulli-202512040232-4-5be241.csv"

These files will be lost if you end your R session or if you remove the
`fit` object and force (or wait for) garbage collection.

``` r
files <- fit$output_files()
file.exists(files)
```

    [1] TRUE TRUE TRUE TRUE

``` r
rm(fit)
gc()
```

              used (Mb) gc trigger  (Mb) max used (Mb)
    Ncells 1217983 65.1    2504474 133.8  1589948 85.0
    Vcells 2123403 16.3    8388608  64.0  4907306 37.5

``` r
file.exists(files)
```

    [1] FALSE FALSE FALSE FALSE

### Non-temporary files

To save these files to a non-temporary location there are two options.
You can either specify the `output_dir` argument to `mod$sample()` or
use `fit$save_output_files()` after fitting the model.

``` r
# see ?save_output_files for info on optional arguments
fit$save_output_files(dir = "path/to/directory")
```

``` r
fit <- mod$sample(
  data = data_list,
  output_dir = "path/to/directory"
)
```

## Reading CmdStan output into R

### Lazy CSV reading

With the exception of some diagnostic information, the CSV files are not
read into R until their contents are requested by calling a method that
requires them (e.g., `fit$draws()`, `fit$summary()`, etc.). If we
examine the structure of the `fit` object, notice how the `Private` slot
`draws_` is `NULL`, indicating that the CSV files haven’t yet been read
into R.

``` r
str(fit)
```

    Classes 'CmdStanMCMC', 'CmdStanFit', 'R6' <CmdStanMCMC>
      Inherits from: <CmdStanFit>
      Public:
        clone: function (deep = FALSE) 
        cmdstan_diagnose: function () 
        cmdstan_summary: function (flags = NULL) 
        code: function () 
        config_files: function (include_failed = FALSE) 
        constrain_variables: function (unconstrained_variables, transformed_parameters = TRUE, 
        data_file: function () 
        diagnostic_summary: function (diagnostics = c("divergences", "treedepth", "ebfmi"), 
        draws: function (variables = NULL, inc_warmup = FALSE, format = getOption("cmdstanr_draws_format", 
        expose_functions: function (global = FALSE, verbose = FALSE) 
        functions: environment
        grad_log_prob: function (unconstrained_variables, jacobian = TRUE, jacobian_adjustment = NULL) 
        hessian: function (unconstrained_variables, jacobian = TRUE, jacobian_adjustment = NULL) 
        init: function () 
        init_model_methods: function (seed = 1, verbose = FALSE, hessian = FALSE) 
        initialize: function (runset) 
        inv_metric: function (matrix = TRUE) 
        latent_dynamics_files: function (include_failed = FALSE) 
        log_prob: function (unconstrained_variables, jacobian = TRUE, jacobian_adjustment = NULL) 
        loo: function (variables = "log_lik", r_eff = FALSE, moment_match = FALSE, 
        lp: function () 
        metadata: function () 
        metric_files: function (include_failed = FALSE) 
        num_chains: function () 
        num_procs: function () 
        output: function (id = NULL) 
        output_files: function (include_failed = FALSE) 
        print: function (variables = NULL, ..., digits = 2, max_rows = getOption("cmdstanr_max_rows", 
        profile_files: function (include_failed = FALSE) 
        profiles: function () 
        return_codes: function () 
        runset: CmdStanRun, R6
        sampler_diagnostics: function (inc_warmup = FALSE, format = getOption("cmdstanr_draws_format", 
        save_config_files: function (dir = ".", basename = NULL, timestamp = TRUE, random = TRUE) 
        save_data_file: function (dir = ".", basename = NULL, timestamp = TRUE, random = TRUE) 
        save_latent_dynamics_files: function (dir = ".", basename = NULL, timestamp = TRUE, random = TRUE) 
        save_metric_files: function (dir = ".", basename = NULL, timestamp = TRUE, random = TRUE) 
        save_object: function (file, ...) 
        save_output_files: function (dir = ".", basename = NULL, timestamp = TRUE, random = TRUE) 
        save_profile_files: function (dir = ".", basename = NULL, timestamp = TRUE, random = TRUE) 
        summary: function (variables = NULL, ...) 
        time: function () 
        unconstrain_draws: function (files = NULL, draws = NULL, format = getOption("cmdstanr_draws_format", 
        unconstrain_variables: function (variables) 
        variable_skeleton: function (transformed_parameters = TRUE, generated_quantities = TRUE) 
      Private:
        draws_: NULL
        init_: NULL
        inv_metric_: list
        metadata_: list
        model_methods_env_: environment
        profiles_: NULL
        read_csv_: function (variables = NULL, sampler_diagnostics = NULL, format = getOption("cmdstanr_draws_format", 
        return_codes_: 0 0 0 0
        sampler_diagnostics_: 2 1 1 1 1 2 2 1 2 1 1 1 1 2 2 2 1 2 1 1 1 1 1 2 2 1 1 1  ...
        warmup_draws_: NULL
        warmup_sampler_diagnostics_: NULL 

After we call a method that requires the draws then if we reexamine the
structure of the object we will see that the `draws_` slot in `Private`
is no longer empty.

``` r
draws <- fit$draws() # force CSVs to be read into R
str(fit)
```

    Classes 'CmdStanMCMC', 'CmdStanFit', 'R6' <CmdStanMCMC>
      Inherits from: <CmdStanFit>
      Public:
        clone: function (deep = FALSE) 
        cmdstan_diagnose: function () 
        cmdstan_summary: function (flags = NULL) 
        code: function () 
        config_files: function (include_failed = FALSE) 
        constrain_variables: function (unconstrained_variables, transformed_parameters = TRUE, 
        data_file: function () 
        diagnostic_summary: function (diagnostics = c("divergences", "treedepth", "ebfmi"), 
        draws: function (variables = NULL, inc_warmup = FALSE, format = getOption("cmdstanr_draws_format", 
        expose_functions: function (global = FALSE, verbose = FALSE) 
        functions: environment
        grad_log_prob: function (unconstrained_variables, jacobian = TRUE, jacobian_adjustment = NULL) 
        hessian: function (unconstrained_variables, jacobian = TRUE, jacobian_adjustment = NULL) 
        init: function () 
        init_model_methods: function (seed = 1, verbose = FALSE, hessian = FALSE) 
        initialize: function (runset) 
        inv_metric: function (matrix = TRUE) 
        latent_dynamics_files: function (include_failed = FALSE) 
        log_prob: function (unconstrained_variables, jacobian = TRUE, jacobian_adjustment = NULL) 
        loo: function (variables = "log_lik", r_eff = FALSE, moment_match = FALSE, 
        lp: function () 
        metadata: function () 
        metric_files: function (include_failed = FALSE) 
        num_chains: function () 
        num_procs: function () 
        output: function (id = NULL) 
        output_files: function (include_failed = FALSE) 
        print: function (variables = NULL, ..., digits = 2, max_rows = getOption("cmdstanr_max_rows", 
        profile_files: function (include_failed = FALSE) 
        profiles: function () 
        return_codes: function () 
        runset: CmdStanRun, R6
        sampler_diagnostics: function (inc_warmup = FALSE, format = getOption("cmdstanr_draws_format", 
        save_config_files: function (dir = ".", basename = NULL, timestamp = TRUE, random = TRUE) 
        save_data_file: function (dir = ".", basename = NULL, timestamp = TRUE, random = TRUE) 
        save_latent_dynamics_files: function (dir = ".", basename = NULL, timestamp = TRUE, random = TRUE) 
        save_metric_files: function (dir = ".", basename = NULL, timestamp = TRUE, random = TRUE) 
        save_object: function (file, ...) 
        save_output_files: function (dir = ".", basename = NULL, timestamp = TRUE, random = TRUE) 
        save_profile_files: function (dir = ".", basename = NULL, timestamp = TRUE, random = TRUE) 
        summary: function (variables = NULL, ...) 
        time: function () 
        unconstrain_draws: function (files = NULL, draws = NULL, format = getOption("cmdstanr_draws_format", 
        unconstrain_variables: function (variables) 
        variable_skeleton: function (transformed_parameters = TRUE, generated_quantities = TRUE) 
      Private:
        draws_: -6.7488137 -7.3283887 -7.0892006 -6.8032797 -7.1263639 - ...
        init_: NULL
        inv_metric_: list
        metadata_: list
        model_methods_env_: environment
        profiles_: NULL
        read_csv_: function (variables = NULL, sampler_diagnostics = NULL, format = getOption("cmdstanr_draws_format", 
        return_codes_: 0 0 0 0
        sampler_diagnostics_: 2 1 1 1 1 2 2 1 2 1 1 1 1 2 2 2 1 2 1 1 1 1 1 2 2 1 1 1  ...
        warmup_draws_: NULL
        warmup_sampler_diagnostics_: NULL 

For models with many parameters, transformed parameters, or generated
quantities, if only some are requested (e.g., by specifying the
`variables` argument to `fit$draws()`) then CmdStanR will only read in
the requested variables (unless they have already been read in).

### read_cmdstan_csv()

Internally, the
[`read_cmdstan_csv()`](https://mc-stan.org/cmdstanr/dev/reference/read_cmdstan_csv.md)
function is used to read the CmdStan CSV files into R. This function is
exposed to users, so you can also call it directly.

``` r
# see ?read_cmdstan_csv for info on optional arguments controlling
# what information is read in
csv_contents <- read_cmdstan_csv(fit$output_files())
str(csv_contents)
```

    List of 8
     $ metadata                       :List of 42
      ..$ stan_version_major  : num 2
      ..$ stan_version_minor  : num 37
      ..$ stan_version_patch  : num 0
      ..$ start_datetime      : chr "2025-12-04 02:32:33 UTC"
      ..$ method              : chr "sample"
      ..$ save_warmup         : int 0
      ..$ thin                : num 1
      ..$ gamma               : num 0.05
      ..$ kappa               : num 0.75
      ..$ t0                  : num 10
      ..$ init_buffer         : num 75
      ..$ term_buffer         : num 50
      ..$ window              : num 25
      ..$ save_metric         : int 0
      ..$ algorithm           : chr "hmc"
      ..$ engine              : chr "nuts"
      ..$ metric              : chr "diag_e"
      ..$ stepsize_jitter     : num 0
      ..$ num_chains          : num 1
      ..$ id                  : num [1:4] 1 2 3 4
      ..$ init                : num [1:4] 2 2 2 2
      ..$ seed                : num 31749990
      ..$ refresh             : num 100
      ..$ sig_figs            : num 8
      ..$ profile_file        : chr "/tmp/Rtmp6PSBgr/bernoulli-profile-202512040232-1-2c6e11.csv"
      ..$ save_cmdstan_config : int 0
      ..$ stanc_version       : chr "stanc3 v2.37.0"
      ..$ sampler_diagnostics : chr [1:6] "accept_stat__" "stepsize__" "treedepth__" "n_leapfrog__" ...
      ..$ variables           : chr [1:2] "lp__" "theta"
      ..$ step_size_adaptation: num [1:4] 0.895 0.892 0.942 0.929
      ..$ model_name          : chr "bernoulli_model"
      ..$ adapt_engaged       : int 1
      ..$ adapt_delta         : num 0.8
      ..$ max_treedepth       : num 10
      ..$ step_size           : num [1:4] 1 1 1 1
      ..$ iter_warmup         : num 1000
      ..$ iter_sampling       : num 1000
      ..$ threads_per_chain   : num 1
      ..$ time                :'data.frame':    4 obs. of  4 variables:
      .. ..$ chain_id: num [1:4] 1 2 3 4
      .. ..$ warmup  : num [1:4] 0.002 0.002 0.002 0.002
      .. ..$ sampling: num [1:4] 0.005 0.005 0.005 0.005
      .. ..$ total   : num [1:4] 0.007 0.007 0.007 0.007
      ..$ stan_variable_sizes :List of 2
      .. ..$ lp__ : num 1
      .. ..$ theta: num 1
      ..$ stan_variables      : chr [1:2] "lp__" "theta"
      ..$ model_params        : chr [1:2] "lp__" "theta"
     $ time                           :List of 2
      ..$ total : int NA
      ..$ chains:'data.frame':  4 obs. of  4 variables:
      .. ..$ chain_id: num [1:4] 1 2 3 4
      .. ..$ warmup  : num [1:4] 0.002 0.002 0.002 0.002
      .. ..$ sampling: num [1:4] 0.005 0.005 0.005 0.005
      .. ..$ total   : num [1:4] 0.007 0.007 0.007 0.007
     $ inv_metric                     :List of 4
      ..$ 1: num 0.588
      ..$ 2: num 0.545
      ..$ 3: num 0.577
      ..$ 4: num 0.527
     $ step_size                      :List of 4
      ..$ 1: num 0.895
      ..$ 2: num 0.892
      ..$ 3: num 0.942
      ..$ 4: num 0.929
     $ warmup_draws                   : NULL
     $ post_warmup_draws              : 'draws_array' num [1:1000, 1:4, 1:2] -6.75 -7.33 -7.09 -6.8 -7.13 ...
      ..- attr(*, "dimnames")=List of 3
      .. ..$ iteration: chr [1:1000] "1" "2" "3" "4" ...
      .. ..$ chain    : chr [1:4] "1" "2" "3" "4"
      .. ..$ variable : chr [1:2] "lp__" "theta"
     $ warmup_sampler_diagnostics     : NULL
     $ post_warmup_sampler_diagnostics: 'draws_array' num [1:1000, 1:4, 1:6] 0.999 0.863 1 0.965 0.932 ...
      ..- attr(*, "dimnames")=List of 3
      .. ..$ iteration: chr [1:1000] "1" "2" "3" "4" ...
      .. ..$ chain    : chr [1:4] "1" "2" "3" "4"
      .. ..$ variable : chr [1:6] "accept_stat__" "stepsize__" "treedepth__" "n_leapfrog__" ...

### as_cmdstan_fit()

If you need to manually create fitted model objects from CmdStan CSV
files use
[`as_cmdstan_fit()`](https://mc-stan.org/cmdstanr/dev/reference/read_cmdstan_csv.md).

``` r
fit2 <- as_cmdstan_fit(fit$output_files())
```

This is pointless in our case since we have the original `fit` object,
but this function can be used to create fitted model objects
(`CmdStanMCMC`, `CmdStanMLE`, etc.) from any CmdStan CSV files.

### Saving and accessing advanced algorithm info (latent dynamics)

If `save_latent_dynamics` is set to `TRUE` when running the `$sample()`
method then additional CSV files are created (one per chain) that
provide access to quantities used under the hood by Stan’s
implementation of dynamic Hamiltonian Monte Carlo.

CmdStanR does not yet provide a special method for processing these
files but they can be read into R using R’s standard CSV reading
functions.

``` r
fit <- mod$sample(data = data_list, save_latent_dynamics = TRUE)
```

``` r
fit$latent_dynamics_files()
```

    [1] "/tmp/Rtmp6PSBgr/bernoulli-diagnostic-202512040232-1-057096.csv"
    [2] "/tmp/Rtmp6PSBgr/bernoulli-diagnostic-202512040232-2-057096.csv"
    [3] "/tmp/Rtmp6PSBgr/bernoulli-diagnostic-202512040232-3-057096.csv"
    [4] "/tmp/Rtmp6PSBgr/bernoulli-diagnostic-202512040232-4-057096.csv"

``` r
# read one of the files in
x <- utils::read.csv(fit$latent_dynamics_files()[1], comment.char = "#")
head(x)
```

           lp__ accept_stat__ stepsize__ treedepth__ n_leapfrog__ divergent__
    1 -7.008996     0.9351107   1.069334           1            3           0
    2 -7.299006     0.8867785   1.069334           1            1           0
    3 -6.828942     1.0000000   1.069334           1            1           0
    4 -6.820257     1.0000000   1.069334           1            1           0
    5 -6.753109     0.9935051   1.069334           2            3           0
    6 -6.897199     0.9607730   1.069334           1            3           0
      energy__      theta    p_theta    g_theta
    1 7.059322 -0.6338870  0.4400119  1.1595562
    2 7.380108 -0.4323169 -0.5585757  1.7228786
    3 7.122224 -0.8359846 -1.0622074  0.6285738
    4 6.850743 -0.8502093  0.3424676  0.5926674
    5 6.854350 -1.1662395  0.6240870 -0.1495753
    6 6.920060 -0.7444621  0.2965665  0.8643500

The column `lp__` is also provided via `fit$draws()`, and the columns
`accept_stat__`, `stepsize__`, `treedepth__`, `n_leapfrog__`,
`divergent__`, and `energy__` are also provided by
`fit$sampler_diagnostics()`, but there are several columns unique to the
latent dynamics file.

``` r
head(x[, c("theta", "p_theta", "g_theta")])
```

           theta    p_theta    g_theta
    1 -0.6338870  0.4400119  1.1595562
    2 -0.4323169 -0.5585757  1.7228786
    3 -0.8359846 -1.0622074  0.6285738
    4 -0.8502093  0.3424676  0.5926674
    5 -1.1662395  0.6240870 -0.1495753
    6 -0.7444621  0.2965665  0.8643500

Our model has a single parameter `theta` and the three columns above
correspond to `theta` in the *unconstrained* space (`theta` on the
constrained space is accessed via `fit$draws()`), the auxiliary momentum
`p_theta`, and the gradient `g_theta`. In general, each of these three
columns will exist for *every* parameter in the model.

## Developing using CmdStanR

CmdStanR can of course be used for developing other packages that
require compiling and running Stan models as well as using new or custom
Stan features available through CmdStan.

### Pre-compiled Stan models in R packages

You may compile a Stan model at runtime (e.g. just before sampling), or
you may compile all the models inside the package file system in advance
at installation time. The latter avoids compilations at runtime, which
matters in centrally managed R installations where users should not
compile their own software.

To pre-compile all the models in a package, you may create top-level
scripts `configure` and `configure.win` which run
[`cmdstan_model()`](https://mc-stan.org/cmdstanr/dev/reference/cmdstan_model.md)
with `compile = TRUE` and save the compiled executables somewhere inside
the `inst/` folder of the package source. The
[`instantiate`](https://wlandau.github.io/instantiate/) package helps
developers configure packages this way, and it documents other topics
such as submitting to CRAN and administering CmdStan. Kevin Ushey’s
[`configure`](https://github.com/kevinushey/configure) package helps
create and manage package configuration files in general.

### Troubleshooting and debugging

When developing or testing new features it might be useful to have more
information on how CmdStan is called internally and to see more
information printed when compiling or running models. This can be
enabled for an entire R session by setting the option
`"cmdstanr_verbose"` to `TRUE`.

``` r
options("cmdstanr_verbose"=TRUE)

mod <- cmdstan_model(stan_file, force_recompile = TRUE)
```

    Running make /tmp/Rtmp6PSBgr/model-37094af83906 \
      "STANCFLAGS +=  --name='bernoulli_model'"

    --- Translating Stan model to C++ code ---
    bin/stanc --name='bernoulli_model' --o=/tmp/Rtmp6PSBgr/model-37094af83906.hpp /tmp/Rtmp6PSBgr/model-37094af83906.stan

    --- Compiling C++ code ---
    g++ -Wno-deprecated-declarations -std=c++17 -pthread -D_REENTRANT -Wno-sign-compare -Wno-ignored-attributes -Wno-class-memaccess      -I stan/lib/stan_math/lib/tbb_2020.3/include    -O3 -I src -I stan/src -I stan/lib/rapidjson_1.1.0/ -I lib/CLI11-1.9.1/ -I stan/lib/stan_math/ -I stan/lib/stan_math/lib/eigen_3.4.0 -I stan/lib/stan_math/lib/boost_1.87.0 -I stan/lib/stan_math/lib/sundials_6.1.1/include -I stan/lib/stan_math/lib/sundials_6.1.1/src/sundials    -DBOOST_DISABLE_ASSERTS          -c -Wno-ignored-attributes   -x c++ -o /tmp/Rtmp6PSBgr/model-37094af83906.o /tmp/Rtmp6PSBgr/model-37094af83906.hpp

    --- Linking model ---
    g++ -Wno-deprecated-declarations -std=c++17 -pthread -D_REENTRANT -Wno-sign-compare -Wno-ignored-attributes -Wno-class-memaccess      -I stan/lib/stan_math/lib/tbb_2020.3/include    -O3 -I src -I stan/src -I stan/lib/rapidjson_1.1.0/ -I lib/CLI11-1.9.1/ -I stan/lib/stan_math/ -I stan/lib/stan_math/lib/eigen_3.4.0 -I stan/lib/stan_math/lib/boost_1.87.0 -I stan/lib/stan_math/lib/sundials_6.1.1/include -I stan/lib/stan_math/lib/sundials_6.1.1/src/sundials    -DBOOST_DISABLE_ASSERTS               -Wl,-L,"/home/runner/.cmdstan/cmdstan-2.37.0/stan/lib/stan_math/lib/tbb"   -Wl,-rpath,"/home/runner/.cmdstan/cmdstan-2.37.0/stan/lib/stan_math/lib/tbb"      /tmp/Rtmp6PSBgr/model-37094af83906.o src/cmdstan/main.o       -ltbb   stan/lib/stan_math/lib/sundials_6.1.1/lib/libsundials_nvecserial.a stan/lib/stan_math/lib/sundials_6.1.1/lib/libsundials_cvodes.a stan/lib/stan_math/lib/sundials_6.1.1/lib/libsundials_idas.a stan/lib/stan_math/lib/sundials_6.1.1/lib/libsundials_kinsol.a  stan/lib/stan_math/lib/tbb/libtbb.so.2 -o /tmp/Rtmp6PSBgr/model-37094af83906
    rm /tmp/Rtmp6PSBgr/model-37094af83906.o /tmp/Rtmp6PSBgr/model-37094af83906.hpp
    stan_version_major = 2
    stan_version_minor = 37
    stan_version_patch = 0
    STAN_THREADS=false
    STAN_MPI=false
    STAN_OPENCL=false
    STAN_NO_RANGE_CHECKS=false
    STAN_CPP_OPTIMS=false

``` r
fit <- mod$sample(
  data = data_list,
  chains = 1,
  iter_warmup = 100,
  iter_sampling = 100
)
```

    Running MCMC with 1 chain...

    Running ./bernoulli 'id=1' random 'seed=1376020223' data \
      'file=/tmp/Rtmp6PSBgr/standata-37094b7f9511.json' output \
      'file=/tmp/Rtmp6PSBgr/bernoulli-202512040232-1-1e1263.csv' \
      'profile_file=/tmp/Rtmp6PSBgr/bernoulli-profile-202512040232-1-3dca3e.csv' \
      'method=sample' 'num_samples=100' 'num_warmup=100' 'save_warmup=0' \
      'algorithm=hmc' 'engine=nuts' adapt 'engaged=1'
    Chain 1 method = sample (Default) 
    Chain 1   sample 
    Chain 1     num_samples = 100 
    Chain 1     num_warmup = 100 
    Chain 1     save_warmup = false (Default) 
    Chain 1     thin = 1 (Default) 
    Chain 1     adapt 
    Chain 1       engaged = true (Default) 
    Chain 1       gamma = 0.05 (Default) 
    Chain 1       delta = 0.8 (Default) 
    Chain 1       kappa = 0.75 (Default) 
    Chain 1       t0 = 10 (Default) 
    Chain 1       init_buffer = 75 (Default) 
    Chain 1       term_buffer = 50 (Default) 
    Chain 1       window = 25 (Default) 
    Chain 1       save_metric = false (Default) 
    Chain 1     algorithm = hmc (Default) 
    Chain 1       hmc 
    Chain 1         engine = nuts (Default) 
    Chain 1           nuts 
    Chain 1             max_depth = 10 (Default) 
    Chain 1         metric = diag_e (Default) 
    Chain 1         metric_file =  (Default) 
    Chain 1         stepsize = 1 (Default) 
    Chain 1         stepsize_jitter = 0 (Default) 
    Chain 1     num_chains = 1 (Default) 
    Chain 1 id = 1 (Default) 
    Chain 1 data 
    Chain 1   file = /tmp/Rtmp6PSBgr/standata-37094b7f9511.json 
    Chain 1 init = 2 (Default) 
    Chain 1 random 
    Chain 1   seed = 1376020223 
    Chain 1 output 
    Chain 1   file = /tmp/Rtmp6PSBgr/bernoulli-202512040232-1-1e1263.csv 
    Chain 1   diagnostic_file =  (Default) 
    Chain 1   refresh = 100 (Default) 
    Chain 1   sig_figs = 8 (Default) 
    Chain 1   profile_file = /tmp/Rtmp6PSBgr/bernoulli-profile-202512040232-1-3dca3e.csv 
    Chain 1   save_cmdstan_config = false (Default) 
    Chain 1 num_threads = 1 (Default) 
    Chain 1 Gradient evaluation took 2e-06 seconds 
    Chain 1 1000 transitions using 10 leapfrog steps per transition would take 0.02 seconds. 
    Chain 1 Adjust your expectations accordingly! 
    Chain 1 WARNING: There aren't enough warmup iterations to fit the 
    Chain 1          three stages of adaptation as currently configured. 
    Chain 1          Reducing each adaptation stage to 15%/75%/10% of 
    Chain 1          the given number of warmup iterations: 
    Chain 1            init_buffer = 15 
    Chain 1            adapt_window = 75 
    Chain 1            term_buffer = 10 
    Chain 1 Iteration:   1 / 200 [  0%]  (Warmup) 
    Chain 1 Iteration: 100 / 200 [ 50%]  (Warmup) 
    Chain 1 Iteration: 101 / 200 [ 50%]  (Sampling) 
    Chain 1 Iteration: 200 / 200 [100%]  (Sampling) 
    Chain 1  Elapsed Time: 0 seconds (Warm-up) 
    Chain 1                0 seconds (Sampling) 
    Chain 1                0 seconds (Total) 
    Chain 1 finished in 0.0 seconds.
