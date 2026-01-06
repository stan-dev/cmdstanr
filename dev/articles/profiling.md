# Profiling Stan programs with CmdStanR

### Introduction

This vignette demonstrates how to use the new profiling functionality
introduced in CmdStan 2.26.0.

Profiling identifies which parts of a Stan program are taking the
longest time to run and is therefore a useful guide when working on
optimizing the performance of a model.

However, be aware that the statistical assumptions that go into a model
are the most important factors in overall model performance. It is often
not possible to make up for model problems with just brute force
computation. For ideas on how to address performance of your model from
a statistical perspective, see Gelman (2020).

``` r
library(cmdstanr)
check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)
```

### Adding profiling statements to a Stan program

Consider a simple logistic regression with parameters `alpha` and
`beta`, covariates `X`, and outcome `y`.

    data {
      int<lower=1> k;
      int<lower=0> n;
      matrix[n, k] X;
      array[n] int y;
    }
    parameters {
      vector[k] beta;
      real alpha;
    }
    model {
      beta ~ std_normal();
      alpha ~ std_normal();

      y ~ bernoulli_logit(X * beta + alpha);
    }

A simple question is how much time do the prior calculations take
compared against the likelihood? To answer this we surround the prior
and likelihood calculations with `profile` statements.

    profile("priors") {
      target += std_normal_lpdf(beta);
      target += std_normal_lpdf(alpha);
    }
    profile("likelihood") {
      target += bernoulli_logit_lpmf(y | X * beta + alpha);
    }

In general we recommend using a separate `.stan` file, but for
convenience in this vignette we’ll write the Stan program as a string
and use
[`write_stan_file()`](https://mc-stan.org/cmdstanr/dev/reference/write_stan_file.md)
to write it to a temporary file.

``` r
profiling_bernoulli_logit <- write_stan_file('
data {
  int<lower=1> k;
  int<lower=0> n;
  matrix[n, k] X;
  array[n] int y;
}
parameters {
  vector[k] beta;
  real alpha;
}
model {
  profile("priors") {
    target += std_normal_lpdf(beta);
    target += std_normal_lpdf(alpha);
  }
  profile("likelihood") {
    target += bernoulli_logit_lpmf(y | X * beta + alpha);
  }
}
')
```

We can then run the model as usual and Stan will collect the profiling
information for any sections with `profile` statements.

``` r
# Compile the model
model <- cmdstan_model(profiling_bernoulli_logit)

# Generate some fake data
n <- 1000
k <- 20
X <- matrix(rnorm(n * k), ncol = k)

y <- 3 * X[,1] - 2 * X[,2] + 1
p <- runif(n)
y <- ifelse(p < (1 / (1 + exp(-y))), 1, 0)
stan_data <- list(k = ncol(X), n = nrow(X), y = y, X = X)

# Run one chain of the model
fit <- model$sample(data = stan_data, chains = 1)
```

### Accessing the profiling information from R

The raw profiling information can then be accessed with the
`$profiles()` method, which returns a list containing one data frame per
chain (profiles across multiple chains are not automatically
aggregated). Details on the column names are available in the [CmdStan
documentation](https://mc-stan.org/docs/2_26/cmdstan-guide/stan-csv.html#profiling-csv-output-file).

``` r
fit$profiles()
```

    [[1]]
            name       thread_id  total_time forward_time reverse_time chain_stack
    1 likelihood 140460522403648 0.641631280  0.507179180  0.134452100       52590
    2     priors 140460522403648 0.004988607  0.003564542  0.001424065       35060
      no_chain_stack autodiff_calls no_autodiff_calls
    1       35077530          17530                 1
    2          35060          17530                 1

The `total_time` column is the total time spent inside a given profile
statement. It is clear that the vast majority of time is spent in the
likelihood function.

### Comparing to a faster version of the model

Stan’s specialized glm functions can be used to make models like this
faster. In this case the likelihood can be replaced with

    target += bernoulli_logit_glm_lpmf(y | X, alpha, beta);

We’ll keep the same [`profile()`](https://rdrr.io/r/stats/profile.html)
statements so that the profiling information for the new model is
collected automatically just like for the previous one.

``` r
profiling_bernoulli_logit_glm <- write_stan_file('
data {
  int<lower=1> k;
  int<lower=0> n;
  matrix[n, k] X;
  array[n] int y;
}
parameters {
  vector[k] beta;
  real alpha;
}
model {
  profile("priors") {
    target += std_normal_lpdf(beta);
    target += std_normal_lpdf(alpha);
  }
  profile("likelihood") {
    target += bernoulli_logit_glm_lpmf(y | X, alpha, beta);
  }
}
')
```

``` r
model_glm <- cmdstan_model(profiling_bernoulli_logit_glm)
fit_glm <- model_glm$sample(data = stan_data, chains = 1)
```

``` r
fit_glm$profiles()
```

    [[1]]
            name       thread_id  total_time forward_time reverse_time chain_stack
    1     priors 140071255643968 0.003628615  0.002726424  0.000902191       34246
    2 likelihood 140071255643968 0.416657970  0.415693850  0.000964120       51369
      no_chain_stack autodiff_calls no_autodiff_calls
    1          34246          17123                 1
    2          17123          17123                 1

We can see from the `total_time` column that this is much faster than
the previous model.

### Per-gradient timings, and memory usage

The other columns of the profiling output are documented in the [CmdStan
documentation](https://mc-stan.org/docs/2_26/cmdstan-guide/stan-csv.html#profiling-csv-output-file).

The timing numbers are broken down by forward pass and reverse pass, and
the `chain_stack` and `no_chain_stack` columns contain information about
how many autodiff variables were saved in the process of performing a
calculation.

These numbers are all totals – times are the total times over the whole
calculation, and `chain_stack` counts are similarly the total counts of
autodiff variables used over the whole calculation. It is often
convenient to have per-gradient calculations (which will be more stable
across runs with different seeds). To compute these, use the
`autodiff_calls` column.

``` r
profile_chain_1 <- fit$profiles()[[1]]
per_gradient_timing <- profile_chain_1$total_time/profile_chain_1$autodiff_calls
print(per_gradient_timing) # two elements for the two profile statements in the model
```

    [1] 3.660190e-05 2.845754e-07

### Accessing and saving the profile files

After sampling (or optimization or variational inference) finishes,
CmdStan stores the profiling data in CSV files in a temporary location.
The paths of the profiling CSV files can be retrieved using
`$profile_files()`.

``` r
fit$profile_files()
```

    [1] "/tmp/RtmpMVWFiT/model_6580008f67848265f3dfd0e7ae3b0600-profile-202601062213-1-80695d.csv"

These can be saved to a more permanent location with the
`$save_profile_files()` method.

``` r
# see ?save_profile_files for info on optional arguments
fit$save_profile_files(dir = "path/to/directory")
```

## References

Gelman, Andrew, Aki Vehtari, Daniel Simpson, Charles C. Margossian, Bob
Carpenter, Yuling Yao, Lauren Kennedy, Jonah Gabry, Paul-Christian
Bürkner, and Martin Modrák. 2020. “Bayesian Workflow.”
<https://arxiv.org/abs/2011.01808>.
