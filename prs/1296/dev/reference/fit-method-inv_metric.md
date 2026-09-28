# Extract inverse metric (inverse mass matrix) after MCMC

Extract the inverse metric (inverse mass matrix) for each MCMC chain.

The inverse metric is defined over the unconstrained parameter space, so
its entries do not necessarily correspond one-to-one with the
constrained parameters declared in the `parameters` block (some
transformations from constrained to unconstrained change dimensions).
See **Examples** for a way to add names to the entries of the inverse
metric when the model has parameters that change dimensions when
unconstrained.

## Usage

``` r
inv_metric(matrix = TRUE)
```

## Arguments

- matrix:

  (logical) If a diagonal metric was used, setting `matrix = FALSE`
  returns a list containing just the diagonals of the matrices instead
  of the full matrices. Setting `matrix = FALSE` has no effect for dense
  metrics.

## Value

A list of length equal to the number of MCMC chains. See the `matrix`
argument for details.

## See also

[`CmdStanMCMC`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMCMC.md)

## Examples

``` r
# \dontrun{
stan_file <- write_stan_file("
  parameters {
    simplex[3] theta;
  }
  model {
    theta ~ dirichlet(rep_vector(1, 3));
  }
")
mod <- cmdstan_model(stan_file)

# use higher output precision so the simplex remains valid after CSV rounding
fit <- mod$sample(chains = 1, sig_figs = 10)
#> Running MCMC with 1 chain...
#> 
#> Chain 1 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 1 Iteration:  100 / 2000 [  5%]  (Warmup) 
#> Chain 1 Iteration:  200 / 2000 [ 10%]  (Warmup) 
#> Chain 1 Iteration:  300 / 2000 [ 15%]  (Warmup) 
#> Chain 1 Iteration:  400 / 2000 [ 20%]  (Warmup) 
#> Chain 1 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 1 Iteration:  600 / 2000 [ 30%]  (Warmup) 
#> Chain 1 Iteration:  700 / 2000 [ 35%]  (Warmup) 
#> Chain 1 Iteration:  800 / 2000 [ 40%]  (Warmup) 
#> Chain 1 Iteration:  900 / 2000 [ 45%]  (Warmup) 
#> Chain 1 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 1 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 1 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
#> Chain 1 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
#> Chain 1 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
#> Chain 1 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
#> Chain 1 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 1 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
#> Chain 1 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
#> Chain 1 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
#> Chain 1 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
#> Chain 1 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 1 finished in 0.0 seconds.

# even though theta has 3 elements, the inverse metric is 2x2 because the
# simplex constraint reduces the dimension. we set `matrix = FALSE` in this case
# because we estimated a diagonal matrix (we didn't set `metric = "dense_e"`
# when fitting the model), so we can just look at the diagonal elements
inv_metric <- fit$inv_metric(matrix = FALSE)

# the list has 1 element since we only ran 1 chain for simplicity
print(inv_metric)
#> $`1`
#> [1] 1.40627 1.80322
#> 

# get names of unconstrained parameters and add them to the inverse metric
# (unconstrain_draws() requires compiling additional methods)
inv_metric_names <- posterior::variables(fit$unconstrain_draws())
inv_metric <- lapply(inv_metric, stats::setNames, nm = inv_metric_names)

# the names will be theta[1] and theta[2], but these are not the same as
# the first two elements of the constrained theta in the Stan program
inv_metric
#> $`1`
#> theta[1] theta[2] 
#>  1.40627  1.80322 
#> 
# }
```
