# Access console output

For MCMC, the `$output()` method returns the stdout and stderr of all
chains as a list of character vectors if `id=NULL`. If the `id` argument
is specified it instead pretty prints the console output for a single
chain.

For optimization and variational inference `$output()` just pretty
prints the console output.

## Usage

``` r
output(id = NULL)
```

## Arguments

- id:

  (integer) The chain id. Ignored if the model was not fit using MCMC.

## See also

[`CmdStanMCMC`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMCMC.md),
[`CmdStanMLE`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMLE.md),
[`CmdStanVB`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanVB.md),
[`CmdStanGQ`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanGQ.md)

## Examples

``` r
# \dontrun{
fit_mcmc <- cmdstanr_example("logistic", method = "sample")
fit_mcmc$output(1)
#> 
#> method = sample (Default)
#>   sample
#>     num_samples = 1000 (Default)
#>     num_warmup = 1000 (Default)
#>     save_warmup = false (Default)
#>     thin = 1 (Default)
#>     adapt
#>       engaged = true (Default)
#>       gamma = 0.05 (Default)
#>       delta = 0.8 (Default)
#>       kappa = 0.75 (Default)
#>       t0 = 10 (Default)
#>       init_buffer = 75 (Default)
#>       term_buffer = 50 (Default)
#>       window = 25 (Default)
#>       save_metric = false (Default)
#>     algorithm = hmc (Default)
#>       hmc
#>         engine = nuts (Default)
#>           nuts
#>             max_depth = 10 (Default)
#>         metric = diag_e (Default)
#>         metric_file =  (Default)
#>         stepsize = 1 (Default)
#>         stepsize_jitter = 0 (Default)
#>     num_chains = 1 (Default)
#> id = 1 (Default)
#> data
#>   file = /home/runner/work/_temp/Library/cmdstanr/logistic.data.json
#> init = 2 (Default)
#> random
#>   seed = 1099664120
#> output
#>   file = /tmp/Rtmppl3pRK/logistic-202607202215-1-249f51.csv
#>   diagnostic_file =  (Default)
#>   refresh = 100 (Default)
#>   sig_figs = 8 (Default)
#>   profile_file = /tmp/Rtmppl3pRK/logistic-profile-202607202215-1-4dc2c8.csv
#>   save_cmdstan_config = false (Default)
#> num_threads = 1 (Default)
#> 
#> 
#> Gradient evaluation took 9e-06 seconds
#> 1000 transitions using 10 leapfrog steps per transition would take 0.09 seconds.
#> Adjust your expectations accordingly!
#> 
#> 
#> Iteration:    1 / 2000 [  0%]  (Warmup)
#> Iteration:  100 / 2000 [  5%]  (Warmup)
#> Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Iteration:  300 / 2000 [ 15%]  (Warmup)
#> Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Iteration:  500 / 2000 [ 25%]  (Warmup)
#> Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Iteration:  700 / 2000 [ 35%]  (Warmup)
#> Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Iteration:  900 / 2000 [ 45%]  (Warmup)
#> Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Iteration: 1100 / 2000 [ 55%]  (Sampling)
#> Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Iteration: 1300 / 2000 [ 65%]  (Sampling)
#> Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Iteration: 1500 / 2000 [ 75%]  (Sampling)
#> Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Iteration: 1700 / 2000 [ 85%]  (Sampling)
#> Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Iteration: 1900 / 2000 [ 95%]  (Sampling)
#> Iteration: 2000 / 2000 [100%]  (Sampling)
#> 
#>  Elapsed Time: 0.019 seconds (Warm-up)
#>                0.053 seconds (Sampling)
#>                0.072 seconds (Total)
out <- fit_mcmc$output()
str(out)
#> List of 4
#>  $ : chr [1:75] "" "method = sample (Default)" "  sample" "    num_samples = 1000 (Default)" ...
#>  $ : chr [1:75] "" "method = sample (Default)" "  sample" "    num_samples = 1000 (Default)" ...
#>  $ : chr [1:75] "" "method = sample (Default)" "  sample" "    num_samples = 1000 (Default)" ...
#>  $ : chr [1:75] "" "method = sample (Default)" "  sample" "    num_samples = 1000 (Default)" ...

fit_mle <- cmdstanr_example("logistic", method = "optimize")
fit_mle$output()
#> 
#> method = optimize
#>   optimize
#>     algorithm = lbfgs (Default)
#>       lbfgs
#>         init_alpha = 0.001 (Default)
#>         tol_obj = 1e-12 (Default)
#>         tol_rel_obj = 10000 (Default)
#>         tol_grad = 1e-08 (Default)
#>         tol_rel_grad = 1e+07 (Default)
#>         tol_param = 1e-08 (Default)
#>         history_size = 5 (Default)
#>     jacobian = false (Default)
#>     iter = 2000 (Default)
#>     save_iterations = false (Default)
#> id = 1 (Default)
#> data
#>   file = /home/runner/work/_temp/Library/cmdstanr/logistic.data.json
#> init = 2 (Default)
#> random
#>   seed = 1792129562
#> output
#>   file = /tmp/Rtmppl3pRK/logistic-202607202215-1-6169b8.csv
#>   diagnostic_file =  (Default)
#>   refresh = 100 (Default)
#>   sig_figs = 8 (Default)
#>   profile_file = /tmp/Rtmppl3pRK/logistic-profile-202607202215-1-3c98e4.csv
#>   save_cmdstan_config = false (Default)
#> num_threads = 1 (Default)
#> 
#> Initial log joint probability = -124.053
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes 
#>        7      -63.9218   0.000438087    0.00107529           1           1       10   
#> Optimization terminated normally: 
#>   Convergence detected: relative gradient magnitude is below tolerance

fit_vb <- cmdstanr_example("logistic", method = "variational")
fit_vb$output()
#> 
#> method = variational
#>   variational
#>     algorithm = meanfield (Default)
#>       meanfield
#>     iter = 10000 (Default)
#>     grad_samples = 1 (Default)
#>     elbo_samples = 100 (Default)
#>     eta = 1 (Default)
#>     adapt
#>       engaged = true (Default)
#>       iter = 50 (Default)
#>     tol_rel_obj = 0.01 (Default)
#>     eval_elbo = 100 (Default)
#>     output_samples = 1000 (Default)
#> id = 1 (Default)
#> data
#>   file = /home/runner/work/_temp/Library/cmdstanr/logistic.data.json
#> init = 2 (Default)
#> random
#>   seed = 1973439605
#> output
#>   file = /tmp/Rtmppl3pRK/logistic-202607202215-1-07c51c.csv
#>   diagnostic_file =  (Default)
#>   refresh = 100 (Default)
#>   sig_figs = 8 (Default)
#>   profile_file = /tmp/Rtmppl3pRK/logistic-profile-202607202215-1-580773.csv
#>   save_cmdstan_config = false (Default)
#> num_threads = 1 (Default)
#> 
#> ------------------------------------------------------------
#> EXPERIMENTAL ALGORITHM:
#>   This procedure has not been thoroughly tested and may be unstable
#>   or buggy. The interface is subject to change.
#> ------------------------------------------------------------
#> 
#> 
#> 
#> Gradient evaluation took 9e-06 seconds
#> 1000 transitions using 10 leapfrog steps per transition would take 0.09 seconds.
#> Adjust your expectations accordingly!
#> 
#> 
#> Begin eta adaptation.
#> Iteration:   1 / 250 [  0%]  (Adaptation)
#> Iteration:  50 / 250 [ 20%]  (Adaptation)
#> Iteration: 100 / 250 [ 40%]  (Adaptation)
#> Iteration: 150 / 250 [ 60%]  (Adaptation)
#> Iteration: 200 / 250 [ 80%]  (Adaptation)
#> Success! Found best value [eta = 1] earlier than expected.
#> 
#> Begin stochastic gradient ascent.
#>   iter             ELBO   delta_ELBO_mean   delta_ELBO_med   notes 
#>    100          -66.790             1.000            1.000
#>    200          -66.398             0.503            1.000
#>    300          -66.186             0.336            0.006   MEDIAN ELBO CONVERGED
#> 
#> Drawing a sample of size 1000 from the approximate posterior... 
#> COMPLETED.
# }
```
