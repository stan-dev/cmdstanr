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
#>   seed = 1078114322
#> output
#>   file = /tmp/RtmphVHATg/logistic-202512300134-1-4ec1b2.csv
#>   diagnostic_file =  (Default)
#>   refresh = 100 (Default)
#>   sig_figs = 8 (Default)
#>   profile_file = /tmp/RtmphVHATg/logistic-profile-202512300134-1-94de2b.csv
#>   save_cmdstan_config = false (Default)
#> num_threads = 1 (Default)
#> 
#> 
#> Gradient evaluation took 7e-06 seconds
#> 1000 transitions using 10 leapfrog steps per transition would take 0.07 seconds.
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
#>                0.055 seconds (Sampling)
#>                0.074 seconds (Total)
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
#>   seed = 1549478816
#> output
#>   file = /tmp/RtmphVHATg/logistic-202512300134-1-779e0f.csv
#>   diagnostic_file =  (Default)
#>   refresh = 100 (Default)
#>   sig_figs = 8 (Default)
#>   profile_file = /tmp/RtmphVHATg/logistic-profile-202512300134-1-04d697.csv
#>   save_cmdstan_config = false (Default)
#> num_threads = 1 (Default)
#> 
#> Initial log joint probability = -163.231
#>     Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes 
#>        7      -63.9218   0.000352153    0.00102562       0.944       0.944       11   
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
#>   seed = 1243032231
#> output
#>   file = /tmp/RtmphVHATg/logistic-202512300134-1-048036.csv
#>   diagnostic_file =  (Default)
#>   refresh = 100 (Default)
#>   sig_figs = 8 (Default)
#>   profile_file = /tmp/RtmphVHATg/logistic-profile-202512300134-1-780bd8.csv
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
#> Gradient evaluation took 7e-06 seconds
#> 1000 transitions using 10 leapfrog steps per transition would take 0.07 seconds.
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
#>    100          -66.476             1.000            1.000
#>    200          -66.497             0.500            1.000
#>    300          -66.520             0.334            0.000   MEDIAN ELBO CONVERGED
#> 
#> Drawing a sample of size 1000 from the approximate posterior... 
#> COMPLETED.
# }
```
