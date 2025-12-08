# Extract metadata from CmdStan CSV files

The `$metadata()` method returns a list of information gathered from the
CSV output files, including the CmdStan configuration used when fitting
the model. See **Examples** and
[`read_cmdstan_csv()`](https://mc-stan.org/cmdstanr/dev/reference/read_cmdstan_csv.md).

## Usage

``` r
metadata()
```

## See also

[`CmdStanMCMC`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMCMC.md),
[`CmdStanMLE`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMLE.md),
[`CmdStanVB`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanVB.md),
[`CmdStanGQ`](https://mc-stan.org/cmdstanr/dev/reference/CmdStanGQ.md)

## Examples

``` r
# \dontrun{
fit_mcmc <- cmdstanr_example("logistic", method = "sample")
str(fit_mcmc$metadata())
#> List of 42
#>  $ stan_version_major  : num 2
#>  $ stan_version_minor  : num 37
#>  $ stan_version_patch  : num 0
#>  $ start_datetime      : chr "2025-12-08 21:43:13 UTC"
#>  $ method              : chr "sample"
#>  $ save_warmup         : int 0
#>  $ thin                : num 1
#>  $ gamma               : num 0.05
#>  $ kappa               : num 0.75
#>  $ t0                  : num 10
#>  $ init_buffer         : num 75
#>  $ term_buffer         : num 50
#>  $ window              : num 25
#>  $ save_metric         : int 0
#>  $ algorithm           : chr "hmc"
#>  $ engine              : chr "nuts"
#>  $ metric              : chr "diag_e"
#>  $ stepsize_jitter     : num 0
#>  $ num_chains          : num 1
#>  $ id                  : num [1:4] 1 2 3 4
#>  $ init                : num [1:4] 2 2 2 2
#>  $ seed                : num 2.02e+09
#>  $ refresh             : num 100
#>  $ sig_figs            : num 8
#>  $ profile_file        : chr "/tmp/RtmpsxTeyM/logistic-profile-202512082143-1-8741c0.csv"
#>  $ save_cmdstan_config : int 0
#>  $ stanc_version       : chr "stanc3 v2.37.0"
#>  $ sampler_diagnostics : chr [1:6] "accept_stat__" "stepsize__" "treedepth__" "n_leapfrog__" ...
#>  $ variables           : chr [1:105] "lp__" "alpha" "beta[1]" "beta[2]" ...
#>  $ step_size_adaptation: num [1:4] 0.797 0.779 0.701 0.846
#>  $ model_name          : chr "logistic_model"
#>  $ adapt_engaged       : int 1
#>  $ adapt_delta         : num 0.8
#>  $ max_treedepth       : num 10
#>  $ step_size           : num [1:4] 1 1 1 1
#>  $ iter_warmup         : num 1000
#>  $ iter_sampling       : num 1000
#>  $ threads_per_chain   : num 1
#>  $ time                :'data.frame':    4 obs. of  4 variables:
#>   ..$ chain_id: num [1:4] 1 2 3 4
#>   ..$ warmup  : num [1:4] 0.02 0.02 0.019 0.019
#>   ..$ sampling: num [1:4] 0.054 0.055 0.057 0.053
#>   ..$ total   : num [1:4] 0.074 0.075 0.076 0.072
#>  $ stan_variable_sizes :List of 4
#>   ..$ lp__   : num 1
#>   ..$ alpha  : num 1
#>   ..$ beta   : num 3
#>   ..$ log_lik: num 100
#>  $ stan_variables      : chr [1:4] "lp__" "alpha" "beta" "log_lik"
#>  $ model_params        : chr [1:105] "lp__" "alpha" "beta[1]" "beta[2]" ...

fit_mle <- cmdstanr_example("logistic", method = "optimize")
str(fit_mle$metadata())
#> List of 32
#>  $ stan_version_major : num 2
#>  $ stan_version_minor : num 37
#>  $ stan_version_patch : num 0
#>  $ start_datetime     : chr "2025-12-08 21:43:14 UTC"
#>  $ method             : chr "optimize"
#>  $ algorithm          : chr "lbfgs"
#>  $ init_alpha         : num 0.001
#>  $ tol_obj            : num 1e-12
#>  $ tol_rel_obj        : num 10000
#>  $ tol_grad           : num 1e-08
#>  $ tol_rel_grad       : num 1e+07
#>  $ tol_param          : num 1e-08
#>  $ history_size       : num 5
#>  $ jacobian           : int 0
#>  $ iter               : num 2000
#>  $ save_iterations    : int 0
#>  $ id                 : num 1
#>  $ init               : num 2
#>  $ seed               : num 1.45e+09
#>  $ refresh            : num 100
#>  $ sig_figs           : num 8
#>  $ profile_file       : chr "/tmp/RtmpsxTeyM/logistic-profile-202512082143-1-059667.csv"
#>  $ save_cmdstan_config: int 0
#>  $ stanc_version      : chr "stanc3 v2.37.0"
#>  $ sampler_diagnostics: chr(0) 
#>  $ variables          : chr [1:105] "lp__" "alpha" "beta[1]" "beta[2]" ...
#>  $ model_name         : chr "logistic_model"
#>  $ threads            : num 1
#>  $ time               :'data.frame': 0 obs. of  0 variables
#>  $ stan_variable_sizes:List of 4
#>   ..$ lp__   : num 1
#>   ..$ alpha  : num 1
#>   ..$ beta   : num 3
#>   ..$ log_lik: num 100
#>  $ stan_variables     : chr [1:4] "lp__" "alpha" "beta" "log_lik"
#>  $ model_params       : chr [1:105] "lp__" "alpha" "beta[1]" "beta[2]" ...

fit_vb <- cmdstanr_example("logistic", method = "variational")
str(fit_vb$metadata())
#> List of 30
#>  $ stan_version_major : num 2
#>  $ stan_version_minor : num 37
#>  $ stan_version_patch : num 0
#>  $ start_datetime     : chr "2025-12-08 21:43:14 UTC"
#>  $ method             : chr "variational"
#>  $ algorithm          : chr "meanfield"
#>  $ iter               : num 50
#>  $ grad_samples       : num 1
#>  $ elbo_samples       : num 100
#>  $ eta                : num 1
#>  $ tol_rel_obj        : num 0.01
#>  $ eval_elbo          : num 100
#>  $ output_samples     : num 1000
#>  $ id                 : num 1
#>  $ init               : num 2
#>  $ seed               : num 5.64e+08
#>  $ refresh            : num 100
#>  $ sig_figs           : num 8
#>  $ profile_file       : chr "/tmp/RtmpsxTeyM/logistic-profile-202512082143-1-3e0474.csv"
#>  $ save_cmdstan_config: int 0
#>  $ stanc_version      : chr "stanc3 v2.37.0"
#>  $ sampler_diagnostics: chr(0) 
#>  $ variables          : chr [1:106] "lp__" "lp_approx__" "alpha" "beta[1]" ...
#>  $ model_name         : chr "logistic_model"
#>  $ adapt_engaged      : int 1
#>  $ threads            : num 1
#>  $ time               :'data.frame': 0 obs. of  0 variables
#>  $ stan_variable_sizes:List of 5
#>   ..$ lp__       : num 1
#>   ..$ lp_approx__: num 1
#>   ..$ alpha      : num 1
#>   ..$ beta       : num 3
#>   ..$ log_lik    : num 100
#>  $ stan_variables     : chr [1:5] "lp__" "lp_approx__" "alpha" "beta" ...
#>  $ model_params       : chr [1:106] "lp__" "lp_approx__" "alpha" "beta[1]" ...
# }
```
