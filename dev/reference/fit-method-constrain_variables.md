# Transform a set of unconstrained parameter values to the constrained scale

The `$constrain_variables()` method transforms input parameters to the
constrained scale.

## Usage

``` r
constrain_variables(
  unconstrained_variables,
  transformed_parameters = TRUE,
  generated_quantities = TRUE
)
```

## Arguments

- unconstrained_variables:

  (numeric) A vector of unconstrained parameters to constrain.

- transformed_parameters:

  (logical) Whether to return transformed parameters implied by
  newly-constrained parameters (defaults to TRUE).

- generated_quantities:

  (logical) Whether to return generated quantities implied by
  newly-constrained parameters (defaults to TRUE).

## See also

[`log_prob()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-log_prob.md),
[`grad_log_prob()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-grad_log_prob.md),
`constrain_variables()`,
[`unconstrain_variables()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-unconstrain_variables.md),
[`unconstrain_draws()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-unconstrain_draws.md),
[`variable_skeleton()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-variable_skeleton.md),
[`hessian()`](https://mc-stan.org/cmdstanr/dev/reference/fit-method-hessian.md)

## Examples

``` r
# \dontrun{
fit_mcmc <- cmdstanr_example("logistic", method = "sample", force_recompile = TRUE)
fit_mcmc$constrain_variables(unconstrained_variables = c(0.5, 1.2, 1.1, 2.2))
#> ar: creating stan/lib/stan_math/lib/sundials_6.1.1/lib/libsundials_nvecserial.a
#> ar: creating stan/lib/stan_math/lib/sundials_6.1.1/lib/libsundials_cvodes.a
#> ar: creating stan/lib/stan_math/lib/sundials_6.1.1/lib/libsundials_idas.a
#> ar: creating stan/lib/stan_math/lib/sundials_6.1.1/lib/libsundials_kinsol.a
#> /home/runner/.cmdstan/cmdstan-2.37.0/stan/lib/stan_math/lib/tbb_2020.3/build/Makefile.tbb:28: CONFIG: cfg=release arch=intel64 compiler=gcc target=linux runtime=cc13.3.0_libc2.39_kernel6.11.0
#> In file included from ../tbb_2020.3/src/tbb/concurrent_hash_map.cpp:17:
#> ../tbb_2020.3/include/tbb/concurrent_hash_map.h:347:23: warning: ‘template<class _Category, class _Tp, class _Distance, class _Pointer, class _Reference> struct std::iterator’ is deprecated [-Wdeprecated-declarations]
#>   347 |         : public std::iterator<std::forward_iterator_tag,Value>
#>       |                       ^~~~~~~~
#> In file included from /usr/include/c++/13/bits/stl_construct.h:61,
#>                  from /usr/include/c++/13/bits/stl_tempbuf.h:61,
#>                  from /usr/include/c++/13/memory:66,
#>                  from ../tbb_2020.3/include/tbb/tbb_stddef.h:452,
#>                  from ../tbb_2020.3/include/tbb/concurrent_hash_map.h:23:
#> /usr/include/c++/13/bits/stl_iterator_base_types.h:127:34: note: declared here
#>   127 |     struct _GLIBCXX17_DEPRECATED iterator
#>       |                                  ^~~~~~~~
#> In file included from ../tbb_2020.3/src/tbb/concurrent_queue.cpp:22:
#> ../tbb_2020.3/include/tbb/internal/_concurrent_queue_impl.h:749:21: warning: ‘template<class _Category, class _Tp, class _Distance, class _Pointer, class _Reference> struct std::iterator’ is deprecated [-Wdeprecated-declarations]
#>   749 |         public std::iterator<std::forward_iterator_tag,Value> {
#>       |                     ^~~~~~~~
#> In file included from /usr/include/c++/13/bits/stl_construct.h:61,
#>                  from /usr/include/c++/13/bits/stl_tempbuf.h:61,
#>                  from /usr/include/c++/13/memory:66,
#>                  from ../tbb_2020.3/include/tbb/tbb_stddef.h:452,
#>                  from ../tbb_2020.3/src/tbb/concurrent_queue.cpp:17:
#> /usr/include/c++/13/bits/stl_iterator_base_types.h:127:34: note: declared here
#>   127 |     struct _GLIBCXX17_DEPRECATED iterator
#>       |                                  ^~~~~~~~
#> ../tbb_2020.3/include/tbb/internal/_concurrent_queue_impl.h:1013:21: warning: ‘template<class _Category, class _Tp, class _Distance, class _Pointer, class _Reference> struct std::iterator’ is deprecated [-Wdeprecated-declarations]
#>  1013 |         public std::iterator<std::forward_iterator_tag,Value> {
#>       |                     ^~~~~~~~
#> /usr/include/c++/13/bits/stl_iterator_base_types.h:127:34: note: declared here
#>   127 |     struct _GLIBCXX17_DEPRECATED iterator
#>       |                                  ^~~~~~~~
#> cc1plus: note: unrecognized command-line option ‘-Wno-unknown-warning-option’ may have been intended to silence earlier diagnostics
#> cc1plus: note: unrecognized command-line option ‘-Wno-unknown-warning-option’ may have been intended to silence earlier diagnostics
#> $alpha
#> [1] 0.5
#> 
#> $beta
#> [1] 1.2 1.1 2.2
#> 
#> $log_lik
#>   [1] -0.671996961 -0.076521408 -0.050097680 -0.859061944 -4.368940350
#>   [6] -0.026348601 -1.399709850 -1.607383090 -0.197580264 -0.161519876
#>  [11] -0.020289680 -0.005109469 -3.391453198 -0.438505774 -0.019831358
#>  [16] -0.320576250 -0.491982174 -0.985897601 -0.760711848 -0.135968222
#>  [21] -0.090365458 -0.289106971 -2.401995550 -0.741279075 -1.130539252
#>  [26] -0.051754355 -0.052810285 -0.592894549 -4.203884680 -1.627902707
#>  [31] -0.086396696 -0.055223416 -0.702223347 -1.437214411 -4.277397542
#>  [36] -3.430371053 -0.029399304 -2.582700752 -0.082096143 -0.613542693
#>  [41] -1.631715761 -0.780555873 -8.456425184 -1.129873174 -0.803760727
#>  [46] -0.375921603 -0.200697408 -1.179516083 -0.288812923 -0.028251618
#>  [51] -2.203830461 -0.542549041 -0.108332542 -1.410224678 -0.176267923
#>  [56] -6.137792581 -0.301999796 -0.002415079 -1.082073078 -0.661755204
#>  [61] -0.906463865 -0.022520669 -0.200599774 -0.088896080 -4.807633540
#>  [66] -0.027891348 -1.129060197 -0.378523990 -0.287530851 -4.083209231
#>  [71] -4.503703248 -2.056130808 -0.409881228 -2.435290917 -0.022311747
#>  [76] -0.310871780 -1.558477854 -3.416918272 -0.143495888 -0.017064658
#>  [81] -1.422948018 -0.019539438 -0.383208393 -0.083042642 -0.216009949
#>  [86] -3.175676338 -0.547416219 -3.767462435 -1.854701489 -3.595218847
#>  [91] -1.713321036 -4.146153592 -1.389571073 -0.344785402 -0.305319997
#>  [96] -1.233113298 -1.753232181 -0.002618034 -0.608156530 -1.329013367
#> 
# }
```
