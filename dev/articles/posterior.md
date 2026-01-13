# Working with Posteriors

## Summary statistics

We can easily customize the summary statistics reported by `$summary()`
and `$print()`.

``` r
fit <- cmdstanr::cmdstanr_example("schools", method = "sample")
```

    Warning: 90 of 4000 (2.0%) transitions ended with a divergence.
    See https://mc-stan.org/misc/warnings for details.

``` r
fit$summary()
```

       variable       mean     median       sd      mad          q5       q95
    1      lp__ -58.320756 -58.622925 4.918566 5.131560 -66.0873865 -49.96255
    2        mu   6.765315   6.853504 4.477501 4.441462  -0.5815009  13.94432
    3       tau   5.329521   4.561805 3.392980 3.206039   1.2538663  11.88650
    4  theta[1]   9.614873   9.056704 7.011170 6.517894  -0.9236177  21.53582
    5  theta[2]   7.011599   7.109942 5.882246 5.815888  -2.8740180  16.35239
    6  theta[3]   5.762381   6.062503 6.908729 6.531089  -6.0668909  16.06493
    7  theta[4]   6.658352   6.724928 5.967391 5.795691  -3.1409733  16.19117
    8  theta[5]   4.856853   5.073774 6.104925 5.838099  -5.6258228  14.22750
    9  theta[6]   5.766387   6.078693 6.138783 5.934561  -4.5991732  15.27158
    10 theta[7]   9.343667   9.036606 6.217535 5.913166  -0.2544706  19.78362
    11 theta[8]   7.177659   7.188460 6.679874 6.127754  -3.7721206  17.68597
           rhat  ess_bulk  ess_tail
    1  1.016399  359.7406  315.6200
    2  1.004802  537.1418  889.3897
    3  1.016214  358.8192  282.9631
    4  1.003541 1065.6346 1806.9754
    5  1.002917  833.6323 1890.8652
    6  1.003206  876.9884 1844.4931
    7  1.002243  888.6417 2203.8935
    8  1.003200  695.3974 1277.5894
    9  1.002718  825.3426 1678.0983
    10 1.004303 1019.5252 2068.0670
    11 1.004031  950.0452 1717.5853

By default all variables are summaries with the follow functions:

``` r
posterior::default_summary_measures()
```

    [1] "mean"      "median"    "sd"        "mad"       "quantile2"

To change the variables summarized, we use the variables argument

``` r
fit$summary(variables = c("mu", "tau"))
```

      variable     mean   median       sd      mad         q5      q95     rhat
    1       mu 6.765315 6.853504 4.477501 4.441462 -0.5815009 13.94432 1.004802
    2      tau 5.329521 4.561805 3.392980 3.206039  1.2538663 11.88650 1.016214
      ess_bulk ess_tail
    1 537.1418 889.3897
    2 358.8192 282.9631

We can additionally change which functions are used

``` r
fit$summary(variables = c("mu", "tau"), mean, sd)
```

      variable     mean       sd
    1       mu 6.765315 4.477501
    2      tau 5.329521 3.392980

To summarize all variables with non-default functions, it is necessary
to set explicitly set the variables argument, either to `NULL` or the
full vector of variable names.

``` r
fit$summary(variables = NULL, "mean", "median")
```

       variable       mean     median
    1      lp__ -58.320756 -58.622925
    2        mu   6.765315   6.853504
    3       tau   5.329521   4.561805
    4  theta[1]   9.614873   9.056704
    5  theta[2]   7.011599   7.109942
    6  theta[3]   5.762381   6.062503
    7  theta[4]   6.658352   6.724928
    8  theta[5]   4.856853   5.073774
    9  theta[6]   5.766387   6.078693
    10 theta[7]   9.343667   9.036606
    11 theta[8]   7.177659   7.188460

Summary functions can be specified by character string, function, or
using a formula (or anything else supported by
[`rlang::as_function()`](https://rlang.r-lib.org/reference/as_function.html)).
If these arguments are named, those names will be used in the tibble
output. If the summary results are named they will take precedence.

``` r
my_sd <- function(x) c(My_SD = sd(x))
fit$summary(
  c("mu", "tau"), 
  MEAN = mean, 
  "median",
  my_sd,
  ~quantile(.x, probs = c(0.1, 0.9)),
  Minimum = function(x) min(x)
)        
```

      variable     MEAN   median    My_SD      10%       90%     Minimum
    1       mu 6.765315 6.853504 4.477501 0.887486 12.502440 -10.8368510
    2      tau 5.329521 4.561805 3.392980 1.608897  9.942976   0.7203539

Arguments to all summary functions can also be specified with `.args`.

``` r
fit$summary(c("mu", "tau"), quantile, .args = list(probs = c(0.025, .05, .95, .975)))
```

      variable      2.5%         5%      95%    97.5%
    1       mu -2.039372 -0.5815009 13.94432 15.33313
    2      tau  1.093560  1.2538663 11.88650 13.82409

The summary functions are applied to the array of sample values, with
dimension `iter_sampling`x`chains`.

``` r
fit$summary(variables = NULL, dim, colMeans)
```

       variable dim.1 dim.2          1          2          3          4
    1      lp__  1000     4 -57.940185 -57.459900 -58.267713 -59.615226
    2        mu  1000     4   7.116265   6.765985   7.052251   6.126758
    3       tau  1000     4   5.159391   4.798012   5.249758   6.110924
    4  theta[1]  1000     4   9.965227   9.050620   9.975194   9.468451
    5  theta[2]  1000     4   7.498667   6.885330   7.271205   6.391194
    6  theta[3]  1000     4   6.219639   6.012905   6.002472   4.814506
    7  theta[4]  1000     4   7.177726   6.617560   6.775441   6.062680
    8  theta[5]  1000     4   5.009475   5.175509   5.170864   4.071566
    9  theta[6]  1000     4   6.211830   5.758704   6.051311   5.043702
    10 theta[7]  1000     4   9.483258   8.951447   9.533311   9.406654
    11 theta[8]  1000     4   7.650810   7.091832   7.564064   6.403932

For this reason users may have unexpected results if they use
[`stats::var()`](https://rdrr.io/r/stats/cor.html) directly, as it will
return a covariance matrix. An alternative is the
[`distributional::variance()`](https://pkg.mitchelloharawild.com/distributional/reference/variance.html)
function, which can also be accessed via
[`posterior::variance()`](https://pkg.mitchelloharawild.com/distributional/reference/variance.html).

``` r
fit$summary(c("mu", "tau"), posterior::variance, ~var(as.vector(.x)))
```

      variable posterior::variance ~var(as.vector(.x))
    1       mu            20.04802            20.04802
    2      tau            11.51232            11.51232

Summary functions need not be numeric, but these won’t work with
`$print()`.

``` r
strict_pos <- function(x) if (all(x > 0)) "yes" else "no"
fit$summary(variables = NULL, "Strictly Positive" = strict_pos)
```

       variable Strictly Positive
    1      lp__                no
    2        mu                no
    3       tau               yes
    4  theta[1]                no
    5  theta[2]                no
    6  theta[3]                no
    7  theta[4]                no
    8  theta[5]                no
    9  theta[6]                no
    10 theta[7]                no
    11 theta[8]                no

``` r
# fit$print(variables = NULL, "Strictly Positive" = strict_pos)
```

For more information, see
[`posterior::summarise_draws()`](https://mc-stan.org/posterior/reference/draws_summary.html),
which is called by `$summary()`.

## Extracting posterior draws/samples

The
[`$draws()`](https://mc-stan.org/cmdstanr/reference/fit-method-draws.html)
method can be used to extract the posterior draws in formats provided by
the [**posterior**](https://mc-stan.org/posterior/) package. Here we
demonstrate only the `draws_array` and `draws_df` formats, but the
**posterior** package supports other useful formats as well.

``` r
# default is a 3-D draws_array object from the posterior package
# iterations x chains x variables
draws_arr <- fit$draws() # or format="array"
str(draws_arr)
```

     'draws_array' num [1:1000, 1:4, 1:11] -62.1 -61.3 -65.7 -62.3 -61.9 ...
     - attr(*, "dimnames")=List of 3
      ..$ iteration: chr [1:1000] "1" "2" "3" "4" ...
      ..$ chain    : chr [1:4] "1" "2" "3" "4"
      ..$ variable : chr [1:11] "lp__" "mu" "tau" "theta[1]" ...

``` r
# draws x variables data frame
draws_df <- fit$draws(format = "df")
str(draws_df)
```

    draws_df [4,000 × 14] (S3: draws_df/draws/tbl_df/tbl/data.frame)
     $ lp__      : num [1:4000] -62.1 -61.3 -65.7 -62.3 -61.9 ...
     $ mu        : num [1:4000] 9.45 8.3 7.18 14.58 8.05 ...
     $ tau       : num [1:4000] 2.89 7.73 7.83 9.74 4.56 ...
     $ theta[1]  : num [1:4000] 4.47 4.66 28.35 18.82 4.38 ...
     $ theta[2]  : num [1:4000] 8.24 10.1 10.44 14.88 3.87 ...
     $ theta[3]  : num [1:4000] 1.6 3.47 16.71 16.04 2 ...
     $ theta[4]  : num [1:4000] 16.56 13.11 4.21 2.07 16.29 ...
     $ theta[5]  : num [1:4000] 8.61 9.57 4.53 2.41 13.09 ...
     $ theta[6]  : num [1:4000] 14.08 18.04 -4.96 7.73 9.37 ...
     $ theta[7]  : num [1:4000] 8.79 11.78 18.94 18.14 4.92 ...
     $ theta[8]  : num [1:4000] 10.713 4.423 20.456 18.698 0.802 ...
     $ .chain    : int [1:4000] 1 1 1 1 1 1 1 1 1 1 ...
     $ .iteration: int [1:4000] 1 2 3 4 5 6 7 8 9 10 ...
     $ .draw     : int [1:4000] 1 2 3 4 5 6 7 8 9 10 ...

``` r
print(draws_df)
```

    # A draws_df: 1000 iterations, 4 chains, and 11 variables
       lp__   mu  tau theta[1] theta[2] theta[3] theta[4] theta[5]
    1   -62  9.4  2.9      4.5     8.24      1.6    16.56      8.6
    2   -61  8.3  7.7      4.7    10.10      3.5    13.11      9.6
    3   -66  7.2  7.8     28.4    10.44     16.7     4.21      4.5
    4   -62 14.6  9.7     18.8    14.88     16.0     2.07      2.4
    5   -62  8.0  4.6      4.4     3.87      2.0    16.29     13.1
    6   -59  5.8  5.3     12.5     0.31     10.4    12.50      8.3
    7   -63 10.9  4.7      4.2     3.85     15.1     1.61      5.2
    8   -65  8.6  7.0     17.0    15.23     10.6    -4.47     -3.2
    9   -64 11.7 11.6     17.7    15.32      5.4    21.99     -4.1
    10  -66 16.9  9.5     27.9    15.86     15.0     0.05     18.3
    # ... with 3990 more draws, and 3 more variables
    # ... hidden reserved variables {'.chain', '.iteration', '.draw'}

To convert an existing draws object to a different format use the
`posterior::as_draws_*()` functions.

To manipulate the `draws` objects use the various methods described in
the **posterior** package
[vignettes](https://mc-stan.org/posterior/articles/index.html) and
[documentation](https://mc-stan.org/posterior/reference/index.html).

### Structured draws similar to `rstan::extract()`

The **posterior** package’s `rvar` format provides a multidimensional,
sample-based representation of random variables. See
<https://mc-stan.org/posterior/articles/rvar.html> for details. In
addition to being useful in its own right, this format also allows
CmdStanR users to obtain draws in a similar format to
`rstan::extract()`.

Suppose we have a parameter `matrix[2,3] x`. The `rvar` format lets you
interact with `x` as if it’s a `2 x 3` matrix and automatically applies
operations over the many posterior draws of `x`. To instead directly
access the draws of `x` while maintaining the structure of the matrix
use
[`posterior::draws_of()`](https://mc-stan.org/posterior/reference/draws_of.html).
For example:

``` r
draws <- posterior::as_draws_rvars(fit$draws())
x_rvar <- draws$x
x_array <- posterior::draws_of(draws$x)
```

The object `x_rvar` will be an `rvar` that can be used like a `2 x 3`
matrix, with the draws handled behind the scenes. The object `x_array`
will be a `4000 x 2 x 3` array (assuming `4000` posterior draws), which
is the same as it would be after being extracted from the list returned
by `rstan::extract()`.
