#' @param chains (positive integer) The number of Markov chains to run. The
#'   default is 4.
#' @param parallel_chains (positive integer) The _maximum_ number of MCMC chains
#'   to run in parallel. If `parallel_chains` is not specified then the default
#'   is to look for the option `"mc.cores"`, which can be set for an entire \R
#'   session by `options(mc.cores=value)`. If the `"mc.cores"` option has not
#'   been set then the default is `1`.
#' @param chain_ids (integer vector) A vector of chain IDs. Must contain as many
#'   unique positive integers as the number of chains. If not set, the default
#'   chain IDs are used (integers starting from `1`).
#' @param threads_per_chain (positive integer) If the model was
#'   [compiled][model-method-compile] with threading support, the number of
#'   threads to use in parallelized sections _within_ an MCMC chain (e.g., when
#'   using the Stan functions `reduce_sum()` or `map_rect()`). This is in
#'   contrast with `parallel_chains`, which specifies the number of chains to
#'   run in parallel. The actual number of CPU cores used is
#'   `parallel_chains*threads_per_chain`. For an example of using threading see
#'   the Stan case study
#'   [Reduce Sum: A Minimal Example](https://mc-stan.org/users/documentation/case-studies/reduce_sum_tutorial.html).
#'
#' @param iter_sampling (positive integer) The number of post-warmup iterations
#'   to run per chain. Note: in the CmdStan User's Guide this is referred to as
#'   `num_samples`.
#' @param iter_warmup (positive integer) The number of warmup iterations to run
#'   per chain. Note: in the CmdStan User's Guide this is referred to as
#'   `num_warmup`.
#' @param save_warmup (logical) Should warmup iterations be saved? The default
#'   is `FALSE`.
#' @param thin (positive integer) The period between saved samples. This should
#'   typically be left at its default (no thinning) unless memory is a problem.
#' @param max_treedepth (positive integer) The maximum allowed tree depth for
#'   the NUTS engine. See the _Tree Depth_ section of the CmdStan User's Guide
#'   for more details.
#' @param adapt_engaged (logical) Do warmup adaptation? The default is `TRUE`.
#'   If a precomputed inverse metric is specified via the `inv_metric` argument
#'   (or `metric_file`) then, if `adapt_engaged=TRUE`, Stan will use the
#'   provided inverse metric just as an initial guess during adaptation. To turn
#'   off adaptation when using a precomputed inverse metric set
#'   `adapt_engaged=FALSE`.
#' @param adapt_delta (real in `(0,1)`) The adaptation target acceptance
#'   statistic.
#' @param step_size (positive real) The _initial_ step size for the discrete
#'   approximation to continuous Hamiltonian dynamics. This is further tuned
#'   during warmup.
#' @param metric (string) One of `"diag_e"`, `"dense_e"`, or `"unit_e"`,
#'   specifying the geometry of the base manifold. See the _Euclidean Metric_
#'   section of the CmdStan User's Guide for more details. To specify a
#'   precomputed (inverse) metric, see the `inv_metric` argument below.
#' @param metric_file (character vector) The paths to JSON or
#'   Rdump files (one per chain) compatible with CmdStan that contain
#'   precomputed inverse metrics. The `metric_file` argument is inherited from
#'   CmdStan but is confusing in that the entry in JSON or Rdump file(s) must be
#'   named `inv_metric`, referring to the _inverse_ metric. We recommend instead
#'   using CmdStanR's `inv_metric` argument (see below) to specify an inverse
#'   metric directly using a vector or matrix from your \R session.
#' @param inv_metric (vector, matrix) A vector (if `metric='diag_e'`) or a
#'   matrix (if `metric='dense_e'`) for initializing the inverse metric. This
#'   can be used as an alternative to the `metric_file` argument. A vector is
#'   interpreted as a diagonal metric. The inverse metric is usually set to an
#'   estimate of the posterior covariance. See the `adapt_engaged` argument
#'   above for details about (and control over) how specifying a precomputed
#'   inverse metric interacts with adaptation.
#' @param init_buffer (nonnegative integer) Width of initial fast timestep
#'   adaptation interval during warmup.
#' @param term_buffer (nonnegative integer) Width of final fast timestep
#'   adaptation interval during warmup.
#' @param window (nonnegative integer) Initial width of slow timestep/metric
#'   adaptation interval.
#' @param fixed_param (logical) When `TRUE`, call CmdStan with argument
#'   `"algorithm=fixed_param"`. The default is `FALSE`. The fixed parameter
#'   sampler generates a new sample without changing the current state of the
#'   Markov chain; only generated quantities may change. This can be useful
#'   when, for example, trying to generate pseudo-data using the generated
#'   quantities block. If the parameters block is empty then using
#'   `fixed_param=TRUE` is mandatory. When `fixed_param=TRUE` the `chains` and
#'   `parallel_chains` arguments will be set to `1`.
#' @param show_messages (logical) When `TRUE` (the default), prints all
#'   output during the sampling process, such as iteration numbers and elapsed times.
#'   If the output is silenced then the [`$output()`][fit-method-output] method of
#'   the resulting fit object can be used to display the silenced messages.
#' @param show_exceptions (logical) When `TRUE` (the default), prints all
#'   informational messages, for example rejection of the current proposal.
#'   Disable if you wish to silence these messages, but this is not usually
#'   recommended unless you are very confident that the model is correct up to
#'   numerical error. If the messages are silenced then the
#'   [`$output()`][fit-method-output] method of the resulting fit object can be
#'   used to display the silenced messages.
#' @param diagnostics (character vector) The diagnostics to automatically check
#'   and warn about after sampling. Setting this to an empty string `""` or
#'   `NULL` can be used to prevent CmdStanR from automatically reading in the
#'   sampler diagnostics from CSV if you wish to manually read in the results
#'   and validate them yourself, for example using [read_cmdstan_csv()]. The
#'   currently available diagnostics are `"divergences"`, `"treedepth"`,
#'   and `"ebfmi"` (the default is to check all of them).
#'
#'   These diagnostics are also available after fitting. The
#'   [`$sampler_diagnostics()`][fit-method-sampler_diagnostics] method provides
#'   access the diagnostic values for each iteration and the
#'   [`$diagnostic_summary()`][fit-method-diagnostic_summary] method provides
#'   summaries of the diagnostics and can regenerate the warning messages.
#'
#'   Diagnostics like R-hat and effective sample size are _not_ currently
#'   available via the `diagnostics` argument but can be checked after fitting
#'   using the [`$summary()`][fit-method-summary] method.
#'
