#!/usr/bin/env Rscript
#
# Times the parts of cmdstanr that sit between a user and a fit: the forced
# build of an example model, the make query for stanc flags, and the no-rebuild
# path (constructing on a current executable, one guarded method, and a
# log_prob() loop on a fit) on a small program with an include and on a
# generated 400-parameter program. Numbers, not tests: timing tests are flaky
# on CI, so this is run by hand and the results go in a work log or a PR.
#
#   Rscript dev-notes/timing.R <checkout dir>
#
# The checkout is loaded with devtools::load_all(), so two checkouts can be
# compared from one shell by alternating them in fresh processes, which keeps
# the page cache fair to both:
#
#   for i in 1 2 3 4; do
#     Rscript dev-notes/timing.R ~/cmdstanr-master
#     Rscript dev-notes/timing.R ~/cmdstanr
#   done
#
# The first process on each side runs slower than the rest (cold cache for the
# compiler and headers), so take medians of the later ones.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) stop("usage: Rscript dev-notes/timing.R <checkout dir>")
checkout <- normalizePath(args[[1]])
devtools::load_all(checkout, quiet = TRUE)
cat("checkout:", checkout, "\ncmdstan:", cmdstan_version(), "\n")

secs <- function(expr) system.time(expr)[["elapsed"]]
# Takes a function, not an expression: a lazy argument inside a loop runs once.
loop_ms <- function(f, n) {
  1000 * system.time(for (i in seq_len(n)) f())[["elapsed"]]
}
ms <- function(f, n = 10) round(loop_ms(f, n) / n, 1)
dir <- tempfile("timing-")
dir.create(file.path(dir, "inc"), recursive = TRUE)

# A forced build of the schools example, then the pieces of it.
schools_data <- file.path(checkout, "inst", "schools.data.json")
cat("\n== schools example, forced build (s)\n")
cat("STANCFLAGS make query:", secs(get_cmdstan_flags("STANCFLAGS")), "\n")
cat("cmdstanr_example(force_recompile = TRUE), 4 chains of 1 + 1 iterations:",
    secs(capture.output(cmdstanr_example("schools", force_recompile = TRUE,
                                         iter_warmup = 1, iter_sampling = 1,
                                         refresh = 0))), "\n")
schools <- file.path(dir, "schools.stan")
file.copy(file.path(checkout, "inst", "schools.stan"), schools)
cat("cmdstan_model(force_recompile = TRUE):",
    secs(mod <- cmdstan_model(schools, force_recompile = TRUE)), "\n")
cat("$sample(), 4 chains of 1 + 1 iterations:",
    secs(mod$sample(data = schools_data, chains = 4, iter_warmup = 1,
                    iter_sampling = 1, refresh = 0, show_messages = FALSE)),
    "\n")

# The no-rebuild path, per model.
writeLines("functions {\n  real twice(real x) { return 2 * x; }\n}",
           file.path(dir, "inc", "fns.stan"))
writeLines(c(
  "#include fns.stan",
  "data { int<lower=0> N; array[N] int<lower=0,upper=1> y; }",
  "parameters { real<lower=0,upper=1> theta; }",
  "model { theta ~ beta(1,1); y ~ bernoulli(twice(theta) / 2); }"
), file.path(dir, "bern.stan"))
p <- sprintf("p%d", 1:400)
writeLines(c(
  "parameters {", sprintf("  real %s;", p), "}",
  "model {", sprintf("  %s ~ normal(0, 1);", p), "}"
), file.path(dir, "big.stan"))

time_model <- function(label, stan_file, data, include_paths, unconstrained) {
  mod <- cmdstan_model(stan_file, include_paths = include_paths)
  cat(sprintf("\n== %s: %d lines, %.1f MB executable (ms)\n", label,
              length(readLines(stan_file)), file.size(mod$exe_file()) / 1e6))
  cat("constructor, no rebuild:",
      ms(function() cmdstan_model(stan_file, include_paths = include_paths)),
      "\n")
  cat("stanc --info alone:",
      ms(function() stanc_info(stan_file, include_paths)), "\n")
  cat("$cmdstan_defaults(), a guarded member:",
      ms(function() mod$cmdstan_defaults()), "\n")
  fit <- mod$sample(data = data, chains = 1, iter_warmup = 100,
                    iter_sampling = 100, refresh = 0, show_messages = FALSE,
                    seed = 1)
  fit$init_model_methods()
  theta <- unconstrained(fit)
  cat("1000 log_prob() calls, total:",
      round(loop_ms(function() fit$log_prob(theta), 1000)), "\n")
  cat("1000 grad_log_prob() calls, total:",
      round(loop_ms(function() fit$grad_log_prob(theta), 1000)), "\n")
}
time_model("bernoulli with one include", file.path(dir, "bern.stan"),
           data = list(N = 10, y = c(0, 1, 0, 0, 0, 0, 0, 0, 0, 1)),
           include_paths = file.path(dir, "inc"),
           unconstrained = function(fit) {
             fit$unconstrain_variables(list(theta = 0.3))
           })
time_model("generated, 400 parameters", file.path(dir, "big.stan"), data = NULL,
           include_paths = NULL, unconstrained = function(fit) rep(0.1, 400))
