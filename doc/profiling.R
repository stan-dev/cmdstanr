params <-
list(EVAL = FALSE)

## ----settings-knitr, include=FALSE--------------------------------------------
stopifnot(require(knitr))
opts_chunk$set(
  # collapse = TRUE,
  dev = "png",
  dpi = 150,
  fig.asp = 0.618,
  fig.width = 5,
  out.width = "60%",
  fig.align = "center",
  comment = NA,
  eval = if (isTRUE(exists("params"))) params$EVAL else FALSE
)

## ----library, message=FALSE---------------------------------------------------
#  library(cmdstanr)
#  check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)

## ----profiling_bernoulli_logit.stan-------------------------------------------
#  profiling_bernoulli_logit <- write_stan_file('
#  data {
#    int<lower=1> k;
#    int<lower=0> n;
#    matrix[n, k] X;
#    array[n] int y;
#  }
#  parameters {
#    vector[k] beta;
#    real alpha;
#  }
#  model {
#    profile("priors") {
#      target += std_normal_lpdf(beta);
#      target += std_normal_lpdf(alpha);
#    }
#    profile("likelihood") {
#      target += bernoulli_logit_lpmf(y | X * beta + alpha);
#    }
#  }
#  ')

## ----fit-model, message=FALSE, results='hide'---------------------------------
#  # Compile the model
#  model <- cmdstan_model(profiling_bernoulli_logit)
#  
#  # Generate some fake data
#  n <- 1000
#  k <- 20
#  X <- matrix(rnorm(n * k), ncol = k)
#  
#  y <- 3 * X[,1] - 2 * X[,2] + 1
#  p <- runif(n)
#  y <- ifelse(p < (1 / (1 + exp(-y))), 1, 0)
#  stan_data <- list(k = ncol(X), n = nrow(X), y = y, X = X)
#  
#  # Run one chain of the model
#  fit <- model$sample(data = stan_data, chains = 1)

## ----profiles-----------------------------------------------------------------
#  fit$profiles()

## ----profiling_bernoulli_logit_glm.stan---------------------------------------
#  profiling_bernoulli_logit_glm <- write_stan_file('
#  data {
#    int<lower=1> k;
#    int<lower=0> n;
#    matrix[n, k] X;
#    array[n] int y;
#  }
#  parameters {
#    vector[k] beta;
#    real alpha;
#  }
#  model {
#    profile("priors") {
#      target += std_normal_lpdf(beta);
#      target += std_normal_lpdf(alpha);
#    }
#    profile("likelihood") {
#      target += bernoulli_logit_glm_lpmf(y | X, alpha, beta);
#    }
#  }
#  ')

## ----fit-model-glm, message=FALSE, results='hide'-----------------------------
#  model_glm <- cmdstan_model(profiling_bernoulli_logit_glm)
#  fit_glm <- model_glm$sample(data = stan_data, chains = 1)

## ----profiles-glm-------------------------------------------------------------
#  fit_glm$profiles()

## ----per-gradient-------------------------------------------------------------
#  profile_chain_1 <- fit$profiles()[[1]]
#  per_gradient_timing <- profile_chain_1$total_time/profile_chain_1$autodiff_calls
#  print(per_gradient_timing) # two elements for the two profile statements in the model

## ----profile_files------------------------------------------------------------
#  fit$profile_files()

## ----save_profile_files, eval=FALSE-------------------------------------------
#  # see ?save_profile_files for info on optional arguments
#  fit$save_profile_files(dir = "path/to/directory")

