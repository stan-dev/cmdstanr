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

## ----install, eval=FALSE------------------------------------------------------
#  # we recommend running this is a fresh R session or restarting your current session
#  install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

## ----library, message=FALSE---------------------------------------------------
#  library(cmdstanr)
#  library(posterior)
#  library(bayesplot)
#  color_scheme_set("brightblue")

## ----check-toolchain----------------------------------------------------------
#  check_cmdstan_toolchain()

## ----install_cmdstan-1, include = FALSE---------------------------------------
#  if (!dir.exists(cmdstan_default_path())) {
#    install_cmdstan()
#  }

## ----install_cmdstan-2, eval=FALSE--------------------------------------------
#  install_cmdstan(cores = 2)

## ----set_cmdstan_path, eval=FALSE---------------------------------------------
#  set_cmdstan_path(PATH_TO_CMDSTAN)

## ----cmdstan_path-------------------------------------------------------------
#  cmdstan_path()
#  cmdstan_version()

## ----cmdstan_model------------------------------------------------------------
#  file <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
#  mod <- cmdstan_model(file)

## ----compile------------------------------------------------------------------
#  mod$print()

## ----exe_file-----------------------------------------------------------------
#  mod$exe_file()

## ----sample-------------------------------------------------------------------
#  # names correspond to the data block in the Stan program
#  data_list <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))
#  
#  fit <- mod$sample(
#    data = data_list,
#    seed = 123,
#    chains = 4,
#    parallel_chains = 4,
#    refresh = 500 # print update every 500 iters
#  )

## ----summary, eval=FALSE------------------------------------------------------
#  fit$summary()
#  fit$summary(variables = c("theta", "lp__"), "mean", "sd")
#  
#  # use a formula to summarize arbitrary functions, e.g. Pr(theta <= 0.5)
#  fit$summary("theta", pr_lt_half = ~ mean(. <= 0.5))
#  
#  # summarise all variables with default and additional summary measures
#  fit$summary(
#    variables = NULL,
#    posterior::default_summary_measures(),
#    extra_quantiles = ~posterior::quantile2(., probs = c(.0275, .975))
#  )

## ---- echo=FALSE--------------------------------------------------------------
#   # NOTE: the hack of using print.data.frame in chunks with echo=FALSE
#   # is used because the pillar formatting of posterior draws_summary objects
#   # isn't playing nicely with pkgdown::build_articles().
#   options(digits = 2)
#   print.data.frame(fit$summary())
#   print.data.frame(fit$summary(variables = c("theta", "lp__"), "mean", "sd"))
#   print.data.frame(fit$summary("theta", pr_lt_half = ~ mean(. <= 0.5)))
#   print.data.frame(fit$summary(
#     variables = NULL,
#     posterior::default_summary_measures(),
#     extra_quantiles = ~posterior::quantile2(., probs = c(.0275, .975))
#   ))

## ----draws, message=FALSE-----------------------------------------------------
#  # default is a 3-D draws_array object from the posterior package
#  # iterations x chains x variables
#  draws_arr <- fit$draws() # or format="array"
#  str(draws_arr)
#  
#  # draws x variables data frame
#  draws_df <- fit$draws(format = "df")
#  str(draws_df)
#  print(draws_df)

## ----as_draws-----------------------------------------------------------------
#  # this should be identical to draws_df created via draws(format = "df")
#  draws_df_2 <- as_draws_df(draws_arr)
#  identical(draws_df, draws_df_2)

## ----plots, message=FALSE-----------------------------------------------------
#  mcmc_hist(fit$draws("theta"))

## ----sampler_diagnostics------------------------------------------------------
#  # this is a draws_array object from the posterior package
#  str(fit$sampler_diagnostics())
#  
#  # this is a draws_df object from the posterior package
#  str(fit$sampler_diagnostics(format = "df"))

## ----diagnostic_summary-------------------------------------------------------
#  fit$diagnostic_summary()

## ----fit-with-warnings, results='hold'----------------------------------------
#  fit_with_warning <- cmdstanr_example("schools")

## ----diagnostic_summary-with-warnings-----------------------------------------
#  diagnostics <- fit_with_warning$diagnostic_summary()
#  print(diagnostics)
#  
#  # number of divergences reported in warning is the sum of the per chain values
#  sum(diagnostics$num_divergent)

## ----stanfit, eval=FALSE------------------------------------------------------
#  stanfit <- rstan::read_stan_csv(fit$output_files())

## ----optimize-----------------------------------------------------------------
#  fit_mle <- mod$optimize(data = data_list, seed = 123)
#  fit_mle$print() # includes lp__ (log prob calculated by Stan program)
#  fit_mle$mle("theta")

## ----plot-mle, message = FALSE------------------------------------------------
#  mcmc_hist(fit$draws("theta")) +
#    vline_at(fit_mle$mle("theta"), size = 1.5)

## ----optimize-map-------------------------------------------------------------
#  fit_map <- mod$optimize(
#    data = data_list,
#    jacobian = TRUE,
#    seed = 123
#  )

## ----laplace------------------------------------------------------------------
#  fit_laplace <- mod$laplace(
#      mode = fit_map,
#      draws = 4000,
#      data = data_list,
#      seed = 123,
#      refresh = 1000
#    )
#  fit_laplace$print("theta")
#  mcmc_hist(fit_laplace$draws("theta"), binwidth = 0.025)

## ----variational--------------------------------------------------------------
#  fit_vb <- mod$variational(
#    data = data_list,
#    seed = 123,
#    draws = 4000
#  )
#  fit_vb$print("theta")
#  mcmc_hist(fit_vb$draws("theta"), binwidth = 0.025)

## ----pathfinder---------------------------------------------------------------
#  fit_pf <- mod$pathfinder(
#    data = data_list,
#    seed = 123,
#    draws = 4000
#  )
#  fit_pf$print("theta")

## ----plot-compare-pf, message = FALSE-----------------------------------------
#  mcmc_hist(fit_pf$draws("theta"), binwidth = 0.025) +
#    ggplot2::labs(subtitle = "Approximate posterior from pathfinder") +
#    ggplot2::xlim(0, 1)

## ----plot-compare-vb, message = FALSE-----------------------------------------
#  mcmc_hist(fit_vb$draws("theta"), binwidth = 0.025) +
#    ggplot2::labs(subtitle = "Approximate posterior from variational") +
#    ggplot2::xlim(0, 1)

## ----plot-compare-laplace, message = FALSE------------------------------------
#  mcmc_hist(fit_laplace$draws("theta"), binwidth = 0.025) +
#    ggplot2::labs(subtitle = "Approximate posterior from Laplace") +
#    ggplot2::xlim(0, 1)

## ----plot-compare-mcmc, message = FALSE---------------------------------------
#  mcmc_hist(fit$draws("theta"), binwidth = 0.025) +
#    ggplot2::labs(subtitle = "Posterior from MCMC") +
#    ggplot2::xlim(0, 1)

## ----save_object, eval=FALSE--------------------------------------------------
#  fit$save_object(file = "fit.RDS")
#  
#  # can be read back in using readRDS
#  fit2 <- readRDS("fit.RDS")

## ----save_object_qs_full, eval = FALSE----------------------------------------
#  # Load CmdStan output files into the fitted model object.
#  fit$draws() # Load posterior draws into the object.
#  try(fit$sampler_diagnostics(), silent = TRUE) # Load sampler diagnostics.
#  try(fit$init(), silent = TRUE) # Load user-defined initial values.
#  try(fit$profiles(), silent = TRUE) # Load profiling samples.
#  
#  # Save the object to a file.
#  qs::qsave(x = fit, file = "fit.qs")
#  
#  # Read the object.
#  fit2 <- qs::qread("fit.qs")

## ----save_object_qs_small, eval = FALSE---------------------------------------
#  # Load posterior draws into the fitted model object and omit other output.
#  fit$draws()
#  
#  # Save the object to a file.
#  qs::qsave(x = fit, file = "fit.qs")
#  
#  # Read the object.
#  fit2 <- qs::qread("fit.qs")

