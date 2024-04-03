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

## ----setup, message=FALSE-----------------------------------------------------
#  library(cmdstanr)
#  check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)

## ----start-clean, include=FALSE-----------------------------------------------
#  exe <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli")
#  unlink(exe)

## ----compile------------------------------------------------------------------
#  stan_file <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
#  mod <- cmdstan_model(stan_file)
#  mod$print()
#  mod$stan_file()
#  mod$exe_file()

## ----already-compiled---------------------------------------------------------
#  mod <- cmdstan_model(stan_file)

## ----compile-options, eval=FALSE----------------------------------------------
#  mod <- cmdstan_model(
#    stan_file,
#    force_recompile = TRUE,
#    include_paths = "paths/to/directories/with/included/files",
#    cpp_options = list(stan_threads = TRUE, STANC2 = TRUE)
#  )

## ----compile-method-----------------------------------------------------------
#  unlink(mod$exe_file())
#  mod <- cmdstan_model(stan_file, compile = FALSE)
#  mod$exe_file() # not yet created
#  mod$compile()
#  mod$exe_file()

## ----stan_file_pedantic-------------------------------------------------------
#  stan_file_pedantic <- write_stan_file("
#  data {
#    int N;
#    array[N] int y;
#  }
#  parameters {
#    // should have <lower=0> but omitting to demonstrate pedantic mode
#    real lambda;
#  }
#  model {
#    y ~ poisson(lambda);
#  }
#  ")

## ----pedantic-compile, collapse = TRUE----------------------------------------
#  mod_pedantic <- cmdstan_model(stan_file_pedantic, pedantic = TRUE)

## ----pedantic-check_syntax, collapse=TRUE-------------------------------------
#  mod_pedantic$check_syntax(pedantic = TRUE)

## ----pedantic-check_syntax-2, collapse=TRUE-----------------------------------
#  file.remove(mod_pedantic$exe_file()) # delete compiled executable
#  rm(mod_pedantic)
#  
#  mod_pedantic <- cmdstan_model(stan_file_pedantic, compile = FALSE)
#  mod_pedantic$check_syntax(pedantic = TRUE)

## ----stan_file_variables------------------------------------------------------
#  stan_file_variables <- write_stan_file("
#  data {
#    int<lower=1> J;
#    vector<lower=0>[J] sigma;
#    vector[J] y;
#  }
#  parameters {
#    real mu;
#    real<lower=0> tau;
#    vector[J] theta_raw;
#  }
#  transformed parameters {
#    vector[J] theta = mu + tau * theta_raw;
#  }
#  model {
#    target += normal_lpdf(tau | 0, 10);
#    target += normal_lpdf(mu | 0, 10);
#    target += normal_lpdf(theta_raw | 0, 1);
#    target += normal_lpdf(y | theta, sigma);
#  }
#  ")
#  mod_v <- cmdstan_model(stan_file_variables)
#  variables <- mod_v$variables()

## ----variables-list-names-----------------------------------------------------
#  names(variables)
#  names(variables$data)
#  names(variables$parameters)
#  names(variables$transformed_parameters)
#  names(variables$generated_quantities)

## ----variable-type-dims-------------------------------------------------------
#  variables$data$J
#  variables$data$sigma
#  variables$parameters$tau
#  variables$transformed_parameters$theta

## ----compile-with-dir, eval = FALSE-------------------------------------------
#  mod <- cmdstan_model(stan_file, dir = "path/to/directory/for/executable")

## ----print-program-again------------------------------------------------------
#  mod$print()

## ----data-list, eval=FALSE----------------------------------------------------
#  # data block has 'N' and 'y'
#  data_list <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))
#  fit <- mod$sample(data = data_list)

## ----write_stan_json----------------------------------------------------------
#  data_list <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))
#  json_file <- tempfile(fileext = ".json")
#  write_stan_json(data_list, json_file)
#  cat(readLines(json_file), sep = "\n")

## ----data-json, eval=FALSE----------------------------------------------------
#  fit <- mod$sample(data = json_file)

## ----data-rdump, eval=FALSE---------------------------------------------------
#  rdump_file <- tempfile(fileext = ".data.R")
#  rstan::stan_rdump(names(data_list), file = rdump_file, envir = list2env(data_list))
#  cat(readLines(rdump_file), sep = "\n")
#  fit <- mod$sample(data = rdump_file)

## ----sample-tempdir, results = "hide"-----------------------------------------
#  data_list <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))
#  fit <- mod$sample(data = data_list)

## ----output_files-------------------------------------------------------------
#  fit$output_files()

## ----gc-----------------------------------------------------------------------
#  files <- fit$output_files()
#  file.exists(files)
#  
#  rm(fit)
#  gc()
#  
#  file.exists(files)

## ----save_output_files, eval=FALSE--------------------------------------------
#  # see ?save_output_files for info on optional arguments
#  fit$save_output_files(dir = "path/to/directory")

## ----output_dir, eval = FALSE-------------------------------------------------
#  fit <- mod$sample(
#    data = data_list,
#    output_dir = "path/to/directory"
#  )

## ----refit, include=FALSE-----------------------------------------------------
#  fit <- mod$sample(data = data_list)

## ----csv-not-read-------------------------------------------------------------
#  str(fit)

## ----for-csv-reading----------------------------------------------------------
#  draws <- fit$draws() # force CSVs to be read into R
#  str(fit)

## ----read_cmdstan_csv---------------------------------------------------------
#  # see ?read_cmdstan_csv for info on optional arguments controlling
#  # what information is read in
#  csv_contents <- read_cmdstan_csv(fit$output_files())
#  str(csv_contents)

## ----as_cmdstan_fit-----------------------------------------------------------
#  fit2 <- as_cmdstan_fit(fit$output_files())

## ----save_latent_dynamics, results = "hide"-----------------------------------
#  fit <- mod$sample(data = data_list, save_latent_dynamics = TRUE)

## ----read-latent-dynamics-----------------------------------------------------
#  fit$latent_dynamics_files()
#  
#  # read one of the files in
#  x <- utils::read.csv(fit$latent_dynamics_files()[1], comment.char = "#")
#  head(x)

## ----explore-latent-dynamics--------------------------------------------------
#  head(x[, c("theta", "p_theta", "g_theta")])

## ----verbose-mode-------------------------------------------------------------
#  options("cmdstanr_verbose"=TRUE)
#  
#  mod <- cmdstan_model(stan_file, force_recompile = TRUE)
#  fit <- mod$sample(
#    data = data_list,
#    chains = 1,
#    iter_warmup = 100,
#    iter_sampling = 100
#  )

## ----include=FALSE------------------------------------------------------------
#  options("cmdstanr_verbose" = FALSE)

