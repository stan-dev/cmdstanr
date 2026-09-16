set_cmdstan_path()

test_that("$variables() work correctly with example models", {
  mod <- testing_model("bernoulli")
  expect_equal(names(mod$variables()$data), c("N", "y"))
  expect_equal(names(mod$variables()$parameters), c("theta"))
  expect_equal(mod$variables()$data$N$type, "int")
  expect_equal(mod$variables()$data$N$dimensions, 0)
  expect_equal(mod$variables()$data$y$type, "int")
  expect_equal(mod$variables()$data$y$dimensions, 1)
  expect_equal(mod$variables()$parameters$theta$type, "real")
  expect_equal(mod$variables()$parameters$theta$dimensions, 0)
  expect_equal(length(mod$variables()$transformed_parameters), 0)
  expect_equal(length(mod$variables()$generated_quantities), 0)
  expect_true(is.list(mod$variables()$transformed_parameters))
  expect_true(is.list(mod$variables()$generated_quantities))

  mod <- testing_model("bernoulli_log_lik")
  expect_equal(names(mod$variables()$data), c("N", "y"))
  expect_equal(names(mod$variables()$parameters), c("theta"))
  expect_equal(names(mod$variables()$generated_quantities), c("log_lik"))
  expect_equal(mod$variables()$generated_quantities$log_lik$type, "real")
  expect_equal(mod$variables()$generated_quantities$log_lik$dimensions, 1)

  mod <- testing_model("logistic")
  expect_equal(names(mod$variables()$data), c("N", "K", "y", "X"))
  expect_equal(names(mod$variables()$parameters), c("alpha", "beta"))
  expect_equal(mod$variables()$data$N$type, "int")
  expect_equal(mod$variables()$data$N$dimensions, 0)
  expect_equal(mod$variables()$data$K$type, "int")
  expect_equal(mod$variables()$data$K$dimensions, 0)
  expect_equal(mod$variables()$data$y$type, "int")
  expect_equal(mod$variables()$data$y$dimensions, 1)
  expect_equal(mod$variables()$data$X$type, "real")
  expect_equal(mod$variables()$data$X$dimensions, 2)
  expect_equal(mod$variables()$parameters$alpha$type, "real")
  expect_equal(mod$variables()$parameters$alpha$dimensions, 0)
  expect_equal(mod$variables()$parameters$beta$type, "real")
  expect_equal(mod$variables()$parameters$beta$dimensions, 1)
})

test_that("$variables() work correctly with multidimensional variables", {
  code <- "
  data {
    array[1,2,3,4,5,6,7,8] int y;
    array[1,2,3,4] vector[4] x;
  }
  parameters {
    real z;
  }
  transformed parameters {
    array[1,2,3] real p;
    array[2] matrix[2,3] pp;
  }
  "
  stan_file <- write_stan_file(code)
  mod <- cmdstan_model(stan_file)
  expect_equal(names(mod$variables()$data), c("y", "x"))
  expect_equal(names(mod$variables()$parameters), c("z"))
  expect_equal(names(mod$variables()$transformed_parameters), c("p", "pp"))
  expect_equal(mod$variables()$data$y$type, "int")
  expect_equal(mod$variables()$data$y$dimensions, 8)
  expect_equal(mod$variables()$data$x$type, "real")
  expect_equal(mod$variables()$data$x$dimensions, 5)
  expect_equal(mod$variables()$parameters$z$type, "real")
  expect_equal(mod$variables()$parameters$z$dimensions, 0)
  expect_equal(mod$variables()$transformed_parameters$p$type, "real")
  expect_equal(mod$variables()$transformed_parameters$p$dimensions, 3)
  expect_equal(mod$variables()$transformed_parameters$pp$type, "real")
  expect_equal(mod$variables()$transformed_parameters$pp$dimensions, 3)
})

test_that("$variables() is refreshed when the model is recompiled", {
  model_dir <- withr::local_tempdir()
  stan_file <- write_stan_file(
    "
    parameters {
      real alpha;
    }
    model {
      alpha ~ std_normal();
    }
    ",
    dir = model_dir,
    basename = "issue1228.stan"
  )
  mod <- cmdstan_model(stan_file)
  expect_equal(names(mod$variables()$parameters), "alpha")

  write_stan_file(
    "
    parameters {
      real beta;
    }
    model {
      beta ~ std_normal();
    }
    ",
    dir = model_dir,
    basename = "issue1228.stan"
  )
  # editing the file alone doesn't invalidate the cached variables
  expect_equal(names(mod$variables()$parameters), "alpha")

  # a fresh construction sees the edited file and rebuilds
  mod <- expect_compilation(cmdstan_model(stan_file))
  expect_equal(names(mod$variables()$parameters), "beta")

  # the fitting methods validate inits against the refreshed variables
  expect_no_message(
    utils::capture.output(
      mod$sample(
        chains = 1,
        iter_warmup = 10,
        iter_sampling = 10,
        refresh = 0,
        init = list(list(beta = 0)),
        diagnostics = NULL,
        show_messages = FALSE
      )
    ),
    message = "Init values were only set for a subset of parameters"
  )
})

test_that("$variables() errors when the model was not created from a Stan file", {
  code <- "
  parameters {
    real y;
  }
  model {
    y ~ std_normal();
  }
  "
  stan_file <- write_stan_file(code)
  mod <- cmdstan_model(stan_file)
  mod_exe <- cmdstan_model(exe_file = mod$exe_file())
  expect_error(
    mod_exe$variables(),
    "'$variables()' cannot be used because the 'CmdStanModel' was not created with a Stan file.",
    fixed = TRUE
  )
})

test_that("$variables() works with #includes, explicit or auto-detected paths", {
  data_code <- "
    data {
      int N;
    }
  "
  model_code <- "
    #include includes/data.stan
    parameters {
      vector[N] y;
    }
    model {
      y ~ std_normal();
    }
  "

  model_dir <- withr::local_tempdir(pattern = "include path")
  include_dir <- file.path(model_dir, "includes")
  dir.create(include_dir, recursive = TRUE)
  model_file <- write_stan_file(code = model_code, dir = model_dir)
  write_stan_file(code = data_code, basename = "data.stan", dir = include_dir)

  mod_explicit <- cmdstan_model(stan_file = model_file, include_paths = model_dir)
  vars_explicit <- mod_explicit$variables()

  # a second construction with the same include path reuses the executable
  mod_reused <- expect_no_recompilation(
    cmdstan_model(stan_file = model_file, include_paths = model_dir)
  )
  expect_equal(mod_reused$variables(), vars_explicit)

  # the include path is auto-detected without being supplied, and also reuses
  mod_automatic <- expect_no_recompilation(cmdstan_model(stan_file = model_file))
  expect_equal(mod_automatic$variables(), vars_explicit)
})
