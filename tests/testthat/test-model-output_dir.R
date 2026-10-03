skip_on_cran()

set_cmdstan_path()

local_output_sandbox <- function(pattern = "sandbox", .local_envir = parent.frame()) {
  withr::local_tempdir(pattern = pattern, .local_envir = .local_envir)
}

test_that("all fitting methods work with output_dir", {
  sandbox <- local_output_sandbox()
  for (method in c("sample", "optimize", "variational")) {
    method_dir <- file.path(sandbox, method)
    dir.create(method_dir, recursive = TRUE, showWarnings = FALSE)

    # WSL models use internal WSL tempdir
    if (!os_is_wsl()) {
      # no output_dir means should use tempdir
      fit <- testing_fit("bernoulli", method = method, seed = 123)
      expect_equal(fit$runset$args$output_dir, absolute_path(tempdir()))
      files <- list.files(method_dir)
    }
    # specifying output_dir
    call_args  <- list(
      "bernoulli",
      method = method,
      seed = 123,
      output_dir = method_dir,
      save_cmdstan_config = TRUE
    )
    if (method == "sample") {
      call_args$save_metric <- TRUE
    }
    fit <- do.call(testing_fit, call_args)
    # Normalize to account for platform-specific path representations.
    expect_equal(normalizePath(fit$runset$args$output_dir),
                 normalizePath(method_dir))
    files <- normalizePath(list.files(method_dir, full.names = TRUE))
    expect_equal(files[grepl("\\.csv$", files)],
                 normalizePath(fit$output_files()))
    if (method == "sample") {
      mult <- 3
      expect_equal(files[grepl("metric", files)],
                   normalizePath(fit$metric_files()))
      expect_equal(files[grepl("config", files)],
                   normalizePath(fit$config_files()))
    } else {
      mult <- 2
      expect_equal(files[grepl("config", files)],
                   normalizePath(fit$config_files()))
    }
    expect_equal(length(list.files(method_dir)), mult * fit$num_procs())


    # specifying output_dir
    fit <- testing_fit("bernoulli", method = method, seed = 123,
                       output_basename = "custom")
    n_files <- length(fit$output_files())
    files <- sprintf("custom-%02d.csv", seq_len(n_files))
    expect_equal(basename(fit$output_files()), files)
  }

  # specifying output_dir and save_latent_dynamics
  fit <- testing_fit("bernoulli", method = "sample", seed = 123,
                     output_dir = file.path(sandbox, "sample"),
                     save_latent_dynamics = TRUE)

  files <- list.files(file.path(sandbox, "sample"))
  expect_equal(
    sum(grepl("diagnostic", files)),
    fit$num_procs()
  )
  expect_equal(
    normalizePath(fit$latent_dynamics_files()),
    normalizePath(list.files(
      file.path(sandbox, "sample"),
      pattern = "diagnostic",
      full.names = TRUE
    ))
  )
})

test_that("explicit WSL output paths are usable by Windows R", {
  skip_if_not(os_is_wsl())
  output_dir <- local_output_sandbox("wsl-output-dir")
  mod <- testing_model("logistic_profiling")
  utils::capture.output(
    fit <- mod$sample(
      data = testing_data("logistic"),
      chains = 1,
      parallel_chains = 1,
      seed = 123,
      refresh = 0,
      output_dir = output_dir,
      save_latent_dynamics = TRUE,
      save_cmdstan_config = TRUE,
      save_metric = TRUE
    )
  )
  paths <- c(
    fit$output_files(),
    fit$latent_dynamics_files(),
    fit$profile_files(),
    fit$config_files(),
    fit$metric_files()
  )
  expect_equal(file.exists(paths), rep(TRUE, length(paths)))
  expect_equal(
    normalizePath(dirname(paths)),
    rep(normalizePath(output_dir), length(paths))
  )
  expect_output(fit$cmdstan_summary(), "Inference for Stan model")
  expect_output(fit$cmdstan_diagnose(), "Processing complete")

  # All generated file types should remain usable when moved by Windows R.
  save_root <- local_output_sandbox("wsl-save-files")
  save_dirs <- file.path(
    save_root,
    c("output", "diagnostic", "profile", "config", "metric")
  )
  for (dir in save_dirs) {
    dir.create(dir)
  }
  saved_paths <- suppressMessages(c(
    fit$save_output_files(save_dirs[1]),
    fit$save_latent_dynamics_files(save_dirs[2]),
    fit$save_profile_files(save_dirs[3]),
    fit$save_config_files(save_dirs[4]),
    fit$save_metric_files(save_dirs[5])
  ))
  expect_equal(file.exists(saved_paths), rep(TRUE, length(saved_paths)))
})

test_that("explicit WSL UNC output_dir remains supported", {
  skip_if_not(os_is_wsl())
  # This covers explicit output only; #1113's temporary input paths are separate.
  output_dir <- repair_path(file.path(wsl_dir_prefix(), wsl_tempdir()))
  withr::defer(unlink(output_dir, recursive = TRUE))
  fit <- testing_fit(
    "bernoulli",
    method = "optimize",
    output_dir = output_dir
  )

  expect_equal(file.exists(fit$output_files()), TRUE)
  expect_equal(
    normalizePath(dirname(fit$output_files())),
    normalizePath(output_dir)
  )
})

test_that("error if output_dir is invalid", {
  sandbox <- local_output_sandbox()
  expect_error(
    testing_fit("bernoulli", output_dir = "NOT_A_DIR"),
    "Directory 'NOT_A_DIR' does not exist",
    fixed = TRUE
  )
  expect_error(
    testing_fit("bernoulli", output_dir = TRUE),
    "No directory provided"
  )

  if (!os_is_windows()) {
    # FIXME: how do I create an unreadable file on windows?
    not_readable <- file.path(sandbox, "locked")
    dir.create(not_readable, mode = "220")
    skip_if(file.access(not_readable, 4) == 0,
            "temp filesystem does not support unreadable test directories")
    expect_error(
      testing_fit("bernoulli", output_dir = not_readable),
      "not readable"
    )
  }
})

test_that("output_dir works with trailing /", {
  test_dir <- withr::local_tempdir(pattern = "output_dir")
  fit <- testing_fit(
    "bernoulli",
    method = "sample",
    seed = 123,
    output_dir = paste0(test_dir,"/")
  )
  expect_equal(normalizePath(fit$runset$args$output_dir),
               normalizePath(test_dir))
  expect_equal(length(list.files(test_dir)), fit$num_procs())
})
