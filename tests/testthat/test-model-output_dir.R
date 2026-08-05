set_cmdstan_path()

local_output_sandbox <- function(pattern = "sandbox", .local_envir = parent.frame()) {
  withr::local_tempdir(pattern = pattern, .local_envir = .local_envir)
}

test_that("WSL output paths stay host-native until command composition", {
  # Use minimal method arguments so this test exercises path handling without
  # launching CmdStan.
  method_args <- list(
    method = "sample",
    save_metric = NULL,
    validate = function(num_procs) invisible(),
    compose = function(idx, args) args
  )
  # Cover system and non-system Windows drives as well as a WSL UNC path.
  host_dirs <- c(
    "C:/output",
    "D:/output",
    "//wsl$/Ubuntu/home/user/output"
  )
  wsl_dirs <- c(
    "/mnt/c/output",
    "/mnt/d/output",
    "/home/user/output"
  )
  as_wsl_path <- function(path = NULL, revert = FALSE) {
    if (is.null(path) || revert) {
      return(path)
    }
    path <- sub("//wsl$/Ubuntu", "", path, fixed = TRUE)
    for (i in seq_along(host_dirs)) {
      path <- sub(host_dirs[i], wsl_dirs[i], path, fixed = TRUE)
    }
    path
  }
  # Simulate Windows R using WSL so this boundary test runs on every platform.
  with_mocked_bindings(
    {
      args <- lapply(host_dirs, function(output_dir) {
        CmdStanArgs$new(
          model_name = "model",
          exe_file = "model",
          proc_ids = 1,
          method_args = method_args,
          output_dir = output_dir,
          output_basename = "model"
        )
      })
      output_files <- file.path(host_dirs, "model-1.csv")
      expect_equal(
        vapply(args, function(x) x$output_dir, character(1)),
        host_dirs
      )
      expect_equal(
        vapply(args, function(x) x$new_files("output"), character(1)),
        output_files
      )
      cmdstan_output_files <- vapply(seq_along(args), function(i) {
        command_args <- args[[i]]$compose_all_args(
          output_file = output_files[i]
        )
        sub("file=", "", command_args[grepl("^file=", command_args)], fixed = TRUE)
      }, character(1))
      expect_equal(cmdstan_output_files, file.path(wsl_dirs, "model-1.csv"))

      command_args <- args[[1]]$compose_all_args(
        output_file = output_files[1],
        profile_file = file.path(host_dirs[1], "model-profile-1.csv"),
        latent_dynamics_file = file.path(host_dirs[1], "model-diagnostic-1.csv")
      )
      expect_in("diagnostic_file=/mnt/c/output/model-diagnostic-1.csv", command_args)
      expect_in("profile_file=/mnt/c/output/model-profile-1.csv", command_args)

      # Omitting output_dir must still use the faster WSL-native temp directory.
      default_args <- CmdStanArgs$new(
        model_name = "model",
        exe_file = "model",
        proc_ids = 1,
        method_args = method_args,
        output_basename = "model"
      )
      expect_equal(default_args$output_dir, "//wsl$/Ubuntu/tmp/cmdstanr")
      expect_in(
        "file=/tmp/cmdstanr/model-1.csv",
        default_args$compose_all_args(
          output_file = default_args$new_files("output")
        )
      )
    },
    os_is_wsl = function() TRUE,
    wsl_safe_path = as_wsl_path,
    wsl_dir_prefix = function(...) "//wsl$/Ubuntu",
    wsl_tempdir = function() "/tmp/cmdstanr",
    validate_cmdstan_args = function(self) invisible()
  )
})

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
    files <- paste0("custom-", 1:n_files, ".csv")
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
  # Unlike the mocked test above, this exercises the full Windows/WSL workflow.
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
