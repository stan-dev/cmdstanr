context("fitted-shared-methods")

if (not_on_cran()) {
  set_cmdstan_path()

  fits <- list()
  fits[["sample"]] <- testing_fit("logistic", method = "sample",
                                  seed = 123, save_latent_dynamics = TRUE)
  fits[["variational"]] <- testing_fit("logistic", method = "variational",
                                       seed = 123, save_latent_dynamics = TRUE)
  fits[["optimize"]] <- testing_fit("logistic", method = "optimize", seed = 123)
  fit_bern <- testing_fit("bernoulli", method = "sample", seed = 123)
  fits[["generate_quantities"]] <- testing_fit("bernoulli_ppc", method = "generate_quantities", fitted_params = fit_bern, seed = 123)
  all_methods <- c("sample", "optimize", "variational", "generate_quantities")
}

test_that("*_files() methods return the right number of paths", {
  skip_on_cran()
  for (method in all_methods) {
    expect_length(fits[[method]]$output_files(), fits[[method]]$num_procs())
    expect_length(fits[[method]]$data_file(), 1)
    if (method %in% c("sample", "variational")) {
      expect_length(fits[[method]]$latent_dynamics_files(), fits[[method]]$num_procs())
    }
  }
})

test_that("saving csv output files works", {
  skip_on_cran()

  for (method in all_methods) {
    fit <- fits[[method]]
    old_paths <- fit$output_files()
    checkmate::expect_file_exists(old_paths, extension = "csv")

    expect_message(
      paths <- fit$save_output_files(tempdir(), basename = "testing-output"),
      paste("Moved", fit$num_procs(), "files and set internal paths")
    )
    checkmate::expect_file_exists(paths, extension = "csv")
    expect_true(all(file.size(paths) > 0))

    should_match <- paste0("testing-output-",
                           format(Sys.time(), "%Y%m%d%H%M"),
                           "-",
                           seq_len(fit$num_procs()))
    for (j in seq_along(paths)) {
      expect_match(paths[j], should_match[j])
    }

    expect_false(any(file.exists(old_paths)))
    expect_equal(fit$output_files(), paths)
  }
})

test_that("saving diagnostic csv output works", {
  skip_on_cran()

  for (method in all_methods) {
    fit <- fits[[method]]
    if (!(method %in% c("sample", "variational"))) {
      expect_error(
        fit$save_latent_dynamics_files(),
        "No latent dynamics files found. Set 'save_latent_dynamics=TRUE' when fitting the model",
        fixed = TRUE
      )
      next
    }

    old_paths <- fit$latent_dynamics_files()
    checkmate::expect_file_exists(old_paths, extension = "csv")

    expect_message(
      paths <- fit$save_latent_dynamics_files(tempdir(), basename = "testing-output"),
      paste("Moved", fit$num_procs(), "files and set internal paths")
    )
    checkmate::expect_file_exists(paths, extension = "csv")
    expect_true(all(file.size(paths) > 0))

    should_match <- paste0("testing-output-diagnostic-",
                           format(Sys.time(), "%Y%m%d%H%M"),
                           "-",
                           seq_len(fit$num_procs()))

    for (j in seq_along(paths)) {
      expect_match(paths[j], should_match[j])
    }

    expect_false(any(file.exists(old_paths)))
    expect_equal(fit$latent_dynamics_files(), paths)
  }
})

test_that("saving data file works", {
  skip_on_cran()

  for (method in all_methods) {
    fit <- fits[[method]]
    old_path <- fit$data_file()
    checkmate::expect_file_exists(old_path, extension = "json")

    expect_message(
      path <- fit$save_data_file(tempdir(), basename = NULL,
                                 timestamp = FALSE, random = FALSE),
      "Moved data file and set internal path"
    )
    checkmate::expect_file_exists(path, extension = "json")
    expect_true(file.size(path) > 0)
    if(method == "generate_quantities") {
      expect_equal(basename(path), "bernoulli_ppc.json")
    } else {
      expect_equal(basename(path), "logistic.json")
    }
    expect_false(file.exists(old_path))
    expect_equal(fit$data_file(), path)
  }
})

test_that("cmdstan_summary() and cmdstan_diagnose() work correctly", {
  skip_on_cran()
  for (method in all_methods) {
    fit <- fits[[method]]
    if (method == "optimize") {
      expect_error(fit$cmdstan_summary(), "Not available for optimize method")
      expect_error(fit$cmdstan_diagnose(), "Not available for optimize method")
    } else if (method == "generate_quantities") {
      expect_error(fit$cmdstan_summary(), "Not available for generate_quantities method")
      expect_error(fit$cmdstan_diagnose(), "Not available for generate_quantities method")
    } else {
      expect_output(fit$cmdstan_summary(), "Inference for Stan model")
      expect_output(fit$cmdstan_diagnose(), "Processing complete")
    }
  }
})

test_that("draws() method returns a 'draws' object", {
  skip_on_cran()
  for (method in all_methods) {
    fit <- fits[[method]]
    draws <- fit$draws()
    expect_type(draws, "double")
    expect_s3_class(draws, "draws")
  }
})

test_that("save_object() method works", {
  skip_on_cran()
  for (method in all_methods) {
    fit <- fits[[method]]
    temp_rds_file <- tempfile(fileext = ".RDS")
    fit$save_object(temp_rds_file)
    fit2 <- readRDS(temp_rds_file)
    expect_identical(fit2$summary(), fit$summary())
  }

  # check after garbage collection too
  temp_rds_file <- tempfile(fileext = ".RDS")
  fit <- testing_fit("logistic", method = "sample", seed = 123)
  fit$save_object(temp_rds_file)
  s <- fit$summary()
  rm(fit); gc()
  fit <- readRDS(temp_rds_file)
  expect_identical(fit$summary(), s)
})

test_that("metadata() returns list", {
  for (method in all_methods) {
    fit <- fits[[method]]
    expect_type(fit$metadata(), "list")
    expect_equal(fit$metadata()$method, method)
  }
})

test_that("output and latent dynamics files are cleaned up correctly", {
  skip_on_cran()
  for (method in c("sample", "variational")) {
    fit <- testing_fit("logistic", method = method, seed = 123, save_latent_dynamics = TRUE)
    out_files <- fit$output_files()
    latent_dynamics_files <- fit$latent_dynamics_files()
    expect_true(all(file.exists(out_files)))
    expect_true(all(file.exists(latent_dynamics_files)))
    rm(fit)
    gc()
    expect_true(!any(file.exists(out_files)))
    expect_true(!any(file.exists(latent_dynamics_files)))

    fit <- testing_fit("logistic", method = method, seed = 123, save_latent_dynamics = TRUE)
    fit$save_output_files(dir = tempdir())
    out_files <- fit$output_files()
    latent_dynamics_files <- fit$latent_dynamics_files()
    expect_true(all(file.exists(out_files)))
    expect_true(all(file.exists(latent_dynamics_files)))
    rm(fit)
    gc()
    expect_true(all(file.exists(out_files)))
    expect_true(!any(file.exists(latent_dynamics_files)))
    file.remove(out_files)

    fit <- testing_fit("logistic", method = method, seed = 123, save_latent_dynamics = TRUE)
    fit$save_latent_dynamics_files(dir = tempdir())
    out_files <- fit$output_files()
    latent_dynamics_files <- fit$latent_dynamics_files()
    expect_true(all(file.exists(out_files)))
    expect_true(all(file.exists(latent_dynamics_files)))
    rm(fit)
    gc()
    expect_true(!any(file.exists(out_files)))
    expect_true(all(file.exists(latent_dynamics_files)))
    file.remove(latent_dynamics_files)

    fit <- testing_fit("logistic", method = method, seed = 123, save_latent_dynamics = TRUE)
    fit$save_output_files(dir = tempdir())
    fit$save_latent_dynamics_files(dir = tempdir())
    out_files <- fit$output_files()
    latent_dynamics_files <- fit$latent_dynamics_files()
    expect_true(all(file.exists(out_files)))
    expect_true(all(file.exists(latent_dynamics_files)))
    rm(fit)
    gc()
    expect_true(all(file.exists(out_files)))
    expect_true(all(file.exists(latent_dynamics_files)))
    file.remove(out_files)
    file.remove(latent_dynamics_files)
  }
})

test_that("CmdStanArgs erorrs if idx is out of proc_ids range", {
  skip_on_cran()
  data_file <- test_path("resources", "data", "bernoulli.data.json")
  mod <- testing_model("bernoulli")
  arg <- CmdStanArgs$new(
    method_args = SampleArgs$new(),
    model_name = "bernoulli",
    exe_file = mod$exe_file(),
    data_file = data_file,
    proc_ids = c(1,2,3,4)
  )
  expect_error(
    arg$compose_all_args(idx = 5),
    "Index \\(5\\) exceeds number of CmdStan processes \\(4\\)."
  )
})
