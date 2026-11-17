skip_if_not_installed("nanoparquet", minimum_version = "0.5.0")
set_cmdstan_path()

fit_parquet <- testing_fit(
  "logistic",
  method = "sample", seed = 123, chains = 2,
  iter_warmup = 100, iter_sampling = 100, thin = 3, save_warmup = TRUE
)

read_parquet_draws <- function(path) {
  posterior::as_draws_df(as.data.frame(nanoparquet::read_parquet(path)))
}

read_parquet_stan_metadata <- function(path) {
  kv <- nanoparquet::read_parquet_metadata(path)$file_meta_data$key_value_metadata[[1]]
  list(
    format_version = kv$value[kv$key == "stan.format_version"],
    metadata = jsonlite::fromJSON(kv$value[kv$key == "stan.metadata"])
  )
}

expect_same_draws <- function(object, expected) {
  object <- posterior::subset_draws(object, variable = posterior::variables(expected))
  expect_equal(as.data.frame(object), as.data.frame(posterior::as_draws_df(expected)))
}

# copy CSV files to a temporary directory and change their lines with fun
edit_csv_files <- function(files, fun, dir = withr::local_tempdir(.local_envir = parent.frame())) {
  out <- file.path(dir, basename(files))
  for (i in seq_along(files)) {
    writeLines(fun(readLines(files[i]), i), out[i])
  }
  out
}

data_line_index <- function(lines) {
  which(nzchar(lines) & !startsWith(lines, "#"))[-1]
}

test_that("cmdstan_csv_to_parquet() gives the draws of $draws()", {
  out <- withr::local_tempfile(fileext = ".parquet")
  out_warmup <- withr::local_tempfile(fileext = ".parquet")
  # chunks of 7 lines cross the adaptation comments and the warmup end
  expect_identical(
    cmdstan_csv_to_parquet(
      fit_parquet$output_files(), out,
      warmup_file = out_warmup, chunk_rows = 7
    ),
    out
  )
  draws <- read_parquet_draws(out)
  expect_identical(posterior::nchains(draws), 2L)
  expect_identical(posterior::niterations(draws), 34L)
  expect_same_draws(draws, fit_parquet$draws())
  expect_same_draws(draws, fit_parquet$sampler_diagnostics())

  warmup <- read_parquet_draws(out_warmup)
  all_draws <- fit_parquet$draws(inc_warmup = TRUE)
  expect_identical(posterior::niterations(warmup), 34L)
  expect_same_draws(warmup, posterior::subset_draws(all_draws, iteration = 1:34))

  pq_names <- names(nanoparquet::read_parquet(out))
  expect_identical(utils::tail(pq_names, 3), c(".chain", ".iteration", ".draw"))
  expect_identical(list.files(dirname(out), pattern = "^\\.tmp_"), character())
})

test_that("cmdstan_csv_to_parquet() result does not depend on chunk_rows", {
  out_default <- withr::local_tempfile(fileext = ".parquet")
  out_one <- withr::local_tempfile(fileext = ".parquet")
  warmup_default <- withr::local_tempfile(fileext = ".parquet")
  warmup_one <- withr::local_tempfile(fileext = ".parquet")
  files <- fit_parquet$output_files()
  cmdstan_csv_to_parquet(files, out_default, warmup_file = warmup_default)
  cmdstan_csv_to_parquet(files, out_one, warmup_file = warmup_one, chunk_rows = 1)
  expect_identical(
    nanoparquet::read_parquet(out_one),
    nanoparquet::read_parquet(out_default)
  )
  expect_identical(
    nanoparquet::read_parquet(warmup_one),
    nanoparquet::read_parquet(warmup_default)
  )
  # the default gives one row group for each chain
  expect_identical(nrow(nanoparquet::read_parquet_metadata(out_default)$row_groups), 2L)
})

test_that("cmdstan_csv_to_parquet() stores the CmdStan metadata", {
  out <- withr::local_tempfile(fileext = ".parquet")
  cmdstan_csv_to_parquet(fit_parquet$output_files(), out)
  m <- read_parquet_stan_metadata(out)
  expect_identical(m$format_version, "1")
  meta <- m$metadata
  expect_identical(meta$method, "sample")
  expect_identical(meta$num_chains, 2L)
  expect_identical(meta$thin, 3L)
  expect_identical(meta$chains$id, 1:2)
  expect_equal(meta$chains$step_size_adaptation, fit_parquet$metadata()$step_size_adaptation)
  expect_equal(meta$chains$inv_metric[[2]], unname(fit_parquet$inv_metric(matrix = FALSE)[[2]]))
  expect_identical(meta$variables, fit_parquet$metadata()$variables)
  expect_equal(meta$stan_variable_sizes, fit_parquet$metadata()$stan_variable_sizes)
  expect_identical(meta$sampler_diagnostics, posterior::variables(fit_parquet$sampler_diagnostics()))
  expect_named(meta$chains$time, c("chain_id", "warmup", "sampling", "total"))
})

test_that("cmdstan_csv_to_parquet() stores a dense metric", {
  fit <- testing_fit(
    "logistic",
    method = "sample", seed = 123, chains = 1,
    iter_warmup = 300, iter_sampling = 10, metric = "dense_e"
  )
  out <- withr::local_tempfile(fileext = ".parquet")
  cmdstan_csv_to_parquet(fit$output_files(), out)
  meta <- read_parquet_stan_metadata(out)$metadata
  expect_identical(dim(meta$chains$inv_metric[[1]]), c(4L, 4L))
  expect_equal(meta$chains$inv_metric[[1]], unname(fit$inv_metric()[[1]]))
})

test_that("cmdstan_csv_to_parquet() keeps one-element vectors as JSON arrays", {
  fit <- testing_fit(
    "bernoulli",
    method = "sample", seed = 123, chains = 1,
    iter_warmup = 100, iter_sampling = 10
  )
  out <- withr::local_tempfile(fileext = ".parquet")
  cmdstan_csv_to_parquet(fit$output_files(), out)
  kv <- nanoparquet::read_parquet_metadata(out)$file_meta_data$key_value_metadata[[1]]
  json <- kv$value[kv$key == "stan.metadata"]
  expect_match(json, '"stan_variable_sizes":{"lp__":[1],"theta":[1]}', fixed = TRUE)
  expect_match(json, '"inv_metric":[', fixed = TRUE)
})

test_that("cmdstan_csv_to_parquet() reads compressed CSV files", {
  csv_files <- fit_parquet$output_files()
  gz_files <- file.path(withr::local_tempdir(), paste0(basename(csv_files), ".gz"))
  for (i in seq_along(csv_files)) {
    con <- gzfile(gz_files[i], "w")
    writeLines(readLines(csv_files[i]), con)
    close(con)
  }
  out_csv <- withr::local_tempfile(fileext = ".parquet")
  out_gz <- withr::local_tempfile(fileext = ".parquet")
  cmdstan_csv_to_parquet(csv_files, out_csv)
  cmdstan_csv_to_parquet(gz_files, out_gz)
  expect_identical(nanoparquet::read_parquet(out_gz), nanoparquet::read_parquet(out_csv))
})

test_that("cmdstan_csv_to_parquet() writes columns when there are no draws", {
  fit <- testing_fit(
    "logistic",
    method = "sample", seed = 123, chains = 2,
    iter_warmup = 50, iter_sampling = 0
  )
  out <- withr::local_tempfile(fileext = ".parquet")
  out_warmup <- withr::local_tempfile(fileext = ".parquet")
  cmdstan_csv_to_parquet(fit$output_files(), out, warmup_file = out_warmup)
  for (path in c(out, out_warmup)) {
    pq <- nanoparquet::read_parquet(path)
    expect_identical(nrow(pq), 0L)
    expect_true(all(c("lp__", "beta[1]", ".chain", ".iteration", ".draw") %in% names(pq)))
  }
})

test_that("cmdstan_csv_to_parquet() replaces an old warmup file", {
  fit <- testing_fit(
    "logistic",
    method = "sample", seed = 123, chains = 1,
    iter_warmup = 50, iter_sampling = 20
  )
  out <- withr::local_tempfile(fileext = ".parquet")
  out_warmup <- withr::local_tempfile(fileext = ".parquet")
  cmdstan_csv_to_parquet(fit_parquet$output_files(), out, warmup_file = out_warmup)
  expect_identical(nrow(nanoparquet::read_parquet(out_warmup)), 68L)
  cmdstan_csv_to_parquet(fit$output_files(), out, warmup_file = out_warmup, overwrite = TRUE)
  expect_identical(nrow(nanoparquet::read_parquet(out)), 20L)
  expect_identical(nrow(nanoparquet::read_parquet(out_warmup)), 0L)
})

test_that("cmdstan_csv_to_parquet() writes no warmup draws for fixed_param", {
  fit <- testing_fit(
    "bernoulli",
    method = "sample", seed = 123, chains = 1,
    iter_warmup = 30, iter_sampling = 20, fixed_param = TRUE
  )
  # CmdStan itself accepts save_warmup with fixed_param, but writes no warmup draws
  files <- edit_csv_files(fit$output_files(), function(lines, i) {
    sub("save_warmup = (false|0)", "save_warmup = true", lines)
  })
  expect_true(any(grepl("save_warmup = true", readLines(files))))
  out <- withr::local_tempfile(fileext = ".parquet")
  out_warmup <- withr::local_tempfile(fileext = ".parquet")
  expect_no_warning(cmdstan_csv_to_parquet(files, out, warmup_file = out_warmup))
  expect_same_draws(read_parquet_draws(out), fit$draws())
  expect_identical(nrow(nanoparquet::read_parquet(out_warmup)), 0L)
})

test_that("cmdstan_csv_to_parquet() warns about a chain with missing draws", {
  files <- edit_csv_files(fit_parquet$output_files(), function(lines, i) {
    if (i == 2) {
      idx <- data_line_index(lines)
      lines <- lines[-utils::tail(idx, 5)]
    }
    lines
  })
  out <- withr::local_tempfile(fileext = ".parquet")
  expect_warning(
    cmdstan_csv_to_parquet(files, out),
    "Chain 2 .* has 63 draws, but the sampler arguments give 68"
  )
  expect_identical(nrow(nanoparquet::read_parquet(out)), 34L + 29L)
})

test_that("cmdstan_csv_to_parquet() stops at a malformed line and keeps old output", {
  files <- edit_csv_files(fit_parquet$output_files(), function(lines, i) {
    if (i == 2) {
      idx <- utils::tail(data_line_index(lines), 1)
      lines[idx] <- sub(",[^,]*$", "", lines[idx])
    }
    lines
  })
  out <- withr::local_tempfile(fileext = ".parquet")
  cmdstan_csv_to_parquet(fit_parquet$output_files(), out)
  old <- nanoparquet::read_parquet(out)
  for (chunk_rows in list(NULL, 1, 7)) {
    expect_error(
      cmdstan_csv_to_parquet(files, out, chunk_rows = chunk_rows, overwrite = TRUE),
      "Malformed data line"
    )
  }
  expect_identical(nanoparquet::read_parquet(out), old)
  expect_identical(list.files(dirname(out), pattern = "^\\.tmp_"), character())
})

test_that("cmdstan_csv_to_parquet() checks its arguments", {
  out <- withr::local_tempfile(fileext = ".parquet")
  cmdstan_csv_to_parquet(fit_parquet$output_files(), out)
  expect_error(
    cmdstan_csv_to_parquet(fit_parquet$output_files(), out),
    "already exists"
  )
  expect_no_error(
    cmdstan_csv_to_parquet(fit_parquet$output_files(), out, overwrite = TRUE)
  )
  same_path <- file.path(dirname(out), ".", basename(out))
  expect_error(
    cmdstan_csv_to_parquet(
      fit_parquet$output_files(), out,
      warmup_file = same_path, overwrite = TRUE
    ),
    "must be different"
  )
  fit_optimize <- testing_fit("logistic", method = "optimize", seed = 123)
  expect_error(
    cmdstan_csv_to_parquet(fit_optimize$output_files(), withr::local_tempfile()),
    "Only CSV files from the MCMC sampler"
  )
})
