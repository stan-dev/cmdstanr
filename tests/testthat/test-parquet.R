skip_if_not_installed("nanoparquet", minimum_version = "0.5.0")
set_cmdstan_path()

fit_parquet <- testing_fit(
  "logistic",
  method = "sample",
  seed = 123,
  chains = 2,
  iter_warmup = 100,
  iter_sampling = 100,
  thin = 3,
  save_warmup = TRUE
)

read_parquet_draws <- function(path) {
  posterior::as_draws_df(as.data.frame(nanoparquet::read_parquet(path)))
}

read_parquet_stan_metadata <- function(path) {
  kv <- nanoparquet::read_parquet_metadata(
    path
  )$file_meta_data$key_value_metadata[[1]]
  list(
    format_version = kv$value[kv$key == "stan.format_version"],
    metadata = jsonlite::fromJSON(kv$value[kv$key == "stan.metadata"])
  )
}

expect_same_draws <- function(object, expected) {
  object <- posterior::subset_draws(
    object,
    variable = posterior::variables(expected)
  )
  expect_equal(
    as.data.frame(object),
    as.data.frame(posterior::as_draws_df(expected))
  )
}

# compare with read_cmdstan_csv(); the order of the metadata fields differs
expect_same_contents <- function(object, expected) {
  sort_metadata <- function(x) {
    x$metadata <- x$metadata[sort(names(x$metadata))]
    x
  }
  expect_equal(sort_metadata(object), sort_metadata(expected))
}

# copy CSV files to a temporary directory and change their lines with fun
edit_csv_files <- function(
  files,
  fun,
  dir = withr::local_tempdir(.local_envir = parent.frame())
) {
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
      fit_parquet$output_files(),
      out,
      warmup_file = out_warmup,
      chunk_rows = 7
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
  expect_same_draws(
    warmup,
    posterior::subset_draws(all_draws, iteration = 1:34)
  )

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
  cmdstan_csv_to_parquet(
    files,
    out_one,
    warmup_file = warmup_one,
    chunk_rows = 1
  )
  expect_identical(
    nanoparquet::read_parquet(out_one),
    nanoparquet::read_parquet(out_default)
  )
  expect_identical(
    nanoparquet::read_parquet(warmup_one),
    nanoparquet::read_parquet(warmup_default)
  )
  # the default gives one row group for each chain
  expect_identical(
    nrow(nanoparquet::read_parquet_metadata(out_default)$row_groups),
    2L
  )
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
  # the CSV values true and false are JSON booleans
  expect_true(meta$save_warmup)
  expect_true(meta$adapt_engaged)
  expect_identical(meta$chains$id, 1:2)
  expect_equal(
    meta$chains$step_size_adaptation,
    fit_parquet$metadata()$step_size_adaptation
  )
  expect_equal(
    meta$chains$inv_metric[[2]],
    unname(fit_parquet$inv_metric(matrix = FALSE)[[2]])
  )
  expect_identical(meta$variables, fit_parquet$metadata()$variables)
  expect_equal(
    meta$stan_variable_sizes,
    fit_parquet$metadata()$stan_variable_sizes
  )
  expect_identical(
    meta$sampler_diagnostics,
    posterior::variables(fit_parquet$sampler_diagnostics())
  )
  expect_named(meta$chains$time, c("chain_id", "warmup", "sampling", "total"))
})

test_that("cmdstan_csv_to_parquet() stores a dense metric", {
  fit <- testing_fit(
    "logistic",
    method = "sample",
    seed = 123,
    chains = 1,
    iter_warmup = 300,
    iter_sampling = 10,
    metric = "dense_e"
  )
  out <- withr::local_tempfile(fileext = ".parquet")
  cmdstan_csv_to_parquet(fit$output_files(), out)
  meta <- read_parquet_stan_metadata(out)$metadata
  expect_identical(dim(meta$chains$inv_metric[[1]]), c(4L, 4L))
  expect_equal(meta$chains$inv_metric[[1]], unname(fit$inv_metric()[[1]]))
  expect_same_contents(
    read_cmdstan_parquet(out),
    read_cmdstan_csv(fit$output_files())
  )
})

test_that("cmdstan_csv_to_parquet() keeps one-element vectors as JSON arrays", {
  fit <- testing_fit(
    "bernoulli",
    method = "sample",
    seed = 123,
    chains = 1,
    iter_warmup = 100,
    iter_sampling = 10
  )
  out <- withr::local_tempfile(fileext = ".parquet")
  cmdstan_csv_to_parquet(fit$output_files(), out)
  kv <- nanoparquet::read_parquet_metadata(
    out
  )$file_meta_data$key_value_metadata[[1]]
  json <- kv$value[kv$key == "stan.metadata"]
  expect_match(
    json,
    '"stan_variable_sizes":{"lp__":[1],"theta":[1]}',
    fixed = TRUE
  )
  expect_match(json, '"inv_metric":[', fixed = TRUE)
})

test_that("cmdstan_csv_to_parquet() reads compressed CSV files", {
  csv_files <- fit_parquet$output_files()
  gz_files <- file.path(
    withr::local_tempdir(),
    paste0(basename(csv_files), ".gz")
  )
  for (i in seq_along(csv_files)) {
    con <- gzfile(gz_files[i], "w")
    writeLines(readLines(csv_files[i]), con)
    close(con)
  }
  out_csv <- withr::local_tempfile(fileext = ".parquet")
  out_gz <- withr::local_tempfile(fileext = ".parquet")
  cmdstan_csv_to_parquet(csv_files, out_csv)
  cmdstan_csv_to_parquet(gz_files, out_gz)
  expect_identical(
    nanoparquet::read_parquet(out_gz),
    nanoparquet::read_parquet(out_csv)
  )
})

test_that("cmdstan_csv_to_parquet() reads bzip2 CSV files from $sample()", {
  fit <- testing_fit(
    "logistic",
    method = "sample",
    seed = 123,
    chains = 2,
    iter_warmup = 100,
    iter_sampling = 100,
    thin = 3,
    save_warmup = TRUE,
    compress = "bzip2"
  )
  expect_match(fit$output_files(), "\\.csv\\.bz2$")
  out_csv <- withr::local_tempfile(fileext = ".parquet")
  out_bz2 <- withr::local_tempfile(fileext = ".parquet")
  cmdstan_csv_to_parquet(fit_parquet$output_files(), out_csv)
  cmdstan_csv_to_parquet(fit$output_files(), out_bz2)
  expect_identical(
    nanoparquet::read_parquet(out_bz2),
    nanoparquet::read_parquet(out_csv)
  )
})

test_that("cmdstan_csv_to_parquet() names tuple and complex variables", {
  stan_file <- write_stan_file(
    "
    parameters {
      complex z;
      vector[2] v;
    }
    model {
      get_real(z) ~ normal(0, 1);
      get_imag(z) ~ normal(0, 1);
      v ~ normal(0, 1);
    }
    generated quantities {
      tuple(real, array[2] real) t = (get_real(z), {v[1], v[2]});
      array[2] tuple(int, complex) at = {(1, z), (2, 2 * z)};
      complex_vector[2] zv = [z, 2 * z]';
    }
    ",
    dir = withr::local_tempdir()
  )
  utils::capture.output(
    fit <- cmdstan_model(stan_file)$sample(
      chains = 2,
      iter_warmup = 100,
      iter_sampling = 20,
      refresh = 0,
      seed = 1
    )
  )
  out <- withr::local_tempfile(fileext = ".parquet")
  cmdstan_csv_to_parquet(fit$output_files(), out)
  draws <- read_parquet_draws(out)
  expect_true(all(c("z[real]", "z[imag]") %in% posterior::variables(draws)))
  expect_true(any(grepl(":", posterior::variables(draws), fixed = TRUE)))
  expect_same_draws(draws, fit$draws())
  meta <- read_parquet_stan_metadata(out)$metadata
  expect_identical(meta$variables, fit$metadata()$variables)
  expect_equal(meta$stan_variable_sizes, fit$metadata()$stan_variable_sizes)
  expect_same_contents(
    read_cmdstan_parquet(out),
    read_cmdstan_csv(fit$output_files())
  )
})

test_that("cmdstan_csv_to_parquet() writes columns when there are no draws", {
  fit <- testing_fit(
    "logistic",
    method = "sample",
    seed = 123,
    chains = 2,
    iter_warmup = 50,
    iter_sampling = 0
  )
  out <- withr::local_tempfile(fileext = ".parquet")
  out_warmup <- withr::local_tempfile(fileext = ".parquet")
  cmdstan_csv_to_parquet(fit$output_files(), out, warmup_file = out_warmup)
  for (path in c(out, out_warmup)) {
    pq <- nanoparquet::read_parquet(path)
    expect_identical(nrow(pq), 0L)
    expect_true(all(
      c("lp__", "beta[1]", ".chain", ".iteration", ".draw") %in% names(pq)
    ))
  }
})

test_that("cmdstan_csv_to_parquet() replaces an old warmup file", {
  fit <- testing_fit(
    "logistic",
    method = "sample",
    seed = 123,
    chains = 1,
    iter_warmup = 50,
    iter_sampling = 20
  )
  out <- withr::local_tempfile(fileext = ".parquet")
  out_warmup <- withr::local_tempfile(fileext = ".parquet")
  cmdstan_csv_to_parquet(
    fit_parquet$output_files(),
    out,
    warmup_file = out_warmup
  )
  expect_identical(nrow(nanoparquet::read_parquet(out_warmup)), 68L)
  cmdstan_csv_to_parquet(
    fit$output_files(),
    out,
    warmup_file = out_warmup,
    overwrite = TRUE
  )
  expect_identical(nrow(nanoparquet::read_parquet(out)), 20L)
  expect_identical(nrow(nanoparquet::read_parquet(out_warmup)), 0L)
})

test_that("cmdstan_csv_to_parquet() writes no warmup draws for fixed_param", {
  fit <- testing_fit(
    "bernoulli",
    method = "sample",
    seed = 123,
    chains = 1,
    iter_warmup = 30,
    iter_sampling = 20,
    fixed_param = TRUE
  )
  # CmdStan accepts save_warmup with fixed_param but writes no warmup draws
  files <- edit_csv_files(fit$output_files(), function(lines, i) {
    sub("save_warmup = (false|0)", "save_warmup = true", lines)
  })
  expect_true(any(grepl("save_warmup = true", readLines(files))))
  out <- withr::local_tempfile(fileext = ".parquet")
  out_warmup <- withr::local_tempfile(fileext = ".parquet")
  expect_no_warning(cmdstan_csv_to_parquet(
    files,
    out,
    warmup_file = out_warmup
  ))
  expect_same_draws(read_parquet_draws(out), fit$draws())
  expect_identical(nrow(nanoparquet::read_parquet(out_warmup)), 0L)
  x <- read_cmdstan_parquet(out, warmup_file = out_warmup)
  expect_equal(x$post_warmup_draws, fit$draws())
  expect_null(x$warmup_draws)
  expect_null(x$post_warmup_sampler_diagnostics)
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

test_that("cmdstan_csv_to_parquet() stops at a malformed line", {
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
      cmdstan_csv_to_parquet(
        files,
        out,
        chunk_rows = chunk_rows,
        overwrite = TRUE
      ),
      "Malformed data line"
    )
  }
  expect_identical(nanoparquet::read_parquet(out), old)
  expect_identical(list.files(dirname(out), pattern = "^\\.tmp_"), character())
})

test_that("cmdstan_csv_to_parquet() stops at a non-numeric value", {
  files <- edit_csv_files(fit_parquet$output_files(), function(lines, i) {
    if (i == 1) {
      idx <- data_line_index(lines)[3]
      lines[idx] <- sub("^[^,]*", "abc", lines[idx])
    }
    lines
  })
  out <- withr::local_tempfile(fileext = ".parquet")
  expect_error(
    cmdstan_csv_to_parquet(files, out),
    "Malformed data line .* rows 1 and"
  )
  expect_false(file.exists(out))
})

test_that("cmdstan_csv_to_parquet() stops when the CSV headers differ", {
  swap_columns <- function(lines) {
    idx <- which(nzchar(lines) & !startsWith(lines, "#"))[1]
    lines[idx] <- sub(
      "stepsize__,treedepth__",
      "treedepth__,stepsize__",
      lines[idx],
      fixed = TRUE
    )
    lines
  }
  files <- edit_csv_files(fit_parquet$output_files(), function(lines, i) {
    if (i == 2) swap_columns(lines) else lines
  })
  out <- withr::local_tempfile(fileext = ".parquet")
  expect_error(
    cmdstan_csv_to_parquet(files, out),
    "differs from the first file"
  )
  # a second file with a header but no draws
  files <- edit_csv_files(fit_parquet$output_files(), function(lines, i) {
    if (i == 2) swap_columns(lines[-data_line_index(lines)]) else lines
  })
  expect_error(
    cmdstan_csv_to_parquet(files, out),
    "differs from the first file"
  )
  expect_false(file.exists(out))
})

test_that("cmdstan_csv_to_parquet() checks its arguments", {
  dir <- withr::local_tempdir()
  expect_error(
    cmdstan_csv_to_parquet(
      fit_parquet$output_files(),
      file.path(dir, "x.parquet"),
      warmup_file = file.path(dir, ".", "x.parquet")
    ),
    "must be different"
  )
  expect_identical(list.files(dir, all.files = TRUE, no.. = TRUE), character())
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
      fit_parquet$output_files(),
      out,
      warmup_file = same_path,
      overwrite = TRUE
    ),
    "must be different"
  )
  fit_optimize <- testing_fit("logistic", method = "optimize", seed = 123)
  expect_error(
    cmdstan_csv_to_parquet(
      fit_optimize$output_files(),
      withr::local_tempfile()
    ),
    "Only CSV files from the MCMC sampler"
  )
})

test_that("read_cmdstan_parquet() gives the result of read_cmdstan_csv()", {
  files <- fit_parquet$output_files()
  out <- withr::local_tempfile(fileext = ".parquet")
  out_warmup <- withr::local_tempfile(fileext = ".parquet")
  cmdstan_csv_to_parquet(files, out, warmup_file = out_warmup)
  expect_same_contents(
    read_cmdstan_parquet(out, warmup_file = out_warmup),
    read_cmdstan_csv(files)
  )
  selections <- list(
    list(variables = "beta", sampler_diagnostics = ""),
    list(variables = "", sampler_diagnostics = "divergent__"),
    list(variables = c("alpha", "beta[2]"), format = "draws_df")
  )
  for (args in selections) {
    expect_same_contents(
      do.call(
        read_cmdstan_parquet,
        c(list(out, warmup_file = out_warmup), args)
      ),
      do.call(read_cmdstan_csv, c(list(files), args))
    )
  }
  expect_error(
    read_cmdstan_parquet(out, variables = "gamma"),
    "Can't find the following variable(s) in the output: gamma",
    fixed = TRUE
  )
  expect_error(
    read_cmdstan_parquet(out, sampler_diagnostics = "gamma__"),
    "Can't find the following sampler diagnostic(s)",
    fixed = TRUE
  )
  # without the warmup file
  x <- read_cmdstan_parquet(out)
  expect_null(x$warmup_draws)
  expect_null(x$warmup_sampler_diagnostics)
  expect_identical(x$metadata$save_warmup, 0L)
  expect_equal(x$post_warmup_draws, read_cmdstan_csv(files)$post_warmup_draws)
})

test_that("read_cmdstan_parquet() rejects other Parquet files", {
  out <- withr::local_tempfile(fileext = ".parquet")
  nanoparquet::write_parquet(data.frame(a = 1), out)
  expect_error(read_cmdstan_parquet(out), "no CmdStan metadata")
})

test_that("as_cmdstan_fit() makes a fit from a Parquet file", {
  files <- fit_parquet$output_files()
  out <- withr::local_tempfile(fileext = ".parquet")
  out_warmup <- withr::local_tempfile(fileext = ".parquet")
  cmdstan_csv_to_parquet(files, out, warmup_file = out_warmup)
  fit_pq <- as_cmdstan_fit(out, warmup_file = out_warmup)
  fit_csv <- as_cmdstan_fit(files)
  expect_s3_class(fit_pq, "CmdStanMCMC")
  expect_identical(fit_pq$output_files(), out)
  expect_identical(fit_pq$num_chains(), 2L)
  expect_equal(fit_pq$draws(), fit_parquet$draws())
  expect_equal(
    fit_pq$draws(inc_warmup = TRUE),
    fit_parquet$draws(inc_warmup = TRUE)
  )
  expect_equal(
    fit_pq$sampler_diagnostics(inc_warmup = TRUE),
    fit_parquet$sampler_diagnostics(inc_warmup = TRUE)
  )
  expect_equal(fit_pq$inv_metric(), fit_parquet$inv_metric())
  expect_equal(fit_pq$time(), fit_csv$time())
  expect_equal(
    fit_pq$metadata()[sort(names(fit_pq$metadata()))],
    fit_csv$metadata()[sort(names(fit_csv$metadata()))]
  )
  expect_equal(
    suppressWarnings(fit_pq$summary()),
    suppressWarnings(fit_parquet$summary())
  )
  expect_error(fit_pq$code(), "not available")
  # variables selects the draws that the object reads first
  fit_alpha <- as_cmdstan_fit(out, variables = "alpha")
  expect_equal(fit_alpha$draws(), fit_parquet$draws("alpha"))
  # without the warmup file, the object has no warmup draws
  expect_error(as_cmdstan_fit(out)$draws(inc_warmup = TRUE), "without them")
  expect_error(
    as_cmdstan_fit(files, warmup_file = out_warmup),
    "can only be used with a Parquet file"
  )
})
