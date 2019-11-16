context("json")

test_that("test JSON output unboxing", {
  temp_file <- tempfile()
  N <- 10
  write_stan_json(list(N = N), file = temp_file)
  json_output <- readLines(temp_file)
  expect_known_output(cat(json_output, sep = "\n"),
                      file = test_path("answers", "json-unboxing.json"))
})

test_that("test JSON output boolean", {
  temp_file <- tempfile()
  N <- c(TRUE, FALSE, TRUE)
  write_stan_json(list(N = N), file = temp_file)
  json_output <- readLines(temp_file)
  expect_known_output(cat(json_output, sep = "\n"),
                      file = test_path("answers", "json-boolean.json"))
})

test_that("test JSON output factor", {
  temp_file <- tempfile()
  N <- factor(c(0,1,2,2,1,0), labels = c("c1", "c2", "c3"))
  write_stan_json(list(N = N), file = temp_file)
  json_output <- readLines(temp_file)
  expect_known_output(cat(json_output, sep = "\n"),
                      file = test_path("answers", "json-factor.json"))
})

test_that("test JSON output integer vector", {
  temp_file <- tempfile()
  N <- c(1.0, 2.0, 3, 4)

  write_stan_json(list(N = N), file = temp_file)
  json_output <- readLines(temp_file)
  expect_known_output(cat(json_output, sep = "\n"),
                      file = test_path("answers", "json-integer.json"))
})

test_that("test JSON output data frame and matrix", {
  temp_file_df <- tempfile()
  temp_file_mat <- tempfile()
  x <- 1:3
  y <- c(0.2, 0.3, 0.4)
  df <- data.frame(x = x, y = y)
  mat <- as.matrix(cbind(x, y))

  write_stan_json(list(X = df), file = temp_file_df)
  write_stan_json(list(X = mat), file = temp_file_mat)
  json_output_mat <- readLines(temp_file_df)
  json_output_df <- readLines(temp_file_mat)
  expect_identical(json_output_df, json_output_mat)
  expect_known_output(cat(json_output_df, sep = "\n"),
                      file = test_path("answers", "json-df-matrix.json"))
})

test_that("test JSON output lists", {
  temp_file <- tempfile()
  I <- 2; J <- 3;
  N <- lapply(1:I, function(i) rep(26, J))

  write_stan_json(list(N = N), file = temp_file)
  json_output <- readLines(temp_file)
  expect_known_output(cat(json_output, sep = "\n"),
                      file = test_path("answers", "json-lists.json"))
})

test_that("test JSON errors", {
  skip_on_cran()
  temp_file <- tempfile()

  N <- c(1.0, 2.0, 3, 4)
  expect_error(
    write_stan_json(list(N = N), file = c(1,2)),
    "The supplied filename is invalid!"
  )
  expect_error(
    write_stan_json(list(N = N), file = ""),
    "The supplied filename is invalid!"
  )

  I <- 2; J <- 3;
  N <- lapply(1:I, function(i) {
    if (i==1)
      rep(26, J)
    else
      rep(26, J-1)
  })
  expect_error(
    write_stan_json(list(N = N), file = "abc.txt"),
    "All matrices/vectors in the list must be the same size!"
  )
  N <- lapply(1:I, function(i) {
    if (i==1)
      rep(26, J)
    else
      J
  })
  expect_error(
    write_stan_json(list(N = N), file = "abc.txt"),
    "All matrices/vectors in the list must be the same size!"
  )
  I <- 2; J <- 3;
  N <- lapply(1:I, function(i) {
    if (i==1)
      rep(26, J)
    else
      NULL
  })
  expect_error(
    write_stan_json(list(N = N), file = "abc.txt"),
    "All matrices/vectors in the list must be the same size!"
  )

  expect_error(
    write_stan_json(list(N = "STRING"), file = "abc.txt"),
    "Variable 'N' is of invalid type"
  )
})
