context("json")

test_that("JSON output unboxing works", {
  temp_file <- tempfile()
  N <- 10
  write_stan_json(list(N = N), file = temp_file)
  json_output <- readLines(temp_file)
  expect_known_output(cat(json_output, sep = "\n"),
                      file = test_path("answers", "json-unboxing.json"))
})

test_that("JSON output for boolean is correct", {
  temp_file <- tempfile()
  N <- c(TRUE, FALSE, TRUE)
  write_stan_json(list(N = N), file = temp_file)
  json_output <- readLines(temp_file)
  expect_known_output(cat(json_output, sep = "\n"),
                      file = test_path("answers", "json-boolean.json"))
})

test_that("JSON output for factors is correct", {
  temp_file <- tempfile()
  N <- factor(c(0,1,2,2,1,0), labels = c("c1", "c2", "c3"))
  write_stan_json(list(N = N), file = temp_file)
  json_output <- readLines(temp_file)
  expect_known_output(cat(json_output, sep = "\n"),
                      file = test_path("answers", "json-factor.json"))
})

test_that("JSON output for integer vector is correct", {
  temp_file <- tempfile()
  N <- c(1.0, 2.0, 3, 4)

  write_stan_json(list(N = N), file = temp_file)
  json_output <- readLines(temp_file)
  expect_known_output(cat(json_output, sep = "\n"),
                      file = test_path("answers", "json-integer.json"))
})

test_that("JSON output for data frame and matrix is correct", {
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

test_that("JSON output for list of vectors is correct", {
  temp_file <- tempfile()
  N <- list(c(1,2,3), c(4,5,6))

  write_stan_json(list(N = N), file = temp_file)
  json_output <- readLines(temp_file)
  expect_known_output(cat(json_output, sep = "\n"),
                      file = test_path("answers", "json-vector-lists.json"))
})

test_that("JSON output for list of matrices is correct", {
  temp_file <- tempfile()
  matrices <- list(
    matrix(1:4, nrow = 2, byrow = FALSE),
    matrix(5:8, nrow = 2, byrow = TRUE)
  )
  write_stan_json(list(M = matrices), file = temp_file)
  json_output <- readLines(temp_file)
  expect_known_output(cat(json_output, sep = "\n"),
                      file = test_path("answers", "json-matrix-lists.json"))
})

test_that("write_stan_json() throws correct errors", {
  skip_on_cran()
  temp_file <- tempfile()

  expect_error(
    write_stan_json(list(N = c(1.0, 2.0, 3, 4)), file = c(1,2)),
    "The supplied filename is invalid!"
  )
  expect_error(
    write_stan_json(list(N = N), file = ""),
    "The supplied filename is invalid!"
  )

  expect_error(
    write_stan_json(list(N = list(c(26, 26, 26), c(26, 26))), file = "abc.txt"),
    "All matrices/vectors in list 'N' must be the same size!"
  )
  expect_error(
    write_stan_json(list(N = list(c(26, 26, 26), 3)), file = "abc.txt"),
    "All matrices/vectors in list 'N' must be the same size!"
  )
  expect_error(
    write_stan_json(list(N = list(c(26, 26, 26), NULL)), file = "abc.txt"),
    "All matrices/vectors in list 'N' must be the same size!"
  )
  expect_error(
    write_stan_json(list(N = list(c(26, 26, 26), matrix(c(26, 26, 26), ncol = 1))), file = "abc.txt"),
    "All matrices/vectors in list 'N' must be the same size!"
  )
  expect_error(
    write_stan_json(list(N = list(matrix(1:8, ncol = 2), matrix(1:9, ncol = 3))), file = "abc.txt"),
    "All matrices/vectors in list 'N' must be the same size!"
  )

  expect_error(
    write_stan_json(list(N = list("abc", "def")), file = "abc.txt"),
    "All elements in list 'N' must be numeric!"
  )

  expect_error(
    write_stan_json(list(N = "STRING"), file = "abc.txt"),
    "Variable 'N' is of invalid type"
  )
})
