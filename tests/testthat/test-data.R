skip_on_cran()

set_cmdstan_path()

test_that("empty data list converted to NULL", {
  stan_file <- write_stan_file(
    "
  parameters {
    real y;
  }
  model {
    y ~ std_normal();
  }
  "
  )
  expect_null(process_data(list()))
  mod <- mock_cmdstan_model(stan_file)
  expect_null(process_data(list(), model_variables = mod$variables()))
})

test_that("process_data works for inputs of length one", {
  data <- list(val = 5)
  stan_file <- write_stan_file(
    "
  data {
    real val;
  }
  "
  )
  mod <- mock_cmdstan_model(stan_file)
  expect_equal(
    jsonlite::read_json(process_data(data, model_variables = mod$variables())),
    list(val = 5)
  )
  stan_file <- write_stan_file(
    "
  data {
    int val;
  }
  "
  )
  mod <- mock_cmdstan_model(stan_file)
  expect_equal(
    jsonlite::read_json(process_data(data, model_variables = mod$variables())),
    list(val = 5)
  )
  stan_file <- write_stan_file(
    "
  data {
    vector[1] val;
  }
  "
  )
  mod <- mock_cmdstan_model(stan_file)
  expect_equal(
    jsonlite::read_json(process_data(data, model_variables = mod$variables())),
    list(val = list(5))
  )
})

test_that("process_data errors on NULL data variables", {
  stan_file <- write_stan_file(
    "
  data {
    int N;
  }
  "
  )
  mod <- mock_cmdstan_model(stan_file)
  expect_error(
    process_data(list(N = NULL), model_variables = mod$variables()),
    "Variable 'N' is NULL"
  )
})

test_that("process_data() errors on missing variables", {
  stan_file <- write_stan_file(
    "
  data {
    real val1;
    real val2;
  }
  "
  )
  mod <- mock_cmdstan_model(stan_file)
  expect_error(
    process_data(data = list(val1 = 5), model_variables = mod$variables()),
    "Missing input data for the following data variables: val2."
  )
  expect_error(
    process_data(data = list(val = 1), model_variables = mod$variables()),
    "Missing input data for the following data variables: val1, val2."
  )
  stan_file_no_data <- write_stan_file(
    "
  transformed data {
    real val1 = 1;
    real val2 = 2;
  }
  "
  )
  mod <- mock_cmdstan_model(stan_file_no_data)
  v <- process_data(data = list(val1 = 5), model_variables = mod$variables())
  expect_type(v, "character")
})

test_that("process_data() correctly casts integers and floating point numbers", {
  stan_file <- write_stan_file(
    "
  data {
    int a;
    real b;
  }
  "
  )
  mod <- mock_cmdstan_model(stan_file)
  test_file <- process_data(
    list(a = 1, b = 2),
    model_variables = mod$variables()
  )
  expect_match(
    "  \"a\": 1,",
    readLines(test_file)[2],
    fixed = TRUE
  )
  expect_match(
    "  \"b\": 2.0",
    readLines(test_file)[3],
    fixed = TRUE
  )
  test_file <- process_data(
    list(a = 1L, b = 1774000000),
    model_variables = mod$variables()
  )
  expect_match(
    "  \"a\": 1,",
    readLines(test_file)[2],
    fixed = TRUE
  )
  expect_match(
    "  \"b\": 1774000000.0",
    readLines(test_file)[3],
    fixed = TRUE
  )

  stan_file <- write_stan_file(
    "
  data {
    array[3,3] int<lower=0> k;
  }
  "
  )
  mod <- mock_cmdstan_model(stan_file)
  test_file <- process_data(
    list(
      k = matrix(
        c(18, 18, 16, 13, 9, 6, 4, 4, 4),
        nrow = 3,
        ncol = 3,
        byrow = TRUE
      )
    ),
    model_variables = mod$variables()
  )
  expect_match(
    "  \"k\": [",
    readLines(test_file)[2],
    fixed = TRUE
  )
  expect_match(
    "    [18, 18, 16],",
    readLines(test_file)[3],
    fixed = TRUE
  )
})

test_that("process_data warns on int coercion", {
  stan_file <- write_stan_file(
    "
  data {
    int a;
    real b;
  }
  "
  )
  mod <- mock_cmdstan_model(stan_file)
  expect_warning(
    process_data(list(a = 1.1, b = 2.1), model_variables = mod$variables()),
    "A non-integer value was supplied for 'a'! It will be truncated to an integer."
  )

  stan_file <- write_stan_file(
    "
  data {
    array[3] int a;
  }
  "
  )
  mod <- mock_cmdstan_model(stan_file)
  expect_warning(
    process_data(list(a = c(1, 2.1, 3)), model_variables = mod$variables()),
    "A non-integer value was supplied for 'a'! It will be truncated to an integer."
  )

  expect_no_warning(
    process_data(list(a = c(1, 2, 3)), model_variables = mod$variables())
  )
  expect_no_warning(
    process_data(
      list(a = factor(c("a", "b", "c"))),
      model_variables = mod$variables()
    )
  )
})

test_that("process_data accepts lists of matrices/vectors for int variables", {
  stan_file <- write_stan_file(
    "
  data {
    array[4,3,2] int x;
  }
  "
  )
  mod <- mock_cmdstan_model(stan_file)
  model_variables <- mod$variables()

  a <- matrix(1:6, nrow = 3, ncol = 2)
  arr <- array(dim = c(4, 3, 2))
  for (i in 1:4) {
    arr[i, , ] <- a
  }

  from_array <- readLines(process_data(
    list(x = arr),
    model_variables = model_variables
  ))
  from_int_list <- readLines(process_data(
    list(x = list(a, a, a, a)),
    model_variables = model_variables
  ))
  expect_identical(from_int_list, from_array)

  # a list of doubles must give the same result as a list of integers (#817)
  storage.mode(a) <- "double"
  from_dbl_list <- readLines(process_data(
    list(x = list(a, a, a, a)),
    model_variables = model_variables
  ))
  expect_identical(from_dbl_list, from_array)

  # values are written as integers, not as decimals
  expect_false(any(grepl(".", from_dbl_list, fixed = TRUE)))

  stan_file <- write_stan_file(
    "
  data {
    array[2,3] int x;
  }
  "
  )
  mod <- mock_cmdstan_model(stan_file)
  test_file <- process_data(
    list(x = list(c(1, 2, 3), c(4, 5, 6))),
    model_variables = mod$variables()
  )
  expect_equal(
    jsonlite::read_json(test_file, simplifyVector = TRUE),
    list(x = matrix(1:6, nrow = 2, ncol = 3, byrow = TRUE))
  )
})

test_that("process_data accepts data frames for int variables", {
  stan_file <- write_stan_file(
    "
  data {
    array[2,2] int x;
  }
  "
  )
  mod <- mock_cmdstan_model(stan_file)
  model_variables <- mod$variables()

  df <- data.frame(a = c(1, 2), b = c(3, 4))
  from_df <- readLines(process_data(
    list(x = df),
    model_variables = model_variables
  ))
  from_matrix <- readLines(process_data(
    list(x = data.matrix(df)),
    model_variables = model_variables
  ))
  expect_identical(from_df, from_matrix)
  expect_false(any(grepl(".", from_df, fixed = TRUE)))
})

test_that("process_data errors on invalid types", {
  stan_file <- write_stan_file(
    "
  data {
    array[2,2] int x;
  }
  "
  )
  mod <- mock_cmdstan_model(stan_file)
  model_variables <- mod$variables()

  expect_error(
    process_data(
      list(x = data.frame(a = c(1, 2), b = c("v", "w"))),
      model_variables = model_variables
    ),
    "Variable 'x' has columns of invalid type: b."
  )
  expect_error(
    process_data(list(x = c("v", "w")), model_variables = model_variables),
    "Variable 'x' is of invalid type."
  )
  # NAs inside a list are reported as NAs rather than as a coercion failure
  expect_error(
    process_data(
      list(x = list(c(1, NA), c(3, 4))),
      model_variables = model_variables
    ),
    "Variable 'x' has NA values"
  )
})

test_that("process_data errors on a factor for a non-int variable", {
  stan_file <- write_stan_file(
    "
  data {
    int a;
    array[2] int b;
    real c;
    vector[2] d;
  }
  "
  )
  mod <- mock_cmdstan_model(stan_file)
  model_variables <- mod$variables()
  data <- list(a = 1L, b = c(1L, 2L), c = 2.5, d = c(1, 2))

  expect_error(
    process_data(
      modifyList(data, list(c = factor("x"))),
      model_variables = model_variables
    ),
    "A factor was supplied for 'c', which is declared as 'real'."
  )
  # vectors and matrices are also reported as 'real'
  expect_error(
    process_data(
      modifyList(data, list(d = factor(c("x", "y")))),
      model_variables = model_variables
    ),
    "A factor was supplied for 'd', which is declared as 'real'."
  )

  # a factor column of a data frame is caught too, before data.matrix()
  # converts it to codes
  stan_file <- write_stan_file(
    "
  data {
    matrix[2,1] x;
  }
  "
  )
  mod_matrix <- mock_cmdstan_model(stan_file)
  expect_error(
    process_data(
      list(x = data.frame(a = factor(c("b", "a")))),
      model_variables = mod_matrix$variables()
    ),
    "A factor was supplied for 'x', which is declared as 'real'."
  )

  # factors are still allowed for int variables
  expect_no_error(
    process_data(
      modifyList(data, list(a = factor("x"))),
      model_variables = model_variables
    )
  )
  expect_no_error(
    process_data(
      modifyList(data, list(b = factor(c("x", "y")))),
      model_variables = model_variables
    )
  )
})

test_that("factors work for length-1 arrays", {
  # array() drops the factor class, so the length-1 reshaping used to leave a
  # character array behind for these
  stan_file <- write_stan_file(
    "
  data {
    array[1] int a;
    array[1] real b;
  }
  "
  )
  mod <- mock_cmdstan_model(stan_file)
  model_variables <- mod$variables()
  data <- list(a = 1L, b = 2.5)

  # read without simplification, which would make [1] indistinguishable from 1
  test_file <- process_data(
    modifyList(data, list(a = factor("x"))),
    model_variables = model_variables
  )
  expect_equal(jsonlite::read_json(test_file)$a, list(1L))

  expect_error(
    process_data(
      modifyList(data, list(b = factor("x"))),
      model_variables = model_variables
    ),
    "A factor was supplied for 'b', which is declared as 'real'."
  )

  # the length-1 reshaping still works for non-factors
  test_file <- process_data(
    modifyList(data, list(a = 5)),
    model_variables = model_variables
  )
  expect_equal(jsonlite::read_json(test_file)$a, list(5L))
})

test_that("write_stan_json() writes a tuple from a list using variables", {
  variables <- variables_stan_file(testing_stan_file("tuple_complex"))$data
  data <- list(
    d_pair = list(3, c(1, 2)),
    d_arr = list(list(1, 2), list(3, 4)),
    d_zv = c(5 + 6i, 7 + 8i)
  )
  file <- tempfile(fileext = ".json")
  write_stan_json(data, file, variables = variables, always_decimal = TRUE)
  written <- jsonlite::fromJSON(file, simplifyVector = FALSE)

  expect_equal(written$d_pair, list(`1` = 3L, `2` = list(1, 2)))
  expect_length(written$d_arr, 2)
  expect_equal(written$d_arr[[1]], list(`1` = 1, `2` = 2))
  expect_equal(written$d_arr[[2]], list(`1` = 3, `2` = 4))
  expect_equal(written$d_zv, list(list(5, 6), list(7, 8)))
})

test_that("write_stan_json() writes a 2-D array of tuples from a list-array", {
  element <- list(type = "real", dimensions = 0L)
  declaration <- list(x = list(type = list(element, element), dimensions = 2L))
  x <- array(
    list(list(1, 1), list(2, 1), list(1, 2), list(2, 2)),
    dim = c(2, 2)
  )

  file <- tempfile(fileext = ".json")
  write_stan_json(list(x = x), file, variables = declaration)
  written <- jsonlite::fromJSON(file, simplifyVector = FALSE)

  expect_equal(written$x[[2]][[1]], list(`1` = 2, `2` = 1))
})

test_that("write_stan_json() writes an empty array of tuples", {
  element <- list(type = "real", dimensions = 0L)
  declaration <- list(t = list(type = list(element, element), dimensions = 2L))

  file <- tempfile(fileext = ".json")
  write_stan_json(
    list(t = array(list(), dim = c(2, 0))),
    file,
    variables = declaration
  )
  written <- jsonlite::fromJSON(file, simplifyVector = FALSE)

  expect_equal(written$t, list(list(), list()))
})

test_that("write_stan_json() writes a list of complex vectors", {
  file <- tempfile(fileext = ".json")
  write_stan_json(list(z = list(c(1 + 2i, 3 + 4i), c(5 + 6i, 7 + 8i))), file)
  written <- jsonlite::fromJSON(file, simplifyVector = FALSE)

  expect_equal(written$z[[1]], list(list(1, 2), list(3, 4)))
  expect_equal(written$z[[2]], list(list(5, 6), list(7, 8)))
})

test_that("write_stan_json() writes complex scalars, vectors and matrices", {
  data <- list(
    zs = 1 + 2i,
    zvec = c(1 + 2i, 3 + 4i),
    zmat = matrix(c(1 + 10i, 2 + 20i, 3 + 30i, 4 + 40i), 2, 2)
  )
  file <- tempfile(fileext = ".json")
  write_stan_json(data, file)
  written <- jsonlite::fromJSON(file, simplifyVector = FALSE)

  expect_equal(written$zs, list(1, 2))
  expect_equal(written$zvec, list(list(1, 2), list(3, 4)))
  expect_equal(written$zmat[[1]][[1]], list(1, 10))
  expect_equal(written$zmat[[2]][[2]], list(4, 40))
})

test_that("write_stan_json() writes a length-1 complex_vector as an array", {
  declaration <- list(zvec = list(type = "complex", dimensions = 1L))
  file <- tempfile(fileext = ".json")
  write_stan_json(list(zvec = 1 + 2i), file, variables = declaration)
  written <- jsonlite::fromJSON(file, simplifyVector = FALSE)

  expect_equal(written$zvec, list(list(1, 2)))
})

test_that("write_stan_json() writes an int tuple element without a decimal", {
  declaration <- list(
    t = list(type = list(list(type = "int", dimensions = 0L)), dimensions = 0L)
  )
  file <- tempfile(fileext = ".json")
  write_stan_json(
    list(t = list(3)),
    file,
    variables = declaration,
    always_decimal = TRUE
  )
  expect_equal(readLines(file)[2], "  \"t\": {")
  expect_equal(trimws(readLines(file)[3]), "\"1\": 3")
})

test_that("write_stan_json() converts a list to an array without variables", {
  file <- tempfile(fileext = ".json")
  write_stan_json(list(x = list(1:3, 4:6)), file)
  expect_equal(
    jsonlite::read_json(file, simplifyVector = TRUE)$x,
    matrix(c(1:3, 4:6), nrow = 2, byrow = TRUE)
  )
})

test_that("write_stan_json() errors for a tuple given the wrong shape", {
  element <- list(type = "real", dimensions = 0L)
  declaration <- list(t = list(type = list(element, element), dimensions = 0L))
  file <- tempfile(fileext = ".json")
  expect_error(
    write_stan_json(list(t = 5), file, variables = declaration),
    "is declared as a tuple and must be a list"
  )
  expect_error(
    write_stan_json(list(t = list(1, 2, 3)), file, variables = declaration),
    "is a tuple with 2 elements, but 3 were supplied"
  )
})

test_that("Floating-point differences do not cause truncation towards 0", {
  stan_file <- write_stan_file(
    "
  data {
    int a;
    real b;
  }
  "
  )
  mod <- mock_cmdstan_model(stan_file)
  a <- 10 * (3 - 2.7)
  expect_false(is.integer(a))
  test_file <- process_data(
    list(a = a, b = 2.0),
    model_variables = mod$variables()
  )
  expect_match(
    "  \"a\": 3,",
    readLines(test_file)[2],
    fixed = TRUE
  )
})
