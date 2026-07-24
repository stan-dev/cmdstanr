expect_json_snapshot <- function(path, name = basename(path)) {
  expect_snapshot_file(path, name = name, cran = TRUE)
}

test_that("JSON output unboxing works", {
  temp_file <- tempfile()
  N <- 10
  write_stan_json(list(N = N), file = temp_file)
  expect_json_snapshot(temp_file, "json-unboxing.json")
})

test_that("JSON output for boolean is correct", {
  temp_file <- tempfile()
  N <- c(TRUE, FALSE, TRUE)
  write_stan_json(list(N = N), file = temp_file)
  expect_json_snapshot(temp_file, "json-boolean.json")
})

test_that("JSON output for factors is correct", {
  temp_file <- tempfile()
  N <- factor(c(0,1,2,2,1,0), labels = c("c1", "c2", "c3"))
  write_stan_json(list(N = N), file = temp_file)
  expect_json_snapshot(temp_file, "json-factor.json")
})

test_that("JSON output for integer vector is correct", {
  temp_file <- tempfile()
  N <- c(1.0, 2.0, 3, 4)

  write_stan_json(list(N = N), file = temp_file)
  expect_json_snapshot(temp_file, "json-integer.json")
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
  expect_identical(readLines(temp_file_df), readLines(temp_file_mat))

  expect_json_snapshot(temp_file_df, "json-df-matrix.json")
})

test_that("JSON output for list of vectors is correct", {
  temp_file <- tempfile()
  N <- list(c(1,2,3), c(4,5,6))

  write_stan_json(list(N = N), file = temp_file)
  expect_json_snapshot(temp_file, "json-vector-lists.json")
})

test_that("JSON output for list of matrices is correct", {
  temp_file <- tempfile()
  matrices <- list(
    matrix(1:4, nrow = 2, byrow = FALSE),
    matrix(5:8, nrow = 2, byrow = TRUE)
  )
  write_stan_json(list(M = matrices), file = temp_file)
  expect_json_snapshot(temp_file, "json-matrix-lists.json")
})

test_that("JSON output for table is correct", {
  temp_file <- tempfile()
  f <- factor(rep(1:4, each = 5))

  write_stan_json(list(x = table(f)), file = temp_file)
  expect_json_snapshot(temp_file, "json-table-vector.json")

  write_stan_json(list(x = table(f, f)), file = temp_file)
  expect_json_snapshot(temp_file, "json-table-matrix.json")

  write_stan_json(list(x = table(f, f, f)), file = temp_file)
  expect_json_snapshot(temp_file, "json-table-array.json")
})

test_that("write_stan_json errors if NAs", {
  expect_error(
    write_stan_json(list(y = 1, N = NA), tempfile()),
    "Variable 'N' has NA values"
  )
  expect_error(
    write_stan_json(list(x = matrix(NA, 1, 1)), tempfile()),
    "Variable 'x' has NA values"
  )
  expect_error(
    write_stan_json(list(x = list(1, NA)), tempfile()),
    "Variable 'x' has NA values"
  )
})

test_that("write_stan_json errors if NULL variables", {
  expect_error(
    write_stan_json(list(N = NULL), tempfile()),
    "Variable 'N' is NULL"
  )
})

test_that("write_stan_json() errors if data is not a list", {
  expect_error(
    write_stan_json(1:10),
    "'data' must be a list"
  )
})

test_that("write_stan_json() errors if bad filename", {
  temp_file <- tempfile()

  expect_error(
    write_stan_json(list(N = 10), file = c(1,2)),
    "The supplied filename is invalid!"
  )
  expect_error(
    write_stan_json(list(N = 10), file = ""),
    "The supplied filename is invalid!"
  )
})

test_that("write_stan_json() errors if vectors/matrices in same list are different sizes", {
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
})

test_that("a list contributes one leading dimension", {
  # e.g. `array[K,L] vector[J] v` as a list of K matrices with dimensions LxJ
  K <- 2; L <- 3; J <- 4
  arr <- array(1:(K * L * J), dim = c(K, L, J))
  lst <- lapply(seq_len(K), function(k) arr[k, , ])

  temp_file_list <- tempfile()
  temp_file_arr <- tempfile()
  write_stan_json(list(v = lst), temp_file_list)
  write_stan_json(list(v = arr), temp_file_arr)
  expect_identical(readLines(temp_file_list), readLines(temp_file_arr))

  # nested lists are not supported
  expect_error(
    write_stan_json(list(v = list(list(1:4, 5:8), list(9:12, 13:16))), tempfile()),
    "All elements in list 'v' must be numeric or logical!"
  )
})

test_that("logical elements of a list are converted to integers", {
  temp_file_list <- tempfile()
  temp_file_arr <- tempfile()
  matrices <- list(
    matrix(c(TRUE, FALSE, TRUE, FALSE), nrow = 2),
    matrix(c(FALSE, TRUE, FALSE, TRUE), nrow = 2)
  )
  write_stan_json(list(x = matrices), temp_file_list)
  write_stan_json(list(x = list_to_array(matrices)), temp_file_arr)

  # 0/1 rather than JSON true/false, matching a plain logical variable
  expect_identical(readLines(temp_file_list), readLines(temp_file_arr))
  expect_false(any(grepl("true|false", readLines(temp_file_list))))

  # factors are still not allowed as list elements
  expect_error(
    write_stan_json(list(x = list(factor("a"), factor("b"))), tempfile()),
    "All elements in list 'x' must be numeric or logical!"
  )
})

test_that("factors are written as level indices", {
  temp_file <- tempfile()
  read_x <- function(file) jsonlite::read_json(file, simplifyVector = TRUE)$x

  # the level indices are written, not the values themselves
  write_stan_json(list(x = factor(c(10, 9, 8))), temp_file)
  expect_equal(read_x(temp_file), c(3L, 2L, 1L))

  # the order of the levels determines the indices
  write_stan_json(list(x = factor(c("foo", "bar"))), temp_file)
  expect_equal(read_x(temp_file), c(2L, 1L))

  write_stan_json(list(x = factor(c("foo", "bar"), levels = c("foo", "bar"))), temp_file)
  expect_equal(read_x(temp_file), c(1L, 2L))

  # an unused level shifts the indices of the levels after it
  write_stan_json(list(x = factor(c("b", "c"), levels = c("a", "b", "c"))), temp_file)
  expect_equal(read_x(temp_file), c(2L, 3L))

  # factor columns of a data frame are converted the same way
  write_stan_json(list(x = data.frame(a = factor(c(10, 9, 8)))), temp_file)
  expect_equal(read_x(temp_file), matrix(c(3L, 2L, 1L), ncol = 1))
})

test_that("write_stan_json() errors if invalid types", {
  expect_error(
    write_stan_json(list(N = list("abc", "def")), file = "abc.txt"),
    "All elements in list 'N' must be numeric or logical!"
  )

  expect_error(
    write_stan_json(list(N = "STRING"), file = "abc.txt"),
    "Variable 'N' is of invalid type"
  )
})

test_that("write_stan_json() errors if data frame has columns of invalid type", {
  # data.matrix() would silently coerce these instead of erroring
  expect_error(
    write_stan_json(list(N = data.frame(a = 1:2, b = c("x", "y"))), tempfile()),
    "Variable 'N' has columns of invalid type: b."
  )
  expect_error(
    write_stan_json(list(N = data.frame(a = as.Date(c("2020-01-01", "2020-01-02")))), tempfile()),
    "Variable 'N' has columns of invalid type: a."
  )
  expect_error(
    write_stan_json(list(N = data.frame(a = as.POSIXct("2020-01-01", tz = "UTC"))), tempfile()),
    "Variable 'N' has columns of invalid type: a."
  )
  expect_error(
    write_stan_json(list(N = data.frame(a = c(1 + 2i, 3 + 4i))), tempfile()),
    "Variable 'N' has columns of invalid type: a."
  )

  # all invalid columns are reported, not just the first
  expect_error(
    write_stan_json(list(N = data.frame(a = 1:2, b = c("x", "y"), c = c("v", "w"))), tempfile()),
    "Variable 'N' has columns of invalid type: b, c."
  )

  # numeric, integer, logical and factor columns are still allowed
  expect_no_error(
    write_stan_json(
      list(N = data.frame(a = c(1.5, 2.5), b = 1:2, c = c(TRUE, FALSE), d = factor(c("x", "y")))),
      tempfile()
    )
  )
})

test_that("write_stan_json() errors if bad names", {
  expect_error(
    write_stan_json(list(x = 1, y = 2, x = 3), file = tempfile()),
    "Duplicate names not allowed in 'data'"
  )

  expect_error(
    write_stan_json(list(1, 2), tempfile()),
    "All elements in 'data' list must have names"
  )

  expect_error(
    write_stan_json(list(a = 1, 2), tempfile()),
    "All elements in 'data' list must have names"
  )
})

test_that("write_stan_json() works with always_decimal = TRUE", {
  test_file <- tempfile(fileext = ".json")
  write_stan_json(list(a = 1L, b = 2), test_file, always_decimal = FALSE)
  expect_match(
    "  \"a\": 1,",
    readLines(test_file)[2],
    fixed = TRUE
  )
  expect_match(
    "  \"b\": 2",
    readLines(test_file)[3],
    fixed = TRUE
  )
  write_stan_json(list(a = 1L, b = 2), test_file, always_decimal = TRUE)
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
})
