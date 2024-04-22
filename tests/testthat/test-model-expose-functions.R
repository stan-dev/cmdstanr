context("model-expose-functions")

set_cmdstan_path()

function_decl <- "
functions {
  int rtn_int(int x) { return x; }
  real rtn_real(real x) { return x; }
  vector rtn_vec(vector x) { return x; }
  row_vector rtn_rowvec(row_vector x) { return x; }
  matrix rtn_matrix(matrix x) { return x; }

  array[] int rtn_int_array(array[] int x) { return x; }
  array[] real rtn_real_array(array[] real x) { return x; }
  array[] vector rtn_vec_array(array[] vector x) { return x; }
  array[] row_vector rtn_rowvec_array(array[] row_vector x) { return x; }
  array[] matrix rtn_matrix_array(array[] matrix x) { return x; }

  tuple(int, int)  rtn_tuple_int(tuple(int, int) x) { return x; }
  tuple(real, real)  rtn_tuple_real(tuple(real, real) x) { return x; }
  tuple(vector, vector)  rtn_tuple_vec(tuple(vector, vector) x) { return x; }
  tuple(row_vector, row_vector)  rtn_tuple_rowvec(tuple(row_vector, row_vector) x) { return x; }
  tuple(matrix, matrix)  rtn_tuple_matrix(tuple(matrix, matrix) x) { return x; }

  tuple(array[] int, array[] int)  rtn_tuple_int_array(tuple(array[] int, array[] int) x) { return x; }
  tuple(array[] real, array[] real)  rtn_tuple_real_array(tuple(array[] real, array[] real) x) { return x; }
  tuple(array[] vector, array[] vector)  rtn_tuple_vec_array(tuple(array[] vector, array[] vector) x) { return x; }
  tuple(array[] row_vector, array[] row_vector)  rtn_tuple_rowvec_array(tuple(array[] row_vector, array[] row_vector) x) { return x; }
  tuple(array[] matrix, array[] matrix)  rtn_tuple_matrix_array(tuple(array[] matrix, array[] matrix) x) { return x; }

  tuple(int, tuple(int, int))  rtn_nest_tuple_int(tuple(int, tuple(int, int)) x) { return x; }
  tuple(int, tuple(real, real))  rtn_nest_tuple_real(tuple(int, tuple(real, real)) x) { return x; }
  tuple(int, tuple(vector, vector))  rtn_nest_tuple_vec(tuple(int, tuple(vector, vector)) x) { return x; }
  tuple(int, tuple(row_vector, row_vector))  rtn_nest_tuple_rowvec(tuple(int, tuple(row_vector, row_vector)) x) { return x; }
  tuple(int, tuple(matrix, matrix))  rtn_nest_tuple_matrix(tuple(int, tuple(matrix, matrix)) x) { return x; }

  tuple(int, tuple(array[] int, array[] int))  rtn_nest_tuple_int_array(tuple(int, tuple(array[] int, array[] int)) x) { return x; }
  tuple(int, tuple(array[] real, array[] real))  rtn_nest_tuple_real_array(tuple(int, tuple(array[] real, array[] real)) x) { return x; }
  tuple(int, tuple(array[] vector, array[] vector))  rtn_nest_tuple_vec_array(tuple(int, tuple(array[] vector, array[] vector)) x) { return x; }
  tuple(int, tuple(array[] row_vector, array[] row_vector))  rtn_nest_tuple_rowvec_array(tuple(int, tuple(array[] row_vector, array[] row_vector)) x) { return x; }
  tuple(int, tuple(array[] matrix, array[] matrix))  rtn_nest_tuple_matrix_array(tuple(int, tuple(array[] matrix, array[] matrix)) x) { return x; }

  complex rtn_complex(complex x) { return x; }
  complex_vector rtn_complex_vec(complex_vector x) { return x; }
  complex_row_vector rtn_complex_rowvec(complex_row_vector x) { return x; }
  complex_matrix rtn_complex_matrix(complex_matrix x) { return x; }

  array[] complex rtn_complex_array(array[] complex x) { return x; }
  array[] complex_vector rtn_complex_vec_array(array[] complex_vector x) { return x; }
  array[] complex_row_vector rtn_complex_rowvec_array(array[] complex_row_vector x) { return x; }
  array[] complex_matrix rtn_complex_matrix_array(array[] complex_matrix x) { return x; }

  tuple(complex, complex)  rtn_tuple_complex(tuple(complex, complex) x) { return x; }
  tuple(complex_vector, complex_vector)  rtn_tuple_complex_vec(tuple(complex_vector, complex_vector) x) { return x; }
  tuple(complex_row_vector, complex_row_vector)  rtn_tuple_complex_rowvec(tuple(complex_row_vector, complex_row_vector) x) { return x; }
  tuple(complex_matrix, complex_matrix)  rtn_tuple_complex_matrix(tuple(complex_matrix, complex_matrix) x) { return x; }

  tuple(array[] complex, array[] complex)  rtn_tuple_complex_array(tuple(array[] complex, array[] complex) x) { return x; }
  tuple(array[] complex_vector, array[] complex_vector)  rtn_tuple_complex_vec_array(tuple(array[] complex_vector, array[] complex_vector) x) { return x; }
  tuple(array[] complex_row_vector, array[] complex_row_vector)  rtn_tuple_complex_rowvec_array(tuple(array[] complex_row_vector, array[] complex_row_vector) x) { return x; }
  tuple(array[] complex_matrix, array[] complex_matrix)  rtn_tuple_complex_matrix_array(tuple(array[] complex_matrix, array[] complex_matrix) x) { return x; }

  tuple(int, tuple(complex, complex))  rtn_nest_tuple_complex(tuple(int, tuple(complex, complex)) x) { return x; }
  tuple(int, tuple(complex_vector, complex_vector))  rtn_nest_tuple_complex_vec(tuple(int, tuple(complex_vector, complex_vector)) x) { return x; }
  tuple(int, tuple(complex_row_vector, complex_row_vector))  rtn_nest_tuple_complex_rowvec(tuple(int, tuple(complex_row_vector, complex_row_vector)) x) { return x; }
  tuple(int, tuple(complex_matrix, complex_matrix))  rtn_nest_tuple_complex_matrix(tuple(int, tuple(complex_matrix, complex_matrix)) x) { return x; }

  tuple(int, tuple(array[] complex, array[] complex))  rtn_nest_tuple_complex_array(tuple(int, tuple(array[] complex, array[] complex)) x) { return x; }
  tuple(int, tuple(array[] complex_vector, array[] complex_vector))  rtn_nest_tuple_complex_vec_array(tuple(int, tuple(array[] complex_vector, array[] complex_vector)) x) { return x; }
  tuple(int, tuple(array[] complex_row_vector, array[] complex_row_vector))  rtn_nest_tuple_complex_rowvec_array(tuple(int, tuple(array[] complex_row_vector, array[] complex_row_vector)) x) { return x; }
  tuple(int, tuple(array[] complex_matrix, array[] complex_matrix))  rtn_nest_tuple_complex_matrix_array(tuple(int, tuple(array[] complex_matrix, array[] complex_matrix)) x) { return x; }
}"
stan_prog <- paste(function_decl,
                  paste(readLines(testing_stan_file("bernoulli")),
                        collapse = "\n"),
                  collapse = "\n")
model <- write_stan_file(stan_prog)
data_list <- testing_data("bernoulli")
mod <- cmdstan_model(model, force_recompile = TRUE)
fit <- mod$sample(data = data_list)


test_that("Functions can be exposed in model object", {
  skip_if(os_is_wsl())
  expect_no_error(mod$expose_functions(verbose = TRUE))
})


test_that("Functions handle types correctly", {
  skip_if(os_is_wsl())

  ### Scalar

  expect_equal(mod$functions$rtn_int(10), 10)
  expect_equal(mod$functions$rtn_real(1.67), 1.67)

  ### Container

  vec <- c(1.2,234,0.3,-0.4)
  rowvec <- t(vec)
  matrix <- matrix(c(2.11, -6.35, 4.87, -0.9871), nrow = 2, ncol = 2)

  expect_equal(mod$functions$rtn_vec(vec), vec)
  expect_equal(mod$functions$rtn_rowvec(vec), t(vec))
  expect_equal(mod$functions$rtn_matrix(matrix), matrix)
  expect_equal(mod$functions$rtn_int_array(1:5), 1:5)
  expect_equal(mod$functions$rtn_real_array(vec), vec)

  ### Array of Container

  vec_array <- list(vec, vec * 2, vec + 0.1)
  rowvec_array <- list(rowvec, rowvec * 2, rowvec + 0.1)
  matrix_array <- list(matrix, matrix * 2, matrix + 0.1)

  expect_equal(mod$functions$rtn_vec_array(vec_array), vec_array)
  expect_equal(mod$functions$rtn_rowvec_array(rowvec_array), rowvec_array)
  expect_equal(mod$functions$rtn_matrix_array(matrix_array), matrix_array)

  ### Tuple of Scalar

  tuple_int <- list(10, 35)
  tuple_dbl <- list(31.87, -19.09)
  expect_equal(mod$functions$rtn_tuple_int(tuple_int), tuple_int)
  expect_equal(mod$functions$rtn_tuple_real(tuple_dbl), tuple_dbl)

  ### Tuple of Container

  tuple_vec <- list(vec, vec * 12)
  tuple_rowvec <- list(rowvec, rowvec * 0.5)
  tuple_matrix <- list(matrix, matrix * 0.23)
  tuple_int_array <- list(1:10, -3:2)

  expect_equal(mod$functions$rtn_tuple_vec(tuple_vec), tuple_vec)
  expect_equal(mod$functions$rtn_tuple_rowvec(tuple_rowvec), tuple_rowvec)
  expect_equal(mod$functions$rtn_tuple_matrix(tuple_matrix), tuple_matrix)
  expect_equal(mod$functions$rtn_tuple_int_array(tuple_int_array), tuple_int_array)
  expect_equal(mod$functions$rtn_tuple_real_array(tuple_vec), tuple_vec)

  ### Tuple of Container Arrays

  tuple_vec_array <- list(vec_array, vec_array)
  tuple_rowvec_array <- list(rowvec_array, rowvec_array)
  tuple_matrix_array <- list(matrix_array, matrix_array)

  expect_equal(mod$functions$rtn_tuple_vec_array(tuple_vec_array), tuple_vec_array)
  expect_equal(mod$functions$rtn_tuple_rowvec_array(tuple_rowvec_array), tuple_rowvec_array)
  expect_equal(mod$functions$rtn_tuple_matrix_array(tuple_matrix_array), tuple_matrix_array)

  ### Nested Tuple of Scalar

  nest_tuple_int <- list(10, tuple_int)
  nest_tuple_dbl <- list(31, tuple_dbl)
  expect_equal(mod$functions$rtn_nest_tuple_int(nest_tuple_int), nest_tuple_int)
  expect_equal(mod$functions$rtn_nest_tuple_real(nest_tuple_dbl), nest_tuple_dbl)

  ### Nested Tuple of Container

  nest_tuple_vec <- list(12, tuple_vec)
  nest_tuple_rowvec <- list(2, tuple_rowvec)
  nest_tuple_matrix <- list(-23, tuple_matrix)
  nest_tuple_int_array <- list(21, tuple_int_array)

  expect_equal(mod$functions$rtn_nest_tuple_vec(nest_tuple_vec), nest_tuple_vec)
  expect_equal(mod$functions$rtn_nest_tuple_rowvec(nest_tuple_rowvec), nest_tuple_rowvec)
  expect_equal(mod$functions$rtn_nest_tuple_matrix(nest_tuple_matrix), nest_tuple_matrix)
  expect_equal(mod$functions$rtn_nest_tuple_int_array(nest_tuple_int_array), nest_tuple_int_array)
  expect_equal(mod$functions$rtn_nest_tuple_real_array(nest_tuple_vec), nest_tuple_vec)

  ### Nested Tuple of Container Arrays

  nest_tuple_vec_array <- list(-21, tuple_vec_array)
  nest_tuple_rowvec_array <- list(1000, tuple_rowvec_array)
  nest_tuple_matrix_array <- list(0, tuple_matrix_array)

  expect_equal(mod$functions$rtn_nest_tuple_vec_array(nest_tuple_vec_array), nest_tuple_vec_array)
  expect_equal(mod$functions$rtn_nest_tuple_rowvec_array(nest_tuple_rowvec_array), nest_tuple_rowvec_array)
  expect_equal(mod$functions$rtn_nest_tuple_matrix_array(nest_tuple_matrix_array), nest_tuple_matrix_array)
})

test_that("Functions handle complex types correctly", {
  skip_if(os_is_wsl())

  ### Scalar

  complex_scalar <- complex(real = 2.1, imaginary = 21.3)

  expect_equal(mod$functions$rtn_complex(complex_scalar), complex_scalar)

  ### Container

  complex_vec <- complex(real = c(2,1.5,0.11, 1.2), imaginary = c(11.2,21.5,6.1,3.2))
  complex_rowvec <- t(complex_vec)
  complex_matrix <- matrix(complex_vec, nrow=2, ncol=2)

  expect_equal(mod$functions$rtn_complex_vec(complex_vec), complex_vec)
  expect_equal(mod$functions$rtn_complex_rowvec(complex_rowvec), complex_rowvec)
  expect_equal(mod$functions$rtn_complex_matrix(complex_matrix), complex_matrix)
  expect_equal(mod$functions$rtn_complex_array(complex_vec), complex_vec)

  ### Array of Container

  complex_vec_array <- list(complex_vec, complex_vec * 2, complex_vec + 0.1)
  complex_rowvec_array <- list(complex_rowvec, complex_rowvec * 2, complex_rowvec + 0.1)
  complex_matrix_array <- list(complex_matrix, complex_matrix * 2, complex_matrix + 0.1)

  expect_equal(mod$functions$rtn_complex_vec_array(complex_vec_array), complex_vec_array)
  expect_equal(mod$functions$rtn_complex_rowvec_array(complex_rowvec_array), complex_rowvec_array)
  expect_equal(mod$functions$rtn_complex_matrix_array(complex_matrix_array), complex_matrix_array)

  ### Tuple of Scalar

  tuple_complex <- list(complex_vec[1], complex_vec[2])
  expect_equal(mod$functions$rtn_tuple_complex(tuple_complex), tuple_complex)

  ### Tuple of Container

  tuple_complex_vec <- list(complex_vec, complex_vec * 1.2)
  tuple_complex_rowvec <- list(complex_rowvec, complex_rowvec * 0.5)
  tuple_complex_matrix <- list(complex_matrix, complex_matrix * 10.2)

  expect_equal(mod$functions$rtn_tuple_complex_array(tuple_complex_vec), tuple_complex_vec)
  expect_equal(mod$functions$rtn_tuple_complex_vec(tuple_complex_vec), tuple_complex_vec)
  expect_equal(mod$functions$rtn_tuple_complex_rowvec(tuple_complex_rowvec), tuple_complex_rowvec)
  expect_equal(mod$functions$rtn_tuple_complex_matrix(tuple_complex_matrix), tuple_complex_matrix)

  ### Tuple of Container Arrays

  tuple_complex_vec_array <- list(complex_vec_array, complex_vec_array)
  tuple_complex_rowvec_array <- list(complex_rowvec_array, complex_rowvec_array)
  tuple_complex_matrix_array <- list(complex_matrix_array, complex_matrix_array)

  expect_equal(mod$functions$rtn_tuple_complex_vec_array(tuple_complex_vec_array), tuple_complex_vec_array)
  expect_equal(mod$functions$rtn_tuple_complex_rowvec_array(tuple_complex_rowvec_array), tuple_complex_rowvec_array)
  expect_equal(mod$functions$rtn_tuple_complex_matrix_array(tuple_complex_matrix_array), tuple_complex_matrix_array)

  ### Nested Tuple of Scalar

  nest_tuple_complex <- list(31, tuple_complex)
  expect_equal(mod$functions$rtn_nest_tuple_complex(nest_tuple_complex), nest_tuple_complex)

  ### Nested Tuple of Container

  nest_tuple_complex_vec <- list(12, tuple_complex_vec)
  nest_tuple_complex_rowvec <- list(2, tuple_complex_rowvec)
  nest_tuple_complex_matrix <- list(-23, tuple_complex_matrix)
  nest_tuple_complex_array <- list(21, tuple_complex_vec)

  expect_equal(mod$functions$rtn_nest_tuple_complex_array(nest_tuple_complex_vec), nest_tuple_complex_vec)
  expect_equal(mod$functions$rtn_nest_tuple_complex_vec(nest_tuple_complex_vec), nest_tuple_complex_vec)
  expect_equal(mod$functions$rtn_nest_tuple_complex_rowvec(nest_tuple_complex_rowvec), nest_tuple_complex_rowvec)
  expect_equal(mod$functions$rtn_nest_tuple_complex_matrix(nest_tuple_complex_matrix), nest_tuple_complex_matrix)

  ### Nested Tuple of Container Arrays

  nest_tuple_complex_vec_array <- list(-21, tuple_complex_vec_array)
  nest_tuple_complex_rowvec_array <- list(1000, tuple_complex_rowvec_array)
  nest_tuple_complex_matrix_array <- list(0, tuple_complex_matrix_array)

  expect_equal(mod$functions$rtn_nest_tuple_complex_vec_array(nest_tuple_complex_vec_array), nest_tuple_complex_vec_array)
  expect_equal(mod$functions$rtn_nest_tuple_complex_rowvec_array(nest_tuple_complex_rowvec_array), nest_tuple_complex_rowvec_array)
  expect_equal(mod$functions$rtn_nest_tuple_complex_matrix_array(nest_tuple_complex_matrix_array), nest_tuple_complex_matrix_array)
})

test_that("Functions can be exposed in fit object", {
  skip_if(os_is_wsl())
  fit$expose_functions(verbose = TRUE)

  expect_equal(
    fit$functions$rtn_vec(c(1,2,3,4)),
    c(1,2,3,4)
  )
})

test_that("Compiled functions can be copied to global environment", {
  skip_if(os_is_wsl())
  expect_message(
    fit$expose_functions(global = TRUE),
    "Functions already compiled, copying to global environment",
    fixed = TRUE
  )

  expect_equal(
    rtn_vec(c(1,2,3,4)),
    c(1,2,3,4)
  )
})


test_that("Functions can be compiled with model", {
  skip_if(os_is_wsl())
  mod <- cmdstan_model(model, force_recompile = TRUE, compile_standalone = TRUE)
  fit <- mod$sample(data = data_list)

  expect_message(
    fit$expose_functions(),
    "Functions already compiled, nothing to do!",
    fixed = TRUE
  )

  expect_equal(
    fit$functions$rtn_vec(c(1,2,3,4)),
    c(1,2,3,4)
  )

  expect_message(
    fit$expose_functions(global = TRUE),
    "Functions already compiled, copying to global environment",
    fixed = TRUE
  )

  expect_equal(
    rtn_vec(c(1,2,3,4)),
    c(1,2,3,4)
  )
})

test_that("compile_standalone warns but doesn't error if no functions", {
  stan_no_funs_block <- write_stan_file("
    parameters {
      real x;
    }
    model {
      x ~ std_normal();
    }
  ")
  expect_warning(
    mod1 <- cmdstan_model(stan_no_funs_block, compile = TRUE, compile_standalone = TRUE, force_recompile = TRUE),
    "No standalone functions found to compile and expose to R"
  )
  checkmate::expect_r6(mod1, "CmdStanModel")

  stan_empty_funs_block <- write_stan_file("
   functions {
   }
  ")
  expect_warning(
    mod2 <- cmdstan_model(stan_empty_funs_block, compile = TRUE, compile_standalone = TRUE, force_recompile = TRUE),
    "No standalone functions found to compile and expose to R"
  )
  checkmate::expect_r6(mod2, "CmdStanModel")
})

test_that("rng functions can be exposed", {
  skip_if(os_is_wsl())
  function_decl <- "functions { real wrap_normal_rng(real mu, real sigma) { return normal_rng(mu, sigma); } }"
  stan_prog <- paste(function_decl,
                     paste(readLines(testing_stan_file("bernoulli")),
                           collapse = "\n"),
                     collapse = "\n")
  model <- write_stan_file(stan_prog)
  data_list <- testing_data("bernoulli")
  mod <- cmdstan_model(model, force_recompile = TRUE)
  fit <- mod$sample(data = data_list)

  set.seed(10)
  fit$expose_functions(verbose = TRUE)

  expect_equal(
    fit$functions$wrap_normal_rng(5,10),
    -4.5298764235381225873
  )

  expect_equal(
    fit$functions$wrap_normal_rng(5,10),
    8.1295902610102039887
  )
})

test_that("Overloaded functions give meaningful errors", {
  skip_if(os_is_wsl())

  funcode <- "
  functions {
    real fun1(real x) { return x; }
    vector fun1(vector x) { return x; }
    real fun2(real x) { return x; }
    matrix fun3(matrix x) { return x; }
    real fun3(real x) { return x; }
  }
  "

  funmod <- cmdstan_model(write_stan_file(funcode), force_recompile = TRUE)
  expect_error(funmod$expose_functions(),
               "Overloaded functions are currently not able to be exposed to R! The following overloaded functions were found: fun1, fun3")
})

test_that("Exposing external functions errors before v2.32", {
  skip_if(os_is_wsl())

  fake_cmdstan_version("2.26.0")

  tmpfile <- tempfile(fileext = ".hpp")
  hpp <-
  "
  #include <ostream>
  namespace standalone_external_model_namespace {
    int rtn_int(int x, std::ostream *pstream__) { return x; }
  }"
  cat(hpp, file = tmpfile, sep = "\n")
  stanfile <- file.path(tempdir(), "standalone_external.stan")
  cat("functions { int rtn_int(int x); }\n", file = stanfile)
  expect_error({
    cmdstan_model(
      stan_file = stanfile,
      user_header = tmpfile,
      compile_standalone = TRUE
    )
  },
  "Exporting standalone functions with external C++ is not available before CmdStan 2.32",
  fixed = TRUE)

  reset_cmdstan_version()
})

test_that("Exposing functions with precompiled model gives meaningful error", {
  skip_if(os_is_wsl())

  stan_file <- write_stan_file("
    functions {
      real a_plus_b(real a, real b) { return a + b; }
    }
    parameters { real x; }
    model { x ~ std_normal(); }
  ")
  mod1 <- cmdstan_model(stan_file, compile_standalone = TRUE,
                        force_recompile = TRUE)
  expect_equal(7.5, mod1$functions$a_plus_b(5, 2.5))

  mod2 <- cmdstan_model(stan_file)
  expect_error(
    mod2$expose_functions(),
    "Exporting standalone functions is not possible with a pre-compiled Stan model!",
    fixed = TRUE
  )
})
