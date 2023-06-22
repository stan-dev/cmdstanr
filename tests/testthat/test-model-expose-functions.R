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

  expect_equal(mod$functions$rtn_int(10), 10)
  expect_equal(mod$functions$rtn_real(1.67), 1.67)

  vec <- c(1.2,234,0.3,-0.4)
  rowvec <- t(vec)
  matrix <- matrix(c(2.11, -6.35, 4.87, -0.9871), nrow = 2, ncol = 2)

  expect_equal(mod$functions$rtn_vec(vec), vec)
  expect_equal(mod$functions$rtn_rowvec(vec), t(vec))
  expect_equal(mod$functions$rtn_matrix(matrix), matrix)
  expect_equal(mod$functions$rtn_int_array(1:5), 1:5)
  expect_equal(mod$functions$rtn_real_array(vec), vec)

  vec_array <- list(vec, vec * 2, vec + 0.1)
  rowvec_array <- list(rowvec, rowvec * 2, rowvec + 0.1)
  matrix_array <- list(matrix, matrix * 2, matrix + 0.1)

  expect_equal(mod$functions$rtn_vec_array(vec_array), vec_array)
  expect_equal(mod$functions$rtn_rowvec_array(rowvec_array), rowvec_array)
  expect_equal(mod$functions$rtn_matrix_array(matrix_array), matrix_array)
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

test_that("rng functions can be exposed", {
  skip_if(os_is_wsl())
  function_decl <- "functions { real normal_rng(real mu) { return normal_rng(mu, 1); } }"
  stan_prog <- paste(function_decl,
                     paste(readLines(testing_stan_file("bernoulli")),
                           collapse = "\n"),
                     collapse = "\n")
  model <- write_stan_file(stan_prog)
  data_list <- testing_data("bernoulli")
  mod <- cmdstan_model(model, force_recompile = TRUE)
  fit <- mod$sample(data = data_list)

  fit$expose_functions(verbose = TRUE)

  expect_equal(
    fit$functions$normal_rng(5, seed = 10),
    3.8269637967017344771
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
