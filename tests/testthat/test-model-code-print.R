context("model-code-print")

set_cmdstan_path()
stan_program <- testing_stan_file("bernoulli")
mod <- testing_model("bernoulli")


test_that("code() and print() methods work", {
  expect_known_output(mod$print(), file = test_path("answers", "model-print-output.stan"))
  expect_known_value(mod$code(), file = test_path("answers", "model-code-output.rds"))
})

test_that("code() and print() still work if file is removed", {
  code <- "
  parameters {
    real y;
  }
  model {
    y ~ std_normal();
  }
  "
  stan_file_tmp <- write_stan_file(code)
  code_answer <- readLines(stan_file_tmp)
  mod_removed_stan_file <- cmdstan_model(stan_file_tmp, compile = FALSE)
  file.remove(stan_file_tmp)
  expect_identical(mod_removed_stan_file$code(), code_answer)
})

test_that("code() doesn't change when file changes (unless model is recreated)", {
  code_1 <- "
  parameters {
    real y;
  }
  model {
    y ~ std_normal();
  }
  "
  code_2 <- "
  parameters {
    real x;
  }
  model {
    x ~ std_normal();
  }
  "
  stan_file_1 <- write_stan_file(code_1)
  stan_file_2 <- write_stan_file(code_2)
  code_1_answer <- readLines(stan_file_1)
  code_2_answer <- readLines(stan_file_2)

  mod <- cmdstan_model(stan_file_1, compile = FALSE)
  expect_identical(mod$code(), code_1_answer)
  expect_identical(utils::capture.output(mod$print()), code_1_answer)

  # overwrite with new code, but mod$code() shouldn't change
  file.copy(stan_file_2, stan_file_1, overwrite = TRUE)
  expect_identical(mod$code(), code_1_answer)

  # recreate CmdStanModel object, now mod$code() should change
  mod <- cmdstan_model(stan_file_1, compile = FALSE)
  expect_identical(mod$code(), code_2_answer)
  expect_identical(utils::capture.output(mod$print()), code_2_answer)
})

test_that("code() warns and print() errors if only exe and no Stan file", {
  mod_exe <- cmdstan_model(exe_file = mod$exe_file())
  expect_warning(
    expect_null(mod_exe$code()),
    "'$code()' will return NULL because the 'CmdStanModel' was not created with a Stan file",
    fixed = TRUE
  )
  expect_error(
    mod_exe$print(),
    "'$print()' cannot be used because the 'CmdStanModel' was not created with a Stan file.",
    fixed = TRUE
  )
})

test_that("check_syntax() errors if only exe and no Stan file", {
  mod_exe <- cmdstan_model(exe_file = mod$exe_file())
  expect_error(
    mod_exe$check_syntax(),
    "'$check_syntax()' cannot be used because the 'CmdStanModel' was not created with a Stan file.",
    fixed = TRUE
  )
})
