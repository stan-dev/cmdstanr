stan_program <- cmdstan_example_file()
file_that_doesnt_exist <- "placeholder_doesnt_exist"
file_that_exists <- "placeholder_exists"
file.create(file_that_exists)
withr::defer(
  if (file.exists(file_that_exists)) file.remove(file_that_exists),
  teardown_env()
)

skip_message <- "To be fixed in a later version."

test_that("warning when no recompile and no info", {
  skip(skip_message)
  with_mocked_cli(
    compile_ret = list(),
    info_ret = list(status = 1),
    code = expect_warning({
      mod <- cmdstan_model(
        stan_file = stan_program,
        exe_file = file_that_exists,
        compile = FALSE
      )
    }, "Recompiling is recommended.")
  )
})

test_that("recompiles when force_recompile flag set",
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(),
    code = expect_mock_compile({
      mod <- cmdstan_model(stan_file = stan_program, force_recompile = TRUE)
    })
  )
)

test_that("no mismatch results in no recompile", with_mocked_cli(
  compile_ret = list(status = 0),
  info_ret = list(
    status = 0,
    stdout = "
      stan_version_major = 2
      stan_version_minor = 35
      stan_version_patch = 0
      STAN_THREADS=false
      STAN_MPI=false
      STAN_OPENCL=false
      STAN_NO_RANGE_CHECKS=false
      STAN_CPP_OPTIMS=false
    "
  ),
  code = expect_no_mock_compile({
    mod <- cmdstan_model(stan_file = stan_program, exe_file = file_that_exists)
  })
))

test_that("mismatch results in recompile.", {
  skip(skip_message)
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(
      status = 0,
      stdout =  "
        stan_version_major = 2
        stan_version_minor = 35
        stan_version_patch = 0
        STAN_THREADS=false
        STAN_MPI=false
        STAN_OPENCL=false
        STAN_NO_RANGE_CHECKS=false
        STAN_CPP_OPTIMS=false
      "
    ),
    code = expect_mock_compile({
      mod <- cmdstan_model(
        stan_file = stan_program,
        exe_file = file_that_exists,
        cpp_options = list(stan_threads = TRUE)
      )
    })
  )
})

test_that("recompile when cpp args don't match binary", {
  skip(skip_message)
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(
      status = 0,
      stdout = "
        stan_version_major = 2
        stan_version_minor = 38
        stan_version_patch = 0
        STAN_THREADS=false
        STAN_MPI=false
        STAN_OPENCL=true
        STAN_NO_RANGE_CHECKS=false
        STAN_CPP_OPTIMS=false
      "
    ),
    expect_mock_compile({
      mod_gq <- cmdstan_model(
        testing_stan_file("bernoulli_ppc"),
        exe_file = file_that_exists,
        cpp_options = list(stan_threads = TRUE)
      )
    })
  )
})