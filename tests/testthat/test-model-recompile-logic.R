# A mocked compile produces a real (empty) executable at the destination, so the
# model compiled here must be a temporary copy. Compiling the installed example
# in place would replace the CmdStan installation's own executable.
example_exe <- cmdstan_ext(strip_ext(cmdstan_example_file()))
example_exe_before <- file.info(example_exe)[, c("size", "mtime")]

model_dir <- withr::local_tempdir()
stan_program <- file.path(model_dir, "bernoulli.stan")
file.copy(cmdstan_example_file(), stan_program)
# Keep the program older than the placeholder executables created below, so the
# up-to-date check doesn't force a recompile on timestamp resolution alone.
Sys.setFileTime(stan_program, Sys.time() - 60)

file_that_doesnt_exist <- withr::local_tempfile(pattern = "placeholder_doesnt_exist")
file_that_exists <- withr::local_tempfile(pattern = "placeholder_exists")
file.create(file_that_exists)

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
    # The mocked compile now leaves an executable behind, so the constructor
    # queries it for compilation info. Report a failure rather than an empty
    # list, which model_compile_info() cannot interpret.
    info_ret = list(status = 1),
    code = expect_mock_compile({
      mod <- cmdstan_model(stan_file = stan_program, force_recompile = TRUE)
    })
  )
)

test_that("a mocked successful compile installs an executable", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)
  exe <- cmdstan_ext(strip_ext(stan_file))

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = mod <- cmdstan_model(stan_file = stan_file, force_recompile = TRUE)
  )

  expect_equal(mod$exe_file(), exe)
  expect_true(file.exists(exe))
})

test_that("a mocked failed compile installs no executable", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli.stan")
  file.copy(stan_program, stan_file)
  exe <- cmdstan_ext(strip_ext(stan_file))

  with_mocked_cli(
    compile_ret = list(status = 1),
    info_ret = list(status = 1),
    code = expect_error(
      cmdstan_model(stan_file = stan_file, force_recompile = TRUE),
      "An error occurred during compilation"
    )
  )

  expect_false(file.exists(exe))
})

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

# Deliberately the last test in this file: it checks that none of the mocked
# compiles above installed anything over the CmdStan installation's own example
# executable. A git diff would not catch this, since that executable is not part
# of the repository, and a truncating overwrite changes no tracked file at all.
test_that("mocked compiles leave the CmdStan installation untouched", {
  expect_equal(
    file.info(example_exe)[, c("size", "mtime")],
    example_exe_before
  )
})
