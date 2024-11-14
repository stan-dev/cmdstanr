stan_program <- cmdstan_example_file()
file_that_doesnt_exist <- 'placeholder_doesnt_exist'
file_that_exists <- 'placeholder_exists'
file.create(file_that_exists)
on.exit(if(file.exists(file_that_exists)) file.remove(file_that_exists))

test_that("warning when no recompile and no info",
  with_mocked_cli(compile_ret = list(), info_ret = list(status = 1), code = expect_warning({
    mod <- cmdstan_model(stan_file = stan_program, exe_file = file_that_exists, compile = FALSE)
  }, "Recompiling is recommended."))
)

test_that("recompiles when force_recompile flag set", 
  with_mocked_cli(compile_ret = list(status=0), info_ret = list(), code = expect_mock_compile({
    mod <- cmdstan_model(stan_file = stan_program, force_recompile = TRUE)
  }))
)

test_that("No mismatch results in no recompile.", with_mocked_cli(
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

test_that("Mismatch results in recompile.", with_mocked_cli(
  compile_ret = list(status=0),
  info_ret = list(
    status=0,
    stdout= "
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
    mod <- cmdstan_model(stan_file = stan_program, exe_file = file_that_exists, cpp_options = list(stan_threads = TRUE))
  })
))
test_that("$exe_info(), $precompile_cpp_options() return expected data without recompile", 
  with_mocked_cli(
    compile_ret = list(status=0),
    info_ret = list(
      status=0,
      stdout= "
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
    code = {
      file.create(file_that_exists)
      expect_no_mock_compile({
        mod <- cmdstan_model(
          stan_file = stan_program,
          exe_file = file_that_exists,
          compile = FALSE,
          cpp_options = list(Stan_Threads = TRUE, stan_opencl = NULL, aBc = FALSE)
        )
      })
      expect_equal_ignore_order(
        mod$exe_info(),
        list(
          stan_version = '2.38.0',
          stan_threads = FALSE,
          stan_mpi = FALSE,
          stan_opencl = TRUE,
          stan_no_range_checks = FALSE,
          stan_cpp_optims = FALSE
        )
      )
      expect_equal_ignore_order(
        mod$precompile_cpp_options(),
        list(
          stan_threads = TRUE,
          stan_opencl = NULL, 
          abc = FALSE
        )
      )
    }
  )
)

test_that("$exe_info_fallback() logic works as expected with cpp_options",
  with_mocked_cli(
    compile_ret = list(status=0),
    info_ret = list(
      status = 1,
      stdout = ''
    ),
    code = {
      expect_warning(
        expect_no_mock_compile({
          mod <- cmdstan_model(
            stan_file = stan_program,
            exe_file = file_that_exists,
            compile = FALSE,
            cpp_options = list(Stan_Threads = TRUE, stan_Opencl = NULL, aBc = FALSE, dEf = NULL)
          )
        }),
        'Retrieving exe_file info failed'
      )
      # cmdstan_model call same as above
      # Because we use testthat 3e, cannot nest expect_warning() with itself
      expect_warning(
        expect_no_mock_compile({
          mod <- cmdstan_model(
            stan_file = stan_program,
            exe_file = file_that_exists,
            compile = FALSE,
            cpp_options = list(Stan_Threads = TRUE, stan_Opencl = NULL, aBc = FALSE, dEf = NULL)
          )
        }),
      'Recompiling is recommended'
      )
      expect_equal(
        mod$exe_info(),
        NULL
      )
      expect_equal_ignore_order(
        mod$exe_info_fallback(),
        list(
          stan_version = cmdstan_version(),
          stan_threads = TRUE,
          stan_mpi = FALSE,
          stan_opencl = FALSE,
          stan_no_range_checks = FALSE,
          stan_cpp_optims = FALSE,
          abc = FALSE,
          def = NULL
        )
      )
      expect_equal_ignore_order(
        mod$precompile_cpp_options(),
        list(
          stan_threads = TRUE,
          stan_opencl = NULL,
          abc = FALSE,
          def = NULL
        )
      )
    }
  )
)

test_that("$exe_info_fallback() logic works as expected without cpp_options",
  with_mocked_cli(
    compile_ret = list(status=0),
    info_ret = list(
      status = 1,
      stdout = ""
    ),
    code = {
      expect_warning(
        expect_no_mock_compile({
          mod <- cmdstan_model(
            exe_file = file_that_exists
          )
        }),
        'Retrieving exe_file info failed'
      )
      # cmdstan_model call same as above
      # Because we use testthat 3e, cannot nest expect_warning() with itself
      expect_warning(
        expect_no_mock_compile({
          mod <- cmdstan_model(
            exe_file = file_that_exists
          )
        }),
        "Recompiling is recommended"
      )
      expect_equal(
        mod$exe_info(),
        NULL
      )
      expect_equal_ignore_order(
        mod$exe_info_fallback(),
        list(
          stan_version = cmdstan_version(),
          stan_threads = FALSE,
          stan_mpi = FALSE,
          stan_opencl = FALSE,
          stan_no_range_checks = FALSE,
          stan_cpp_optims = FALSE
        )
      )
      expect_equal_ignore_order(
        mod$precompile_cpp_options(),
        list()
      )
    }
  )
)

test_that("Recompile when cpp args don't match binary", {
  with_mocked_cli(
    compile_ret = list(status=0),
    info_ret = list(
      status=0,
      stdout= "
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
      mod_gq <- cmdstan_model(testing_stan_file("bernoulli_ppc"), exe_file = file_that_exists, cpp_options = list(stan_threads = TRUE))
    })
  )
})