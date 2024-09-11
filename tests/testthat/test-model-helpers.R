test_that("parse_exe_info_string works", {
  expect_equal_ignore_order(
    parse_exe_info_string("
      stan_version_major = 2
      stan_version_minor = 38
      stan_version_patch = 0
      STAN_THREADS=false
      STAN_MPI=false
      STAN_OPENCL=true
      STAN_NO_RANGE_CHECKS=false
      STAN_CPP_OPTIMS=false
    "),
    list(
      stan_version = '2.38.0',
      stan_threads = FALSE,
      stan_mpi = FALSE,
      stan_opencl = TRUE,
      stan_no_range_checks = FALSE,
      stan_cpp_optims = FALSE
    )
  )
})
  
test_that("validate_precompile_cpp_options works", {
  expect_equal_ignore_order(
    validate_precompile_cpp_options(list(Stan_Threads = TRUE, STAN_OPENCL = NULL, aBc = FALSE)),
    list(
      stan_threads = TRUE,
      stan_opencl = NULL, 
      abc = FALSE
    )
  )
  expect_warning(validate_precompile_cpp_options(list(STAN_OPENCL= FALSE)))
})


test_that('exe_info cpp_options comparison works', {
  exe_info_all_flags_off <- exe_info_style_cpp_options(list())
  exe_info_all_flags_off[['stan_version']] <- '35.0.0'

  expect_true(exe_info_reflects_cpp_options(exe_info_all_flags_off, list()))
  expect_true(exe_info_reflects_cpp_options(list(stan_opencl = FALSE), list(stan_opencl = NULL)))
  expect_not_true(exe_info_reflects_cpp_options(list(stan_opencl = FALSE), list(stan_opencl = FALSE)))
})