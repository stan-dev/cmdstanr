test_that("test parse_exe_info_string", {
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
  
test_that("test validate_precompile_cpp_options", {
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