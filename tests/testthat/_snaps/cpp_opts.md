# lowercase stan_threads behavior remains unchanged

    Code
      assert_valid_threads(2L, list(stan_threads = FALSE))
    Condition
      Warning:
      'threads' is set but the model was not compiled with 'cpp_options = list(stan_threads = TRUE)' so 'threads' will have no effect!
    Code
      assert_valid_threads(2L, list(stan_threads = "dummy string"))
    Condition
      Warning:
      'threads' is set but the model was not compiled with 'cpp_options = list(stan_threads = TRUE)' so 'threads' will have no effect!

# cpp option checks do not use partial matching

    Code
      assert_valid_opencl(c(0L, 0L), list(stan_opencl_x = TRUE))
    Condition
      Error:
      ! 'opencl_ids' is set but the model was not compiled for use with OpenCL.
      Recompile the model with 'cpp_options = list(stan_opencl = TRUE)'

