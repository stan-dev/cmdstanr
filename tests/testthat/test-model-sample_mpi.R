context("model-sample_mpi")

test_that("sample_mpi() works", {
  skip_if(!mpi_toolchain_present())
  mpi_file <- write_stan_file("
  functions {
    vector test(vector beta, vector theta, real[] x, int[] y) {
      return theta;
    }
  }
  transformed data {
    vector[4] a;
    vector[5] b[4] = {[1,1,1,1,1]', [2,2,2,2,2]', [3,3,3,3,3]', [4,4,4,4,4]'};
    real x[4,4];
    int y[4,4];
  }
  parameters {
    real beta;
  }
  model {
    beta ~ std_normal();
  }
  generated quantities {
    vector[20] c = map_rect(test, a, b, x, y);
  }
  ")
  if (os_is_macos()) {
    tbb_cxx_type <- "clang"
  } else {
    tbb_cxx_type <- "gcc"
  }
  cpp_options = list(cxx="mpicxx", stan_mpi = TRUE, tbb_cxx_type=tbb_cxx_type)
  mod_mpi <- cmdstan_model(mpi_file, cpp_options = cpp_options)

  if (os_is_wsl()) {
    # Default GHA WSL install runs as root, which MPI discourages
    # Specify that this is safe to ignore for this test
    Sys.setenv("OMPI_ALLOW_RUN_AS_ROOT"=1)
    Sys.setenv("OMPI_ALLOW_RUN_AS_ROOT_CONFIRM"=1)
    Sys.setenv("WSLENV"="OMPI_ALLOW_RUN_AS_ROOT/u:OMPI_ALLOW_RUN_AS_ROOT_CONFIRM/u")
  }

  utils::capture.output(
    f <- mod_mpi$sample_mpi(chains = 1, mpi_args = list("n" = 1))
  )
  expect_equal(f$metadata()$mpi_enable, 1)
  expect_equal(
    as.numeric(posterior::subset_draws(f$draws("c"), iteration = 1)),
    c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4)
  )
})
