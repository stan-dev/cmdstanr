context("mpi")

test_that("mpi_sample() works", {
  skip_on_cran()
  skip_if(!nzchar(Sys.getenv("CMDSTANR_RUN_MPI_TESTS")))
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
  cpp_options = list(cxx="mpicxx", stan_mpi = TRUE, tbb_cxx_type="gcc")
  mod_mpi <- cmdstan_model(mpi_file, cpp_options = cpp_options)
  utils::capture.output(
    f <- mod_mpi$mpi_sample(chains = 1, mpi_args = list("n" = 1))
  )
  expect_equal(f$metadata()$mpi_enable, 1)
  expect_equal(
    as.numeric(posterior::subset_draws(f$draws("c"), iteration = 1)),
    c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4)
  )
})
