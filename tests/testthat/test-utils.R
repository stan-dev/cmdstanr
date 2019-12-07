context("utils")

if (not_on_cran()) {
  set_cmdstan_path()
  fit_mcmc <- testing_fit("logistic", method = "sample",
                          seed = 123, num_chains = 2)
}

test_that("cmdstan_summary works if bin/stansummary deleted file", {
  skip_on_cran()
  delete_and_run <- function() {
    file.remove(file.path(cmdstan_path(), "bin", cmdstan_ext("stansummary")))
    fit_mcmc$cmdstan_summary()
  }
  expect_output(delete_and_run(), "Inference for Stan model: logistic_model\\n2 chains: each with iter")
})

test_that("cmdstan_diagnose works if bin/diagnose deleted file", {
  skip_on_cran()
  delete_and_run <- function() {
    file.remove(file.path(cmdstan_path(), "bin", cmdstan_ext("diagnose")))
    fit_mcmc$cmdstan_diagnose()
  }
  expect_output(delete_and_run(), "Checking sampler transitions treedepth")
})

test_that("repair_path() fixes slashes", {
  # all slashes should be single "/", and no trailing slash
  expect_equal(repair_path("a//b\\c/"), "a/b/c")
})

test_that("repair_path works with zero length path or non-string path", {
  expect_equal(repair_path(""), "")
  expect_equal(repair_path(5), 5)
})

test_that("list_to_array works with empty list", {
  expect_equal(list_to_array(list()), NULL)
})

test_that("list_to_array fails for non-numeric values", {
  expect_error(list_to_array(list(k = "test")), "All elements of the list must be numeric!")
})
