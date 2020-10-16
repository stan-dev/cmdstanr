# context("utils")
#
# if (not_on_cran()) {
#   set_cmdstan_path()
#   fit_mcmc <- testing_fit("logistic", method = "sample",
#                           seed = 123, chains = 2)
# }
#
# test_that("check_divergences() works", {
#   skip_on_cran()
#   csv_files <- c(test_path("resources", "csv", "model1-2-no-warmup.csv"))
#   csv_output <- read_cmdstan_csv(csv_files)
#   output <- "14 of 100 \\(14.0%\\) transitions ended with a divergence."
#   expect_message(check_divergences(csv_output), output)
#
#   csv_files <- c(test_path("resources", "csv", "model1-2-no-warmup.csv"),
#                  test_path("resources", "csv", "model1-2-no-warmup.csv"))
#   csv_output <- read_cmdstan_csv(csv_files)
#   output <- "28 of 200 \\(14.0%\\) transitions ended with a divergence."
#   expect_message(check_divergences(csv_output), output)
#
#   csv_files <- c(test_path("resources", "csv", "model1-2-warmup.csv"))
#   csv_output <- read_cmdstan_csv(csv_files)
#   output <- "1 of 100 \\(1.0%\\) transitions ended with a divergence."
#   expect_message(check_divergences(csv_output), output)
#
#
#   fit_wramup_no_samples <- testing_fit("logistic", method = "sample",
#                           seed = 123, chains = 1,
#                           iter_sampling = 0,
#                           iter_warmup = 10,
#                           save_warmup = TRUE,
#                           validate_csv = FALSE)
#   csv_output <- read_cmdstan_csv(fit_wramup_no_samples$output_files())
#   expect_message(check_divergences(csv_output), regexp = NA)
# })
#
# test_that("check_sampler_transitions_treedepth() works", {
#   skip_on_cran()
#   csv_files <- c(test_path("resources", "csv", "model1-2-no-warmup.csv"))
#   csv_output <- read_cmdstan_csv(csv_files)
#   output <- "16 of 100 \\(16.0%\\) transitions hit the maximum treedepth limit of 5 or 2\\^5-1 leapfrog steps."
#   expect_message(check_sampler_transitions_treedepth(csv_output), output)
#
#   csv_files <- c(test_path("resources", "csv", "model1-2-no-warmup.csv"),
#                  test_path("resources", "csv", "model1-2-no-warmup.csv"))
#   csv_output <- read_cmdstan_csv(csv_files)
#   output <- "32 of 200 \\(16.0%\\) transitions hit the maximum treedepth limit of 5 or 2\\^5-1 leapfrog steps."
#   expect_message(check_sampler_transitions_treedepth(csv_output), output)
#
#   csv_files <- c(test_path("resources", "csv", "model1-2-warmup.csv"))
#   csv_output <- read_cmdstan_csv(csv_files)
#   output <- "1 of 100 \\(1.0%\\) transitions hit the maximum treedepth limit of 5 or 2\\^5-1 leapfrog steps."
#   expect_message(check_sampler_transitions_treedepth(csv_output), output)
#
# })
#
# test_that("cmdstan_summary works if bin/stansummary deleted file", {
#   skip_on_cran()
#   delete_and_run <- function() {
#     file.remove(file.path(cmdstan_path(), "bin", cmdstan_ext("stansummary")))
#     fit_mcmc$cmdstan_summary()
#   }
#   expect_output(delete_and_run(), "Inference for Stan model: logistic_model\\n2 chains: each with iter")
# })
#
# test_that("cmdstan_diagnose works if bin/diagnose deleted file", {
#   skip_on_cran()
#   delete_and_run <- function() {
#     file.remove(file.path(cmdstan_path(), "bin", cmdstan_ext("diagnose")))
#     fit_mcmc$cmdstan_diagnose()
#   }
#   expect_output(delete_and_run(), "Checking sampler transitions treedepth")
# })
#
# test_that("repair_path() fixes slashes", {
#   # all slashes should be single "/", and no trailing slash
#   expect_equal(repair_path("a//b\\c/"), "a/b/c")
# })
#
# test_that("repair_path works with zero length path or non-string path", {
#   expect_equal(repair_path(""), "")
#   expect_equal(repair_path(5), 5)
# })
#
# test_that("list_to_array works with empty list", {
#   expect_equal(list_to_array(list()), NULL)
# })
#
# test_that("list_to_array fails for non-numeric values", {
#   expect_error(list_to_array(list(k = "test"), name = "test-list"),
#                "All elements in list 'test-list' must be numeric!")
# })
#
# test_that("cpp_options_to_compile_flags() works", {
#   options = list(
#     stan_threads = TRUE
#   )
#   expect_equal(cpp_options_to_compile_flags(options), "STAN_THREADS=TRUE")
#   options = list(
#     stan_threads = TRUE,
#     stanc2 = TRUE
#   )
#   expect_equal(cpp_options_to_compile_flags(options), "STAN_THREADS=TRUE STANC2=TRUE")
#   options = list()
#   expect_equal(cpp_options_to_compile_flags(options), NULL)
# })
#
# test_that("cmdstan_make_local() works", {
#   exisiting_make_local <- cmdstan_make_local()
#   make_local_path <- file.path(cmdstan_path(), "make", "local")
#   if (file.exists(make_local_path)) {
#     file.remove(make_local_path)
#   }
#   expect_equal(cmdstan_make_local(), NULL)
#   cpp_options = list(
#    "CXX" = "clang++",
#    "CXXFLAGS+= -march-native",
#    TEST1 = TRUE,
#    "TEST2" = FALSE
#   )
#   expect_equal(cmdstan_make_local(cpp_options = cpp_options),
#                c(
#                  "CXX=clang++",
#                  "CXXFLAGS+= -march-native",
#                  "TEST1=true",
#                  "TEST2=false"
#                  ))
#   expect_equal(cmdstan_make_local(cpp_options = list("TEST3" = TRUE)),
#                c(
#                  "CXX=clang++",
#                  "CXXFLAGS+= -march-native",
#                  "TEST1=true",
#                  "TEST2=false",
#                  "TEST3=true"
#                ))
#   expect_equal(cmdstan_make_local(cpp_options = list("TEST4" = TRUE), append = FALSE),
#                c("TEST4=true"))
#   cmdstan_make_local(cpp_options = as.list(exisiting_make_local), append = FALSE)
# })
#
# test_that("variable_dims() works", {
#   expect_null(variable_dims(NULL))
#
#   vars <- c("a", "b[1]", "b[2]", "b[3]", "c[1,1]", "c[1,2]")
#   vars_dims <- list(a = 1, b = 3, c = c(1,2))
#   expect_equal(variable_dims(vars), vars_dims)
#
#   vars <- c("a", "b")
#   vars_dims <- list(a = 1, b = 1)
#   expect_equal(variable_dims(vars), vars_dims)
#
#   vars <- c("c[1,1]", "c[1,2]", "c[1,3]", "c[2,1]", "c[2,2]", "c[2,3]", "b[1]", "b[2]", "b[3]", "b[4]")
#   vars_dims <- list(c = c(2,3), b = 4)
#   expect_equal(variable_dims(vars), vars_dims)
#
#   # make sure not confused by one name being last substring of another name
#   vars <- c("a[1]", "a[2]", "aa[1]", "aa[2]", "aa[3]")
#   expect_equal(variable_dims(vars), list(a = 2, aa = 3))
#
#   # wrong dimensions for descending order
#   vars <- c("c[1,1]", "c[1,2]", "c[1,3]", "c[2,3]", "c[2,2]", "c[2,1]", "b[4]", "b[2]", "b[3]", "b[1]")
#   vars_dims <- list(c = c(2,1), b = 1)
#   expect_equal(variable_dims(vars), vars_dims)
# })
