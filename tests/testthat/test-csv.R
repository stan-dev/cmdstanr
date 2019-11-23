context("read-sample-csv")

if (not_on_cran()) {

}

test_that("read_sample_csv() fails for different stan version", {
  skip_on_cran()
  csv_files <- c(test_path("resources", "csv", "model1-1-nowarmup.csv"),
                 test_path("resources", "csv", "model1-3-diff_name.csv"))
  expect_error(read_sample_csv(csv_files),
               "Supplied CSV files were not generated with the same version of Cmdstan!")
})
