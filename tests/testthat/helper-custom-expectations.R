expect_experimental_warning <- function(object) {
  expect_warning(
    object,
    regexp = "experimental and the structure of returned object may change"
  )
}

expect_sample_output <- function(object, num_chains = NULL) {
  output <- "Running MCMC with"
  if (!is.null(num_chains)) {
    output <- paste(output, num_chains, "chain")
  }
  expect_output(object, output)
}

expect_optim_output <- function(object) {
  expect_experimental_warning(
    expect_output(
      object,
      regexp = "Initial log joint probability"
    )
  )
}

expect_vb_output <- function(object) {
  expect_experimental_warning(
    expect_output(
      object,
      regexp = "Drawing a sample of size"
    )
  )
}
