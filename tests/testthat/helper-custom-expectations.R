expect_experimental_warning <- function(object) {
  expect_warning(
    object,
    regexp = "experimental and the structure of returned object may change"
  )
}

expect_sample_output <- function(object) {
  expect_output(object, "Gradient evaluation took")
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
