expect_sample_output <- function(object, num_chains = NULL) {

  output <- "Running MCMC with"
  if (!is.null(num_chains)) {
    if (num_chains == 1) {
      output <- paste(output, num_chains, "chain")
    } else {
      output <- paste(output, num_chains, "sequential chain")
    }
  }
  expect_output(object, output)
}

expect_optim_output <- function(object) {
  expect_output(
    object,
    regexp = "Initial log joint probability"
  )
}

expect_vb_output <- function(object) {
  expect_output(
    object,
    regexp = "Drawing a sample of size"
  )
}

expect_gq_output <- function(object, num_chains = NULL) {

  output <- "Running standalone generated quantities after "
  if (!is.null(num_chains)) {
    if (num_chains == 1) {
      output <- paste(output, num_chains, "chain")
    } else {
      output <- paste(output, num_chains, "sequential chain")
    }
  }
  expect_output(object, output)
}

expect_interactive_message <- function(object, regexp = NULL) {
  # Non-interactive message suppression failing under Windows CI,
  # temporarily skip message check only on Windows
  if (os_is_windows() && !os_is_wsl()) {
    return(object)
  }
  if (interactive()) {
    expect_message(object = object, regexp = regexp)
  } else {
    expect_silent(object = object)
  }
}
