#' @param ... arguments passed to mod$compile()
expect_compilation <- function(mod, ...) {
  if(length(mod$exe_file()) > 0 && file.exists(mod$exe_file())) {
    before_mtime <- file.mtime(mod$exe_file())
  } else {
    before_mtime <- NULL
  }
  expect_interactive_message(mod$compile(...), "Compiling Stan program...")
  if(length(mod$exe_file()) == 0 || !file.exists(mod$exe_file())) {
    fail(sprint("Model executable '%s' does not exist after compilation.", mod$exe_file()))
  }
  if(!is.null(before_mtime)) {
    after_mtime <- file.mtime(mod$exe_file())
    expect(before_mtime != after_mtime, sprintf("Exe file '%s' has NOT changed, despite expecting (re)compilation", mod$exe_file()))
  }
  invisible(mod)
}

#' @param ... arguments passed to mod$compile()
expect_no_recompilation <- function(mod, ...) {
  if(length(mod$exe_file()) == 0 || !file.exists(mod$exe_file())) {
    fail(sprint("Model executable '%s' does not exist, cannot test if recompilation is triggerred.", mod$exe_file()))
  }

  before_mtime <- file.mtime(mod$exe_file())
  expect_interactive_message(mod$compile(...), "Model executable is up to date!")
  after_mtime <- file.mtime(mod$exe_file())
  expect(before_mtime == after_mtime, sprintf("Model executable '%s' has changed, despite expecting no recompilation", mod$exe_file()))
  invisible(mod)
}

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

expect_laplace_output <- function(object) {
  expect_output(
    object,
    regexp = "Generating draws"
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
