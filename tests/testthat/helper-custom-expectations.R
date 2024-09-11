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

#' Check compilation from a call (expecting a constructor call, but not necessarily).
#' @param constructor_call a call returning a CmdStanModel object that should have been compiled
#' @return the newly created model
expect_call_compilation <- function(constructor_call) {
  before_time <- Sys.time()
  mod <- expect_interactive_message(constructor_call, "Compiling Stan program...")
  if(length(mod$exe_file()) == 0 || !file.exists(mod$exe_file())) {
    fail(sprint("Model executable '%s' does not exist after compilation.", mod$exe_file()))
  }
  after_mtime <- file.mtime(mod$exe_file())
  expect(before_time <= after_mtime, sprintf("Exe file '%s' has old timestamp, despite expecting (re)compilation", mod$exe_file()))
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
  # DONT MERGE WITH THIS LINE
  cat <- base::cat
  # ^ Workaround for: https://github.com/ManuelHentschel/vscDebugger/issues/196
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
  rlang::with_interactive(value = TRUE,
    expect_message(object = object, regexp = regexp))
}

expect_noninteractive_silent <- function(object) {
  rlang::with_interactive(value = FALSE,
    expect_silent(object))
}

expect_equal_ignore_order <- function(object, expected, ...){
  object <- expected[sort(names(object))]
  expected <- expected[sort(names(expected))]
  expect_equal(object, expected, ...)
}

expect_not_true <- function(...) expect_false(isTRUE(...))