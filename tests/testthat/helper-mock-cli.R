real_wcr <- wsl_compatible_run

# Use distinct contents so tests can tell successive builds apart.
mock_exe_contents <- local({
  n <- 0L
  function() {
    n <<- n + 1L
    paste0("mock executable ", n)
  }
})

with_mocked_cli <- function(code, compile_ret, info_ret) {
  code <- substitute(code)
  caller <- parent.frame()
  local_mocked_bindings(
    wsl_compatible_run = function(command, args, ...) {
      if (
        # Match the configured make command.
        !is.null(command)
        && command == make_cmd()
        && !is.null(args)
        && startsWith(basename(args[1]), "model-")
      ) {
        message("mock-compile-was-called")
        # Successful builds create an executable artifact, just like make.
        if (isTRUE(compile_ret$status == 0)) {
          mock_exe <- wsl_safe_path(args[1], revert = TRUE)
          writeLines(mock_exe_contents(), mock_exe)
          Sys.chmod(mock_exe, "0755", use_umask = FALSE)
        }
        compile_ret
      } else if (!is.null(args) && args[1] == "info") {
        info_ret
      } else {
        real_wcr(command = command, args = args, ...)
      }
    },
    .package = "cmdstanr",
    .env = caller
  )
  rlang::eval_bare(code, env = caller)
}

######## Mock Compile Expectations #######

# These helpers mimic `assert_called` and `assert_not_called` in other languages.
#
# Logic
# `expect_mock_compile`
#     passes if mock_compile is called (at all, doesn't matter how many times)
#     fails if mock_compile is never called
# `expect_no_mock_compile` is the inverse. It
#      passes if mock_compile is *not* called at all
#      fails if mock_compile is called (even once)
#
# Implementation:
# `with_mocked_cli`
#    if a compile is triggered
#      emits a message with the contents `mock-compile-was-called`
#      (defined as wsl_compatible_run being called with make model-*)
# `expect_mock_compile` checks for this message:
#     passes if it detects such a message
#     fails if it does not
# `expect_no_mock_compile`
#      fails if a message with exactly this text is detected
#      passes if no such message is detected
#      messages with any other text does not impact `expect_no_mock_compile`

expect_mock_compile <- function(object, ...) {
  expect_message(object, regexp = "mock-compile-was-called", ...)
}
expect_no_mock_compile <- function(object, ...) {
  expect_no_message(object, message = "mock-compile-was-called", ...)
}
