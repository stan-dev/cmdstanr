real_wcr <- wsl_compatible_run

with_mocked_cli <- function(
  code,
  compile_ret = list(status = 0),
  info_ret = list(status = 0, stdout = "")
) {
  with_mocked_bindings(
    code,
    wsl_compatible_run = function(command, args, ...) {
      if (
        !is.null(command)
        && command == "make"
        && !is.null(args)
        && startsWith(basename(args[1]), "model-")
      ) {
        message("mock-compile-was-called")
        compile_ret
      } else if (!is.null(args) && args[1] == "info") {
        info_ret
      } else {
        real_wcr(command = command, args = args, ...)
      }
    }
  )
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
