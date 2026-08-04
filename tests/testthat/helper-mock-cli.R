real_wcr <- wsl_compatible_run

# Distinct contents for every mocked build in the session. A mock that wrote the
# same empty file each time could not tell "the new artifact was installed" from
# "the old one was left in place", which is the invariant most of these tests
# exist to check.
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
        # make_cmd() rather than "make": production honours $MAKE, so a literal
        # comparison lets the mock be bypassed and the real command run.
        !is.null(command)
        && command == make_cmd()
        && !is.null(args)
        && startsWith(basename(args[1]), "model-")
      ) {
        message("mock-compile-was-called")
        # Real `make` writes the executable named by args[1] when it succeeds and
        # writes nothing when it fails. Without this, code that installs the
        # compiled artifact silently has nothing to install. `isTRUE()` because
        # callers may pass a `compile_ret` with no status at all.
        # Executable mode as well, so that installation losing it is something
        # the tests can notice rather than something the mock never modelled.
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
