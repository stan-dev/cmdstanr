real_wcr <- wsl_compatible_run

with_mocked_cli <- function(code, compile_ret, info_ret){
  with_mocked_bindings(
    code,
    wsl_compatible_run = function(command, args, ...) {
      if (
        !is.null(command)
        && command == 'make'
        && !is.null(args)
        && startsWith(basename(args[1]), 'model-')
      ) {
        message("mock-compile-was-called")
        compile_ret
      } else if (!is.null(args) && args[1] == "info") info_ret
      else real_wcr(command = command, args = args, ...)
    }
  )
}