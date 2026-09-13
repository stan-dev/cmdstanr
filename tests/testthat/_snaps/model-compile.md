# a leftover backup doesn't unwind a compile when warnings are errors

    Code
      withr::with_options(list(warn = 2), model$compile(cpp_options = list(
        stan_threads = TRUE), force_recompile = TRUE))
    Message
      mock-compile-was-called
    Condition
      Error:
      ! (converted from warning) Files left over from the previous build could not be removed: '<dir>/exe-old-<random>', '<dir>/record-old-<random>'.

