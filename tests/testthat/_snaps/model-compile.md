# a leftover backup doesn't unwind a compile when warnings are errors

    Code
      withr::with_options(list(warn = 2), model$compile(cpp_options = list(
        stan_threads = TRUE), force_recompile = TRUE))
    Message
      mock-compile-was-called
    Condition
      Error:
      ! (converted from warning) The previously compiled executable could not be removed. It has been left at '<dir>/exe-old-<random>'.

