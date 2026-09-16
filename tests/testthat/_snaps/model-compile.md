# a leftover backup doesn't unwind a build when warnings are errors

    Code
      withr::with_options(list(warn = 2), cmdstan_model(stan_file, cpp_options = list(
        stan_threads = TRUE)))
    Message
      mock-compile-was-called
    Condition
      Error:
      ! (converted from warning) Files left over from the previous build could not be removed: '<dir>/exe-old-<random>', '<dir>/record-old-<random>'.

