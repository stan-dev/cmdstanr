# check_cmdstan_toolchain(fix = TRUE) is deprecated

    Code
      check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)
    Condition
      Warning:
      The 'fix' argument is deprecated as of CmdStanR 1.0.0 and will be removed in a future release.

# toolchain_PATH_env_var() rejects unsafe toolchain paths

    Code
      toolchain_PATH_env_var()
    Condition
      Error:
      ! The Windows toolchain path contains spaces or parentheses, and CmdStanR could not convert it to a usable short path. Please install or move the toolchain to a path without spaces or parentheses, restart R, and then run cmdstanr::check_cmdstan_toolchain().

# check_rtools4x_windows_toolchain() stops when no toolchain found

    Code
      check_rtools4x_windows_toolchain()
    Condition
      Error:
      ! CmdStanR could not find both make and a C++ compiler in R's configured toolchain or on PATH.
      Please install or reinstall the appropriate Rtools version for this R installation, or add a compatible toolchain to PATH,
      restart R, and then run cmdstanr::check_cmdstan_toolchain().

