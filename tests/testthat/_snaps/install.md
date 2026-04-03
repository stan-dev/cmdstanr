# toolchain checks on Unix work

    Code
      check_unix_cpp_compiler()
    Condition
      Error:
      ! C++ compiler missing.

---

    Code
      check_unix_make()
    Condition
      Error:
      ! make missing.

# check_rtools4x_windows_toolchain reports missing Rtools and make

    Code
      check_rtools4x_windows_toolchain()
    Condition
      Error:
      ! 
      Rtools<version> was not found but is required to run CmdStan with R version <version>.
      Please install or reinstall the appropriate Rtools version for this R installation,
      restart R, and then run cmdstanr::check_cmdstan_toolchain().

---

    Code
      check_rtools4x_windows_toolchain()
    Condition
      Error:
      ! 
      Rtools<version> is missing the required 'make' executable in <rtools_usr_bin>.
      Please reinstall the appropriate Rtools version for this R installation,
      restart R, and then run cmdstanr::check_cmdstan_toolchain().

