# install_cmdstan() errors if invalid version or URL

    Download of CmdStan failed with error: cannot open URL 'https://github.com/stan-dev/cmdstan/releases/download/v2.35.5/cmdstan-2.35.5.tar.gz'
    Please check if the supplied version number is valid.

---

    Download of CmdStan failed with error: cannot open URL 'https://github.com/stan-dev/cmdstan/releases/download/v2.35.5/cmdstan-2.35.5.tar.gz'
    Please check if the supplied release URL is valid.

---

    https://github.com/stan-dev/cmdstan/releases/tag/v2.24.0 is not a .tar.gz archive!cmdstanr supports installing from .tar.gz archives only.

# toolchain checks on Unix work

    A C++ compiler was not found. Please install the 'clang++' or 'g++' compiler, restart R, and run cmdstanr::check_cmdstan_toolchain().

---

    The 'make' tool was not found. Please install 'make', restart R, and then run cmdstanr::check_cmdstan_toolchain().

# check_rtools4x_windows_toolchain reports missing Rtools and make

    
    Rtools44 was not found but is required to run CmdStan with R version 4.5.2.
    Please install or reinstall the appropriate Rtools version for this R installation,
    restart R, and then run cmdstanr::check_cmdstan_toolchain().

# check_rtools4x_windows_toolchain validates install path and empty candidates

    
    Rtools44 is installed in a path with spaces or brackets, which is not supported.
    Please reinstall the appropriate Rtools version for this R installation to a valid path,
    restart R, and then run cmdstanr::check_cmdstan_toolchain().

# check_cmdstan_toolchain(fix = TRUE) is deprecated

    The 'fix' argument is deprecated and will be removed in a future release.

