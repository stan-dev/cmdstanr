# install_executable() leaves the destination alone if staging fails

    Code
      install_executable(fixture$from, fixture$to, fixture$record)
    Condition
      Error:
      ! Could not stage the compiled executable at '<dir>/exe-new-<random>'. The model executable at '<dir>/model-exe' was not modified.

# install_executable() leaves the destination alone if the backup fails

    Code
      install_executable(fixture$from, fixture$to, fixture$record)
    Condition
      Error:
      ! Could not install the compiled executable at '<dir>/model-exe': Could not move '<dir>/model-exe' to '<dir>/exe-old-<random>'. The executable and build record there are as they were.

# install_executable() restores the backup if the install fails

    Code
      install_executable(fixture$from, fixture$to, fixture$record)
    Condition
      Error:
      ! Could not install the compiled executable at '<dir>/model-exe': Could not move '<dir>/exe-new-<random>' to '<dir>/model-exe'. The executable and build record there are as they were.

# install_executable() keeps the backup if it cannot be restored

    Code
      install_executable(fixture$from, fixture$to, fixture$record)
    Condition
      Error:
      ! Could not install the compiled executable at '<dir>/model-exe': Could not move '<dir>/exe-new-<random>' to '<dir>/model-exe'. The previous executable and build record could not all be put back. Files left behind: '<dir>/exe-old-<random>'.

