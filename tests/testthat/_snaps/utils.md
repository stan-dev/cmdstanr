# get_standalone_hpp() reports stanc failures

    Code
      get_standalone_hpp(stan_file, "--canonicalize='deprecations'")
    Message
      stanc: invalid canonicalize value
    Condition
      Error:
      ! An error occurred during compilation! See the message above for more information. (stanc exited with status 124)

# get_standalone_hpp() suggests formatting deprecated syntax

    Code
      get_standalone_hpp(stan_file, character())
    Message
      Syntax error: Use the auto-format flag to stanc
    Condition
      Error:
      ! An error occurred during compilation! See the message above for more information. (stanc exited with status 1)
      To fix deprecated or removed syntax please see ?cmdstanr::format for an example.

# copy_temp_files retains sources if any copy fails

    Code
      copy_temp_files(current_paths = source_paths, new_dir = destination_dir,
        new_basename = "output", ids = 1:2, timestamp = FALSE, random = FALSE)
    Condition
      Error:
      ! Failed to move files: one or more files could not be copied. No original files were removed.

# install_executable() leaves the destination alone if staging fails

    Code
      install_executable(fixture$from, fixture$to)
    Condition
      Error:
      ! Could not stage the compiled executable at '<dir>/exe-new-<random>'. The model executable at '<dir>/model-exe' was not modified.

# install_executable() leaves the destination alone if the backup fails

    Code
      install_executable(fixture$from, fixture$to)
    Condition
      Error:
      ! Could not move the existing executable '<dir>/model-exe' aside. It was not modified.

# install_executable() restores the backup if the install fails

    Code
      install_executable(fixture$from, fixture$to)
    Condition
      Error:
      ! Could not install the compiled executable at '<dir>/model-exe'. The previously compiled executable has been restored.

# install_executable() keeps the backup if it cannot be restored

    Code
      install_executable(fixture$from, fixture$to)
    Condition
      Error:
      ! Could not install the compiled executable at '<dir>/model-exe' and the previously compiled executable could not be restored. It has been kept at '<dir>/exe-old-<random>'.

