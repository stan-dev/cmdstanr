# get_standalone_hpp() reports stanc failures

    Code
      get_standalone_hpp(stan_file, "--canonicalize='deprecations'")
    Message
      stanc: invalid canonicalize value
    Condition
      Error:
      ! stanc exited with status 124.
      Failed to generate the model C++ header.

# get_standalone_hpp() suggests formatting deprecated syntax

    Code
      get_standalone_hpp(stan_file, character())
    Message
      Syntax error: Use the auto-format flag to stanc
    Condition
      Error:
      ! stanc exited with status 1.
      Failed to generate the model C++ header.
      To fix deprecated or removed syntax please see ?cmdstanr::format for an example.

# copy_temp_files retains sources if any copy fails

    Code
      copy_temp_files(current_paths = source_paths, new_dir = destination_dir,
        new_basename = "output", ids = 1:2, timestamp = FALSE, random = FALSE)
    Condition
      Error:
      ! Failed to move files: one or more files could not be copied. No original files were removed.

