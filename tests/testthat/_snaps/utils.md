# get_standalone_hpp() reports stanc failures

    Code
      get_standalone_hpp(stan_file, "--canonicalize='deprecations'")
    Message
      stanc: invalid canonicalize value
    Condition
      Error:
      ! stanc exited with status 124.
      Failed to generate the model C++ header.

# copy_temp_files retains sources if any copy fails

    Code
      copy_temp_files(current_paths = source_paths, new_dir = destination_dir,
        new_basename = "output", ids = 1:2, timestamp = FALSE, random = FALSE)
    Condition
      Error:
      ! Failed to move files: one or more files could not be copied. No original files were removed.

