# threading works with pathfinder()

    Code
      invisible(do.call(mod$pathfinder, pathfinder_args))
    Condition
      Warning:
      'num_threads' is deprecated as of CmdStanR 1.0.0 and will be removed in a future release. Please use 'threads' instead.

# executable metadata takes precedence over compile options

    Code
      mod$sample(data = data_file_json, chains = 1)
    Condition
      Error:
      ! The model executable was built with threading enabled but 'threads_per_chain' was not set!

