# compile() performs stanc checks during dry runs

    Code
      model$compile(force_recompile = TRUE, dry_run = TRUE)
    Condition
      Error:
      ! stanc exited with status 1.
      Failed to generate the model C++ header.

