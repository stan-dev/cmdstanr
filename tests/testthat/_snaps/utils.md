# save_output_files() keeps the sources if any copy fails

    Code
      fit_mcmc$save_output_files(destination_dir, basename = "output", timestamp = FALSE,
        random = FALSE)
    Condition
      Error:
      ! Failed to move files: one or more files could not be copied. No original files were removed.

# local_make_local_backup() stops when file backup creation fails

    Code
      local({
        local_make_local_backup()
      })
    Condition
      Error:
      ! Could not create a verified recovery backup of CmdStan's 'make/local' at '<fake-cmdstan>/make/local.cmdstanr-test-backup'. 'make/local' was not modified.

# local_make_local_backup() stops when sentinel creation fails

    Code
      local({
        local_make_local_backup()
      })
    Condition
      Error:
      ! Could not create a verified recovery backup of CmdStan's 'make/local' at '<fake-cmdstan>/make/local.cmdstanr-test-backup'. 'make/local' was not modified.

# local_make_local_backup() retains a failed recovery backup

    Code
      local({
        local_make_local_backup()
        writeLines("MUTATED=true", make_local_path)
      })
    Condition
      Error:
      ! Could not restore CmdStan's 'make/local'. The recovery backup has been retained at '<fake-cmdstan>/make/local.cmdstanr-test-backup'.

# restore_cmdstan_make_local() preserves the backup when verification fails

    Code
      restore_cmdstan_make_local()
    Condition
      Error:
      ! Could not restore CmdStan's 'make/local'. The recovery backup has been retained at '<fake-cmdstan>/make/local.cmdstanr-test-backup'.

