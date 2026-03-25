# Setting bad path leads to warning (can't find directory)

    Path not set. Can't find directory: BAD_PATH

# Setting bad path from env leads to warning (can't find directory)

    Can't find directory specified by environment variable 'CMDSTAN'. Path not set.

# Existing CMDSTAN env path with no install resets cached state

    No CmdStan installation found in the path specified by the environment variable 'CMDSTAN'.

# Getting missing path leads to error (path not set)

    CmdStan path has not been set yet. See ?set_cmdstan_path.

# cmdstan_version() behaves correctly when version is not set

    CmdStan path has not been set yet. See ?set_cmdstan_path.

# Warning message is thrown if can't detect version number

    Can't find CmdStan makefile to detect version number. Path may not point to valid installation.

# Setting path rejects unsupported CmdStan versions

    CmdStan path not set. CmdStan v2.34.0 is no longer supported. cmdstanr now requires CmdStan v2.35.0 or newer.

