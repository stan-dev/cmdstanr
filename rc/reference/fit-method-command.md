# Return the command that ran CmdStan

The `$command()` method returns the command line CmdStanR ran for each
CmdStan run. A few things to know before running one again:

- The paths are the ones R used. Input files CmdStanR wrote for the run
  (data passed as a list, inits, an inverse metric, the CSV files
  `$laplace()` and `$generate_quantities()` read) are in a temporary
  directory, even when `output_dir` is set, and don't outlive the R
  session.

- The output paths are this fit's CSV files, so running the line again
  overwrites them unless you change the paths after `output`.

- `threads_per_chain` (or `threads`) reaches CmdStan as the
  `STAN_NUM_THREADS` environment variable, not an argument, so set it
  first.

- `$sample_mpi()` runs the line through the MPI launcher, which the line
  doesn't include.

- On Windows CmdStanR puts the TBB library on `PATH` for the run, so the
  terminal needs it there too.

- On WSL the command runs inside the Linux distribution, where the paths
  are already spelled for it.

## Usage

``` r
command()
```

## Value

A character vector with one command per CmdStan run.

## Examples

``` r
# \dontrun{
fit <- cmdstanr_example("logistic", method = "sample")
fit$command()
#> [1] "'/tmp/RtmpOsjGdL/logistic' 'id=1' 'random' 'seed=1199584326' 'data' 'file=/home/runner/work/_temp/Library/cmdstanr/logistic.data.json' 'output' 'file=/tmp/RtmpOsjGdL/logistic-202610192200-01-6cf6ed.csv' 'profile_file=/tmp/RtmpOsjGdL/logistic-profile-202610192200-01-0a0f5a.csv' 'save_cmdstan_config=0' 'method=sample' 'save_warmup=0' 'algorithm=hmc' 'engine=nuts' 'adapt' 'engaged=1' 'save_metric=0'"
#> [2] "'/tmp/RtmpOsjGdL/logistic' 'id=2' 'random' 'seed=1199584326' 'data' 'file=/home/runner/work/_temp/Library/cmdstanr/logistic.data.json' 'output' 'file=/tmp/RtmpOsjGdL/logistic-202610192200-02-6cf6ed.csv' 'profile_file=/tmp/RtmpOsjGdL/logistic-profile-202610192200-02-0a0f5a.csv' 'save_cmdstan_config=0' 'method=sample' 'save_warmup=0' 'algorithm=hmc' 'engine=nuts' 'adapt' 'engaged=1' 'save_metric=0'"
#> [3] "'/tmp/RtmpOsjGdL/logistic' 'id=3' 'random' 'seed=1199584326' 'data' 'file=/home/runner/work/_temp/Library/cmdstanr/logistic.data.json' 'output' 'file=/tmp/RtmpOsjGdL/logistic-202610192200-03-6cf6ed.csv' 'profile_file=/tmp/RtmpOsjGdL/logistic-profile-202610192200-03-0a0f5a.csv' 'save_cmdstan_config=0' 'method=sample' 'save_warmup=0' 'algorithm=hmc' 'engine=nuts' 'adapt' 'engaged=1' 'save_metric=0'"
#> [4] "'/tmp/RtmpOsjGdL/logistic' 'id=4' 'random' 'seed=1199584326' 'data' 'file=/home/runner/work/_temp/Library/cmdstanr/logistic.data.json' 'output' 'file=/tmp/RtmpOsjGdL/logistic-202610192200-04-6cf6ed.csv' 'profile_file=/tmp/RtmpOsjGdL/logistic-profile-202610192200-04-0a0f5a.csv' 'save_cmdstan_config=0' 'method=sample' 'save_warmup=0' 'algorithm=hmc' 'engine=nuts' 'adapt' 'engaged=1' 'save_metric=0'"

# one line per chain
cat(fit$command(), sep = "\n")
#> '/tmp/RtmpOsjGdL/logistic' 'id=1' 'random' 'seed=1199584326' 'data' 'file=/home/runner/work/_temp/Library/cmdstanr/logistic.data.json' 'output' 'file=/tmp/RtmpOsjGdL/logistic-202610192200-01-6cf6ed.csv' 'profile_file=/tmp/RtmpOsjGdL/logistic-profile-202610192200-01-0a0f5a.csv' 'save_cmdstan_config=0' 'method=sample' 'save_warmup=0' 'algorithm=hmc' 'engine=nuts' 'adapt' 'engaged=1' 'save_metric=0'
#> '/tmp/RtmpOsjGdL/logistic' 'id=2' 'random' 'seed=1199584326' 'data' 'file=/home/runner/work/_temp/Library/cmdstanr/logistic.data.json' 'output' 'file=/tmp/RtmpOsjGdL/logistic-202610192200-02-6cf6ed.csv' 'profile_file=/tmp/RtmpOsjGdL/logistic-profile-202610192200-02-0a0f5a.csv' 'save_cmdstan_config=0' 'method=sample' 'save_warmup=0' 'algorithm=hmc' 'engine=nuts' 'adapt' 'engaged=1' 'save_metric=0'
#> '/tmp/RtmpOsjGdL/logistic' 'id=3' 'random' 'seed=1199584326' 'data' 'file=/home/runner/work/_temp/Library/cmdstanr/logistic.data.json' 'output' 'file=/tmp/RtmpOsjGdL/logistic-202610192200-03-6cf6ed.csv' 'profile_file=/tmp/RtmpOsjGdL/logistic-profile-202610192200-03-0a0f5a.csv' 'save_cmdstan_config=0' 'method=sample' 'save_warmup=0' 'algorithm=hmc' 'engine=nuts' 'adapt' 'engaged=1' 'save_metric=0'
#> '/tmp/RtmpOsjGdL/logistic' 'id=4' 'random' 'seed=1199584326' 'data' 'file=/home/runner/work/_temp/Library/cmdstanr/logistic.data.json' 'output' 'file=/tmp/RtmpOsjGdL/logistic-202610192200-04-6cf6ed.csv' 'profile_file=/tmp/RtmpOsjGdL/logistic-profile-202610192200-04-0a0f5a.csv' 'save_cmdstan_config=0' 'method=sample' 'save_warmup=0' 'algorithm=hmc' 'engine=nuts' 'adapt' 'engaged=1' 'save_metric=0'
# }
```
