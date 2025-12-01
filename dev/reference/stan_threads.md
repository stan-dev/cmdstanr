# Set or get the number of threads used to execute Stan models

DEPRECATED. Please use the `threads_per_chain` argument when fitting the
model.

## Usage

``` r
num_threads()

set_num_threads(num_threads)
```

## Arguments

- num_threads:

  (positive integer) The number of threads to set.

## Value

The value of the environment variable `STAN_NUM_THREADS`.
