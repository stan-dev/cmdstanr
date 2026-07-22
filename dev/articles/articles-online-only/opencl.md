# Running Stan on the GPU with OpenCL

## Introduction

This vignette demonstrates how to use the OpenCL capabilities of CmdStan
with CmdStanR.

OpenCL may provide speedups for vectorized probability distribution
functions (functions with the `_lpdf` or `_lpmf` suffix), especially
with large input variables. The crossover point depends on the function
and hardware.

The actual speedup for a model will depend on the particular `lpdf/lpmf`
functions used and whether the `lpdf/lpmf` functions are the bottlenecks
of the model. The more computationally complex the function is, the
larger the expected speedup. The biggest speedups are expected when
using the specialized GLM functions.

In order to establish the bottlenecks in your model we recommend using
[profiling](https://mc-stan.org/cmdstanr/articles/profiling.html).

## OpenCL runtime

OpenCL is supported on most modern CPUs and GPUs. In order to use OpenCL
in CmdStanR, an OpenCL runtime for the target device must be installed.
A guide for the most common devices is available in the CmdStan manual’s
[chapter on
parallelization](https://mc-stan.org/docs/cmdstan-guide/parallelization.html#opencl).

On Windows, CmdStan’s linker must be able to find the `OpenCL.lib`
library installed by the OpenCL runtime. CmdStan automatically links
against OpenCL, but if the library directory is not on the linker’s
search path, add it to `LDFLAGS_OPENCL` in CmdStan’s `make/local` file.
The required path depends on the installed runtime.

This change persists in `make/local`, so inspect the existing
configuration before modifying it:

``` r

# Check the current contents of make/local without modifying them
cmdstan_make_local()

opencl_lib_dir <- "C:/path/to/directory/containing/OpenCL.lib"
cmdstan_make_local(
  cpp_options = list(
    LDFLAGS_OPENCL = paste0('-L"', opencl_lib_dir, '"')
  )
)
```

## Compiling a model with OpenCL

By default, models in CmdStanR are compiled *without* OpenCL support.
Once OpenCL support is enabled, a CmdStan model will make use of OpenCL
if the functions in the model support it. Technically no changes to a
model are required to support OpenCL since the choice of using OpenCL is
handled by the compiler, but it can still be useful to rewrite a model
to be more OpenCL-friendly by using vectorization as much as possible
when using probability distributions.

Consider a simple logistic regression with parameters `alpha` and
`beta`, covariates `X`, and outcome `y`.

    data {
      int<lower=1> k;
      int<lower=0> n;
      matrix[n, k] X;
      array[n] int y;
    }
    parameters {
      vector[k] beta;
      real alpha;
    }
    model {
      target += std_normal_lpdf(beta);
      target += std_normal_lpdf(alpha);
      target += bernoulli_logit_glm_lpmf(y | X, alpha, beta);
    }

Some fake data will be useful to run this model:

``` r

library(cmdstanr)

# Generate some fake data
set.seed(123)
n <- 250000
k <- 20
X <- matrix(rnorm(n * k), ncol = k)
y <- rbinom(n, size = 1, prob = plogis(3 * X[,1] - 2 * X[,2] + 1))
mdata <- list(k = k, n = n, y = y, X = X)
```

In this model, most of the computation will be handled by the
`bernoulli_logit_glm_lpmf` function. Because this is a supported GPU
function, it should be possible to accelerate it with OpenCL. Check
[here](https://mc-stan.org/math/md_doxygen_2parallelism__support_2opencl__support.html)
for a list of functions with OpenCL support.

To build the model with OpenCL support, add
`cpp_options = list(stan_opencl = TRUE)` at the compilation step.

``` r

# Compile the model with STAN_OPENCL=TRUE
mod_cl <- cmdstan_model("opencl-files/bernoulli_logit_glm.stan",
                        cpp_options = list(stan_opencl = TRUE),
                        force_recompile = TRUE)
```

For an integrated GPU, also set `integrated_opencl = TRUE` in
`cpp_options`.

## Running models with OpenCL

When multiple OpenCL platforms or devices are available, their IDs can
be specified to select which device to use. If the system has one GPU
and no OpenCL CPU runtime, the default platform and device IDs of `0`
will typically select the GPU, so the IDs can be omitted. The `clinfo`
tool can be used to figure out for sure which devices are available.

On an Ubuntu system with both CPU and GPU OpenCL support, `clinfo -l`
outputs:

    Platform #0: AMD Accelerated Parallel Processing
     `-- Device #0: gfx906+sram-ecc
    Platform #1: Intel(R) CPU Runtime for OpenCL(TM) Applications
     `-- Device #0: Intel(R) Core(TM) i7-4790 CPU @ 3.60GHz

On this system the GPU is platform ID 0 and device ID 0, while the CPU
is platform ID 1, device ID 0. These can be specified with the
`opencl_ids` argument when running a model. The `opencl_ids` argument is
supplied as a vector of length 2, where the first element is the
platform ID and the second element is the device ID.

``` r

fit_cl <- mod_cl$sample(data = mdata, seed = 123, chains = 4, parallel_chains = 4,
                        opencl_ids = c(0, 0), refresh = 0)
```

We’ll also run a version without OpenCL and compare the run times.

``` r

# no OpenCL version
mod <- cmdstan_model("opencl-files/bernoulli_logit_glm.stan", force_recompile = TRUE)
fit_cpu <- mod$sample(data = mdata, seed = 123, chains = 4,
                      parallel_chains = 4, refresh = 0)
```

The speedup of the OpenCL model is:

``` r

fit_cpu$time()$total / fit_cl$time()$total
```

This ratio is illustrative. It depends on the particular GPU and CPU,
the input problem sizes, whether GPU-supported functions dominate the
computation, and how concurrent chains share the OpenCL device.
