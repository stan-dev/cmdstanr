% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cmdstanr-package.R
\docType{package}
\name{cmdstanr-package}
\alias{cmdstanr-package}
\alias{cmdstanr}
\alias{CmdStanR}
\title{CmdStanR: the R interface to CmdStan}
\description{
\if{html}{
   \figure{logo.png}{options: width="50" alt="https://mc-stan.org/about/logo/"}
   \emph{Stan Development Team}
}

\pkg{CmdStanR}: the \R interface to CmdStan.
}
\details{
CmdStanR (\pkg{cmdstanr} package) is an interface to Stan
(\href{https://mc-stan.org}{mc-stan.org}) for \R users. It provides the
necessary objects and functions to compile a Stan program and run Stan's
algorithms from \R via CmdStan, the shell interface to Stan
(\href{https://mc-stan.org/users/interfaces/cmdstan}{mc-stan.org/users/interfaces/cmdstan}).

\subsection{Different ways of interfacing with Stan’s C++}{

The RStan interface (\href{https://mc-stan.org/rstan/}{\strong{rstan}} package) is
an in-memory interface to Stan and relies on R packages like \strong{Rcpp}
and \strong{inline} to call C++ code from R. On the other hand, the CmdStanR
interface does not directly call any C++ code from R, instead relying on
the CmdStan interface behind the scenes for compilation, running
algorithms, and writing results to output files.
}

\subsection{Advantages of RStan}{
\itemize{
\item Allows other developers to distribute R packages with \emph{pre-compiled}
Stan programs (like \strong{rstanarm}) on CRAN. (Note: As of 2023, this can
mostly be achieved with CmdStanR as well. See \href{https://mc-stan.org/cmdstanr/articles/cmdstanr-internals.html#developing-using-cmdstanr}{Developing using CmdStanR}.)
\item Avoids use of R6 classes, which may result in more familiar syntax for
many R users.
\item CRAN binaries available for Mac and Windows.
}
}

\subsection{Advantages of CmdStanR}{
\itemize{
\item Compatible with latest versions of Stan. Keeping up with Stan releases
is complicated for RStan, often requiring non-trivial changes to the
\strong{rstan} package and new CRAN releases of both \strong{rstan} and
\strong{StanHeaders}. With CmdStanR the latest improvements in Stan will be
available from R immediately after updating CmdStan using
\code{cmdstanr::install_cmdstan()}.
\item Running Stan via external processes results in fewer unexpected
crashes, especially in RStudio.
\item Less memory overhead.
\item More permissive license. RStan uses the GPL-3 license while the
license for CmdStanR is BSD-3, which is a bit more permissive and is
the same license used for CmdStan and the Stan C++ source code.
}
}
}
\section{Getting started}{
 CmdStanR requires a working version of CmdStan. If
you already have CmdStan installed see \code{\link[=cmdstan_model]{cmdstan_model()}} to get started,
otherwise see \code{\link[=install_cmdstan]{install_cmdstan()}} to install CmdStan. The vignette
\href{https://mc-stan.org/cmdstanr/articles/cmdstanr.html}{\emph{Getting started with CmdStanR}}
demonstrates the basic functionality of the package.

For a list of global \link[base:options]{options} see
\link{cmdstanr_global_options}.
}

\examples{
\dontrun{
library(cmdstanr)
library(posterior)
library(bayesplot)
color_scheme_set("brightblue")

# Set path to CmdStan
# (Note: if you installed CmdStan via install_cmdstan() with default settings
# then setting the path is unnecessary but the default below should still work.
# Otherwise use the `path` argument to specify the location of your
# CmdStan installation.)
set_cmdstan_path(path = NULL)

# Create a CmdStanModel object from a Stan program,
# here using the example model that comes with CmdStan
file <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.stan")
mod <- cmdstan_model(file)
mod$print()
# Print with line numbers. This can be set globally using the
# `cmdstanr_print_line_numbers` option.
mod$print(line_numbers = TRUE)

# Data as a named list (like RStan)
stan_data <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))

# Run MCMC using the 'sample' method
fit_mcmc <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 2,
  parallel_chains = 2
)

# Use 'posterior' package for summaries
fit_mcmc$summary()

# Check sampling diagnostics
fit_mcmc$diagnostic_summary()

# Get posterior draws
draws <- fit_mcmc$draws()
print(draws)

# Convert to data frame using posterior::as_draws_df
as_draws_df(draws)

# Plot posterior using bayesplot (ggplot2)
mcmc_hist(fit_mcmc$draws("theta"))

# Run 'optimize' method to get a point estimate (default is Stan's LBFGS algorithm)
# and also demonstrate specifying data as a path to a file instead of a list
my_data_file <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.data.json")
fit_optim <- mod$optimize(data = my_data_file, seed = 123)
fit_optim$summary()

# Run 'optimize' again with 'jacobian=TRUE' and then draw from Laplace approximation
# to the posterior
fit_optim <- mod$optimize(data = my_data_file, jacobian = TRUE)
fit_laplace <- mod$laplace(data = my_data_file, mode = fit_optim, draws = 2000)
fit_laplace$summary()

# Run 'variational' method to use ADVI to approximate posterior
fit_vb <- mod$variational(data = stan_data, seed = 123)
fit_vb$summary()
mcmc_hist(fit_vb$draws("theta"))

# Run 'pathfinder' method, a new alternative to the variational method
fit_pf <- mod$pathfinder(data = stan_data, seed = 123)
fit_pf$summary()
mcmc_hist(fit_pf$draws("theta"))

# Run 'pathfinder' again with more paths, fewer draws per path,
# better covariance approximation, and fewer LBFGSs iterations
fit_pf <- mod$pathfinder(data = stan_data, num_paths=10, single_path_draws=40,
                         history_size=50, max_lbfgs_iters=100)

# Specifying initial values as a function
fit_mcmc_w_init_fun <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 2,
  refresh = 0,
  init = function() list(theta = runif(1))
)
fit_mcmc_w_init_fun_2 <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 2,
  refresh = 0,
  init = function(chain_id) {
    # silly but demonstrates optional use of chain_id
    list(theta = 1 / (chain_id + 1))
  }
)
fit_mcmc_w_init_fun_2$init()

# Specifying initial values as a list of lists
fit_mcmc_w_init_list <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 2,
  refresh = 0,
  init = list(
    list(theta = 0.75), # chain 1
    list(theta = 0.25)  # chain 2
  )
)
fit_optim_w_init_list <- mod$optimize(
  data = stan_data,
  seed = 123,
  init = list(
    list(theta = 0.75)
  )
)
fit_optim_w_init_list$init()
}

}
\seealso{
The CmdStanR website
(\href{https://mc-stan.org/cmdstanr/}{mc-stan.org/cmdstanr}) for online
documentation and tutorials.

The Stan and CmdStan documentation:
\itemize{
\item Stan documentation: \href{https://mc-stan.org/users/documentation/}{mc-stan.org/users/documentation}
\item CmdStan User’s Guide: \href{https://mc-stan.org/docs/cmdstan-guide/}{mc-stan.org/docs/cmdstan-guide}
}

Useful links:
\itemize{
  \item \url{https://mc-stan.org/cmdstanr/}
  \item \url{https://discourse.mc-stan.org}
  \item Report bugs at \url{https://github.com/stan-dev/cmdstanr/issues}
}

}
\author{
\strong{Maintainer}: Andrew Johnson \email{andrew.johnson@arjohnsonau.com} (\href{https://orcid.org/0000-0001-7000-8065}{ORCID})

Authors:
\itemize{
  \item Jonah Gabry \email{jsg2201@columbia.edu}
  \item Rok Češnovar \email{rok.cesnovar@fri.uni-lj.si}
  \item Steve Bronder
}

Other contributors:
\itemize{
  \item Ben Bales [contributor]
  \item Mitzi Morris [contributor]
  \item Mikhail Popov [contributor]
  \item Mike Lawrence [contributor]
  \item William Michael Landau \email{will.landau@gmail.com} (\href{https://orcid.org/0000-0003-1878-3253}{ORCID}) [contributor]
  \item Jacob Socolar [contributor]
  \item Martin Modrák [contributor]
  \item Ven Popov [contributor]
}

}
