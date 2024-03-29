% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit.R
\name{CmdStanLaplace}
\alias{CmdStanLaplace}
\title{CmdStanLaplace objects}
\description{
A \code{CmdStanLaplace} object is the fitted model object returned by the
\code{\link[=model-method-laplace]{$laplace()}} method of a
\code{\link{CmdStanModel}} object.
}
\section{Methods}{
 \code{CmdStanLaplace} objects have the following associated methods,
all of which have their own (linked) documentation pages.
\subsection{Extract contents of fitted model object}{\tabular{ll}{
   \strong{Method} \tab \strong{Description} \cr
   \code{\link[=fit-method-draws]{$draws()}} \tab Return approximate posterior draws as a \code{\link[posterior:draws_matrix]{draws_matrix}}. \cr
   \verb{$mode()} \tab Return the mode as a \code{\link{CmdStanMLE}} object. \cr
   \code{\link[=fit-method-lp]{$lp()}} \tab Return the total log probability density (\code{target}) computed in the model block of the Stan program. \cr
   \code{\link[=fit-method-lp]{$lp_approx()}} \tab Return the log density of the approximation to the posterior. \cr
   \code{\link[=fit-method-init]{$init()}} \tab Return user-specified initial values. \cr
   \code{\link[=fit-method-metadata]{$metadata()}} \tab Return a list of metadata gathered from the CmdStan CSV files. \cr
   \code{\link[=fit-method-code]{$code()}} \tab Return Stan code as a character vector. \cr
}

}

\subsection{Summarize inferences}{\tabular{ll}{
   \strong{Method} \tab \strong{Description} \cr
   \code{\link[=fit-method-summary]{$summary()}} \tab Run \code{\link[posterior:draws_summary]{posterior::summarise_draws()}}. \cr
}

}

\subsection{Save fitted model object and temporary files}{\tabular{ll}{
   \strong{Method} \tab \strong{Description} \cr
   \code{\link[=fit-method-save_object]{$save_object()}} \tab Save fitted model object to a file. \cr
   \code{\link[=fit-method-save_output_files]{$save_output_files()}} \tab Save output CSV files to a specified location. \cr
   \code{\link[=fit-method-save_data_file]{$save_data_file()}} \tab Save JSON data file to a specified location. \cr
   \code{\link[=fit-method-save_latent_dynamics_files]{$save_latent_dynamics_files()}} \tab Save diagnostic CSV files to a specified location. \cr
}

}

\subsection{Report run times, console output, return codes}{\tabular{ll}{
   \strong{Method} \tab \strong{Description} \cr
   \code{\link[=fit-method-time]{$time()}} \tab Report the run time of the Laplace sampling step. \cr
   \code{\link[=fit-method-output]{$output()}} \tab Pretty print the output that was printed to the console. \cr
   \code{\link[=fit-method-return_codes]{$return_codes()}} \tab Return the return codes from the CmdStan runs. \cr
}

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

Other fitted model objects: 
\code{\link{CmdStanDiagnose}},
\code{\link{CmdStanGQ}},
\code{\link{CmdStanMCMC}},
\code{\link{CmdStanMLE}},
\code{\link{CmdStanPathfinder}},
\code{\link{CmdStanVB}}
}
\concept{fitted model objects}
