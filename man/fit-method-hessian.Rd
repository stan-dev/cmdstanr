% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit.R
\name{fit-method-hessian}
\alias{fit-method-hessian}
\alias{hessian}
\title{Calculate the log-probability , the gradient w.r.t. each input, and the hessian
for a given vector of unconstrained parameters}
\usage{
hessian(unconstrained_variables, jacobian = TRUE, jacobian_adjustment = NULL)
}
\arguments{
\item{unconstrained_variables}{(numeric) A vector of unconstrained parameters.}

\item{jacobian}{(logical) Whether to include the log-density adjustments from
un/constraining variables.}

\item{jacobian_adjustment}{Deprecated. Please use \code{jacobian} instead.}
}
\description{
The \verb{$hessian()} method provides access to the Stan model's
\code{log_prob}, its derivative, and its hessian.
}
\examples{
\dontrun{
fit_mcmc <- cmdstanr_example("logistic", method = "sample", force_recompile = TRUE)
# fit_mcmc$init_model_methods(hessian = TRUE)
# fit_mcmc$hessian(unconstrained_variables = c(0.5, 1.2, 1.1, 2.2))
}

}
\seealso{
\code{\link[=log_prob]{log_prob()}}, \code{\link[=grad_log_prob]{grad_log_prob()}}, \code{\link[=constrain_variables]{constrain_variables()}},
\code{\link[=unconstrain_variables]{unconstrain_variables()}}, \code{\link[=unconstrain_draws]{unconstrain_draws()}}, \code{\link[=variable_skeleton]{variable_skeleton()}},
\code{\link[=hessian]{hessian()}}
}
