% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit.R
\name{fit-method-gradients}
\alias{fit-method-gradients}
\alias{gradients}
\title{Extract gradients after diagnostic mode}
\usage{
gradients()
}
\value{
A list of lists. See \strong{Examples}.
}
\description{
Return the data frame containing the gradients for all
parameters.
}
\examples{
\dontrun{
test <- cmdstanr_example("logistic", method = "diagnose")

# retrieve the gradients
test$gradients()
}

}
\seealso{
\code{\link{CmdStanDiagnose}}
}
