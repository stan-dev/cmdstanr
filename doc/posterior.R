params <-
list(EVAL = FALSE)

## ----settings-knitr, include=FALSE--------------------------------------------
stopifnot(require(knitr))
opts_chunk$set(
  # collapse = TRUE,
  dev = "png",
  dpi = 150,
  fig.asp = 0.618,
  fig.width = 5,
  out.width = "60%",
  fig.align = "center",
  comment = NA,
  eval = if (isTRUE(exists("params"))) params$EVAL else FALSE
)

## ----include=FALSE------------------------------------------------------------
#  # Needed temporarily to avoiding weird rendering of posterior's tibbles
#  # in pkgdown sites
#  print.tbl_df <- function(x, ...) {
#    print.data.frame(x)
#  }

## -----------------------------------------------------------------------------
#  fit <- cmdstanr::cmdstanr_example("schools", method = "sample")
#  fit$summary()

## -----------------------------------------------------------------------------
#  posterior::default_summary_measures()

## -----------------------------------------------------------------------------
#  fit$summary(variables = c("mu", "tau"))

## -----------------------------------------------------------------------------
#  fit$summary(variables = c("mu", "tau"), mean, sd)

## -----------------------------------------------------------------------------
#  fit$metadata()$model_params
#  fit$summary(variables = NULL, "mean", "median")

## -----------------------------------------------------------------------------
#  my_sd <- function(x) c(My_SD = sd(x))
#  fit$summary(
#    c("mu", "tau"),
#    MEAN = mean,
#    "median",
#    my_sd,
#    ~quantile(.x, probs = c(0.1, 0.9)),
#    Minimum = function(x) min(x)
#  )

## -----------------------------------------------------------------------------
#  fit$summary(c("mu", "tau"), quantile, .args = list(probs = c(0.025, .05, .95, .975)))

## -----------------------------------------------------------------------------
#  fit$summary(variables = NULL, dim, colMeans)

## -----------------------------------------------------------------------------
#  fit$summary(c("mu", "tau"), posterior::variance, ~var(as.vector(.x)))

## -----------------------------------------------------------------------------
#  strict_pos <- function(x) if (all(x > 0)) "yes" else "no"
#  fit$summary(variables = NULL, "Strictly Positive" = strict_pos)
#  # fit$print(variables = NULL, "Strictly Positive" = strict_pos)

## ----draws, message=FALSE-----------------------------------------------------
#  # default is a 3-D draws_array object from the posterior package
#  # iterations x chains x variables
#  draws_arr <- fit$draws() # or format="array"
#  str(draws_arr)
#  
#  # draws x variables data frame
#  draws_df <- fit$draws(format = "df")
#  str(draws_df)
#  print(draws_df)

