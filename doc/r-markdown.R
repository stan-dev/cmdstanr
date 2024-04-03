params <-
list(EVAL = FALSE)

## ----settings-knitr, include = FALSE------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = if (isTRUE(exists("params"))) params$EVAL else FALSE
)

## ----register-engine, message=FALSE-------------------------------------------
#  library(cmdstanr)
#  register_knitr_engine(override = TRUE)

## ----print-ex1----------------------------------------------------------------
#  ex1$print()

## ----fit-ex1------------------------------------------------------------------
#  fit <- ex1$sample(
#    refresh = 0,
#    seed = 42L
#  )
#  
#  print(fit)

## ----register-engine-no-override----------------------------------------------
#  register_knitr_engine(override = FALSE)

