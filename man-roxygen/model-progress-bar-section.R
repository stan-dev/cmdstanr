#' @section Progress bar:
#' With `show_progress_bar = TRUE`, the method signals one progression across
#' all chains through \pkg{progressr} and, by default, hides CmdStan's
#' iteration lines. \pkg{progressr} shows nothing until you turn its reporting
#' on, once per session, with `progressr::handlers(global = TRUE)`. To pick
#' which bar it draws, call for example `progressr::handlers("cli")` (see
#' [progressr::handlers()]). There is one bar for all chains because RStudio's
#' terminal only supports single-line bars.
#'
#' The bar advances once every `refresh` iterations, and with `refresh = 0`
#' CmdStan prints no iteration lines, so no bar is shown. A small `refresh`
#' adds overhead, because CmdStanR reads every line CmdStan prints, with or
#' without the bar. For a model that samples quickly the overhead is
#' noticeable. For a slow model it is small next to the sampling time, and
#' `refresh = 1` gives the smoothest bar.
#'
#' In a Quarto or R Markdown document, write
#' `if (interactive()) progressr::handlers(global = TRUE)` in the setup chunk,
#' because the call fails while the document renders. Add
#' `options(cmdstanr_progress_bar = interactive())` to keep CmdStan's iteration
#' lines in the rendered output.
