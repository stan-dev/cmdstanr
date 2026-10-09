#' @section Progress bar:
#' ### Setup
#' The bar needs two things: `show_progress_bar = TRUE`, and \pkg{progressr}'s
#' reporting turned on once per session with
#' `progressr::handlers(global = TRUE)`.
#'
#' CmdStanR shows one bar for all chains rather than one per chain, because
#' older versions of RStudio's console only support single-line bars. The
#' default is to show the progress bar instead of CmdStan's iteration lines,
#' but those can be kept with `show_iteration_messages = TRUE`. The per-chain
#' timing lines print once the bar is done.
#'
#' ### Choosing a bar
#' The default progress bar is very simple. To customize it, select one of the
#' many available handlers, for example `progressr::handlers("cli")`. The
#' \pkg{progressr} vignette on
#' [handlers](https://progressr.futureverse.org/articles/progressr-11-handlers.html)
#' lists the bars it ships and their options.
#'
#' ### Overhead and the `refresh` argument
#' The bar advances with CmdStan's iteration lines, one every `refresh`
#' iterations (CmdStan's default is 100). With `refresh = 0` CmdStan prints
#' none, so no bar is shown. A small `refresh` adds overhead, because CmdStanR
#' reads every line CmdStan prints, with or without the bar. For a model that
#' samples quickly the overhead can be noticeable. For a slow model it is small
#' next to the sampling time, and `refresh = 1` gives the smoothest bar.
#'
#' ### Quarto and R Markdown
#' In a Quarto or R Markdown document, write
#' `if (interactive()) progressr::handlers(global = TRUE)` in the setup chunk,
#' because the call fails while the document renders. Add
#' `options(cmdstanr_progress_bar = interactive())` to keep CmdStan's iteration
#' lines in the rendered output.
