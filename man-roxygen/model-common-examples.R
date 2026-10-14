#' @examples
#' \dontrun{
#' # The rest of these examples work the same way for every fitting method.
#'
#' # Draws as a posterior draws object
#' fit$draws()
#' fit$draws("theta", format = "df")
#'
#' # Data as a path to a JSON or R dump file instead of a list
#' data_file <- file.path(
#'   cmdstan_path(),
#'   "examples/bernoulli/bernoulli.data.json"
#' )
#' fit <- mod$<%= method %>(data = data_file, seed = 123, <%= quiet %>)
#'
#' # Initial values from a function
#' fit <- mod$<%= method %>(
#'   data = stan_data,
#'   seed = 123,
#'   init = function() list(theta = runif(1)),
#'   <%= quiet %>
#' )
#' fit$init()
#'
<% if (method == "sample") { %>
#' # Initial values as one list per chain
#' fit <- mod$sample(
#'   data = stan_data,
#'   seed = 123,
#'   chains = 2,
#'   refresh = 0,
#'   init = list(
#'     list(theta = 0.75), # chain 1
#'     list(theta = 0.25)  # chain 2
#'   )
#' )
#' fit$init()
#'
<% } %>
#' # Initial values from a previous fit (can be a fit from a different method)
#' fit2 <- mod$<%= method %>(
#'   data = stan_data,
#'   seed = 123,
#'   init = fit,
#'   <%= quiet %>
#' )
#'
#' # Initial values from a posterior::draws object
#' inits <- fit$draws()
#' fit <- mod$<%= method %>(
#'   data = stan_data,
#'   seed = 123,
#'   init = inits,
#'   <%= quiet %>
#' )
#'
#' # Suppress output from CmdStan while it runs
#' fit <- mod$<%= method %>(
#'   data = stan_data,
#'   seed = 123,
#'   show_messages = FALSE,
#'   show_exceptions = FALSE
#' )
#'
#' # The CSV files CmdStan wrote, in a temporary directory by default. Move
#' # them to keep them past the R session. Replace tempdir below with a
#' # directory you'll keep.
#' fit$output_files()
#'
#' my_dir <- tempdir()
#' fit$save_output_files(dir = my_dir, basename = "bernoulli")
#'
#' # Or use output_dir to have CmdStan write them there in the first place,
#' # here gzip-compressed to save disk space
#' fit <- mod$<%= method %>(
#'   data = stan_data,
#'   seed = 123,
#'   output_dir = my_dir,
#'   output_basename = "bernoulli",
#'   compress = "gzip",
#'   <%= quiet %>
#' )
#' fit$output_files()
#' }
