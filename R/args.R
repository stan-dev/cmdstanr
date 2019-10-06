#' Internal objects for storing CmdStan arguments
#'
#' These objects store arguments for creating the call to CmdStan and provide a
#' `compose()` method for creating a character vector of arguments that can be
#' passed to the `args` argument of [processx::run()].
#'
#' @noRd
#' @details
#' A `CmdStanArgs` object stores arguments _not_ specific to particular methods,
#' as well as one of the following objects containing the method-specific
#' arguments:
#'
#' * `SampleArgs`: stores arguments specific to `method=sample`.
#' * `OptimizeArgs`: stores arguments specific to `method=optimize`.
#' * `FixedParamArgs`: not yet implemented.
#' * `GenerateQuantitiesArgs`: not yet implemented.
#' * `VariationalArgs`: not yet implemented.
#'
NULL


# CmdStanArgs -------------------------------------------------------------

CmdStanArgs <- R6::R6Class(
  "CmdStanArgs",
  lock_objects = FALSE,
  public = list(
    method_args = NULL, # this will be a SampleArgs object (or OptimizeArgs, etc.)
    initialize = function(model_name,
                          exe_file,
                          run_ids,
                          method_args,
                          data_file = NULL,
                          seed = NULL,
                          init = NULL,
                          refresh = NULL) {

      self$model_name <- model_name
      self$exe_file <- exe_file
      self$run_ids <- run_ids
      self$data_file <- data_file
      self$seed <- seed
      self$init <- init
      self$refresh <- refresh
      self$method_args <- method_args

      self$method <- self$method_args$method
      self$method_args$validate(num_runs = length(run_ids))
      self$validate()
    },

    validate = function() {
      # TODO: validate init (see python implementation)
      # TODO: validate that can write to output directory

      if (!length(self$exe_file) || !nzchar(self$exe_file)) {
        stop('Model not compiled. Try running the compile() method first.',
             call. = FALSE)
      }

      # at least 1 run id (chain id)
      checkmate::assert_integerish(self$run_ids,
                                   lower = 1,
                                   min.len = 1,
                                   any.missing = FALSE,
                                   null.ok = FALSE)

      checkmate::assert_integerish(self$refresh, lower = 0, null.ok = TRUE)
      checkmate::assert_file_exists(self$data_file)

      # either no seed, 1 seed, or num_runs seeds (number of chains)
      checkmate::assert(
        combine = "or",
        checkmate::check_integerish(self$seed,
                                    lower = -1,
                                    len = 1,
                                    null.ok = TRUE),
        checkmate::check_integerish(self$seed,
                                    lower = -1,
                                    len = length(self$run_ids),
                                    null.ok = TRUE)
      )

      if (is.null(self$seed)) {
        self$seed <- sample(.Machine$integer.max, length(self$run_ids))
      } else if (length(self$seed) == 1 && length(self$run_ids) > 1) {
        # if one seed but multiple chains then increment seed by 1 for each chain
        # TODO: is this ok?
        self$seed <- c(self$seed, self$seed + 1:(length(self$run_ids) -1))
      }
    },

    # create default basename for csv output file from model name and method
    csv_basename = function() {
      output_csv_basename(self$model_name, self$method)
    },

    # Compose character vector of all arguments to pass to CmdStan
    # @param idx The run id. For MCMC this is the chain id, for optimization
    #   this is just 1.
    # @param output_file File path to csv file where output will be written.
    compose_all_args = function(idx = NULL, output_file = NULL) {
      args <-
        list(
          id = NULL,
          seed = NULL,
          init = NULL,
          data = NULL,
          output = NULL
        )

      idx <- idx %||% 1
      if (!is.null(self$run_ids)) {
        if (idx < 0 || idx > length(self$run_ids)) {
          stop("Index (", idx, ") exceeds number of CmdStan runs",
               " (", length(self$run_ids), ").",
               call. = FALSE)
        }
        args$id <- paste0("id=", self$run_ids[idx])
      }

      if (!is.null(self$seed)) {
        args$seed <- c("random", paste0("seed=", self$seed[idx]))
      }

      if (!is.null(self$init)) {
        args$init <- paste0("init=", self$init[idx])
      }

      if (!is.null(self$data_file)) {
        args$data <- c("data", paste0("file=", self$data_file))
      }

      args$output <- c("output", paste0("file=", output_file))
      if (!is.null(self$refresh)) {
        args$output <- c(args$output, paste0("refresh=", self$refresh))
      }

      args <- do.call(c, append(args, list(use.names = FALSE)))
      self$method_args$compose(idx, args)
    },
    compose_command = function() {
      paste0(if (!os_is_windows()) "./", basename(self$exe_file))
    }
  )
)


# SampleArgs -------------------------------------------------------------

SampleArgs <- R6::R6Class(
  "SampleArgs",
  lock_objects = FALSE,
  public = list(
    method = "sample",

    # Initialize object
    # @note Leaving an argument as `NULL` means to use the CmdStan default.
    # @return `self` invisibly.
    initialize = function(num_warmup = NULL,
                          num_samples = NULL,
                          save_warmup = NULL,
                          thin = NULL,
                          max_depth = NULL,
                          metric = NULL,
                          stepsize = NULL,
                          adapt_engaged = NULL,
                          adapt_delta = NULL) {

      # TODO: cmdstanpy uses different names for these but these are same as
      # regular cmdstan for now
      self$num_warmup <- num_warmup
      self$num_samples <- num_samples

      self$save_warmup <- save_warmup
      self$thin <- thin
      self$max_depth <- max_depth
      self$metric <- metric
      self$metric_file <- character()
      self$stepsize <- stepsize # TODO: cmdstanpy uses step_size but cmdstan is stepsize
      self$adapt_engaged <- adapt_engaged
      self$adapt_delta <- adapt_delta

      if (is.logical(self$adapt_engaged)) {
        self$adapt_engaged <- as.integer(self$adapt_engaged)
      }
      if (is.logical(self$save_warmup)) {
        self$save_warmup <- as.integer(self$save_warmup)
      }
      invisible(self)
    },

    # Validate cmdstan arguments (if not `NULL`)
    # @param num_runs Integer number of CmdStan runs. This is the number of
    #   MCMC chains to run.
    # @return `self` invisibly unless an error is thrown.
    validate = function(num_runs) {
      checkmate::assert_integerish(num_runs,
                                   lower = 1,
                                   len = 1,
                                   any.missing = FALSE,
                                   .var.name = "Number of chains")

      checkmate::assert_integerish(self$thin,
                                   lower = 1,
                                   len = 1,
                                   null.ok = TRUE)
      checkmate::assert_integerish(self$num_samples,
                                   lower = 0,
                                   len = 1,
                                   null.ok = TRUE)
      checkmate::assert_integerish(self$num_warmup,
                                   lower = 0,
                                   len = 1,
                                   null.ok = TRUE)
      checkmate::assert_integerish(self$save_warmup,
                                   lower = 0, upper = 1,
                                   len = 1,
                                   null.ok = TRUE)

      checkmate::assert_integerish(self$adapt_engaged,
                                   lower = 0, upper = 1,
                                   len = 1,
                                   null.ok = TRUE)
      checkmate::assert_numeric(self$adapt_delta,
                                lower = 0, upper = 1,
                                len = 1,
                                null.ok = TRUE)
      checkmate::assert_integerish(self$max_depth,
                                   lower = 1,
                                   len = 1,
                                   null.ok = TRUE)

      if (length(self$stepsize) == 1) {
        checkmate::assert_number(self$stepsize, lower = .Machine$double.eps)
      } else {
        checkmate::assert_numeric(self$stepsize,
                                 lower = .Machine$double.eps,
                                 len = num_runs,
                                 null.ok = TRUE)
      }

      # TODO: implement other checks for metric from cmdstanpy:
      # https://github.com/stan-dev/cmdstanpy/blob/master/cmdstanpy/cmdstan_args.py#L130
      if (length(self$metric) == 1) {
        checkmate::assert_string(self$metric)
        checkmate::assert_subset(self$metric,
                                 empty.ok = FALSE,
                                 choices = c("unit_e", "diag_e", "dense_e"))
      } else {
        checkmate::assert_character(self$metric, len = num_runs, null.ok = TRUE)
      }

      invisible(self)
    },

    # Compose arguments to CmdStan command for sampling-specific
    # non-default arguments
    #
    # @param idx Integer chain id.
    # @param args A character vector of arguments to prepend to the returned
    #   character vector. This will get passed in from CmdStanArgs$compose_all_args().
    # @return A character vector of CmdStan arguments.
    compose = function(idx, args = NULL) {

      # Helper function to make sampler arguments
      # @param arg_name Name of slot in self containing the argument value
      # @param idx Chain id if applicable.
      .make_arg <- function(arg_name, idx = NULL) {
        val <- self[[arg_name]]
        if (is.null(val)) {
          return(NULL)
        }
        if (!is.null(idx) && length(val) >= idx) {
          val <- val[idx]
        }
        arg_name <- sub("adapt_", "", arg_name) # e.g. adapt_delta -> delta
        paste0(arg_name, "=", val)
      }

      new_args <- list(
        "method=sample",
        .make_arg("num_samples"),
        .make_arg("num_warmup"),
        .make_arg("save_warmup"),
        .make_arg("thin"),
        "algorithm=hmc",
        .make_arg("metric", idx),
        .make_arg("stepsize", idx),
        "engine=nuts",
        .make_arg("max_depth"),
        if (!is.null(self$adapt_delta) || !is.null(self$adapt_engaged))
          "adapt",
        .make_arg("adapt_delta"),
        .make_arg("adapt_engaged")
      )

      # convert list to character vector
      new_args <- do.call(c, new_args)
      c(args, new_args)
    }
  )
)


# OptimizeArgs -------------------------------------------------------------

OptimizeArgs <- R6::R6Class(
  "OptimizeArgs",
  lock_objects = FALSE,
  public = list(
    method = "optimize",
    initialize = function(algorithm = NULL,
                          init_alpha = NULL,
                          iter = NULL) {
      self$algorithm <- algorithm
      self$init_alpha <- init_alpha
      self$iter <- iter
      invisible(self)
    },

    validate = function(num_runs) {
      checkmate::assert_subset(self$algorithm, empty.ok = TRUE,
                               choices = c("bfgs", "lbfgs", "newton"))
      checkmate::assert_integerish(self$iter, lower = 0, null.ok = TRUE)
      checkmate::assert_number(self$init_alpha, lower = 0, null.ok = TRUE)
      if (!is.null(self$init_alpha) && isTRUE(self$algorithm == "newton")) {
        stop("'init_alpha' must not be set when algorithm is 'newton'.",
             call. = FALSE)
      }
      invisible(self)
    },

    # Compose arguments to CmdStan command for optimization-specific
    # non-default arguments
    #
    # @param idx Integer chain id. This is ignored but needed do `compose()`
    #   has the same signature as for `SampleArgs`.
    # @param args A character vector of arguments to prepend to the returned
    #   character vector. This will get passed in from
    #   CmdStanArgs$compose_all_args().
    # @return A character vector of CmdStan arguments.
    compose = function(idx = NULL, args = NULL) {
      new_args <- list(
        "method=optimize",
        if (!is.null(self$algorithm)) paste0("algorithm=", self$algorithm),
        if (!is.null(self$init_alpha)) paste0("init_alpha=", self$init_alpha),
        if (!is.null(self$iter)) paste0("iter=", self$iter)
      )
      new_args <- do.call(c, new_args)
      c(args, new_args)
    }
  )
)

# FixedParamArgs -------------------------------------------------------------

FixedParamArgs <- R6::R6Class(
  "FixedParamArgs",
  public = list(
    method = "fixed_param",
    compose = function(idx, args = NULL) c(args, "method=fixed_param"),
    validate = function(num_runs) invisible(self)
  )
)


