# Arguments for NUTS
SampleArgs <- R6::R6Class(
  "SampleArgs",
  lock_objects = FALSE,
  public = list(
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

      if (!is.null(self$save_warmup)) {
        self$save_warmup <- as.integer(self$save_warmup)
      }
      if (!is.null(self$adapt_engaged)) {
        self$save_warmup <- as.integer(self$adapt_engaged)
      }

      self$make_arg <- function(arg_name, idx = NULL) {
        .make_arg(self, arg_name, idx)
      }
      invisible(self)
    },

    # Validate cmdstan arguments (if not `NULL`)
    # @param num_chains Integer number of chains.
    # @return `self` invisibly unless an error is thrown.
    validate = function(num_chains) {
      checkmate::assert_integerish(num_chains,
                                   lower = 1,
                                   len = 1,
                                   any.missing = FALSE)

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
      checkmate::assert_integer(self$save_warmup,
                                lower = 0, upper = 1,
                                len = 1,
                                null.ok = TRUE)

      checkmate::assert_integer(self$adapt_engaged,
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

      checkmate::assert(
        combine = "or",
        checkmate::check_numeric(self$stepsize,
                                 lower = 0,
                                 len = 1,
                                 null.ok = TRUE),
        checkmate::check_numeric(self$stepsize,
                                 lower = 0,
                                 len = num_chains,
                                 null.ok = TRUE)
      )
      checkmate::assert(
        combine = "or",
        checkmate::check_string(self$metric, null.ok = TRUE),
        checkmate::check_character(self$metric, len = num_chains,
                                   null.ok = TRUE)
      )
      # TODO: implement other checks for metric from cmdstanpy:
      # https://github.com/stan-dev/cmdstanpy/blob/master/cmdstanpy/cmdstan_args.py#L130

      invisible(self)
    },

    # Compose CmdStan command for sampling-specific non-default arguments
    #
    # @param idx Integer chain id.
    # @param cmd Optionally, something to prepend to the returned command.
    # @return A character vector of CmdStan arguments for method="sample".
    compose = function(idx, cmd = NULL) {
      args <- list(
        cmd,
        "method=sample",
        self$make_arg("num_samples"),
        self$make_arg("num_warmup"),
        self$make_arg("save_warmup"),
        self$make_arg("thin"),
        "algorithm=hmc",
        self$make_arg("metric", idx),
        self$make_arg("stepsize", idx),
        "engine=nuts",
        self$make_arg("max_depth"),
        if (!is.null(self$adapt_delta) || !is.null(self$adapt_engaged))
          "adapt",
        self$make_arg("adapt_delta"),
        self$make_arg("adapt_engaged")
      )

      # convert list to character vector
      do.call(c, args)
    }
  )
)

OptimizeArgs <- R6::R6Class(
  "OptimizeArgs",
  lock_objects = FALSE,
  public = list(
    initialize = function(algorithm = NULL,
                          init_alpha = NULL,
                          iter = NULL) {
      self$algorithm <- algorithm
      self$init_alpha <- init_alpha
      self$iter <- iter
      self$make_arg <- function(arg_name, idx = NULL) {
        .make_arg(self, arg_name, idx)
      }
      invisible(self)
    },
    compose = function(idx, cmd) {
      args <- list(
        cmd,
        "method=optimize",
        self$make_arg("algorithm"),
        self$make_arg("init_alpha"),
        self$make_arg("iter")
      )
      do.call(c, args)
    },
    validate = function(num_chains) {
      checkmate::assert_subset(self$algorithm, empty.ok = TRUE,
                               choices = c("bfgs", "lbfgs", "newton"))
      checkmate::assert_integerish(self$iter, lower = 0, null.ok = TRUE)
      checkmate::assert_number(self$init_alpha, lower = 0, null.ok = TRUE)
      if (!is.null(self$init_alpha) && isTRUE(self$algorithm == "newton")) {
        stop("'init_alpha' must not be set when algorithm is 'newton'.",
             call. = FALSE)
      }
      invisible(self)
    }
  )
)

FixedParamArgs <- R6::R6Class(
  "FixedParamArgs",
  public = list(
    compose = function(idx, cmd) c(cmd, "method=fixed_param"),
    validate = function(num_chains) invisible(self)
  )
)

CmdStanArgs <- R6::R6Class(
  "CmdStanArgs",
  lock_objects = FALSE,
  public = list(
    method_args = NULL,
    initialize = function(model_name = NULL,
                          exe_file = NULL,
                          chain_ids = NULL,
                          data = NULL,
                          seed = NULL,
                          inits = NULL,
                          output_basename = NULL,
                          refresh = NULL,
                          method_args = NULL) {
      self$model_name <- model_name
      self$exe_file <- exe_file
      self$chain_ids <- chain_ids
      self$data <- method_args
      self$seed <- seed
      self$inits <- inits
      self$output_basename <- output_basename
      self$refresh <- refresh
      self$method_args <- method_args

      if (inherits(method_args, "SampleArgs")) {
        self$method <- "sample"
      } else if (inherits(method_args, "OptimizeArgs")) {
        self$method <- "optimize"
      } else if (inherits(method_args, "FixedParamArgs")) {
        self$method <- "fixed_param"
      } else if (inherits(method_args, "GenerateQuantitiesArgs")) {
        self$method <- "generate_quantities"
      }

      self$method_args$validate(num_chains = length(chain_ids))
      # self$validate()
    }

    # validate = function() {}
  )
)



# @param arg_name Name of slot in self containing the argument value
# @param idx Chain id if applicable.
.make_arg <- function(self, arg_name, idx = NULL) {
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


