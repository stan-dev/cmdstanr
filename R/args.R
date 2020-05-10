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
#' * `VariationalArgs`: stores arguments specific to `method=variational`
#' * `GQArgs`: not yet implemented.
#' * `FixedParamArgs`: not yet implemented.
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
                          save_extra_diagnostics = FALSE,
                          seed = NULL,
                          init = NULL,
                          refresh = NULL,
                          output_dir = NULL) {

      self$model_name <- model_name
      self$exe_file <- exe_file
      self$run_ids <- run_ids
      self$data_file <- data_file
      self$seed <- seed
      self$init <- init
      self$refresh <- refresh
      self$method_args <- method_args
      self$method <- self$method_args$method
      self$save_extra_diagnostics <- save_extra_diagnostics
      if (getRversion() < '3.5.0') {
        self$output_dir <- output_dir %||% tempdir()
      } else {
        self$output_dir <- output_dir %||% tempdir(check = TRUE)
      }

      self$method_args$validate(num_runs = length(self$run_ids))
      self$validate()
    },
    validate = function() {
      validate_cmdstan_args(self)
      self$output_dir <- absolute_path(self$output_dir)
      if (is.character(self$data_file)) {
        self$data_file <- absolute_path(self$data_file)
      }
      if (is.character(self$init)) {
        self$init <- absolute_path(self$init)
      }
      self$init <- maybe_recycle_init(self$init, length(self$run_ids))
      self$seed <- maybe_generate_seed(self$seed, length(self$run_ids))
      invisible(self)
    },

    new_file_names = function(type = c("output", "diagnostic")) {
      basename <- self$model_name
      type <- match.arg(type)
      if (type == "diagnostic") {
        basename <- paste0(basename, "-diagnostic")
      }
      generate_file_names( # defined in utils.R
        basename = basename,
        ext = ".csv",
        ids = self$run_ids,
        timestamp = TRUE,
        random = TRUE
      )
    },
    new_files = function(type = c("output", "diagnostic")) {
      files <- file.path(self$output_dir, self$new_file_names(type))
      invisible(file.create(files))
      files
    },

    # Compose all arguments to pass to CmdStan
    #
    # @param idx The run id. For MCMC this is the chain id, for optimization
    #   this is just 1.
    # @param output_file File path to csv file where output will be written.
    # @param diagnostic_file File path to csv file where diagnostics will be written.
    # @return Character vector of arguments of the form "name=value".
    #
    compose_all_args = function(idx = NULL,
                                output_file = NULL,
                                diagnostic_file = NULL) {
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
      if (!is.null(diagnostic_file)) {
        args$output <- c(args$output, paste0("diagnostic_file=", diagnostic_file))
      }
      if (!is.null(self$refresh)) {
        args$output <- c(args$output, paste0("refresh=", self$refresh))
      }

      args <- do.call(c, append(args, list(use.names = FALSE)))
      self$method_args$compose(idx, args)
    },
    command = function() {
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
    initialize = function(num_warmup = NULL,
                          num_samples = NULL,
                          save_warmup = NULL,
                          thin = NULL,
                          max_depth = NULL,
                          adapt_engaged = NULL,
                          adapt_delta = NULL,
                          stepsize = NULL,
                          metric = NULL,
                          metric_file = NULL,
                          inv_metric = NULL,
                          init_buffer = NULL,
                          term_buffer = NULL,
                          window = NULL,
                          fixed_param = FALSE) {

      # TODO: cmdstanpy uses different names for these but these are same as
      # regular cmdstan for now
      self$num_warmup <- num_warmup
      self$num_samples <- num_samples

      self$save_warmup <- save_warmup
      self$thin <- thin
      self$max_depth <- max_depth
      self$adapt_engaged <- adapt_engaged
      self$adapt_delta <- adapt_delta
      self$stepsize <- stepsize # TODO: cmdstanpy uses step_size but cmdstan is stepsize
      self$metric <- metric
      self$inv_metric <- inv_metric
      self$fixed_param <- fixed_param
      if (!is.null(inv_metric)) {
        if (!is.null(metric_file)) {
          stop("Only one of inv_metric and metric_file can be specified.",
               call. = FALSE)
        }

        # wrap inv_metric in list if not in one
        if (!is.list(inv_metric)) {
          inv_metric <- list(inv_metric)
        }

        # write all inv_metrics to disk
        inv_metric_paths <-
          tempfile(
            pattern = paste0("inv_metric-", seq_along(inv_metric), "-"),
            tmpdir = cmdstan_tempdir(),
            fileext = ".json"
          )
        for (i in seq_along(inv_metric_paths)) {
          write_stan_json(list(inv_metric = inv_metric[[i]]), inv_metric_paths[i])
        }

        self$metric_file <- inv_metric_paths
      } else if (!is.null(metric_file)) {
        self$metric_file <- sapply(metric_file, absolute_path)
      }
      self$init_buffer <- init_buffer
      self$term_buffer <- term_buffer
      self$window <- window

      if (is.logical(self$adapt_engaged)) {
        self$adapt_engaged <- as.integer(self$adapt_engaged)
      }
      if (is.logical(self$save_warmup)) {
        self$save_warmup <- as.integer(self$save_warmup)
      }
      invisible(self)
    },
    validate = function(num_runs) {
      validate_sample_args(self, num_runs)
      self$metric_file <- maybe_recycle_metric_file(self$metric_file, num_runs)
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
      .make_arg <- function(arg_name, idx = NULL) {
        compose_arg(self, arg_name, idx)
      }

      if (self$fixed_param) {
        new_args <- list(
          "method=sample",
          .make_arg("num_samples"),
          .make_arg("num_warmup"),
          .make_arg("save_warmup"),
          .make_arg("thin"),
          "algorithm=fixed_param",
          .make_arg("metric"),
          .make_arg("metric_file", idx),
          .make_arg("stepsize", idx),
          .make_arg("max_depth"),
          if (!is.null(self$adapt_delta) || !is.null(self$adapt_engaged))
            "adapt",
          .make_arg("adapt_delta"),
          .make_arg("adapt_engaged"),
          .make_arg("init_buffer"),
          .make_arg("term_buffer"),
          .make_arg("window")
        )
      } else {
        new_args <- list(
          "method=sample",
          .make_arg("num_samples"),
          .make_arg("num_warmup"),
          .make_arg("save_warmup"),
          .make_arg("thin"),
          "algorithm=hmc",
          .make_arg("metric"),
          .make_arg("metric_file", idx),
          .make_arg("stepsize", idx),
          "engine=nuts",
          .make_arg("max_depth"),
          if (!is.null(self$adapt_delta) || !is.null(self$adapt_engaged))
            "adapt",
          .make_arg("adapt_delta"),
          .make_arg("adapt_engaged"),
          .make_arg("init_buffer"),
          .make_arg("term_buffer"),
          .make_arg("window")
        )
      }

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
      validate_optimize_args(self)
      invisible(self)
    },

    # Compose arguments to CmdStan command for optimization-specific
    # non-default arguments. Works the same way as compose for sampler args,
    # but `idx` is ignored (no multiple chains for optimize or variational)
    compose = function(idx = NULL, args = NULL) {
      .make_arg <- function(arg_name) {
        compose_arg(self, arg_name, idx = NULL)
      }
      new_args <- list(
        "method=optimize",
        .make_arg("algorithm"),
        .make_arg("init_alpha"),
        .make_arg("iter")
      )
      new_args <- do.call(c, new_args)
      c(args, new_args)
    }
  )
)


# VariationalArgs ---------------------------------------------------------

VariationalArgs <- R6::R6Class(
  "VariationalArgs",
  lock_objects = FALSE,
  public = list(
    method = "variational",
    initialize = function(algorithm = NULL,
                          iter = NULL,
                          grad_samples = NULL,
                          elbo_samples = NULL,
                          eta = NULL,
                          adapt_engaged = NULL,
                          adapt_iter = NULL,
                          tol_rel_obj = NULL,
                          eval_elbo = NULL,
                          output_samples = NULL) {
      self$algorithm <- algorithm
      self$iter <- iter
      self$grad_samples <- grad_samples
      self$elbo_samples <- elbo_samples
      self$eta <- eta
      self$tol_rel_obj <- tol_rel_obj
      self$eval_elbo <- eval_elbo
      self$output_samples <- output_samples
      self$adapt_iter <- adapt_iter
      self$adapt_engaged <- adapt_engaged

      if (is.logical(self$adapt_engaged)) {
        self$adapt_engaged <- as.integer(self$adapt_engaged)
      }

      invisible(self)
    },

    validate = function(num_runs) {
      validate_variational_args(self)
    },

    # Compose arguments to CmdStan command for variational-specific
    # non-default arguments. Works the same way as compose for sampler args,
    # but `idx` is ignored (no multiple chains for optimize or variational)
    compose = function(idx = NULL, args = NULL) {
      .make_arg <- function(arg_name) {
        compose_arg(self, arg_name, idx = NULL)
      }
      new_args <- list(
        "method=variational",
        .make_arg("algorithm"),
        .make_arg("iter"),
        .make_arg("grad_samples"),
        .make_arg("elbo_samples"),
        .make_arg("eta"),
        .make_arg("tol_rel_obj"),
        .make_arg("eval_elbo"),
        .make_arg("output_samples"),
        if (!is.null(self$adapt_engaged) || !is.null(self$adapt_iter))
          "adapt",
        .make_arg("adapt_engaged"),
        .make_arg("adapt_iter")
      )
      new_args <- do.call(c, new_args)
      c(args, new_args)
    }
  )
)


# Validate the 'Args' objects --------------------------------------------

#' Validate common (not method-specific) CmdStan arguments
#' @noRd
#' @param self A `CmdStanArgs` object.
#' @return `TRUE` invisibly unless an error is thrown.
validate_cmdstan_args = function(self) {
  validate_exe_file(self$exe_file)

  checkmate::assert_directory_exists(self$output_dir, access = "rw")

  # at least 1 run id (chain id)
  checkmate::assert_integerish(self$run_ids,
                               lower = 1,
                               min.len = 1,
                               any.missing = FALSE,
                               null.ok = FALSE)

  checkmate::assert_flag(self$save_extra_diagnostics)
  checkmate::assert_integerish(self$refresh, lower = 0, null.ok = TRUE)
  if (!is.null(self$data_file)) {
    checkmate::assert_file_exists(self$data_file, access = "r")
  }
  num_runs <- length(self$run_ids)
  validate_init(self$init, num_runs)
  validate_seed(self$seed, num_runs)

  invisible(TRUE)
}

#' Validate arguments for sampling
#' @noRd
#' @param self A `SampleArgs` object.
#' @param num_runs The number of CmdStan runs (number of MCMC chains).
#' @return `TRUE` invisibly unless an error is thrown.
validate_sample_args <- function(self, num_runs) {
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
  checkmate::assert_integerish(self$init_buffer,
                               lower = 0,
                               len = 1,
                               null.ok = TRUE)
  checkmate::assert_integerish(self$term_buffer,
                               lower = 0,
                               len = 1,
                               null.ok = TRUE)
  checkmate::assert_integerish(self$window,
                               lower = 0,
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
  validate_metric(self$metric)
  validate_metric_file(self$metric_file, num_runs)

  invisible(TRUE)
}

#' Validate arguments for optimization
#' @noRd
#' @param self An `OptimizeArgs` object.
#' @return `TRUE` invisibly unless an error is thrown.
validate_optimize_args <- function(self) {
  checkmate::assert_subset(self$algorithm, empty.ok = TRUE,
                           choices = c("bfgs", "lbfgs", "newton"))
  checkmate::assert_integerish(self$iter, lower = 0, null.ok = TRUE, len = 1)
  checkmate::assert_number(self$init_alpha, lower = 0, null.ok = TRUE)
  if (!is.null(self$init_alpha) && isTRUE(self$algorithm == "newton")) {
    stop("'init_alpha' can't be used when algorithm is 'newton'.",
         call. = FALSE)
  }

  invisible(TRUE)
}

#' Validate arguments for variational inference
#' @noRd
#' @param self A `VariationalArgs` object.
#' @return `TRUE` invisibly unless an error is thrown.
validate_variational_args <- function(self) {
  checkmate::assert_subset(self$algorithm, empty.ok = TRUE,
                           choices = c("meanfield", "fullrank"))
  checkmate::assert_integerish(self$iter, null.ok = TRUE,
                               lower = 1, len = 1)
  checkmate::assert_integerish(self$grad_samples, null.ok = TRUE,
                               lower = 1, len = 1)
  checkmate::assert_integerish(self$elbo_samples,  null.ok = TRUE,
                               lower = 1, len = 1)
  checkmate::assert_integerish(self$eval_elbo, null.ok = TRUE,
                               lower = 1, len = 1)
  checkmate::assert_integerish(self$output_samples, null.ok = TRUE,
                               lower = 1, len = 1)
  checkmate::assert_integerish(self$adapt_engaged, null.ok = TRUE,
                               lower = 0, upper = 1, len = 1)
  checkmate::assert_integerish(self$adapt_iter,
                               lower = 1, len = 1,
                               null.ok = TRUE)
  checkmate::assert_number(self$eta, null.ok = TRUE,
                           lower = .Machine$double.eps)
  checkmate::assert_number(self$tol_rel_obj, null.ok = TRUE,
                           lower = .Machine$double.eps)

  invisible(TRUE)
}


# Validation helpers ------------------------------------------------------

#' Validate exe file exists
#' @noRd
#' @param exe_file Path to executable.
#' @return Either throws an error or returns `invisible(TRUE)`
validate_exe_file <- function(exe_file) {
  if (!length(exe_file) ||
      !nzchar(exe_file) ||
      !file.exists(exe_file)) {
    stop('Model not compiled. Try running the compile() method first.',
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate initial values
#'
#' For CmdStan `init` must be `NULL`, a single real number >= 0, or paths to
#' init files for each chain.
#'
#' @noRd
#' @param init User's `init` argument.
#' @param num_runs Number of CmdStan runs (number of chains if MCMC)
#' @return Either throws an error or returns `invisible(TRUE)`.
validate_init <- function(init, num_runs) {
  if (is.null(init)) {
    return(invisible(TRUE))
  }

  if (!is.numeric(init) && !is.character(init)) {
    stop("If specified 'init' must be numeric or a character vector.",
         call. = FALSE)
  } else if (is.numeric(init) && (length(init) > 1 || init < 0)) {
    stop("If 'init' is numeric it must be a single real number >= 0.",
         call. = FALSE)
  } else if (is.character(init)) {
    if (length(init) != 1 && length(init) != num_runs) {
      stop("If 'init' is specified as a character vector it must have ",
           "length 1 or length 'num_chains'.",
           call. = FALSE)
    }
    checkmate::assert_file_exists(init, access = "r")
  }

  invisible(TRUE)
}

#' Recycle init if numeric and length 1
#' @noRd
#' @param init Already validated `init` argument.
#' @param num_runs Number of CmdStan runs.
#' @return `init`, unless numeric and length 1, in which case `rep(init, num_runs)`.
maybe_recycle_init <- function(init, num_runs) {
  if (is.null(init) ||
      length(init) == num_runs) {
    return(init)
  }
  rep(init, num_runs)
}


#' Validate seed
#'
#' `seed` must be `NULL`, a single positive integer, or one positive integer per
#' chain.
#'
#' @noRd
#' @param seed User's `seed` argument.
#' @param num_runs Number of CmdStan runs (number of chains if MCMC)
#' @return Either throws an error or returns `invisible(TRUE)`.
validate_seed <- function(seed, num_runs) {
  if (is.null(seed)) {
    return(invisible(TRUE))
  }
  checkmate::assert_integerish(seed, lower = 1)
  if (length(seed) > 1 && length(seed) != num_runs) {
    stop("If 'seed' is specified it must be a single integer or one per chain.",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Generate seed(s) if missing
#' @noRd
#' @param seed Already validated `seed` argument.
#' @param num_runs Number of CmdStan runs.
#' @return An integer vector of length `num_runs`.
maybe_generate_seed <- function(seed, num_runs) {
  if (is.null(seed)) {
    seed <- sample(.Machine$integer.max, num_runs)
  } else if (length(seed) == 1 && num_runs > 1) {
    seed <- as.integer(seed)
    seed <- c(seed, seed + 1:(num_runs -1))
  }
  seed
}

#' Validate metric
#' @noRd
#' @param metric User's `metric` argument.
#' @param num_runs Number of CmdStan runs (number of MCMC chains).
#' @return Either throws an error or returns `invisible(TRUE)`.
#'
validate_metric <- function(metric) {
  if (is.null(metric)) {
    return(invisible(TRUE))
  }

  checkmate::assert_character(metric, any.missing = FALSE, min.len = 1)
  checkmate::assert_subset(metric, choices = available_metrics())

  return(invisible(TRUE))
}

#' Validate metric file
#' @noRd
#' @param metric_file User's `metric_file` argument.
#' @param num_runs Number of CmdStan runs (number of MCMC chains).
#' @return Either throws an error or returns `invisible(TRUE)`.
#'
validate_metric_file <- function(metric_file, num_runs) {
  if (is.null(metric_file)) {
    return(invisible(TRUE))
  }

  checkmate::assert_file_exists(metric_file, access = "r")

  if (length(metric_file) != 1 && length(metric_file) != num_runs) {
    stop(length(metric_file), " metric(s) provided. Must provide ",
         if (num_runs > 1) "1 or ", num_runs, " metric(s) for ",
         num_runs, " chain(s).")
  }

  invisible(TRUE)
}

#' Recycle metric_file if not NULL
#' @noRd
#' @param metric_file Path to already validated `metric_file` argument.
#' @param num_runs Number of CmdStan runs.
#' @return `rep(metric_file, num_runs)` if metric_file is a single path, otherwise
#'    return `metric_file`.
maybe_recycle_metric_file <- function(metric_file, num_runs) {
  if (is.null(metric_file) ||
      length(metric_file) == num_runs) {
    return(metric_file)
  }
  rep(metric_file, num_runs)
}

available_metrics <- function() {
  c("unit_e", "diag_e", "dense_e")
}

# Composition helpers -----------------------------------------------------

#' Helper function to make valid CmdStan arguments
#' @noRd
#' @param self An Args object (e.g., `SampleArgs`).
#' @param arg_name Name of slot in `self` containing the argument value.
#' @param idx Chain id (only applicable for MCMC).
compose_arg <- function(self, arg_name, idx = NULL) {
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

