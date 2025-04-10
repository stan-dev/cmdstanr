# CmdStanArgs -------------------------------------------------------------

#' Internal objects for storing CmdStan arguments
#'
#' These objects store arguments for creating the call to CmdStan and provide a
#' `compose()` method for creating a character vector of arguments that can be
#' passed to the `args` argument of `processx::process$new()`.
#'
#' @noRd
#' @details
#' A `CmdStanArgs` object stores arguments common to all methods,
#' as well as one of the following objects containing the method-specific
#' arguments:
#'
#' * `SampleArgs`: stores arguments specific to `method=sample`.
#' * `OptimizeArgs`: stores arguments specific to `method=optimize`.
#' * `LaplaceArgs`: stores arguments specific to `method=laplace`.
#' * `VariationalArgs`: stores arguments specific to `method=variational`
#' * `PathfinderArgs`: stores arguments specific to `method=pathfinder`
#' * `GenerateQuantitiesArgs`: stores arguments specific to `method=generate_quantities`
#' * `DiagnoseArgs`: stores arguments specific to `method=diagnose`
#'
CmdStanArgs <- R6::R6Class(
  "CmdStanArgs",
  lock_objects = FALSE,
  public = list(
    method_args = NULL, # this will be a SampleArgs object (or OptimizeArgs, etc.)
    initialize = function(model_name,
                          stan_file = NULL,
                          stan_code = NULL,
                          model_methods_env = NULL,
                          standalone_env = NULL,
                          exe_file,
                          proc_ids,
                          method_args,
                          data_file = NULL,
                          save_latent_dynamics = FALSE,
                          seed = NULL,
                          init = NULL,
                          refresh = NULL,
                          output_dir = NULL,
                          output_basename = NULL,
                          sig_figs = NULL,
                          opencl_ids = NULL,
                          model_variables = NULL,
                          num_threads = NULL,
                          save_cmdstan_config = NULL) {

      self$model_name <- model_name
      self$stan_code <- stan_code
      self$exe_file <- exe_file
      self$model_methods_env <- model_methods_env
      self$standalone_env <- standalone_env
      self$proc_ids <- proc_ids
      self$data_file <- data_file
      self$seed <- seed
      self$refresh <- refresh
      self$sig_figs <- sig_figs
      self$method_args <- method_args
      self$method <- self$method_args$method
      self$save_latent_dynamics <- save_latent_dynamics
      self$using_tempdir <- is.null(output_dir)
      self$model_variables <- model_variables
      self$save_cmdstan_config <- save_cmdstan_config
      if (os_is_wsl()) {
        # Want to ensure that any files under WSL are written to a tempdir within
        # WSL to avoid IO performance issues
        self$output_dir <- ifelse(is.null(output_dir),
                                  file.path(wsl_dir_prefix(), wsl_tempdir()),
                                  wsl_safe_path(output_dir))
      } else if (getRversion() < "3.5.0") {
        self$output_dir <- output_dir %||% tempdir()
      } else {
        if (getRversion() < "3.5.0") {
          self$output_dir <- output_dir %||% tempdir()
        } else {
          self$output_dir <- output_dir %||% tempdir(check = TRUE)
        }
      }
      self$output_dir <- repair_path(self$output_dir)
      self$output_basename <- output_basename
      if (inherits(self$method_args, "PathfinderArgs")) {
        num_inits <- self$method_args$num_paths
      } else {
        num_inits <- length(self$proc_ids)
      }
      init <- process_init(init, num_inits, model_variables)
      self$init <- init
      self$opencl_ids <- opencl_ids
      self$num_threads = NULL
      self$method_args$validate(num_procs = length(self$proc_ids))
      if (is.logical(self$save_cmdstan_config)) {
        self$save_cmdstan_config <- as.integer(self$save_cmdstan_config)
      }
      self$validate()
    },
    validate = function() {
      validate_cmdstan_args(self)
      self$output_dir <- repair_path(absolute_path(self$output_dir))
      if (is.character(self$data_file)) {
        self$data_file <- absolute_path(self$data_file)
      }
      if (is.character(self$init)) {
        self$init <- absolute_path(self$init)
      }
      self$init <- maybe_recycle_init(self$init, length(self$proc_ids))
      self$seed <- maybe_generate_seed(self$seed, length(self$proc_ids))
      invisible(self)
    },

    new_file_names = function(type = c("output", "diagnostic", "profile")) {
      basename <- self$model_name
      type <- match.arg(type)
      if (type == "diagnostic") {
        basename <- paste0(basename, "-diagnostic")
      } else if (type == "profile") {
        basename <- paste0(basename, "-profile")
      }
      if (type == "output" && !is.null(self$output_basename)) {
        basename <- self$output_basename
      }
      generate_file_names(
        basename = basename,
        ext = ".csv",
        ids = self$proc_ids,
        timestamp = is.null(self$output_basename),
        random = is.null(self$output_basename)
      )
    },
    new_files = function(type = c("output", "diagnostic", "profile")) {
      files <- file.path(self$output_dir, self$new_file_names(type))
      files
    },

    #' Compose all arguments to pass to CmdStan
    #'
    #' @noRd
    #' @param idx The run id. For MCMC this is the chain id, for optimization
    #'   this is just 1.
    #' @param output_file File path to csv file where output will be written.
    #' @param profile_file File path to csv file where profile data will be written.
    #' @param latent_dynamics_file File path to csv file where the extra latent
    #'   dynamics information will be written.
    #' @return Character vector of arguments of the form "name=value".
    #'
    compose_all_args = function(idx = NULL,
                                output_file = NULL,
                                profile_file = NULL,
                                latent_dynamics_file = NULL) {
      args <- list()
      idx <- idx %||% 1
      if (!is.null(self$proc_ids)) {
        if (idx < 0 || idx > length(self$proc_ids)) {
          stop("Index (", idx, ") exceeds number of CmdStan processes",
               " (", length(self$proc_ids), ").",
               call. = FALSE)
        }
        args$id <- paste0("id=", self$proc_ids[idx])
      }

      if (!is.null(self$seed)) {
        args$seed <- c("random", paste0("seed=", self$seed[idx]))
      }

      if (!is.null(self$init)) {
        args$init <- paste0("init=", wsl_safe_path(self$init[idx]))
      }

      if (!is.null(self$data_file)) {
        args$data <- c("data", paste0("file=", wsl_safe_path(self$data_file)))
      }

      args$output <- c("output", paste0("file=", wsl_safe_path(output_file)))
      if (!is.null(latent_dynamics_file)) {
        args$output <- c(args$output, paste0("diagnostic_file=", wsl_safe_path(latent_dynamics_file)))
      }
      if (!is.null(self$refresh)) {
        args$output <- c(args$output, paste0("refresh=", self$refresh))
      }

      if (!is.null(self$sig_figs)) {
        args$output <- c(args$output, paste0("sig_figs=", self$sig_figs))
      }

      if (!is.null(profile_file)) {
        args$output <- c(args$output, paste0("profile_file=", wsl_safe_path(profile_file)))
      }
      if (!is.null(self$save_cmdstan_config)) {
        args$output <- c(args$output, paste0("save_cmdstan_config=", self$save_cmdstan_config))
      }
      if (!is.null(self$opencl_ids)) {
        args$opencl <- c("opencl", paste0("platform=", self$opencl_ids[1]), paste0("device=", self$opencl_ids[2]))
      }
      if (!is.null(self$num_threads)) {
        num_threads <- c(args$output, paste0("num_threads=", self$num_threads))
      }
      args <- do.call(c, append(args, list(use.names = FALSE)))
      self$method_args$compose(idx, args)
    },
    command = function() {
      paste0(if (!os_is_windows() || os_is_wsl()) "./", basename(self$exe_file))
    }
  )
)


# SampleArgs -------------------------------------------------------------

SampleArgs <- R6::R6Class(
  "SampleArgs",
  lock_objects = FALSE,
  public = list(
    method = "sample",
    initialize = function(iter_warmup = NULL,
                          iter_sampling = NULL,
                          save_warmup = NULL,
                          thin = NULL,
                          max_treedepth = NULL,
                          adapt_engaged = NULL,
                          adapt_delta = NULL,
                          step_size = NULL,
                          metric = NULL,
                          metric_file = NULL,
                          inv_metric = NULL,
                          init_buffer = NULL,
                          term_buffer = NULL,
                          window = NULL,
                          fixed_param = FALSE,
                          diagnostics = NULL,
                          save_metric = NULL) {

      self$iter_warmup <- iter_warmup
      self$iter_sampling <- iter_sampling
      self$save_warmup <- save_warmup
      self$thin <- thin
      self$max_treedepth <- max_treedepth
      self$adapt_engaged <- adapt_engaged
      self$adapt_delta <- adapt_delta
      self$step_size <- step_size
      self$metric <- metric
      self$inv_metric <- inv_metric
      self$fixed_param <- fixed_param
      self$diagnostics <- diagnostics
      self$save_metric <- save_metric
      if (identical(self$diagnostics, "")) {
        self$diagnostics <- NULL
      }

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
          if (length(inv_metric[[i]]) == 1 && metric == "diag_e") {
            inv_metric[[i]] <- array(inv_metric[[i]], dim = c(1))
          }
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
      if (is.logical(self$save_metric)) {
        self$save_metric <- as.integer(self$save_metric)
      }
      invisible(self)
    },
    validate = function(num_procs) {
      validate_sample_args(self, num_procs)
      self$metric_file <- maybe_recycle_metric_file(self$metric_file, num_procs)
      invisible(self)
    },

    #' Compose arguments to CmdStan command for sampling-specific
    #' non-default arguments
    #'
    #' @noRd
    #' @param idx Integer chain id.
    #' @param args A character vector of arguments to prepend to the returned
    #'   character vector. This will get passed in from `CmdStanArgs$compose_all_args()`.
    #' @return A character vector of CmdStan arguments.
    compose = function(idx, args = NULL) {
      .make_arg <- function(arg_name, cmdstan_arg_name = NULL, idx = NULL) {
        compose_arg(self, arg_name = arg_name, cmdstan_arg_name = cmdstan_arg_name, idx = idx)
      }

      if (self$fixed_param) {
        new_args <- list(
          "method=sample",
          .make_arg("iter_sampling", cmdstan_arg_name = "num_samples"),
          .make_arg("iter_warmup", cmdstan_arg_name = "num_warmup"),
          .make_arg("save_warmup"),
          .make_arg("thin"),
          "algorithm=fixed_param",
          .make_arg("metric"),
          .make_arg("metric_file", idx = idx),
          .make_arg("step_size", cmdstan_arg_name = "stepsize", idx = idx),
          .make_arg("max_treedepth", cmdstan_arg_name = "max_depth"),
          if (!is.null(self$adapt_delta) || !is.null(self$adapt_engaged))
            "adapt",
          .make_arg("adapt_delta"),
          .make_arg("adapt_engaged"),
          .make_arg("init_buffer"),
          .make_arg("term_buffer"),
          .make_arg("window"),
          .make_arg("save_metric")
        )
      } else {
        new_args <- list(
          "method=sample",
          .make_arg("iter_sampling", cmdstan_arg_name = "num_samples"),
          .make_arg("iter_warmup", cmdstan_arg_name = "num_warmup"),
          .make_arg("save_warmup"),
          .make_arg("thin"),
          "algorithm=hmc",
          .make_arg("metric"),
          .make_arg("metric_file", idx = idx),
          .make_arg("step_size", cmdstan_arg_name = "stepsize", idx = idx),
          "engine=nuts",
          .make_arg("max_treedepth", cmdstan_arg_name = "max_depth"),
          if (!is.null(self$adapt_delta) || !is.null(self$adapt_engaged))
            "adapt",
          .make_arg("adapt_delta"),
          .make_arg("adapt_engaged"),
          .make_arg("init_buffer"),
          .make_arg("term_buffer"),
          .make_arg("window"),
          .make_arg("save_metric")
        )
      }
      new_args <- do.call(c, new_args)
      c(args, new_args)
    }
  )
)

# GenerateQuantitiesArgs -------------------------------------------------------------

GenerateQuantitiesArgs <- R6::R6Class(
  "GenerateQuantitiesArgs",
  lock_objects = FALSE,
  public = list(
    method = "generate_quantities",
    initialize = function(fitted_params = NULL) {
      self$fitted_params <- fitted_params
      invisible(self)
    },
    validate = function(num_procs) {
      validate_generate_quantities_args(self)
      invisible(self)
    },

    # Compose arguments to CmdStan command for generate_quantities method
    compose = function(idx = NULL, args = NULL) {
      .make_arg <- function(arg_name, cmdstan_arg_name = NULL, idx = NULL) {
        compose_arg(self, arg_name = arg_name, cmdstan_arg_name = cmdstan_arg_name, idx = idx)
      }
      new_args <- list(
        "method=generate_quantities",
        .make_arg("fitted_params", idx = idx)
      )
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
    initialize = function(iter = NULL,
                          jacobian = NULL,
                          algorithm = NULL,
                          init_alpha = NULL,
                          tol_obj = NULL,
                          tol_rel_obj = NULL,
                          tol_grad = NULL,
                          tol_rel_grad = NULL,
                          tol_param = NULL,
                          history_size = NULL) {
      self$iter <- iter
      self$jacobian <- jacobian
      self$algorithm <- algorithm
      self$init_alpha <- init_alpha
      self$tol_obj <- tol_obj
      self$tol_rel_obj <- tol_rel_obj
      self$tol_grad <- tol_grad
      self$tol_rel_grad <- tol_rel_grad
      self$tol_param <- tol_param
      self$history_size <- history_size
      invisible(self)
    },
    validate = function(num_procs) {
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
        .make_arg("jacobian"),
        .make_arg("iter"),
        .make_arg("algorithm"),
        .make_arg("init_alpha"),
        .make_arg("tol_obj"),
        .make_arg("tol_rel_obj"),
        .make_arg("tol_grad"),
        .make_arg("tol_rel_grad"),
        .make_arg("tol_param"),
        .make_arg("history_size")
      )
      new_args <- do.call(c, new_args)
      c(args, new_args)
    }
  )
)


# LaplaceArgs -------------------------------------------------------------

LaplaceArgs <- R6::R6Class(
  "LaplaceArgs",
  lock_objects = FALSE,
  public = list(
    method = "laplace",
    initialize = function(mode = NULL,
                          draws = NULL,
                          jacobian = TRUE) {
      checkmate::assert_r6(mode, classes = "CmdStanMLE")
      self$mode_object <- mode  # keep the CmdStanMLE for later use (can be returned by CmdStanLaplace$mode())
      # mode <- file path to pass to CmdStan
      # This needs to be a path that can be accessed within WSL
      # since the files are used by CmdStan, not R
      self$mode <- wsl_safe_path(self$mode_object$output_files())
      self$jacobian <- jacobian
      self$draws <- draws
      invisible(self)
    },
    validate = function(num_procs) {
      validate_laplace_args(self)
      invisible(self)
    },

    # Compose arguments to CmdStan command for laplace-specific
    # non-default arguments. Works the same way as compose for sampler args,
    # but `idx` is ignored (no multiple chains for optimize or variational)
    compose = function(idx = NULL, args = NULL) {
      .make_arg <- function(arg_name) {
        compose_arg(self, arg_name, idx = NULL)
      }
      new_args <- list(
        "method=laplace",
        .make_arg("mode"),
        .make_arg("draws"),
        .make_arg("jacobian")
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

    validate = function(num_procs) {
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

# PathfinderArgs ---------------------------------------------------------

PathfinderArgs <- R6::R6Class(
  "PathfinderArgs",
  lock_objects = FALSE,
  public = list(
    method = "pathfinder",
      initialize = function(init_alpha = NULL,
                            tol_obj = NULL,
                            tol_rel_obj = NULL,
                            tol_grad = NULL,
                            tol_rel_grad = NULL,
                            tol_param = NULL,
                            history_size = NULL,
                            single_path_draws = NULL,
                            draws = NULL,
                            num_paths = NULL,
                            max_lbfgs_iters = NULL,
                            num_elbo_draws = NULL,
                            save_single_paths = NULL,
                            psis_resample = NULL,
                            calculate_lp = NULL) {
        self$init_alpha <- init_alpha
        self$tol_obj <- tol_obj
        self$tol_rel_obj <- tol_rel_obj
        self$tol_grad <- tol_grad
        self$tol_rel_grad <- tol_rel_grad
        self$tol_param <- tol_param
        self$history_size <- history_size
        self$num_psis_draws <- draws
        self$num_draws <- single_path_draws
        self$num_paths <- num_paths
        self$max_lbfgs_iters <- max_lbfgs_iters
        self$num_elbo_draws <- num_elbo_draws
        self$save_single_paths <- save_single_paths
        self$psis_resample <- psis_resample
        self$calculate_lp <- calculate_lp
      invisible(self)
    },

    validate = function(num_procs) {
      validate_pathfinder_args(self)
    },

    # Compose arguments to CmdStan command for pathfinder-specific
    # non-default arguments. Works the same way as compose for sampler args,
    # but `idx` (multiple pathfinders are handled in cmdstan)
    compose = function(idx = NULL, args = NULL) {
      .make_arg <- function(arg_name) {
        compose_arg(self, arg_name, idx = NULL)
      }
        new_args <- list(
          "method=pathfinder",
          .make_arg("init_alpha"),
          .make_arg("tol_obj"),
          .make_arg("tol_rel_obj"),
          .make_arg("tol_grad"),
          .make_arg("tol_rel_grad"),
          .make_arg("tol_param"),
          .make_arg("history_size"),
          .make_arg("num_psis_draws"),
          .make_arg("num_draws"),
          .make_arg("num_paths"),
          .make_arg("max_lbfgs_iters"),
          .make_arg("num_elbo_draws"),
          .make_arg("save_single_paths"),
          .make_arg("psis_resample"),
          .make_arg("calculate_lp")
        )
      new_args <- do.call(c, new_args)
      c(args, new_args)
    }
  )
)

# DiagnoseArgs -------------------------------------------------------------

DiagnoseArgs <- R6::R6Class(
  "DiagnoseArgs",
  lock_objects = FALSE,
  public = list(
    method = "diagnose",
    initialize = function(epsilon = NULL, error = NULL) {
      self$epsilon <- epsilon
      self$error <- error
      invisible(self)
    },
    validate = function(num_procs) {
      validate_diagnose_args(self)
      invisible(self)
    },

    # Compose arguments to CmdStan command for diagnose method
    compose = function(idx = NULL, args = NULL) {
      .make_arg <- function(arg_name, cmdstan_arg_name = NULL, idx = NULL) {
        compose_arg(self, arg_name = arg_name, cmdstan_arg_name = cmdstan_arg_name, idx = idx)
      }
      new_args <- list(
        "method=diagnose",
        if (!is.null(self$epsilon) || !is.null(self$error))
          "test=gradient",
        .make_arg("epsilon"),
        .make_arg("error")
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
validate_cmdstan_args <- function(self) {
  validate_exe_file(self$exe_file)
  assert_dir_exists(self$output_dir, access = "rw")

  # at least 1 run id (chain id)
  checkmate::assert_integerish(self$proc_ids,
                               lower = 1,
                               min.len = 1,
                               any.missing = FALSE,
                               null.ok = FALSE)

  checkmate::assert_flag(self$save_latent_dynamics)
  checkmate::assert_integerish(self$refresh, lower = 0, null.ok = TRUE)
  checkmate::assert_integerish(self$sig_figs, lower = 1, upper = 18, null.ok = TRUE)
  checkmate::assert_integerish(self$save_cmdstan_config, lower = 0, upper = 1, len = 1, null.ok = TRUE)
  if (!is.null(self$sig_figs) && cmdstan_version() < "2.25") {
    warning("The 'sig_figs' argument is only supported with cmdstan 2.25+ and will be ignored!", call. = FALSE)
  }
  if (!is.null(self$refresh)) {
    self$refresh <- as.integer(self$refresh)
  }
  if (!is.null(self$data_file)) {
    assert_file_exists(self$data_file, access = "r")
  }
  num_procs <- length(self$proc_ids)
  if (inherits(self$method_args, "PathfinderArgs")) {
    num_inits <- self$method_args$num_paths
  } else {
    num_inits <- length(self$proc_ids)
  }
  validate_init(self$init, num_inits)
  validate_seed(self$seed, num_procs)
  invisible(TRUE)
}

#' Validate arguments for sampling
#' @noRd
#' @param self A `SampleArgs` object.
#' @param num_procs The number of CmdStan processes (number of MCMC chains).
#' @return `TRUE` invisibly unless an error is thrown.
validate_sample_args <- function(self, num_procs) {
  checkmate::assert_integerish(num_procs,
                               lower = 1,
                               len = 1,
                               any.missing = FALSE,
                               .var.name = "Number of chains")
  self$num_procs <- as.integer(self$num_procs)
  checkmate::assert_integerish(self$thin,
                               lower = 1,
                               len = 1,
                               null.ok = TRUE)
  if (!is.null(self$thin)) {
    self$thin <- as.integer(self$thin)
  }
  checkmate::assert_integerish(self$iter_sampling,
                               lower = 0,
                               len = 1,
                               null.ok = TRUE)
  if (!is.null(self$iter_sampling)) {
    self$iter_sampling <- as.integer(self$iter_sampling)
  }
  checkmate::assert_integerish(self$iter_warmup,
                               lower = 0,
                               len = 1,
                               null.ok = TRUE)
  if (!is.null(self$iter_warmup)) {
    self$iter_warmup <- as.integer(self$iter_warmup)
  }
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
  checkmate::assert_integerish(self$max_treedepth,
                               lower = 1,
                               len = 1,
                               null.ok = TRUE)
  if (!is.null(self$max_treedepth)) {
    self$max_treedepth <- as.integer(self$max_treedepth)
  }
  checkmate::assert_integerish(self$init_buffer,
                               lower = 0,
                               len = 1,
                               null.ok = TRUE)
  if (!is.null(self$init_buffer)) {
    self$init_buffer <- as.integer(self$init_buffer)
  }
  checkmate::assert_integerish(self$term_buffer,
                               lower = 0,
                               len = 1,
                               null.ok = TRUE)
  if (!is.null(self$term_buffer)) {
    self$term_buffer <- as.integer(self$term_buffer)
  }
  checkmate::assert_integerish(self$window,
                               lower = 0,
                               len = 1,
                               null.ok = TRUE)
  if (!is.null(self$window)) {
    self$window <- as.integer(self$window)
  }

  if (length(self$step_size) == 1) {
    checkmate::assert_number(self$step_size, lower = .Machine$double.eps)
  } else {
    checkmate::assert_numeric(self$step_size,
                              lower = .Machine$double.eps,
                              len = num_procs,
                              null.ok = TRUE)
  }

  validate_metric(self$metric)
  validate_metric_file(self$metric_file, num_procs)

  checkmate::assert_character(self$diagnostics, null.ok = TRUE, any.missing = FALSE)
  if (!is.null(self$diagnostics)) {
    checkmate::assert_subset(self$diagnostics, empty.ok = FALSE, choices = available_hmc_diagnostics())
  }

  checkmate::assert_integerish(self$save_metric,
                               lower = 0, upper = 1,
                               len = 1,
                               null.ok = TRUE)

  if (is.null(self$adapt_engaged) || (!self$adapt_engaged && !is.null(self$save_metric))) {
    self$save_metric <- 0
  }

  invisible(TRUE)
}

#' Validate arguments for optimization
#' @noRd
#' @param self An `OptimizeArgs` object.
#' @return `TRUE` invisibly unless an error is thrown.
validate_optimize_args <- function(self) {
  checkmate::assert_subset(self$algorithm, empty.ok = TRUE,
                           choices = c("bfgs", "lbfgs", "newton"))
  checkmate::assert_flag(self$jacobian, null.ok = TRUE)
  if (!is.null(self$jacobian)) {
    if (cmdstan_version() < "2.32") {
      warning("The 'jacobian' argument is only supported with cmdstan 2.32+ and will be ignored!", call. = FALSE)
    }
    self$jacobian <- as.integer(self$jacobian)
  }

  checkmate::assert_integerish(self$iter, lower = 1, null.ok = TRUE, len = 1)
  if (!is.null(self$iter)) {
    self$iter <- as.integer(self$iter)
  }

  # check args only available for lbfgs and bfgs
  bfgs_args <- c("init_alpha", "tol_obj", "tol_rel_obj", "tol_grad", "tol_rel_grad", "tol_param")
  for (arg in bfgs_args) {
    # check that arg is positive or NULL and that algorithm='lbfgs' or 'bfgs' is
    # explicitly specified (error if not or if 'newton')
    if (!is.null(self[[arg]]) && is.null(self$algorithm)) {
      stop("Please specify 'algorithm' in order to use '", arg, "'.", call. = FALSE)
    }
    if (!is.null(self[[arg]]) && isTRUE(self$algorithm == "newton")) {
      stop("'", arg, "' can't be used when algorithm is 'newton'.", call. = FALSE)
    }
    checkmate::assert_number(self[[arg]], .var.name = arg, lower = 0, null.ok = TRUE)
  }

  # history_size only available for lbfgs
  if (!is.null(self$history_size)) {
    if (!isTRUE(self$algorithm == "lbfgs")) {
      stop("'history_size' is only allowed if 'algorithm' is specified as 'lbfgs'.", call. = FALSE)
    } else {
      checkmate::assert_integerish(self$history_size, lower = 1, len = 1, null.ok = FALSE)
      self$history_size <- as.integer(self$history_size)
    }
  }

  invisible(TRUE)
}

#' Validate arguments for laplace
#' @noRd
#' @param self A `LaplaceArgs` object.
#' @return `TRUE` invisibly unless an error is thrown.
validate_laplace_args <- function(self) {
  assert_file_exists(self$mode, extension = "csv")
  checkmate::assert_integerish(self$draws, lower = 1, null.ok = TRUE, len = 1)
  if (!is.null(self$draws)) {
    self$draws <- as.integer(self$draws)
  }
  checkmate::assert_flag(self$jacobian, null.ok = FALSE)
  if (self$mode_object$metadata()$jacobian != self$jacobian) {
    stop(
      "'jacobian' argument to optimize and laplace must match!\n",
      "laplace was called with jacobian=", self$jacobian, "\n",
      "optimize was run with jacobian=", as.logical(self$mode_object$metadata()$jacobian),
      call. = FALSE
    )
  }
  self$jacobian <- as.integer(self$jacobian)
  invisible(TRUE)
}

#' Validate arguments for standalone generated quantities
#' @noRd
#' @param self A `GenerateQuantitiesArgs` object.
#' @return `TRUE` invisibly unless an error is thrown.
validate_generate_quantities_args <- function(self) {
  if (!is.null(self$fitted_params)) {
    assert_file_exists(self$fitted_params, access = "r")
  }

  invisible(TRUE)
}

#' Validate arguments for diagnose
#' @noRd
#' @param self A `DiagnoseArgs` object.
#' @return `TRUE` invisibly unless an error is thrown.
validate_diagnose_args <- function(self) {
  checkmate::assert_number(self$epsilon, null.ok = TRUE,
                           lower = .Machine$double.eps)
  checkmate::assert_number(self$error, null.ok = TRUE,
                           lower = .Machine$double.eps)
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
  if (!is.null(self$iter)) {
    self$iter <- as.integer(self$iter)
  }
  checkmate::assert_integerish(self$grad_samples, null.ok = TRUE,
                               lower = 1, len = 1)
  if (!is.null(self$grad_samples)) {
    self$grad_samples <- as.integer(self$grad_samples)
  }
  checkmate::assert_integerish(self$elbo_samples,  null.ok = TRUE,
                               lower = 1, len = 1)
  if (!is.null(self$elbo_samples)) {
    self$elbo_samples <- as.integer(self$elbo_samples)
  }
  checkmate::assert_integerish(self$eval_elbo, null.ok = TRUE,
                               lower = 1, len = 1)
  if (!is.null(self$eval_elbo)) {
    self$eval_elbo <- as.integer(self$eval_elbo)
  }
  checkmate::assert_integerish(self$output_samples, null.ok = TRUE,
                               lower = 1, len = 1, .var.name = "draws")
  if (!is.null(self$output_samples)) {
    self$output_samples <- as.integer(self$output_samples)
  }
  checkmate::assert_integerish(self$adapt_engaged, null.ok = TRUE,
                               lower = 0, upper = 1, len = 1)
  checkmate::assert_integerish(self$adapt_iter,
                               lower = 1, len = 1,
                               null.ok = TRUE)
  if (!is.null(self$adapt_iter)) {
    self$adapt_iter <- as.integer(self$adapt_iter)
  }
  checkmate::assert_number(self$eta, null.ok = TRUE,
                           lower = .Machine$double.eps)
  checkmate::assert_number(self$tol_rel_obj, null.ok = TRUE,
                           lower = .Machine$double.eps)

  invisible(TRUE)
}

#' Validate arguments for pathfinder inference
#' @noRd
#' @param self A `PathfinderArgs` object.
#' @return `TRUE` invisibly unless an error is thrown.
validate_pathfinder_args <- function(self) {

  checkmate::assert_integerish(self$max_lbfgs_iters, lower = 1, null.ok = TRUE, len = 1)
  if (!is.null(self$max_lbfgs_iters)) {
    self$iter <- as.integer(self$max_lbfgs_iters)
  }
  checkmate::assert_integerish(self$num_paths, lower = 1, null.ok = TRUE,
                               len = 1)
  if (!is.null(self$num_paths)) {
    self$num_paths <- as.integer(self$num_paths)
  }
  checkmate::assert_integerish(self$num_draws, lower = 1, null.ok = TRUE,
                               len = 1, .var.name = "single_path_draws")
  if (!is.null(self$num_draws)) {
    self$num_draws <- as.integer(self$num_draws)
  }
  checkmate::assert_integerish(self$num_psis_draws, lower = 1, null.ok = TRUE,
                               len = 1, .var.name = "draws")
  if (!is.null(self$num_psis_draws)) {
    self$num_psis_draws <- as.integer(self$num_psis_draws)
  }
  checkmate::assert_integerish(self$num_elbo_draws, lower = 1, null.ok = TRUE, len = 1)
  if (!is.null(self$num_elbo_draws)) {
    self$num_elbo_draws <- as.integer(self$num_elbo_draws)
  }
  if (!is.null(self$save_single_paths) && is.logical(self$save_single_paths)) {
    self$save_single_paths = as.integer(self$save_single_paths)
  }
  checkmate::assert_integerish(self$save_single_paths, null.ok = TRUE,
                               lower = 0, upper = 1, len = 1)
  if (!is.null(self$save_single_paths)) {
    self$save_single_paths <- 0
  }
  if (!is.null(self$psis_resample) && is.logical(self$psis_resample)) {
    self$psis_resample = as.integer(self$psis_resample)
  }
  checkmate::assert_integerish(self$psis_resample, null.ok = TRUE,
                               lower = 0, upper = 1, len = 1)
  if (!is.null(self$calculate_lp) && is.logical(self$calculate_lp)) {
    self$calculate_lp = as.integer(self$calculate_lp)
  }
  checkmate::assert_integerish(self$calculate_lp, null.ok = TRUE,
                               lower = 0, upper = 1, len = 1)


  # check args only available for lbfgs and bfgs
  bfgs_args <- c("init_alpha", "tol_obj", "tol_rel_obj", "tol_grad", "tol_rel_grad", "tol_param")
  for (arg in bfgs_args) {
    checkmate::assert_number(self[[arg]], .var.name = arg, lower = 0, null.ok = TRUE)
  }

  if (!is.null(self$history_size)) {
      checkmate::assert_integerish(self$history_size, lower = 1, len = 1, null.ok = FALSE)
      self$history_size <- as.integer(self$history_size)
  }

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
    stop("Model not compiled. Try running the compile() method first.",
         call. = FALSE)
  }
  invisible(TRUE)
}


#' Generic for processing inits
#' @noRd
process_init <- function(init, ...) {
  UseMethod("process_init")
}

#' Default method
#' @noRd
#' @export
process_init.default <- function(init, ...) {
  return(init)
}

#' Remove the leftmost dimension if equal to 1
#' @noRd
#' @param x An array like object
.remove_leftmost_dim <- function(x) {
  dims <- dim(x)
  if (length(dims) == 1) {
    return(drop(x))
  } else if (dims[1] == 1) {
    new_dims <- dims[-1]
    # Create a call to subset the array, maintaining all remaining dimensions
    subset_expr <- as.call(c(as.name("["), list(x), 1, rep(TRUE, length(new_dims)), drop = FALSE))
    new_x <- eval(subset_expr)
    return(array(new_x, dim = new_dims))
  } else {
    return(x)
  }
}

#' Write initial values to files if provided as posterior `draws` object
#' @noRd
#' @param init A type that inherits the `posterior::draws` class.
#' @param num_procs Number of inits requested
#' @param model_variables  A list of all parameters with their types and
#'   number of dimensions. Typically the output of `model$variables()$parameters`.
#' @param warn_partial Should a warning be thrown if inits are only specified
#'   for a subset of parameters? Can be controlled by global option
#'   `cmdstanr_warn_inits`.
#' @return A character vector of file paths.
#' @export
process_init.draws <- function(init, num_procs, model_variables = NULL,
                               warn_partial = getOption("cmdstanr_warn_inits", TRUE),
                               ...) {
  if (!is.null(model_variables)) {
    variable_names = names(model_variables$parameters)
  } else {
    variable_names = colnames(draws)[!grepl("__", colnames(draws))]
  }
  draws <- posterior::as_draws_df(init)
  # Since all other process_init functions return `num_proc` inits
  # This will only happen if a raw draws object is passed
  if (nrow(draws) < num_procs) {
    idx <- rep(1:nrow(draws), ceiling(num_procs / nrow(draws)))[1:num_procs]
    draws <- draws[idx,]
  } else if (nrow(draws) > num_procs) {
    draws <- posterior::resample_draws(draws, ndraws = num_procs,
                                       method ="simple_no_replace")
  }
  draws_rvar = posterior::as_draws_rvars(draws)
  variable_names <- variable_names[variable_names %in% names(draws_rvar)]
  draws_rvar <- posterior::subset_draws(draws_rvar, variable = variable_names)
  inits = lapply(1:num_procs, function(draw_iter) {
    init_i = lapply(variable_names, function(var_name) {
      x = .remove_leftmost_dim(posterior::draws_of(
        posterior::subset_draws(draws_rvar[[var_name]], draw=draw_iter)))
      if (model_variables$parameters[[var_name]]$dimensions == 0) {
        return(as.double(x))
      } else {
        return(x)
      }
    })
    bad_names = unlist(lapply(variable_names, function(var_name) {
      x = drop(posterior::draws_of(drop(
        posterior::subset_draws(draws_rvar[[var_name]], draw=draw_iter))))
      if (any(is.infinite(x)) || any(is.na(x))) {
        return(var_name)
      }
      return("")
    }))
    any_na_or_inf = bad_names != ""
    if (any(any_na_or_inf)) {
      err_msg = paste0(paste(bad_names[any_na_or_inf], collapse = ", "), " contains NA or Inf values!")
      if (length(any_na_or_inf) > 1) {
        err_msg = paste0("Variables: ", err_msg)
      } else {
        err_msg = paste0("Variable: ", err_msg)
      }
      stop(err_msg)
    }
    names(init_i) = variable_names
    return(init_i)
  })
  return(process_init(inits, num_procs, model_variables, warn_partial))
}

#' Write initial values to files if provided as list of lists
#' @noRd
#' @param init List of init lists.
#' @param num_procs Number of inits needed.
#' @param model_variables  A list of all parameters with their types and
#'   number of dimensions. Typically the output of `model$variables()$parameters`.
#' @param warn_partial Should a warning be thrown if inits are only specified
#'   for a subset of parameters? Can be controlled by global option
#'   `cmdstanr_warn_inits`.
#' @return A character vector of file paths.
#' @export
process_init.list <- function(init, num_procs, model_variables = NULL,
                              warn_partial = getOption("cmdstanr_warn_inits", TRUE),
                              ...) {
  if (!all(sapply(init, function(x) is.list(x) && !is.data.frame(x)))) {
    stop("If 'init' is a list it must be a list of lists.", call. = FALSE)
  }
  if (length(init) != num_procs) {
    stop("'init' has the wrong length. See documentation of 'init' argument.", call. = FALSE)
  }
  if (any(sapply(init, function(x) length(x) == 0))) {
    stop("'init' contains empty lists.", call. = FALSE)
  }
  if (!is.null(model_variables)) {
    missing_parameter_values <- list()
    parameter_names <- names(model_variables$parameters)
    for (i in seq_along(init)) {
      is_parameter_value_supplied <- parameter_names %in% names(init[[i]])
      if (!all(is_parameter_value_supplied)) {
        missing_parameter_values[[i]] <- parameter_names[!is_parameter_value_supplied]
      }
      for (par_name in parameter_names[is_parameter_value_supplied]) {
        # Make sure that initial values for single-element containers don't get
        # unboxed when writing to JSON
        if (model_variables$parameters[[par_name]]$dimensions == 1 && length(init[[i]][[par_name]]) == 1) {
          init[[i]][[par_name]] <- array(init[[i]][[par_name]], dim = 1)
        }
      }
    }
    if (length(missing_parameter_values) > 0 && isTRUE(warn_partial)) {
      warning_message <- c(
        "Init values were only set for a subset of parameters. \nMissing init values for the following parameters:\n"
      )
      for (i in seq_along(missing_parameter_values)) {
        if (length(init) > 1) {
          line_text <- paste0(" - chain ", i, ": ")
        } else {
          line_text <- ""
        }
        if (length(missing_parameter_values[[i]]) > 0) {
          warning_message <- c(warning_message, paste0(line_text, paste0(missing_parameter_values[[i]], collapse = ", "), "\n"))
        }
      }
      warning_message <- c(warning_message, "\nTo disable this message use options(cmdstanr_warn_inits = FALSE).\n")
      message(warning_message)
    }
  }
  if (any(grepl("\\[", names(unlist(init))))) {
    stop(
      "'init' contains entries with parameter names that include square-brackets, which is not permitted. ",
      "To supply inits for a vector, matrix or array of parameters, ",
      "create a single entry with the parameter's name in the 'init' list ",
      "and specify initial values for the entire parameter container.",
      call. = FALSE)
  }
  init_paths <-
    tempfile(
      pattern = "init-",
      tmpdir = cmdstan_tempdir(),
      fileext = ""
    )
  init_paths <- paste0(init_paths, "_", seq_along(init), ".json")
  for (i in seq_along(init)) {
    write_stan_json(init[[i]], init_paths[i])
  }
  init_paths
}

#' Write initial values to files if provided as function
#' @noRd
#' @param init Function generating a single list of initial values.
#' @param num_procs Number of inits needed.
#' @param model_variables A list of all parameters with their types and
#'   number of dimensions. Typically the output of `model$variables()$parameters`.
#' @return A character vector of file paths.
#' @export
process_init.function <- function(init, num_procs, model_variables = NULL,
                                  warn_partial = getOption("cmdstanr_warn_inits", TRUE),
                                  ...) {
  args <- formals(init)
  if (is.null(args)) {
    fn_test <- init()
    init_list <- lapply(seq_len(num_procs), function(i) init())
  } else {
    if (!identical(names(args), "chain_id")) {
      stop("If 'init' is a function it must have zero arguments ",
           "or only argument 'chain_id'.", call. = FALSE)
    }
    fn_test <- init(1)
    init_list <- lapply(seq_len(num_procs), function(i) init(i))
  }
  if (!is.list(fn_test) || is.data.frame(fn_test)) {
    stop("If 'init' is a function it must return a single list.")
  }
  process_init(init_list, num_procs, model_variables)
}

#' Validate a fit is a valid init
#' @noRd
validate_fit_init = function(init, model_variables) {
  # Convert from data.table to data.frame
  if (all(init$return_codes() == 1)) {
    stop("We are unable to create initial values from a model with no samples. Please check the results of the model used for inits before continuing.")
  } else if (!is.null(model_variables) &&!any(names(model_variables$parameters) %in% init$metadata()$stan_variables)) {
    stop("None of the names of the parameters for the model used for initial values match the names of parameters from the model currently running.")
  }
}

#' Write initial values to files if provided as a `CmdStanMCMC` class
#' @noRd
#' @param init A `CmdStanMCMC` class
#' @param num_procs Number of inits requested
#' @param model_variables  A list of all parameters with their types and
#'   number of dimensions. Typically the output of `model$variables()$parameters`.
#' @param warn_partial Should a warning be thrown if inits are only specified
#'   for a subset of parameters? Can be controlled by global option
#'   `cmdstanr_warn_inits`.
#' @return A character vector of file paths.
#' @export
process_init.CmdStanMCMC <- function(init, num_procs, model_variables = NULL,
                                     warn_partial = getOption("cmdstanr_warn_inits", TRUE),
                                     ...) {
  validate_fit_init(init, model_variables)
  draws_df = init$draws(format = "df")
  if (is.null(model_variables)) {
    model_variables = list(parameters = colnames(draws_df)[2:(length(colnames(draws_df)) - 3)])
  }
  init_draws_df = posterior::resample_draws(draws_df, ndraws = num_procs,
                                            method = "simple_no_replace")
  init_draws_lst = process_init(init_draws_df,
                                num_procs = num_procs, model_variables = model_variables)
  return(init_draws_lst)
}

#' Performs PSIS resampling on the draws from an approxmation method for inits.
#' @noRd
#' @param init A set of draws with `lp__` and `lp_approx__` columns.
#' @param num_procs Number of inits requested
#' @param model_variables  A list of all parameters with their types and
#'   number of dimensions. Typically the output of `model$variables()$parameters`.
#' @param warn_partial Should a warning be thrown if inits are only specified
#'   for a subset of parameters? Can be controlled by global option
#'   `cmdstanr_warn_inits`.
#' @return A character vector of file paths.
#' @importFrom stats aggregate
process_init_approx <- function(init, num_procs, model_variables = NULL,
                                warn_partial = getOption("cmdstanr_warn_inits", TRUE),
                                ...) {
  validate_fit_init(init, model_variables)
  # Convert from data.table to data.frame
  draws_df = init$draws(format = "df")
  if (is.null(model_variables)) {
    model_variables = list(parameters = colnames(draws_df)[3:(length(colnames(draws_df)) - 3)])
  }
  draws_df$lw = draws_df$lp__ - draws_df$lp_approx__
  # Replace NaN and Inf with -Inf
  draws_df$lw[!is.finite(draws_df$lw)] <- -Inf
  # Calculate unique draws based on 'lw' using base R functions
  unique_draws = length(unique(draws_df$lw))
  if (num_procs > unique_draws) {
    if (inherits(init, "CmdStanPathfinder")) {
      algo_name = " Pathfinder "
      extra_msg = " Try running Pathfinder with psis_resample=FALSE."
    } else if (inherits(init, "CmdStanVB")) {
      algo_name = " CmdStanVB "
      extra_msg = ""
    } else if (inherits(init, "CmdStanLaplace")) {
      algo_name = " CmdStanLaplace "
      extra_msg = ""
    } else {
      algo_name = ""
      extra_msg = ""
    }
    stop(paste0("Not enough distinct draws (", num_procs, ") in", algo_name ,
      "fit to create inits.", extra_msg))
  }
  if (unique_draws < (0.95 * nrow(draws_df))) {
    temp_df = stats::aggregate(.draw ~ lw, data = draws_df, FUN = min)
    draws_df = posterior::as_draws_df(merge(temp_df, draws_df, by = 'lw'))
    draws_df$weight = exp(draws_df$lw - max(draws_df$lw))
  } else {
      draws_df$weight = posterior::pareto_smooth(
        exp(draws_df$lw - max(draws_df$lw)), tail = "right", r_eff=1, return_k=FALSE)
  }
  init_draws_df = posterior::resample_draws(draws_df, ndraws = num_procs,
                                            weights = draws_df$weight, method = "simple_no_replace")
  init_draws_lst = process_init(init_draws_df,
                                num_procs = num_procs, model_variables = model_variables, warn_partial)
  return(init_draws_lst)
}


#' Write initial values to files if provided as a `CmdStanPathfinder` class
#' @noRd
#' @param init A `CmdStanPathfinder` class
#' @param num_procs Number of inits requested
#' @param model_variables  A list of all parameters with their types and
#'   number of dimensions. Typically the output of `model$variables()$parameters`.
#' @param warn_partial Should a warning be thrown if inits are only specified
#'   for a subset of parameters? Can be controlled by global option
#'   `cmdstanr_warn_inits`.
#' @return A character vector of file paths.
#' @export
process_init.CmdStanPathfinder <- function(init, num_procs, model_variables = NULL,
                                           warn_partial = getOption("cmdstanr_warn_inits", TRUE),
                                           ...) {
  if (!init$metadata()$calculate_lp) {
    validate_fit_init(init, model_variables)
    # Convert from data.table to data.frame
    draws_df = init$draws(format = "df")
    if (is.null(model_variables)) {
      model_variables = list(parameters = colnames(draws_df)[3:(length(colnames(draws_df)) - 3)])
    }
    draws_df$weight = rep(1.0, nrow(draws_df))
    init_draws_df = posterior::resample_draws(draws_df, ndraws = num_procs,
      weights = draws_df$weight, method = "simple_no_replace")
    init_draws_lst = process_init(init_draws_df,
      num_procs = num_procs, model_variables = model_variables, warn_partial)
    return(init_draws_lst)
  } else {
    process_init_approx(init, num_procs, model_variables, warn_partial)
  }
}

#' Write initial values to files if provided as a `CmdStanVB` class
#' @noRd
#' @param init A `CmdStanVB` class
#' @param num_procs Number of inits requested
#' @param model_variables  A list of all parameters with their types and
#'   number of dimensions. Typically the output of `model$variables()$parameters`.
#' @param warn_partial Should a warning be thrown if inits are only specified
#'   for a subset of parameters? Can be controlled by global option
#'   `cmdstanr_warn_inits`.
#' @return A character vector of file paths.
#' @export
process_init.CmdStanVB <- function(init, num_procs, model_variables = NULL,
                                   warn_partial = getOption("cmdstanr_warn_inits", TRUE),
                                   ...) {
  process_init_approx(init, num_procs, model_variables, warn_partial)
}

#' Write initial values to files if provided as a `CmdStanLaplace` class
#' @noRd
#' @param init A `CmdStanLaplace` class
#' @param num_procs Number of inits requested
#' @param model_variables  A list of all parameters with their types and
#'   number of dimensions. Typically the output of `model$variables()$parameters`.
#' @param warn_partial Should a warning be thrown if inits are only specified
#'   for a subset of parameters? Can be controlled by global option
#'   `cmdstanr_warn_inits`.
#' @return A character vector of file paths.
#' @export
process_init.CmdStanLaplace <- function(init, num_procs, model_variables = NULL,
                                        warn_partial = getOption("cmdstanr_warn_inits", TRUE),
                                        ...) {
  process_init_approx(init, num_procs, model_variables, warn_partial)
}


#' Write initial values to files if provided as a `CmdStanMLE` class
#' @noRd
#' @param init A `CmdStanMLE` class
#' @param num_procs Number of inits requested
#' @param model_variables  A list of all parameters with their types and
#'   number of dimensions. Typically the output of `model$variables()$parameters`.
#' @param warn_partial Should a warning be thrown if inits are only specified
#'   for a subset of parameters? Can be controlled by global option
#'   `cmdstanr_warn_inits`.
#' @return A character vector of file paths.
#' @export
process_init.CmdStanMLE <- function(init, num_procs, model_variables = NULL,
                                    warn_partial = getOption("cmdstanr_warn_inits", TRUE),
                                    ...) {
  # Convert from data.table to data.frame
  validate_fit_init(init, model_variables)
  draws_df = init$draws(format = "df")
  if (is.null(model_variables)) {
    model_variables = list(parameters = colnames(draws_df)[2:(length(colnames(draws_df)) - 3)])
  }
  init_draws_df = draws_df[rep(1, num_procs),]
  init_draws_lst_lst = process_init(init_draws_df,
                                    num_procs = num_procs, model_variables = model_variables, warn_partial)
  return(init_draws_lst_lst)
}

#' Validate initial values
#'
#' For CmdStan `init` must be `NULL`, a single real number >= 0, or paths to
#' init files for each chain.
#'
#' @noRd
#' @param init User's `init` argument or output from `process_init_*()`.
#' @param num_procs Number of CmdStan processes (number of chains if MCMC)
#' @return Either throws an error or returns `invisible(TRUE)`.
validate_init <- function(init, num_procs) {
  if (is.null(init)) {
    return(invisible(TRUE))
  }
  if (!is.numeric(init) && !is.character(init)) {
    stop("Invalid 'init' specification. See documentation of 'init' argument.",
         call. = FALSE)
  } else if (is.numeric(init) && (length(init) > 1 || init < 0)) {
    stop("If 'init' is numeric it must be a single real number >= 0.",
         call. = FALSE)
  } else if (is.character(init)) {
    if (length(init) != 1 && length(init) != num_procs) {
      stop("If 'init' is specified as a character vector it must have ",
           "length 1 or number of chains.",
           call. = FALSE)
    }
    assert_file_exists(init, access = "r")
  }

  invisible(TRUE)
}

#' Recycle init if numeric and length 1
#' @noRd
#' @param init Already validated `init` argument.
#' @param num_procs Number of CmdStan processes.
#' @return `init`, unless numeric and length 1, in which case `rep(init, num_procs)`.
maybe_recycle_init <- function(init, num_procs) {
  if (is.null(init) ||
      length(init) == num_procs) {
    return(init)
  }
  rep(init, num_procs)
}


#' Validate seed
#'
#' `seed` must be `NULL`, a single positive integer, or one positive integer per
#' chain.
#'
#' @noRd
#' @param seed User's `seed` argument.
#' @param num_procs Number of CmdStan processes (number of chains if MCMC)
#' @return Either throws an error or returns `invisible(TRUE)`.
validate_seed <- function(seed, num_procs) {
  if (is.null(seed)) {
    return(invisible(TRUE))
  }
  if (cmdstan_version() < "2.26") {
    lower_seed <- 1
  } else {
    lower_seed <- 0
  }
  checkmate::assert_integerish(seed, lower = lower_seed)
  if (length(seed) > 1 && length(seed) != num_procs) {
    stop("If 'seed' is specified it must be a single integer or one per chain.",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Generate seed(s) if missing
#' @noRd
#' @param seed Already validated `seed` argument.
#' @param num_procs Number of CmdStan processes.
#' @return An integer vector of length `num_procs`.
maybe_generate_seed <- function(seed, num_procs) {
  if (is.null(seed)) {
    seed <- base::rep(base::sample(.Machine$integer.max, 1), num_procs)
  } else if (length(seed) == 1 && num_procs > 1) {
    seed <- base::rep(as.integer(seed), num_procs)
  }
  seed
}

#' Validate metric
#' @noRd
#' @param metric User's `metric` argument.
#' @param num_procs Number of CmdStan processes (number of MCMC chains).
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
#' @param num_procs Number of CmdStan processes (number of MCMC chains).
#' @return Either throws an error or returns `invisible(TRUE)`.
#'
validate_metric_file <- function(metric_file, num_procs) {
  if (is.null(metric_file)) {
    return(invisible(TRUE))
  }

  assert_file_exists(metric_file, access = "r")

  if (length(metric_file) != 1 && length(metric_file) != num_procs) {
    stop(length(metric_file), " metric(s) provided. Must provide ",
         if (num_procs > 1) "1 or ", num_procs, " metric(s) for ",
         num_procs, " chain(s).")
  }

  invisible(TRUE)
}

#' Recycle metric_file if not NULL
#' @noRd
#' @param metric_file Path to already validated `metric_file` argument.
#' @param num_procs Number of CmdStan processes.
#' @return `rep(metric_file, num_procs)` if metric_file is a single path, otherwise
#'    return `metric_file`.
maybe_recycle_metric_file <- function(metric_file, num_procs) {
  if (is.null(metric_file) ||
      length(metric_file) == num_procs) {
    return(metric_file)
  }
  rep(metric_file, num_procs)
}

available_metrics <- function() {
  c("unit_e", "diag_e", "dense_e")
}

# Composition helpers -----------------------------------------------------

#' Helper function to make valid CmdStan arguments
#' @noRd
#' @param self An Args object (e.g., `SampleArgs`).
#' @param arg_name Name of slot in `self` containing the argument value.
#' @param cmdstan_arg_name Name of corresponding argument for CmdStan (if not
#'   the same as arg_name). For example for `arg_name="max_treedepth"` we have
#'   `cmdstan_arg_name="max_depth"` (at least until names change in CmdStan 3).
#' @param idx Chain id (only applicable for MCMC).
compose_arg <- function(self, arg_name, cmdstan_arg_name = NULL, idx = NULL) {
  val <- self[[arg_name]]
  cmdstan_arg_name <- cmdstan_arg_name %||% arg_name

  if (is.null(val)) {
    return(NULL)
  }

  if (os_is_wsl() && (arg_name %in% c("metric_file", "fitted_params"))) {
    val <- sapply(val, wsl_safe_path)
  }
  if (!is.null(idx) && length(val) >= idx) {
    val <- val[idx]
  }
  # e.g. adapt_delta -> delta (to deal with weird hierarchical arg structure in CmdStan 2)
  cmdstan_arg_name <- sub("adapt_", "", cmdstan_arg_name)
  paste0(cmdstan_arg_name, "=", val)
}
