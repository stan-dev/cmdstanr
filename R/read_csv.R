#' Read CmdStan CSV files into R
#'
#' `read_cmdstan_csv()` is used internally by CmdStanR to read CmdStan's output
#' CSV files into \R. It can also be used by CmdStan users as a more flexible
#' and efficient alternative to `rstan::read_stan_csv()`.
#'
#' @export
#' @param files A character vector of paths to the CSV files to read.
#' @param variables Optionally, a character vector naming the variables
#'   (parameters, transformed parameters, and generated quantities) to read in.
#'   * If `NULL` (the default) then all variables are included.
#'   * If an empty string (`variables=""`) then none are included.
#'   * For non-scalar variables all elements or specific elements can be selected:
#'     - `variables = "theta"` selects all elements of `theta`;
#'     - `variables = c("theta[1]", "theta[3]")` selects only the 1st and 3rd elements.
#' @param sampler_diagnostics Works the same way as `variables` but for sampler
#'   diagnostic variables (e.g., `"treedepth__"`, `"accept_stat__"`, etc.).
#'   Ignored if the model was not fit using MCMC.
#'
#' @return A named list with the following components:
#'
#' * `metadata`: A list of the meta information from the run that produced the
#' CSV file(s). See **Examples** below.
#'
#' The other components in the returned list depend on the method that produced
#' the CSV file(s).
#'
#' For [sampling][model-method-sample] the returned list also includes the
#' following components:
#'
#' * `inv_metric`: A list (one element per chain) of inverse mass matrices
#' or their diagonals, depending on the type of metric used.
#' * `step_size`: A list (one element per chain) of the step sizes used.
#' * `warmup_draws`:  If `save_warmup` was `TRUE` when fitting the model then a
#' [`draws_array`][posterior::draws_array] of warmup draws.
#' * `post_warmup_draws`: A [`draws_array`][posterior::draws_array] of
#' post-warmup draws.
#' * `warmup_sampler_diagnostics`:  If `save_warmup` was `TRUE` when fitting the
#' model then a [`draws_array`][posterior::draws_array] of warmup draws of the
#' sampler diagnostic variables.
#' * `post_warmup_sampler_diagnostics`: A [`draws_array`][posterior::draws_array]
#' of post-warmup draws of the sampler diagnostic variables.
#'
#' For [optimization][model-method-optimize] the returned list also includes the
#' following components:
#'
#' * `point_estimates`: Point estimates for the model parameters.
#'
#' For [variational inference][model-method-variational] the returned list also
#' includes the following components:
#'
#' * `draws`: A [`draws_matrix`][posterior::draws_matrix] of draws from the
#' approximate posterior distribution.
#'
#' For [standalone generated quantities][model-method-generate-quantities] the
#' returned list also includes the following components:
#'
#' * `generated_quantities`: A [`draws_matrix`][posterior::draws_matrix] of
#' the generated quantities.
#'
#' @examples
#' \dontrun{
#' stan_program <- tempfile(fileext=".stan")
#' cat("
#' parameters {
#'   real alpha_scalar;
#'   vector[2] theta_vector;
#'   matrix[2,2] tau_matrix;
#' }
#' model {
#'   alpha_scalar ~ std_normal();
#'   theta_vector ~ std_normal();
#'   to_vector(tau_matrix) ~ std_normal();
#' }
#' ", file = stan_program)
#'
#' # only using capture.output to avoid too much printed output in example
#' out <- utils::capture.output(
#'   mod <- cmdstan_model(stan_program),
#'   fit <- mod$sample(save_warmup=TRUE)
#' )
#'
#' # Read in everything
#' x <- read_cmdstan_csv(fit$output_files())
#' str(x)
#'
#' # Don't read in any of the sampler diagnostic variables
#' x <- read_cmdstan_csv(fit$output_files(), sampler_diagnostics = "")
#'
#' # Don't read in any of the parameters or generated quantities
#' x <- read_cmdstan_csv(fit$output_files(), variables = "")
#'
#' # Read in only specific parameters and sampler diagnostics
#' x <- read_cmdstan_csv(
#'   fit$output_files(),
#'   variables = c("alpha_scalar", "theta_vector[2]"),
#'   sampler_diagnostics = c("n_leapfrog__", "accept_stat__")
#' )
#'
#' # For non-scalar parameters all elements can be selected or only some elements,
#' # e.g. all of "theta_vector" but only one element of "tau_matrix"
#' x <- read_cmdstan_csv(
#'   fit$output_files(),
#'   variables = c("theta_vector", "tau_matrix[2,1]")
#' )
#' }
#'
read_cmdstan_csv <- function(files,
                            variables = NULL,
                            sampler_diagnostics = NULL) {
  metadata <- NULL
  warmup_draws <- NULL
  warmup_sampler_diagnostics_draws <- NULL
  post_warmup_draws <- NULL
  post_warmup_sampler_diagnostics_draws <- NULL
  generated_quantities <- NULL
  variational_draws <- NULL
  point_estimates <- NULL
  inv_metric <- list()
  step_size <- list()
  col_types <- NULL
  col_select <- NULL
  not_matching <- c()
  for (output_file in files) {
    checkmate::assert_file_exists(output_file, access = "r", extension = "csv")
    if (is.null(metadata)) {
      metadata <- read_csv_metadata(output_file)
      if (!is.null(metadata$inv_metric)) {
        inv_metric[[metadata$id]] <- metadata$inv_metric
      }
      if (!is.null(metadata$step_size_adaptation)) {
        step_size[[metadata$id]] <- metadata$step_size_adaptation
      }
      id <- metadata$id
    } else {
      csv_file_info <- read_csv_metadata(output_file)
      check <- check_csv_metadata_matches(metadata, csv_file_info)
      if (!is.null(check$error)) {
        stop(check$error, call. = FALSE)
      }
      not_matching <- c(not_matching, check$not_matching)
      metadata$id <- c(metadata$id, csv_file_info$id)
      metadata$seed <- c(metadata$seed, csv_file_info$seed)
      metadata$step_size_adaptation <- c(metadata$step_size_adaptation, csv_file_info$step_size_adaptation)
      metadata$fitted_params <- c(metadata$fitted_params, csv_file_info$fitted_params)

      if (!is.null(csv_file_info$inv_metric)) {
        inv_metric[[csv_file_info$id]] <- csv_file_info$inv_metric
      }
      if (!is.null(csv_file_info$step_size_adaptation)) {
        step_size[[csv_file_info$id]] <- csv_file_info$step_size_adaptation
      }
      id <- csv_file_info$id
    }
    if (is.null(col_select)) {
      if (is.null(variables)) { # variables = NULL returns all
        variables <- metadata$model_params
      } else if (!any(nzchar(variables))) { # if variables = "" returns none
        variables <- NULL
      } else { # filter using variables
        res <- matching_variables(variables, metadata$model_params)
        if (length(res$not_found)) {
          stop("Can't find the following variable(s) in the output: ",
               paste(res$not_found, collapse = ", "), call. = FALSE)
        }
        variables <- res$matching
      }
      if (is.null(sampler_diagnostics)) {
        sampler_diagnostics <- metadata$sampler_diagnostics
      } else if (!any(nzchar(sampler_diagnostics))) { # if sampler_diagnostics = "" returns none
        sampler_diagnostics <- NULL
      } else {
        selected_sampler_diag <- rep(FALSE, length(metadata$sampler_diagnostics))
        not_found <- NULL
        for (p in sampler_diagnostics) {
          matches <- metadata$sampler_diagnostics == p | startsWith(metadata$sampler_diagnostics, paste0(p,"."))
          if (!any(matches)) {
            not_found <- c(not_found, p)
          }
          selected_sampler_diag <- selected_sampler_diag | matches
        }
        if (length(not_found)) {
          stop("Can't find the following sampler diagnostic(s) in the output: ",
               paste(not_found, collapse = ", "), call. = FALSE)
        }
        sampler_diagnostics <- metadata$sampler_diagnostics[selected_sampler_diag]
      }
      if (metadata$method == "generate_quantities") {
        col_select <- c(col_select, variables)
      } else {
        col_select <- "lp__"
        col_select <- c(col_select, variables[variables!="lp__"])
        col_select <- c(col_select, sampler_diagnostics)
      }
    }
    if (metadata$method == "sample") {
      num_warmup_draws <- ceiling(metadata$iter_warmup / metadata$thin)
      num_post_warmup_draws <- ceiling(metadata$iter_sampling / metadata$thin)
      all_draws <- num_warmup_draws + num_post_warmup_draws
    } else if (metadata$method == "variational") {
      all_draws <- metadata$output_samples
    } else if (metadata$method == "optimize") {
      all_draws <- 1
    }

    if (metadata$method == "generate_quantities") {
      # set the first arg as double
      # to silence the type detection info
      col_types <- list()
      col_types[[col_select[1]]] = "d"
      suppressWarnings(
        draws <- vroom::vroom(
          output_file,
          comment = "#",
          delim = ',',
          col_select = col_select,
          col_types = col_types,
          trim_ws = TRUE,
          altrep = FALSE,
          progress = FALSE,
          skip = metadata$lines_to_skip
        )
      )
    } else {
      suppressWarnings(
        draws <- vroom::vroom(
          output_file,
          comment = "#",
          delim = ',',
          trim_ws = TRUE,
          col_select = col_select,
          col_types = c("lp__" = "d"),
          altrep = FALSE,
          progress = FALSE,
          skip = metadata$lines_to_skip,
          n_max = all_draws * 2
        )
      )
      draws <- draws[!is.na(draws$lp__), ]
    }
    if (nrow(draws) > 0) {
      if (metadata$method == "sample") {
        if (metadata$save_warmup == 1) {
          if (length(variables) > 0) {
            warmup_draws <- posterior::bind_draws(
              warmup_draws,
              posterior::as_draws_array(draws[1:num_warmup_draws, variables]),
              along="chain"
            )
            post_warmup_draws <- posterior::bind_draws(
              post_warmup_draws,
              posterior::as_draws_array(draws[(num_warmup_draws+1):all_draws, variables]),
              along="chain"
            )
          }
          if (length(sampler_diagnostics) > 0) {
            warmup_sampler_diagnostics_draws <- posterior::bind_draws(
              warmup_sampler_diagnostics_draws,
              posterior::as_draws_array(draws[1:num_warmup_draws, sampler_diagnostics]),
              along="chain"
            )
            post_warmup_sampler_diagnostics_draws <- posterior::bind_draws(
              post_warmup_sampler_diagnostics_draws,
              posterior::as_draws_array(draws[(num_warmup_draws+1):all_draws, sampler_diagnostics]),
              along="chain"
            )
          }
        } else {
            warmup_draws <- NULL
            warmup_sampler_diagnostics_draws <- NULL
            if (length(variables) > 0) {
              post_warmup_draws <- posterior::bind_draws(
                post_warmup_draws,
                posterior::as_draws_array(draws[, variables]),
                along="chain"
              )
            }
            if (length(sampler_diagnostics) > 0 && all(metadata$algorithm != "fixed_param")) {
              post_warmup_sampler_diagnostics_draws <- posterior::bind_draws(
                post_warmup_sampler_diagnostics_draws,
                posterior::as_draws_array(draws[, sampler_diagnostics]),
                along="chain"
              )
            }
        }
      } else if (metadata$method == "variational") {
        # ignore first line as it's just the mean and lp__ as it's always 0
        variational_draws <- posterior::as_draws_matrix(
          draws[-1, colnames(draws) != "lp__", drop=FALSE]
        )
        if ("log_p__" %in% posterior::variables(variational_draws)) {
          variational_draws <- posterior::rename_variables(variational_draws, lp__ = "log_p__")
        }
        if ("log_g__" %in% posterior::variables(variational_draws)) {
          variational_draws <- posterior::rename_variables(variational_draws, lp_approx__ = "log_g__")
        }
      } else if (metadata$method == "optimize") {
        point_estimates <- posterior::as_draws_matrix(draws[1,, drop=FALSE])[, variables]
      } else if (metadata$method == "generate_quantities") {
          generated_quantities <- posterior::bind_draws(generated_quantities,
                                                        posterior::as_draws_array(draws),
                                                        along="chain")
      }
    }
  }

  if (length(not_matching) > 0) {
    not_matching_list <- paste(unique(not_matching), collapse = ", ")
    warning("The supplied csv files do not match in the following arguments: ",
            paste(not_matching_list, collapse = ", "), call. = FALSE)
  }

  metadata$inv_metric <- NULL
  metadata$lines_to_skip <- NULL
  metadata$model_params <- repair_variable_names(metadata$model_params)
  repaired_variables <- repair_variable_names(variables)
  if (metadata$method == "sample") {
    if (!is.null(warmup_draws)) {
      posterior::variables(warmup_draws) <- repaired_variables
    }
    if (!is.null(post_warmup_draws)) {
      posterior::variables(post_warmup_draws) <- repaired_variables
    }
    list(
      metadata = metadata,
      inv_metric = inv_metric,
      step_size = step_size,
      warmup_draws = warmup_draws,
      post_warmup_draws = post_warmup_draws,
      warmup_sampler_diagnostics = warmup_sampler_diagnostics_draws,
      post_warmup_sampler_diagnostics = post_warmup_sampler_diagnostics_draws
    )
  } else if (metadata$method == "variational") {
    metadata$model_params <- metadata$model_params[metadata$model_params != "lp__"]
    metadata$model_params <- gsub("log_p__", "lp__", metadata$model_params)
    metadata$model_params <- gsub("log_g__", "lp_approx__", metadata$model_params)
    repaired_variables <- repaired_variables[repaired_variables != "lp__"]
    repaired_variables <- gsub("log_p__", "lp__", repaired_variables)
    repaired_variables <- gsub("log_g__", "lp_approx__", repaired_variables)
    if (!is.null(variational_draws)) {
      posterior::variables(variational_draws) <- repaired_variables
    }
    list(
      metadata = metadata,
      draws = variational_draws
    )
  } else if (metadata$method == "optimize") {
    if (!is.null(point_estimates)) {
      posterior::variables(point_estimates) <- repaired_variables
    }
    list(
      metadata = metadata,
      point_estimates = point_estimates
    )
  } else if (metadata$method == "generate_quantities") {
    if (!is.null(generated_quantities)) {
      posterior::variables(generated_quantities) <- repaired_variables
    }
    list(
      metadata = metadata,
      generated_quantities = generated_quantities
    )
  }
}

#' Read CmdStan CSV files from sampling into \R
#'
#' Deprecated. Use [read_cmdstan_csv()] instead.
#'
#' @export
#' @param files,variables,sampler_diagnostics Deprecated. Use
#'   [read_cmdstan_csv()] instead.
#'
read_sample_csv <- function(files,
                            variables = NULL,
                            sampler_diagnostics = NULL) {
  warning("read_sample_csv() is deprecated. Please use read_cmdstan_csv().")
  read_cmdstan_csv(files, variables, sampler_diagnostics)
}


# internal ----------------------------------------------------------------

#' Reads the sampling arguments and the diagonal of the
#' inverse mass matrix from the comments in a CSV file.
#'
#' @noRd
#' @param csv_file A CSV file containing results from CmdStan.
#' @return A list containing all CmdStan settings and, for sampling, the inverse
#'   mass matrix (or its diagonal depending on the metric).
#'
read_csv_metadata <- function(csv_file) {
  checkmate::assert_file_exists(csv_file, access = "r", extension = "csv")
  con  <- file(csv_file, open = "r")
  adaptation_terminated <- FALSE
  param_names_read <- FALSE
  inv_metric_next <- FALSE
  inv_metric_diagonal_next <- FALSE
  csv_file_info <- list()
  csv_file_info[["inv_metric"]] <- NULL
  inv_metric_rows <- 0
  parsing_done <- FALSE
  lines_before_param_names <- 0
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0 && !parsing_done) {
    if (!startsWith(line, "#")) {
      if (!param_names_read) {
        param_names_read <- TRUE
        all_names <- strsplit(line, ",")[[1]]
        csv_file_info[["sampler_diagnostics"]] <- c()
        csv_file_info[["model_params"]] <- c()
        for(x in all_names) {
          if (all(csv_file_info$algorithm != "fixed_param")) {
            if (endsWith(x, "__") && !(x %in% c("lp__", "log_p__", "log_g__"))) {
              csv_file_info[["sampler_diagnostics"]] <- c(csv_file_info[["sampler_diagnostics"]], x)
            } else {
              csv_file_info[["model_params"]] <- c(csv_file_info[["model_params"]], x)
            }
          } else {
            if (!endsWith(x, "__")) {
              csv_file_info[["model_params"]] <- c(csv_file_info[["model_params"]], x)
            }
          }
        }
      }
    } else {
      if (!param_names_read) {
        lines_before_param_names <- lines_before_param_names + 1
      }
      if (!adaptation_terminated) {
        if (regexpr("# Adaptation terminated", line, perl = TRUE) > 0) {
          adaptation_terminated <- TRUE
        } else {
          tmp <- gsub("#", "", line, fixed = TRUE)
          tmp <- gsub("(Default)", "", tmp, fixed = TRUE)
          key_val <- grep("=", tmp, fixed = TRUE, value = TRUE)
          key_val <- strsplit(key_val, split = "=", fixed = TRUE)
          key_val <- rapply(key_val, trimws)
          if (length(key_val) == 2) {
            numeric_val <- suppressWarnings(as.numeric(key_val[2]))
            if (!is.na(numeric_val)) {
              csv_file_info[[key_val[1]]] <- numeric_val
            } else {
              if (nzchar(key_val[2])) {
                csv_file_info[[key_val[1]]] <- key_val[2]
              }
            }
          }
        }
      } else {
        # after adaptation terminated read in the step size and inverse metrics
        if (regexpr("# Step size = ", line, perl = TRUE) > 0) {
          csv_file_info$stepsize_adaptation <- as.numeric(strsplit(line, " = ")[[1]][2])
        } else if (regexpr("# Diagonal elements of inverse mass matrix:", line, perl = TRUE) > 0) {
          inv_metric_diagonal_next <- TRUE
        } else if (regexpr("# Elements of inverse mass matrix:", line, perl = TRUE) > 0){
          inv_metric_next <- TRUE
        } else if (inv_metric_diagonal_next) {
          inv_metric_split <- strsplit(gsub("# ", "", line), ",")
          if ((length(inv_metric_split) == 0) ||
              ((length(inv_metric_split) == 1) && identical(inv_metric_split[[1]], character(0)))) {
            break;
          }
          csv_file_info$inv_metric <- rapply(inv_metric_split, as.numeric)
          parsing_done <- TRUE
        } else if (inv_metric_next) {
          inv_metric_split <- strsplit(gsub("# ", "", line), ",")
          if ((length(inv_metric_split) == 0) ||
              ((length(inv_metric_split) == 1) && identical(inv_metric_split[[1]], character(0)))) {
            parsing_done <- TRUE
            break;
          }
          if (inv_metric_rows == 0) {
            csv_file_info$inv_metric <- rapply(inv_metric_split, as.numeric)
          } else {
            csv_file_info$inv_metric <- c(csv_file_info$inv_metric, rapply(inv_metric_split, as.numeric))
          }
          inv_metric_rows <- inv_metric_rows + 1
        }
      }
    }
  }
  close(con)
  if (is.null(csv_file_info$method)) {
    stop("Supplied CSV file is corrupt!", call. = FALSE)
  }
  if (length(csv_file_info$sampler_diagnostics) == 0 && length(csv_file_info$model_params) == 0) {
    stop("The supplied csv file does not contain any variable names or data!", call. = FALSE)
  }
  if (inv_metric_rows > 0) {
    rows <- inv_metric_rows
    cols <- length(csv_file_info$inv_metric)/inv_metric_rows
    dim(csv_file_info$inv_metric) <- c(rows,cols)
  }

  # rename from old cmdstan names to new cmdstanX names
  csv_file_info$model_name <- csv_file_info$model
  csv_file_info$adapt_engaged <- csv_file_info$engaged
  csv_file_info$adapt_delta <- csv_file_info$delta
  csv_file_info$max_treedepth <- csv_file_info$max_depth
  csv_file_info$step_size <- csv_file_info$stepsize
  csv_file_info$step_size_adaptation <- csv_file_info$stepsize_adaptation
  csv_file_info$iter_warmup <- csv_file_info$num_warmup
  csv_file_info$iter_sampling <- csv_file_info$num_samples
  csv_file_info$threads_per_chain <- csv_file_info$num_threads
  csv_file_info$model <- NULL
  csv_file_info$engaged <- NULL
  csv_file_info$delta <- NULL
  csv_file_info$max_depth <- NULL
  csv_file_info$stepsize <- NULL
  csv_file_info$stepsize_adaptation <- NULL
  csv_file_info$num_warmup <- NULL
  csv_file_info$num_samples <- NULL
  csv_file_info$file <- NULL
  csv_file_info$diagnostic_file <- NULL
  csv_file_info$metric_file <- NULL
  csv_file_info$num_threads <- NULL
  csv_file_info$lines_to_skip <- lines_before_param_names

  csv_file_info
}

#' Check that the sampling information from two CSV files matches.
#' Will throw errors if the sampling information doesn't match. If
#' it returns, the sampling information matches.
#'
#' @noRd
#' @param a,b Two lists returned by `read_csv_metadata()` to compare.
#'
check_csv_metadata_matches <- function(a, b) {
  if (a$model_name != b$model_name) {
    return(list(error = "Supplied CSV files were not generated with the same model!"))
  }
  if (a$method != b$method) {
    return(list(error = "Supplied CSV files were produced by different methods and need to be read in separately!"))
  }
  if ((length(a$model_params) != length(b$model_params)) ||
      !(all(a$model_params == b$model_params) &&
        all(a$sampler_diagnostics == b$sampler_diagnostics))) {
    return(list(error = "Supplied CSV files have samples for different variables!"))
  }
  if (a$method == "sample") {
    if (a$iter_sampling != b$iter_sampling ||
        a$thin != b$thin ||
        a$save_warmup != b$save_warmup ||
        (a$save_warmup == 1 && a$iter_warmup != b$iter_warmup)) {
      return(list(error = "Supplied CSV files dont match in the number of output samples!"))
    }
  } else if (a$method == "variational") {
    if (a$output_samples != b$output_samples) {
      return(list(error = "Supplied CSV files dont match in the number of output samples!"))
    }
  }
  match_list <- c("stan_version_major", "stan_version_minor", "stan_version_patch", "gamma", "kappa",
                  "t0", "init_buffer", "term_buffer", "window", "algorithm", "engine", "max_treedepth",
                  "metric", "step_size", "stepsize_jitter", "adapt_engaged", "adapt_delta", "iter_warmup")
  not_matching <- c()
  for (name in names(a)) {
    if ((name %in% match_list) && (is.null(b[[name]]) ||  all(a[[name]] != b[[name]]))) {
      not_matching <- c(not_matching, name)
    }
  }
  list(not_matching = not_matching)
}



# convert names like beta.1.1 to beta[1,1]
repair_variable_names <- function(names) {
  names <- sub("\\.", "[", names)
  names <- gsub("\\.", ",", names)
  names[grep("\\[", names)] <-
    paste0(names[grep("\\[", names)], "]")
  names
}

# convert names like beta[1,1] to beta.1.1
unrepair_variable_names <- function(names) {
  names <- sub("\\[", "\\.", names)
  names <- gsub(",","\\.",  names)
  names <- gsub("\\]","",  names)
  names
}

remaining_columns_to_read <- function(requested, currently_read, all) {
  if (is.null(requested)) {
    if (is.null(all)) {
      return(NULL)
    }
    requested <- all
  }
  if (!any(nzchar(requested))) {
    return(requested)
  }
  if (is.null(all)) {
    unread <- requested[!(requested %in% currently_read)]
  } else {
    all_remaining <- all[!(all %in% currently_read)]
    unread <- c()
    for (p in requested) {
      if (any(all_remaining == p)) {
        unread <- c(unread, p)
      }
      is_unread_element <- startsWith(all_remaining, paste0(p,"["))
      if (any(is_unread_element)) {
        unread <- c(unread, all_remaining[is_unread_element])
      }
    }
  }
  if (length(unread)) {
    unique(unread)
  } else {
    ""
  }
}
