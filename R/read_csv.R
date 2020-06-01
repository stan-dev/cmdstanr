#' Check that the sampling information from two CSV files matches.
#' Will throw errors if the sampling informations dont match. If
#' it returns, the sampling informations match.
#'
#' @noRd
#' @param a the first sampling info to check
#' @param b the second sampling info to check
#'
check_sampling_csv_info_matches <- function(a, b) {
  if (a$model_name != b$model_name) {
    return(list(error = "Supplied CSV files were not generated wtih the same model!"))
  }
  if ((length(a$model_params) != length(b$model_params)) ||
      !(all(a$model_params == b$model_params) &&
        all(a$sampler_diagnostics == b$sampler_diagnostics))) {
    return(list(error = "Supplied CSV files have samples for different parameters!"))
  }
  if (a$iter_sampling != b$iter_sampling ||
      a$thin != b$thin ||
      a$save_warmup != b$save_warmup ||
      (a$save_warmup == 1 && a$iter_warmup != b$iter_warmup)) {
    return(list(error = "Supplied CSV files dont match in the number of stored samples!"))
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

#' Reads the sampling arguments and the diagonal of the
#' inverse mass matrix from the comments in a CSV file.
#'
#' @noRd
#' @param csv_file A CSV file containing results from sampling
#' @return A list containing all sampling parameters and the
#' diagonal of the inverse mass matrix
#'
read_sample_info_csv <- function(csv_file) {
  checkmate::assert_file_exists(csv_file, access = "r", extension = "csv")
  adaptation_terminated <- FALSE
  param_names_read <- FALSE
  inverse_metric_next <- FALSE
  inverse_metric_diagonal_next <- FALSE
  csv_file_info = list()
  con  <- file(csv_file, open = "r")
  csv_file_info[["inverse_metric"]] <- NULL
  inverse_metric_rows <- 0
  parsing_done <- FALSE
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0 && !parsing_done) {
    if (!startsWith(line, "#")) {
      if (!param_names_read) {
        param_names_read <- TRUE
        all_names <- strsplit(line, ",")[[1]]
        csv_file_info[["sampler_diagnostics"]] <- c()
        csv_file_info[["model_params"]] <- c()
        for(x in all_names) {
          if (csv_file_info$algorithm != "fixed_param") {
            if (endsWith(x, "__") && x != "lp__") {
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
          inverse_metric_diagonal_next <- TRUE
        } else if (regexpr("# Elements of inverse mass matrix:", line, perl = TRUE) > 0){
          inverse_metric_next <- TRUE
        } else if (inverse_metric_diagonal_next) {
          inv_metric_split <- strsplit(gsub("# ", "", line), ",")
          if ((length(inv_metric_split) == 0) ||
              ((length(inv_metric_split) == 1) && identical(inv_metric_split[[1]], character(0)))) {
            break;
          }
          csv_file_info$inverse_metric <- rapply(inv_metric_split, as.numeric)
          parsing_done <- TRUE
        } else if (inverse_metric_next) {
          inv_metric_split <- strsplit(gsub("# ", "", line), ",")
          if ((length(inv_metric_split) == 0) ||
              ((length(inv_metric_split) == 1) && identical(inv_metric_split[[1]], character(0)))) {
            parsing_done <- TRUE
            break;
          }
          if (inverse_metric_rows == 0) {
            csv_file_info$inverse_metric <- rapply(inv_metric_split, as.numeric)
          } else {
            csv_file_info$inverse_metric <- c(csv_file_info$inverse_metric, rapply(inv_metric_split, as.numeric))
          }
          inverse_metric_rows <- inverse_metric_rows + 1
        }
      }
    }
  }
  close(con)
  if (is.null(csv_file_info$method)) {
    stop("Supplied CSV file is corrupt!")
  } else if (csv_file_info$method != "sample") {
    stop("Supplied CSV file was not generated with sampling. Consider using read_optim_csv or read_vb_csv!")
  }
  if (length(csv_file_info$sampler_diagnostics) == 0 && length(csv_file_info$model_params) == 0) {
    stop("The supplied csv file does not contain any parameter names or data!")
  }
  if (inverse_metric_rows > 0) {
    rows <- inverse_metric_rows
    cols <- length(csv_file_info$inverse_metric)/inverse_metric_rows
    dim(csv_file_info$inverse_metric) <- c(rows,cols)
  }

  # rename from old cmdstan names to new cmdstanX names
  csv_file_info$model_name <- csv_file_info$model
  csv_file_info$adapt_engaged <- csv_file_info$engaged
  csv_file_info$adapt_delta <- csv_file_info$delta
  csv_file_info$max_treedepth <- csv_file_info$max_depth
  csv_file_info$step_size <- csv_file_info$stepsize
  csv_file_info$iter_warmup <- csv_file_info$num_warmup
  csv_file_info$iter_sampling <- csv_file_info$num_samples
  csv_file_info$model <- NULL
  csv_file_info$engaged <- NULL
  csv_file_info$delta <- NULL
  csv_file_info$max_depth <- NULL
  csv_file_info$stepsize <- NULL
  csv_file_info$num_warmup <- NULL
  csv_file_info$num_samples <- NULL
  csv_file_info$file <- NULL
  csv_file_info$diagnostic_file <- NULL
  csv_file_info$metric_file <- NULL

  csv_file_info
}

#' Read samples from CmdStan CSV files into \R
#'
#' @export
#' @param files Paths to the CSV files to read.
#' @param pars (character vector) When `NULL`, the function will return
#' draws for all model parameters. When supplied a list, the function
#' will return  draws from the listed model parameters.
#' @param sampler_diagnostics (character vector) When `NULL`, the function will return
#' draws for all sampler diagnostics . When supplied a list, the function
#' will return  draws from the listed sampler diagnostics .
#' @param cores The number of cores to use to read and process the output files
#'
#' @return A named list containing:
#' * `sampling_info`: A list of the arguments used to run the sampler.
#' * `inverse_metric`: A list (one element per chain) of inverse mass matrices
#' or their diagonals, depending on the type of metric used.
#' * `warmup_draws`:  If `save_warmup` was `TRUE` then the warmup samples (iter
#' x chain x variable array).
#' * `post_warmup_draws`: The post-warmup samples (iter x chain x variable array).
#' * `warmup_sampler_diagnostics`:  If `save_warmup` was `TRUE` then the values
#' of the sampler parameters during warmup (iter x chain x variable array).
#' * `sampler_diagnostics`: The post-warmup sampler parameters (iter x chain x
#' variable array).
#'
#' @examples
#' \dontrun{
#' stan_program <- file.path(cmdstan_path(), "examples/bernoulli/bernoulli.stan")
#' mod <- cmdstan_model(stan_program)
#' fit <- mod$sample(data = list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1)))
#'
#' # Read all draws
#' d <- read_sample_csv(fit$output_files())
#' # Read only model parameter draws
#' d <- read_sample_csv(fit$output_files(), pars = NULL, sampler_diagnostics = "")
#' # Read only specific parameter draws
#' d <- read_sample_csv(fit$output_files(), pars = c("beta", "theta"), sampler_diagnostics = "")
#' # Read only specific sampler diagnostic
#' d <- read_sample_csv(fit$output_files(), pars = "", sampler_diagnostics = c("divergent__"))
#' }
#'
read_sample_csv <- function(files,
                            pars = NULL,
                            sampler_diagnostics = NULL,
                            cores = getOption("mc.cores", 1)) {
  sampling_info <- NULL
  warmup_draws <- NULL
  warmup_sampler_diagnostics_draws <- NULL
  post_warmup_draws <- NULL
  post_warmup_sampler_diagnostics_draws <- NULL
  inverse_metric <- list()
  step_size <- list()
  col_types <- NULL
  col_select <- NULL
  not_matching <- c()
  for (output_file in files) {
    checkmate::assert_file_exists(output_file, access = "r", extension = "csv")
    if (is.null(sampling_info)) {
      sampling_info <- read_sample_info_csv(output_file)
      if (!is.null(sampling_info$inverse_metric)) {
        inverse_metric[[sampling_info$id]] <- sampling_info$inverse_metric
      }
      if (!is.null(sampling_info$stepsize_adaptation)) {
        step_size[[sampling_info$id]] <- sampling_info$stepsize_adaptation
      }
      id <- sampling_info$id
    } else {
      csv_file_info <- read_sample_info_csv(output_file)
      check <- check_sampling_csv_info_matches(sampling_info, csv_file_info)
      if (!is.null(check$error)) {
        stop(check$error)
      }
      not_matching <- c(not_matching, check$not_matching)
      sampling_info$id <- c(sampling_info$id, csv_file_info$id)
      sampling_info$seed <- c(sampling_info$seed, csv_file_info$seed)

      if (!is.null(csv_file_info$inverse_metric)) {
        inverse_metric[[csv_file_info$id]] <- csv_file_info$inverse_metric
      }
      if (!is.null(csv_file_info$stepsize_adaptation)) {
        step_size[[csv_file_info$id]] <- csv_file_info$stepsize_adaptation
      }
      id <- csv_file_info$id
    }
    if (is.null(col_select)) {
      if (is.null(pars)) { # pars = NULL returns all parameters
        pars <- sampling_info$model_params
      } else if (!any(nzchar(pars))) { # if pars = "" returns no parameters
        pars <- NULL
      } else { # filter using pars
        pars <- unrepair_variable_names(pars)
        selected_pars <- rep(FALSE, length(sampling_info$model_params))
        not_found <- NULL
        for (p in pars) {
          matches <- sampling_info$model_params == p | startsWith(sampling_info$model_params, paste0(p,"."))
          if (!any(matches)) {
            not_found <- c(not_found, p)
          }
          selected_pars <- selected_pars | matches
        }
        if (length(not_found)) {
          stop("Can't find parameter(s): ", paste(not_found, collapse = ", "), " in the sampling output!")
        }
        pars <- sampling_info$model_params[selected_pars]
      }
      if (is.null(sampler_diagnostics)) {
        sampler_diagnostics <- sampling_info$sampler_diagnostics
      } else if (!any(nzchar(sampler_diagnostics))) { # if sampler_diagnostics = "" dont return any sampler_diagnostics
        sampler_diagnostics <- NULL
      } else {
        selected_sampler_diag <- rep(FALSE, length(sampling_info$sampler_diagnostics))
        not_found <- NULL
        for (p in sampler_diagnostics) {
          matches <- sampling_info$sampler_diagnostics == p | startsWith(sampling_info$sampler_diagnostics, paste0(p,"."))
          if (!any(matches)) {
            not_found <- c(not_found, p)
          }
          selected_sampler_diag <- selected_sampler_diag | matches
        }
        if (length(not_found)) {
          stop("Can't find sampler diagnostic(s): ", paste(not_found, collapse = ", "), " in the sampling output!")
        }
        sampler_diagnostics <- sampling_info$sampler_diagnostics[selected_sampler_diag]
      }
      col_select <- "lp__"
      col_select <- c(col_select, pars[pars!="lp__"])
      col_select <- c(col_select, sampler_diagnostics)
    }
    suppressWarnings(
    draws <- vroom::vroom(output_file,
                            comment = "# ",
                            delim = ',',
                            trim_ws = TRUE,
                            col_select = col_select,
                            col_types = c("lp__" = "d"),
                            altrep = FALSE)
    )
    if (ncol(draws) == 0) {
      stop("The supplied csv file does not contain any sampling data!")
    }
    draws <- draws[!is.na(draws[,1]),]
    if (nrow(draws) > 0) {
      num_warmup_draws <- ceiling(sampling_info$iter_warmup/sampling_info$thin)
      num_post_warmup_draws <- ceiling(sampling_info$iter_sampling/sampling_info$thin)
      all_draws <- num_warmup_draws + num_post_warmup_draws
      if (sampling_info$save_warmup == 1) {
        if (length(pars) > 0) {
          new_warmup_draws <- posterior::as_draws_array(draws[1:num_warmup_draws, pars])
          if (is.null(warmup_draws)) {
            warmup_draws <- new_warmup_draws
          } else {
            warmup_draws <- posterior::bind_draws(warmup_draws,
                                                  new_warmup_draws,
                                                  along="chain")
          }
        }
        if (length(sampler_diagnostics) > 0) {
          new_warmup_sampler_diagnostics_draws <- posterior::as_draws_array(draws[1:num_warmup_draws, sampler_diagnostics])
          if (is.null(warmup_sampler_diagnostics_draws)) {
            warmup_sampler_diagnostics_draws <- new_warmup_sampler_diagnostics_draws
          } else {
            warmup_sampler_diagnostics_draws <- posterior::bind_draws(warmup_sampler_diagnostics_draws,
                                                                      new_warmup_sampler_diagnostics_draws,
                                                                      along="chain")
          }
        }
        if (length(pars) > 0) {
          new_post_warmup_draws <- posterior::as_draws_array(draws[(num_warmup_draws+1):all_draws, pars])
          if (is.null(post_warmup_draws)) {
            post_warmup_draws <- new_post_warmup_draws
          } else {
            post_warmup_draws <- posterior::bind_draws(post_warmup_draws, new_post_warmup_draws, along="chain")
          }
        }
        if (length(sampler_diagnostics) > 0) {
          new_post_warmup_sampler_diagnostics_draws <- posterior::as_draws_array(draws[(num_warmup_draws+1):all_draws, sampler_diagnostics])
          if (is.null(post_warmup_sampler_diagnostics_draws)) {
            post_warmup_sampler_diagnostics_draws <- new_post_warmup_sampler_diagnostics_draws
          } else {
            post_warmup_sampler_diagnostics_draws <- posterior::bind_draws(post_warmup_sampler_diagnostics_draws,
                                                                          new_post_warmup_sampler_diagnostics_draws,
                                                                          along="chain")
          }
        }
      } else {
        warmup_draws <- NULL
        warmup_sampler_diagnostics_draws <- NULL
        if (length(pars) > 0) {
          new_post_warmup_draws <- posterior::as_draws_array(draws[, pars])
          if (is.null(post_warmup_draws)) {
            post_warmup_draws <- new_post_warmup_draws
          } else {
            post_warmup_draws <- posterior::bind_draws(post_warmup_draws,
                                                      new_post_warmup_draws,
                                                      along="chain")
          }
        }
        if (sampling_info$algorithm != "fixed_param") {
          if (length(sampler_diagnostics) > 0) {
            new_post_warmup_sampler_diagnostics_draws <- posterior::as_draws_array(draws[, sampler_diagnostics])
            if (is.null(post_warmup_sampler_diagnostics_draws)) {
              post_warmup_sampler_diagnostics_draws <- new_post_warmup_sampler_diagnostics_draws
            } else {
              post_warmup_sampler_diagnostics_draws <- posterior::bind_draws(post_warmup_sampler_diagnostics_draws,
                                                                            new_post_warmup_sampler_diagnostics_draws,
                                                                            along="chain")
            }
          }
        }
      }
    }
  }
  repaired_model_params <- repair_variable_names(pars)
  if (!is.null(warmup_draws)) {
    posterior::variables(warmup_draws) <- repaired_model_params
  }
  if (!is.null(post_warmup_draws)) {
    posterior::variables(post_warmup_draws) <- repaired_model_params
  }
  if (length(not_matching) > 0) {
    not_matching_list <- paste(unique(not_matching), collapse = ", ")
    warning("The supplied csv files do not match in the following arguments: ", not_matching_list, "!")
  }
  sampling_info$model_params <- repair_variable_names(sampling_info$model_params)
  sampling_info$inverse_metric <- NULL
  list(
    sampling_info = sampling_info,
    inverse_metric = inverse_metric,
    step_size = step_size,
    warmup_draws = warmup_draws,
    post_warmup_draws = post_warmup_draws,
    warmup_sampler_diagnostics = warmup_sampler_diagnostics_draws,
    post_warmup_sampler_diagnostics = post_warmup_sampler_diagnostics_draws
  )
}

# FIXME: also parse the csv header
read_optim_csv <- function(output_file) {
  csv_no_comments <- utils::read.csv(
    output_file,
    comment.char = "#",
    colClasses = "numeric"
  )
  mat <- as.matrix(csv_no_comments)
  colnames(mat) <- repair_variable_names(colnames(mat))

  # not really draws (just point estimate) but this is consistent with
  # names and format for mcmc and vb
  list(
    draws = posterior::as_draws_matrix(mat[1,, drop=FALSE])
  )
}

# FIXME: also parse the csv header
read_vb_csv <- function(output_file) {
  csv_no_comments <- utils::read.csv(
    output_file,
    comment.char = "#",
    colClasses = "numeric"
  )
  # drop first row since according to CmdStan manual it's just the mean
  mat <- as.matrix(csv_no_comments)[-1,, drop=FALSE]
  colnames(mat) <- repair_variable_names(colnames(mat))
  mat <- mat[, colnames(mat) != "lp__", drop=FALSE]
  draws <- posterior::as_draws_matrix(mat)
  draws <- posterior::rename_variables(draws, lp__ = "log_p__", lp_approx__ = "log_g__")
  list(draws = draws)
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
      if (any(all_remaining == p) || any(startsWith(all_remaining, paste0(p,".")))) {
        unread <- c(unread, p)
      }
    }  
  }
  if (length(unread)) {
    unread
  } else {
    ""
  }  
}
