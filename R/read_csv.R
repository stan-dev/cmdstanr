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
  if ((length(a$model_params)!= length(b$model_params)) || !(all(a$model_params == b$model_params) && all(a$sampler_diagnostics == b$sampler_diagnostics))) {
    return(list(error = "Supplied CSV files have samples for different parameters!"))
  }
  if(a$num_samples != b$num_samples ||
    a$thin != b$thin ||
    a$save_warmup != b$save_warmup ||
    (a$save_warmup == 1 && a$num_warmup != b$num_warmup)) {
      return(list(error = "Supplied CSV files dont match in the number of stored samples!"))
  }
  match_list <- c("stan_version_major", "stan_version_minor", "stan_version_patch", "gamma", "kappa",
                  "t0", "init_buffer", "term_buffer", "window", "algorithm", "engine", "max_depth",
                  "metric", "stepsize", "stepsize_jitter", "adapt_engaged", "adapt_delta", "num_warmup")
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
            if (endsWith(x, "__") && x != "lp__"){
              csv_file_info[["sampler_diagnostics"]] <- c(csv_file_info[["sampler_diagnostics"]], x)
            } else {
              csv_file_info[["model_params"]] <- c(csv_file_info[["model_params"]], x)
            }
          } else {
            if (!endsWith(x, "__")){
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
          csv_file_info$step_size <- as.numeric(strsplit(line, " = ")[[1]][2])
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
  if (inverse_metric_rows > 0) {
    rows <- inverse_metric_rows
    cols <- length(csv_file_info$inverse_metric)/inverse_metric_rows
    dim(csv_file_info$inverse_metric) <- c(rows,cols)
  }
  csv_file_info$model_name <- csv_file_info$model
  csv_file_info$model <- NULL
  csv_file_info$adapt_engaged <- csv_file_info$engaged
  csv_file_info$adapt_delta <- csv_file_info$delta
  csv_file_info$engaged <- NULL
  csv_file_info$delta <- NULL
  csv_file_info$file <- NULL
  csv_file_info$diagnostic_file <- NULL
  csv_file_info$metric_file <- NULL
  return(csv_file_info)
}

#' Read sampling results from the supplied CSV files
#'
#' Read sampling results from the supplied CSV files. Returns a list containing
#' sampling arguments, the diagonal of the inverse mass matrix, the post-warmup
#' samples, the sampling parameters and optionally samples produced during
#' warmup.
#'
#' @export
#' @param output_files Paths to the CSV files to read.
#'
#' @return The list of sampling arguments, the diagonal of the inverse mass
#' matrix, the post-warmup samples, the sampling parameters and warmup samples
#' if the run was run with save_warmup = 1.
#'
read_sample_csv <- function(output_files) {
  sampling_info <- NULL
  warmup_draws <- NULL
  warmup_sampler_diagnostics_draws <- NULL
  post_warmup_draws <- NULL
  post_warmup_sampler_diagnostics_draws <- NULL
  inverse_metric = list()
  step_size = list()
  not_matching = c()
  for(output_file in output_files) {
    checkmate::assert_file_exists(output_file, access = "r", extension = "csv")
    # read meta data
    if (is.null(sampling_info)) {
      sampling_info <- read_sample_info_csv(output_file)
      if (!is.null(sampling_info$inverse_metric)) {
        inverse_metric[[sampling_info$id]] <- sampling_info$inverse_metric
      }
      if (!is.null(sampling_info$step_size)) {
        step_size[[sampling_info$id]] <- sampling_info$step_size
      }
      id <- sampling_info$id
    } else {
      csv_file_info <- read_sample_info_csv(output_file)
      # check if sampling info matches
      check <- check_sampling_csv_info_matches(sampling_info,
                                  csv_file_info)
      if (!is.null(check$error)) {
        stop(check$error)
      }
      not_matching <- c(not_matching, check$not_matching)
      sampling_info$id <- c(sampling_info$id,
                            csv_file_info$id)

      if (!is.null(csv_file_info$inverse_metric)) {
        inverse_metric[[csv_file_info$id]] <- csv_file_info$inverse_metric
      }
      if (!is.null(csv_file_info$step_size)) {
        step_size[[csv_file_info$id]] <- csv_file_info$step_size
      }
      id <- csv_file_info$id
    }
    # read sampling data
    draws <- utils::read.csv(output_file, header = TRUE, comment.char = "#")
    if (nrow(draws) > 0) {
      num_warmup_draws <- ceiling(sampling_info$num_warmup/sampling_info$thin)
      num_post_warmup_draws <- ceiling(sampling_info$num_samples/sampling_info$thin)
      all_draws <- num_warmup_draws + num_post_warmup_draws

      if (sampling_info$save_warmup == 1) {

        new_warmup_draws <- posterior::as_draws_array(draws[1:num_warmup_draws, sampling_info$model_params])
        if (is.null(warmup_draws)) {
          warmup_draws <- new_warmup_draws
        } else {
          warmup_draws <- posterior::bind_draws(warmup_draws,
                                                new_warmup_draws,
                                                along="chain")
        }
        new_warmup_sampler_diagnostics_draws <- posterior::as_draws_array(draws[1:num_warmup_draws, sampling_info$sampler_diagnostics])
        if (is.null(warmup_sampler_diagnostics_draws)) {
          warmup_sampler_diagnostics_draws <- new_warmup_sampler_diagnostics_draws
        } else {
          warmup_sampler_diagnostics_draws <- posterior::bind_draws(warmup_sampler_diagnostics_draws,
                                                                    new_warmup_sampler_diagnostics_draws,
                                                                    along="chain")
        }
        new_post_warmup_draws <- posterior::as_draws_array(draws[(num_warmup_draws+1):all_draws, sampling_info$model_params])
        if (is.null(post_warmup_draws)) {
          post_warmup_draws <- new_post_warmup_draws
        } else {
          post_warmup_draws <- posterior::bind_draws(post_warmup_draws, new_post_warmup_draws, along="chain")
        }
        new_post_warmup_sampler_diagnostics_draws <- posterior::as_draws_array(draws[(num_warmup_draws+1):all_draws, sampling_info$sampler_diagnostics])
        if (is.null(post_warmup_sampler_diagnostics_draws)) {
          post_warmup_sampler_diagnostics_draws <- new_post_warmup_sampler_diagnostics_draws
        } else {
          post_warmup_sampler_diagnostics_draws <- posterior::bind_draws(post_warmup_sampler_diagnostics_draws,
                                                                         new_post_warmup_sampler_diagnostics_draws,
                                                                         along="chain")
        }
      } else {
        warmup_draws <- NULL
        warmup_sampler_diagnostics_draws <- NULL
        new_post_warmup_draws <- posterior::as_draws_array(draws[, sampling_info$model_params])
        if (is.null(post_warmup_draws)) {
          post_warmup_draws <- new_post_warmup_draws
        } else {
          post_warmup_draws <- posterior::bind_draws(post_warmup_draws,
                                                     new_post_warmup_draws,
                                                     along="chain")
        }
        if (sampling_info$algorithm != "fixed_param") {
          new_post_warmup_sampler_diagnostics_draws <- posterior::as_draws_array(draws[, sampling_info$sampler_diagnostics])
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
  if (!is.null(warmup_draws)){
    dimnames(warmup_draws)$variable <- repair_variable_names(sampling_info$model_params)
  }
  if (!is.null(post_warmup_draws)){
    dimnames(post_warmup_draws)$variable <- repair_variable_names(sampling_info$model_params)
  }
  if(length(not_matching) > 0) {
    not_matching_list <- paste(unique(not_matching), collapse = ", ")
    warning("The supplied csv files do not match in the following arguments: ", not_matching_list, "!")
  }
  sampling_info$model_params <- repair_variable_names(sampling_info$model_params)
  sampling_info$inverse_metric <- NULL
  sampling_info$step_size <- NULL
  sampling_info$num_iter <- NULL
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
  draws <- posterior::rename_variables(draws, lp__ = log_p__, lp_approx__ = log_g__)
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
