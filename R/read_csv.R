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
    return("Supplied CSV files were not generated wtih the same model!")
  }
  if ((length(a$model_params)!= length(b$model_params)) || !(all(a$model_params == b$model_params) && all(a$sampler_diagnostics == b$sampler_diagnostics))) {
    return("Supplied CSV files have samples for different parameters!")
  }
  dont_match_list <- c("id", "inverse_metric", "step_size", "seed")
  for (name in names(a)) {
    if (!(name %in% dont_match_list) && (is.null(b[[name]]) ||  all(a[[name]] != b[[name]]))) {
      return("Supplied CSV files do not match in all sampling settings!")
    }
  }
  NULL
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
  csv_file_info$inverse_metric_rows <- 0
  parsing_done <- FALSE
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0 && !parsing_done) {
    if (!startsWith(line, "#")) {
      if(!param_names_read) {
        param_names_read <- TRUE
        all_names <- strsplit(line, ",")[[1]]
        csv_file_info[["sampler_diagnostics"]] <- c()
        csv_file_info[["model_params"]] <- c()
        for(x in all_names) {
          if(endsWith(x, "__") && x != "lp__"){
            csv_file_info[["sampler_diagnostics"]] <- c(csv_file_info[["sampler_diagnostics"]], x)
          } else {
            csv_file_info[["model_params"]] <- c(csv_file_info[["model_params"]], x)
          }
        }
      }
    } else {
      if(!adaptation_terminated) {
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
            if(!is.na(numeric_val)) {
              csv_file_info[[key_val[1]]] <- numeric_val
            } else {
              if(nzchar(key_val[2])) {
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
        } else if(inverse_metric_diagonal_next) {          
          inv_metric_split <- strsplit(gsub("# ", "", line), ",")
          if ((length(inv_metric_split) == 0) ||
              ((length(inv_metric_split) == 1) && identical(inv_metric_split[[1]], character(0)))) {
            break;
          }
          csv_file_info$inverse_metric <- rapply(inv_metric_split, as.numeric)
          parsing_done <- TRUE
        } else if(inverse_metric_next) {
          inv_metric_split <- strsplit(gsub("# ", "", line), ",")
          if ((length(inv_metric_split) == 0) ||
              ((length(inv_metric_split) == 1) && identical(inv_metric_split[[1]], character(0)))) {
            parsing_done <- TRUE
            break;
          }
          if(csv_file_info$inverse_metric_rows == 0) {
            csv_file_info$inverse_metric <- rapply(inv_metric_split, as.numeric)
          } else {
            csv_file_info$inverse_metric <- c(csv_file_info$inverse_metric, rapply(inv_metric_split, as.numeric))
          }
          csv_file_info$inverse_metric_rows <- csv_file_info$inverse_metric_rows + 1
        }
      }
    }
  }
  close(con)
  if(is.null(csv_file_info$method)) {
    stop("Supplied CSV file is corrupt!")
  } else if(csv_file_info$method != "sample") {
    stop("Supplied CSV file was not generated with sampling. Consider using read_optim_csv or read_vb_csv!")
  }
  if(csv_file_info$save_warmup == 1) {
    csv_file_info$num_iter <- csv_file_info$num_warmup + csv_file_info$num_samples
  } else {
    csv_file_info$num_iter <- csv_file_info$num_samples
  }
  csv_file_info$num_iter <- csv_file_info$num_iter / csv_file_info$thin
  if(csv_file_info$inverse_metric_rows > 0) {
    rows <- csv_file_info$inverse_metric_rows
    cols <- length(csv_file_info$inverse_metric)/csv_file_info$inverse_metric_rows
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

#' Reads sampling results from the supplied CSV files. Returns a list
#' containing sampling arguments, the diagonal of the inverse mass
#' matrix, the post-warmup samples, the sampling parameters and
#' optionally samples produced during warmup.
#' @export
#' @param output_files Paths to the CSV files to read.
#'
#' @return The list of sampling arguments, the diagonal of the inverse mass
#' matrix, the post-warmup samples, the sampling parameters and warmup samples
#' if the run was run with save_warmup = 1.
#'
read_sample_csv <- function(output_files) {
  sampling_info <- NULL
  warmup_draws_array <- list()
  warmup_sampler_diagnostics_draws <- list()
  post_warmup_draws_array <- list()
  post_warmup_sampler_diagnostics_draws <- list()
  inverse_metric = list()
  step_size = list()
  for(output_file in output_files) {
    checkmate::assert_file_exists(output_file, access = "r", extension = "csv")
    # read meta data
    if (is.null(sampling_info)) {
      sampling_info <- read_sample_info_csv(output_file)
      inverse_metric[[sampling_info$id]] <- sampling_info$inverse_metric
      step_size[[sampling_info$id]] <- sampling_info$step_size
      id <- sampling_info$id
    } else {
      csv_file_info <- read_sample_info_csv(output_file)
      # check if sampling info matches
      error <- check_sampling_csv_info_matches(sampling_info,
                                  csv_file_info)
      if(!is.null(error)) {
        stop(error)
      }
      sampling_info$id <- c(sampling_info$id,
                            csv_file_info$id)
      inverse_metric[[csv_file_info$id]] <- csv_file_info$inverse_metric
      step_size[[csv_file_info$id]] <- csv_file_info$step_size
      id <- csv_file_info$id
    }
    # read sampling data
    draws <- utils::read.csv(output_file, header = TRUE, comment.char = "#")

    if(sampling_info$save_warmup == 1) {
      warmup_draws_array[[id]] <- draws[1:sampling_info$num_warmup/sampling_info$thin, sampling_info$model_params]
      warmup_sampler_diagnostics_draws[[id]] <- draws[1:sampling_info$num_warmup/sampling_info$thin, sampling_info$sampler_diagnostics]
      post_warmup_draws_array[[id]] <- draws[(sampling_info$num_warmup/sampling_info$thin+1):sampling_info$num_iter, sampling_info$model_params]
      post_warmup_sampler_diagnostics_draws[[id]] <- draws[(sampling_info$num_warmup/sampling_info$thin+1):sampling_info$num_iter, sampling_info$sampler_diagnostics]

    } else {
      warmup_draws_array <- NULL
      warmup_sampler_diagnostics_draws <- NULL
      post_warmup_draws_array[[id]] <- draws[, sampling_info$model_params]
      post_warmup_sampler_diagnostics_draws[[id]] <- draws[, sampling_info$sampler_diagnostics]
    }
  }
  sampling_info$model_params <- repair_variable_names(sampling_info$model_params)
  num_chains <- length(sampling_info$id)
  if(!is.null(warmup_draws_array)) {
    if(!is.null(warmup_draws_array) && (length(warmup_draws_array) > 0)) {
      
      warmup_draws_array <- posterior::as_draws_array(array(unlist(do.call(rbind, warmup_draws_array)),
                                                          dim = c(sampling_info$num_warmup/sampling_info$thin, num_chains, length(sampling_info$model_params)),
                                                          dimnames = list(NULL, NULL, sampling_info$model_params)))
    }
    if(!is.null(warmup_sampler_diagnostics_draws) && (length(warmup_sampler_diagnostics_draws) > 0)) {
          warmup_sampler_diagnostics_draws <- posterior::as_draws_array(array(unlist(do.call(rbind, warmup_sampler_diagnostics_draws)),
                                                             dim = c(sampling_info$num_warmup/sampling_info$thin, num_chains, length(sampling_info$sampler_diagnostics)),
                                                             dimnames = list(NULL, NULL, sampling_info$sampler_diagnostics)))
    }
  }
  if(!is.null(post_warmup_draws_array) && (length(post_warmup_draws_array) > 0)) {
    post_warmup_draws_array <- posterior::as_draws_array(array(unlist(do.call(rbind, post_warmup_draws_array)),
                                                             dim = c(sampling_info$num_samples/sampling_info$thin, num_chains, length(sampling_info$model_params)),
                                                             dimnames = list(NULL, NULL, sampling_info$model_params)))
  }
  if(!is.null(post_warmup_sampler_diagnostics_draws) && (length(post_warmup_sampler_diagnostics_draws) > 0)) {
    post_warmup_sampler_diagnostics_draws <- posterior::as_draws_array(array(unlist(do.call(rbind, post_warmup_sampler_diagnostics_draws)),
                                                             dim = c(sampling_info$num_samples/sampling_info$thin, num_chains, length(sampling_info$sampler_diagnostics)),
                                                             dimnames = list(NULL, NULL, sampling_info$sampler_diagnostics)))
  }
  sampling_info$inverse_metric <- NULL
  sampling_info$step_size <- NULL  
  sampling_info$num_iter <- NULL 
  list(
    sampling_info = sampling_info,
    inverse_metric = inverse_metric,
    step_size = step_size,
    warmup_draws = warmup_draws_array,
    post_warmup_draws = post_warmup_draws_array,
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
  list(
    mle = mat[1, colnames(mat) != "lp__"],
    lp = mat[1, colnames(mat) == "lp__"]
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
  drop_cols <- c("lp__", "log_p__", "log_g__")
  keep_cols <- setdiff(colnames(mat), drop_cols)
  list(
    log_p = mat[, "log_p__"],
    log_g = mat[, "log_g__"],
    draws = mat[, keep_cols, drop=FALSE]
  )
}
