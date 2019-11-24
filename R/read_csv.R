#' Check that the sampling information from two CSV files matches.
#' Will throw errors if the sampling informations dont match. If
#' it returns, the sampling informations match.
#'
#' @param a the first sampling info to check
#' @param b the second sampling info to check
#'
check_sampling_csv_info_matches <- function(a, b) {
  if(a$stan_version_major != b$stan_version_major ||
     a$stan_version_minor != b$stan_version_minor ||
     a$stan_version_patch != b$stan_version_patch) {
    return("Supplied CSV files were not generated with the same version of Cmdstan!")
  }
  if(a$model != b$model) {
    return("Supplied CSV files were not generated wtih the same model!")
  }
  if(!all(a$params == b$params)) {
    return("Supplied CSV files have samples for different parameters!")
  }
  if(a$data_file != b$data_file) {
    return("Supplied CSV files have samples from chains run with non-matching data!")
  }
  for(name in names(a)) {
    if(startsWith(name, "sample_")) {
      if (is.null(b[[name]]) ||
          a[[name]] != b[[name]]) {
        return("Supplied CSV files do not match in all sampling settings!")
      }
    }
  }
  NULL
}

#' Reads the sampling arguments and the diagonal of the
#' inverse mass matrix from the comments in a CSV file.
#'
#' @param csv_file A CSV file containing results from sampling
#' @return A list containing all sampling parameters and the
#' diagonal of the inverse mass matrix
#'
read_sample_info_csv <- function(csv_file) {
  checkmate::assert_file_exists(csv_file, access = "r", extension = "stan")
  param_names_read <- FALSE
  sampling_params_read <- FALSE
  diagonal_matrix_next <- FALSE
  arg_prefix <- ""
  csv_file_info = list()
  con  <- file(csv_file, open = "r")
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    if (!startsWith(line, "#")) {
      if(!param_names_read) {
        param_names_read <- TRUE
        all_names <- strsplit(line, ",")[[1]]
        csv_file_info[["sampler_params"]] <- c()
        csv_file_info[["model_params"]] <- c()
        for(x in all_names) {
          if(endsWith(x, "__") && x != "lp__"){
            csv_file_info[["sampler_params"]] <- c(csv_file_info[["sampler_params"]], x)
          } else {
            csv_file_info[["model_params"]] <- c(csv_file_info[["model_params"]], x)
          }
        }
        next;
      } else {
        break;
      }
    }
    # no more sampling settings follow the list of parameters
    if(!param_names_read) {
      tmp <- gsub("#", "", line, fixed = TRUE)
      tmp <- gsub("(Default)", "", tmp, fixed = TRUE)
      key_val <- grep("=", tmp, fixed = TRUE, value = TRUE)
      if(length(key_val) == 0) {
        arg_name <- trimws(tmp)
        if(arg_prefix != "" && !sampling_params_read) {
          arg_prefix <- paste(arg_prefix, arg_name, sep = "_")
        } else {
          arg_prefix <- trimws(tmp)
        }
      } else {
        key_val <- strsplit(key_val, split = "=", fixed = TRUE)
        key_val <- rapply(key_val, trimws)
        if(length(key_val) == 2) {
          numeric_val <- suppressWarnings(as.numeric(key_val[2]))
          # marks the end of sampling args
          if(key_val[1] == "id" || key_val[1] == "init") {
            arg_prefix <- ""
            sampling_params_read <- TRUE
          }
          if(arg_prefix != ""){
            setting_name <- paste(arg_prefix, key_val[1], sep = "_")
          } else {
            setting_name <- key_val[1]
          }
          if(!is.na(numeric_val)) {
            csv_file_info[[setting_name]] <- numeric_val
          } else {
            csv_file_info[[setting_name]] <- key_val[2]
          }
        }
      }
    }
    if (regexpr("# Diagonal elements of inverse mass matrix:", line, perl = TRUE) > 0) {
      diagonal_matrix_next <- TRUE
    } else if(diagonal_matrix_next) {
      csv_file_info$inverse_mass_matrix_diag <- rapply(strsplit(gsub("# ", "", line), ","), as.numeric)
      diagonal_matrix_next <- FALSE
    }
  }
  close(con)
  if(is.null(csv_file_info$inverse_mass_matrix_diag)) {
    csv_file_info$inverse_mass_matrix_diag <- NULL
  }
  if(csv_file_info$method != "sample") {
    stop("Supplied CSV file was not generated with sampling. Consider using read_optim_csv or read_vb_csv!")
  }
  csv_file_info
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
  inverse_mass_matrix_diag <- c()
  sampling_params_draws <- c()
  post_warmup_draws_array <- c()
  warmup_draws_array <- c()
  for(output_file in output_files) {
    checkmate::assert_file_exists(output_file, access = "r", extension = "stan")
    # read meta data
    if (is.null(sampling_info)) {
      sampling_info <- read_sample_info_csv(output_file)
      inverse_mass_matrix_diag <- sampling_info$inverse_mass_matrix_diag
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
      sampling_info$init <- c(sampling_info$init,
                              csv_file_info$init)
      sampling_info$random_seed <- c(sampling_info$random_seed,
                                     csv_file_info$random_seed)
      sampling_info$output_file <- c(sampling_info$output_file,
                                     csv_file_info$output_file)
      sampling_info$output_diagnostic_file <- c(sampling_info$output_diagnostic_file,
                                                csv_file_info$output_diagnostic_file)
      sampling_info$output_refresh <- c(sampling_info$output_refresh,
                                        csv_file_info$output_refresh)
      inverse_mass_matrix_diag <- cbind(inverse_mass_matrix_diag,
                                        csv_file_info$inverse_mass_matrix_diag)
    }
    # read sampling data
    if(sampling_info$sample_save_warmup == 1) {
      num_of_draws <- sampling_info$sample_num_samples + sampling_info$sample_num_warmup
    } else {
      num_of_draws <- sampling_info$sample_num_samples
    }
    draws <- utils::read.csv(output_file, header = TRUE, comment.char = "#")
    sampling_params_draws <- rbind(sampling_params_draws, draws[, sampling_info$sampler_params])
    if(sampling_info$sample_save_warmup == 1) {
      warmup_draws_array <- rbind(post_warmup_draws_array,
                                  draws[1:sampling_info$sample_num_warmup, sampling_info$model_params])
      post_warmup_draws_array <- rbind(post_warmup_draws_array,
                                       draws[(sampling_info$sample_num_warmup+1):num_of_draws, sampling_info$model_params])
    } else {
      warmup_draws_array <- NULL
      post_warmup_draws_array <- rbind(post_warmup_draws_array,
                                       draws[, sampling_info$model_params])
    }
  }
  #inverse mass matrix is returned separately
  sampling_info$inverse_mass_matrix_diag <- NULL
  if(!is.null(inverse_mass_matrix_diag)) {
    dimnames(inverse_mass_matrix_diag) <- list(diagonal_elements = seq(dim(sampling_info$inverse_mass_matrix_diag)[1]),
                                               chain_id = sampling_info$id)
  }
  sampling_info$model_params <- repair_variable_names(sampling_info$model_params)
  num_chains <- length(sampling_info$id)
  if(!is.null(warmup_draws_array)) {
    warmup_draws_array <- posterior::as_draws_array(array(unlist(warmup_draws_array),
                                                          dim = c(sampling_info$sample_num_warmup, num_chains, length(sampling_info$model_params)),
                                                          dimnames = list(NULL, NULL, sampling_info$model_params)))
  }
  post_warmup_draws_array <- posterior::as_draws_array(array(unlist(post_warmup_draws_array),
                                                             dim = c(sampling_info$sample_num_samples, num_chains, length(sampling_info$model_params)),
                                                             dimnames = list(NULL, NULL, sampling_info$model_params)))
  sampling_params_draws <- posterior::as_draws_array(array(unlist(sampling_params_draws),
                                                             dim = c(sampling_info$sample_num_samples+sampling_info$sample_num_warmup, num_chains, length(sampling_info$sampler_params)),
                                                             dimnames = list(NULL, NULL, sampling_info$sampler_params)))
  list(
    sampling_info = sampling_info,
    inverse_mass_matrix_diag = inverse_mass_matrix_diag,
    warmup = warmup_draws_array,
    post_warmup = post_warmup_draws_array,
    sampler = sampling_params_draws
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
