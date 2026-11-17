#' Convert CmdStan CSV files to a Parquet file
#'
#' Convert the CSV output files of the CmdStan MCMC sampler to one Parquet
#' file, without reading all draws into memory. The Parquet file contains the
#' columns of a [`draws_df`][posterior::draws_df] object, and the CmdStan
#' metadata.
#'
#' @export
#' @param files (character vector) Paths to CmdStan CSV files from
#'   `$sample()`, one file per chain. Files compressed with gzip (`.csv.gz`)
#'   or bzip2 (`.csv.bz2`) are first decompressed to a temporary directory.
#' @param file (string) Path of the Parquet file for the post-warmup draws.
#' @param warmup_file (string) Optional path of a second Parquet file for the
#'   warmup draws. If the CSV files do not contain warmup draws, this file has
#'   the columns but no rows. The default is `NULL`, which drops the warmup
#'   draws.
#' @param chunk_rows (positive integer) The number of CSV lines to read at a
#'   time. Each chunk is written as new Parquet row groups; nanoparquet splits
#'   a chunk with more rows than its `num_rows_per_row_group` option. The
#'   default is `NULL`, which selects about 10 million values for each chunk.
#' @param compression (string) The compression of the Parquet files. One of
#'   `"zstd"` (the default), `"snappy"`, `"gzip"` or `"uncompressed"`.
#' @param overwrite (logical) Replace existing output files? The default is
#'   `FALSE`.
#'
#' @return The path of the Parquet file of the post-warmup draws, invisibly.
#'
#' @details
#' This function requires the \pkg{nanoparquet} package. It reads each CSV
#' file in chunks of `chunk_rows` lines and appends each chunk to the Parquet
#' file.
#'
#' The memory use grows with `chunk_rows` times the number of columns. It does
#' not grow with the number of chunks, with one exception: each append reads
#' and writes the Parquet footer, which grows with the number of row groups
#' times the number of columns. For output with very many columns, use a large
#' `chunk_rows` if the memory allows it.
#'
#' The Parquet file has the following columns:
#' * all columns of the CSV files (`lp__`, the sampler diagnostics, and the
#'   model variables), as doubles. The names use the `beta[1,2]` form, as in
#'   `$draws()`.
#' * the integer columns `.chain`, `.iteration` and `.draw`, as in a
#'   [`draws_df`][posterior::draws_df] object. `.chain` is the position of the
#'   file in `files`. `.iteration` starts from 1 in each chain. `.draw`
#'   numbers the draws of all chains consecutively.
#'
#' [read_cmdstan_parquet()] reads the file into the same structure as
#' [read_cmdstan_csv()], with the draws (`lp__` and the model variables) and
#' the sampler diagnostics separate. [as_cmdstan_fit()] makes a fitted model
#' object from the file.
#'
#' `posterior::as_draws_df(nanoparquet::read_parquet(file))` gives all
#' columns in one data frame, and the `col_select` argument of
#' [nanoparquet::read_parquet()]
#' reads only some of the variables. Include `.chain`, `.iteration` and
#' `.draw` in `col_select` to keep the chain structure. Other Parquet
#' readers (for example \pkg{arrow}, \pkg{duckdb} and \pkg{polars}) can also
#' read the file.
#'
#' The key-value metadata of the Parquet file has three entries:
#' * `stan.format_version`: the version of this file layout, now `"1"`.
#' * `stan.draws`: `"sampling"` in `file` and `"warmup"` in `warmup_file`.
#' * `stan.metadata`: the CmdStan metadata as a JSON object. It has the
#'   values that [read_cmdstan_csv()] reads from the CSV comments, for example
#'   the CmdStan version and the sampler arguments, and `variables`,
#'   `sampler_diagnostics`, `stan_variable_sizes` and `num_chains`. Element
#'   `chains` is an array with one object for each chain: the chain `id`, the
#'   `seed`, the adapted step size `step_size_adaptation`, the inverse metric
#'   `inv_metric` (an array, or an array of rows for a dense metric) and the
#'   `time` in seconds. For the `fixed_param` sampler, `variables` and
#'   `sampler_diagnostics` do not list the columns `lp__` and
#'   `accept_stat__`. Use the Parquet schema to get all column names. The CSV
#'   values `true` and `false` are JSON booleans, and the values `NaN`,
#'   `Inf`, `-Inf` and `NA` are the JSON strings `"NaN"`, `"Inf"`, `"-Inf"`
#'   and `"NA"`.
#'
#' Read the metadata with [nanoparquet::read_parquet_metadata()] and
#' [jsonlite::fromJSON()].
#'
#' Only output of the MCMC sampler (`$sample()`) is supported. All CSV
#' files must come from the same model and the same sampler arguments, as in
#' [read_cmdstan_csv()]. The function warns if a chain has a different number
#' of draws than the sampler arguments give, for example a chain that stopped
#' early.
#'
#' @seealso [read_cmdstan_parquet()], [read_cmdstan_csv()],
#'   [`$output_files()`][fit-method-output_files]
#'
#' @examples
#' \dontrun{
#' fit <- cmdstanr_example("logistic", save_warmup = TRUE)
#' pq_file <- file.path(tempdir(), "logistic.parquet")
#' cmdstan_csv_to_parquet(fit$output_files(), pq_file)
#'
#' # all draws
#' draws <- posterior::as_draws_df(nanoparquet::read_parquet(pq_file))
#'
#' # only some variables
#' draws_beta <- posterior::as_draws_df(nanoparquet::read_parquet(
#'   pq_file,
#'   col_select = c("beta[1]", "beta[2]", ".chain", ".iteration", ".draw")
#' ))
#'
#' # metadata
#' pq_metadata <- nanoparquet::read_parquet_metadata(pq_file)
#' kv <- pq_metadata$file_meta_data$key_value_metadata[[1]]
#' metadata <- jsonlite::fromJSON(kv$value[kv$key == "stan.metadata"])
#' metadata$chains$step_size_adaptation
#' }
#'
cmdstan_csv_to_parquet <- function(
  files,
  file,
  warmup_file = NULL,
  chunk_rows = NULL,
  compression = c("zstd", "snappy", "gzip", "uncompressed"),
  overwrite = FALSE
) {
  require_suggested_package("nanoparquet")
  checkmate::assert_character(files, min.len = 1, any.missing = FALSE)
  checkmate::assert_string(file)
  checkmate::assert_string(warmup_file, null.ok = TRUE)
  checkmate::assert_count(chunk_rows, positive = TRUE, null.ok = TRUE)
  checkmate::assert_flag(overwrite)
  compression <- match.arg(compression)
  out_files <- c(file, warmup_file)
  # normalizePath() changes only paths that exist, so normalize the directories
  out_paths <- file.path(
    normalizePath(dirname(out_files), mustWork = FALSE),
    basename(out_files)
  )
  if (anyDuplicated(out_paths)) {
    stop("'file' and 'warmup_file' must be different paths.", call. = FALSE)
  }
  if (!overwrite && any(file.exists(out_files))) {
    stop(
      "Output file already exists: ",
      paste(out_files[file.exists(out_files)], collapse = ", "),
      ". Use 'overwrite = TRUE' to replace it.",
      call. = FALSE
    )
  }
  temp_dir <- withr::local_tempdir()
  withr::local_path(toolchain_PATH_env_var())
  files <- prepare_csv_files(files, temp_dir)
  csv_metadata <- lapply(files, read_csv_metadata)
  if (length(files) > 1) {
    check_csv_metadata_matches(csv_metadata)
  }
  metadata <- csv_metadata[[1]]
  if (metadata$method != "sample") {
    stop(
      "Only CSV files from the MCMC sampler are supported, not method '",
      metadata$method,
      "'.",
      call. = FALSE
    )
  }
  # the fixed_param sampler writes no warmup draws
  n_warmup <- 0
  if (
    isTRUE(metadata$save_warmup == 1) &&
      !isTRUE(metadata$algorithm == "fixed_param")
  ) {
    n_warmup <- ceiling(metadata$iter_warmup / metadata$thin)
  }
  n_post_warmup <- ceiling(metadata$iter_sampling / metadata$thin)
  if (is.null(chunk_rows)) {
    n_columns <- length(metadata$variables) +
      length(metadata$sampler_diagnostics)
    chunk_rows <- ceiling(1e7 / max(1, n_columns))
  }
  # readLines() allocates a vector of length chunk_rows, so limit chunk_rows
  # to about the number of lines in a file (draws, header and comments)
  chunk_rows <- max(1, min(chunk_rows, n_warmup + n_post_warmup + 10000))

  key_value <- c(
    stan.format_version = "1",
    stan.metadata = parquet_stan_metadata(csv_metadata)
  )
  pq_options <- nanoparquet::parquet_options(keep_row_groups = TRUE)
  write_chunk <- function(x, path) {
    if (file.exists(path)) {
      nanoparquet::append_parquet(
        x,
        path,
        compression = compression,
        options = pq_options
      )
    } else {
      draws_kind <- if (identical(path, tmp_warmup)) "warmup" else "sampling"
      nanoparquet::write_parquet(
        x,
        path,
        compression = compression,
        metadata = c(key_value, stan.draws = draws_kind),
        options = pq_options
      )
    }
  }
  add_index_columns <- function(x, chain, iteration, draw) {
    x$.chain <- rep(as.integer(chain), nrow(x))
    x$.iteration <- as.integer(iteration)
    x$.draw <- as.integer(draw)
    x
  }

  # write to temporary files next to the output files, then rename them
  tmp_file <- tempfile(".tmp_", tmpdir = dirname(file), fileext = ".parquet")
  tmp_warmup <- NULL
  if (!is.null(warmup_file)) {
    tmp_warmup <- tempfile(
      ".tmp_",
      tmpdir = dirname(warmup_file),
      fileext = ".parquet"
    )
  }
  on.exit(unlink(c(tmp_file, tmp_warmup)), add = TRUE)

  state <- new.env(parent = emptyenv())
  state$csv_names <- NULL
  state$num_draws <- 0
  state$num_warmup_draws <- 0
  for (i in seq_along(files)) {
    state$row <- 0
    write_lines <- function(lines, header) {
      if (is.null(state$csv_names)) {
        state$csv_names <- header
        state$out_names <- repair_variable_names(header)
      } else if (!identical(header, state$csv_names)) {
        stop(
          "The CSV header of '",
          files[i],
          "' differs from the first file.",
          call. = FALSE
        )
      }
      chunk <- parse_csv_lines(lines, state$out_names, files[i], state$row)
      rows <- state$row + seq_len(nrow(chunk))
      state$row <- state$row + nrow(chunk)
      is_warmup <- rows <= n_warmup
      if (any(is_warmup) && !is.null(tmp_warmup)) {
        w <- chunk[is_warmup, , drop = FALSE]
        draws <- state$num_warmup_draws + seq_len(nrow(w))
        state$num_warmup_draws <- state$num_warmup_draws + nrow(w)
        write_chunk(add_index_columns(w, i, rows[is_warmup], draws), tmp_warmup)
      }
      if (!all(is_warmup)) {
        p <- chunk[!is_warmup, , drop = FALSE]
        draws <- state$num_draws + seq_len(nrow(p))
        state$num_draws <- state$num_draws + nrow(p)
        write_chunk(
          add_index_columns(p, i, rows[!is_warmup] - n_warmup, draws),
          tmp_file
        )
      }
    }
    header <- for_each_csv_chunk(files[i], chunk_rows, write_lines)
    if (!identical(header, state$csv_names)) {
      # a file without draws: check the header here
      if (is.null(state$csv_names)) {
        state$csv_names <- header
        state$out_names <- repair_variable_names(header)
      } else {
        stop(
          "The CSV header of '",
          files[i],
          "' differs from the first file.",
          call. = FALSE
        )
      }
    }
    if (state$row != n_warmup + n_post_warmup) {
      warning(
        "Chain ",
        i,
        " ('",
        files[i],
        "') has ",
        state$row,
        " draws, but the ",
        "sampler arguments give ",
        n_warmup + n_post_warmup,
        ".",
        call. = FALSE
      )
    }
  }
  # an output file without draws still gets the columns and the metadata
  empty <- as.data.frame(
    stats::setNames(
      rep(list(double()), length(state$out_names)),
      state$out_names
    ),
    check.names = FALSE
  )
  empty <- add_index_columns(empty, integer(), integer(), integer())
  for (path in c(tmp_file, tmp_warmup)) {
    if (!file.exists(path)) {
      write_chunk(empty, path)
    }
  }
  if (!file.rename(tmp_file, file)) {
    stop("Could not write '", file, "'.", call. = FALSE)
  }
  if (!is.null(warmup_file) && !file.rename(tmp_warmup, warmup_file)) {
    stop("Could not write '", warmup_file, "'.", call. = FALSE)
  }
  invisible(file)
}

#' Call a function for each chunk of data lines of a CmdStan CSV file.
#'
#' @noRd
#' @param path Path to an uncompressed CmdStan CSV file.
#' @param chunk_rows The number of lines to read at a time.
#' @param fun A function with arguments `lines` (the data lines of a chunk,
#'   without comments and empty lines) and `header` (the column names).
#' @return The column names from the CSV header.
#'
for_each_csv_chunk <- function(path, chunk_rows, fun) {
  con <- base::file(path, open = "r")
  on.exit(close(con), add = TRUE)
  header <- NULL
  repeat {
    lines <- readLines(con, n = chunk_rows)
    if (length(lines) == 0) {
      break
    }
    lines <- lines[nzchar(lines) & !startsWith(lines, "#")]
    if (is.null(header) && length(lines) > 0) {
      header <- strsplit(lines[1], ",", fixed = TRUE)[[1]]
      lines <- lines[-1]
    }
    if (length(lines) > 0) {
      fun(lines, header)
    }
  }
  if (is.null(header)) {
    stop("No CSV header found in '", path, "'.", call. = FALSE)
  }
  header
}

#' Parse data lines of a CmdStan CSV file into a data frame of doubles.
#'
#' @noRd
#' @param lines Data lines, without comments and the header.
#' @param col_names The column names.
#' @param path The CSV file, for error messages.
#' @param rows_before The number of data rows of the file before `lines`.
#' @return A data frame with one row for each line.
#'
parse_csv_lines <- function(lines, col_names, path, rows_before) {
  chunk <- tryCatch(
    suppressWarnings(data.table::fread(
      text = lines,
      header = FALSE,
      sep = ",",
      col.names = col_names,
      colClasses = rep("double", length(col_names)),
      data.table = FALSE,
      showProgress = FALSE
    )),
    error = function(e) e
  )
  if (
    inherits(chunk, "error") ||
      nrow(chunk) != length(lines) ||
      !all(vapply(chunk, is.double, logical(1)))
  ) {
    stop(
      "Malformed data line in '",
      path,
      "' between data rows ",
      rows_before + 1,
      " and ",
      rows_before + length(lines),
      ".",
      if (inherits(chunk, "error")) paste0(" ", conditionMessage(chunk)),
      call. = FALSE
    )
  }
  chunk
}

#' Make the JSON metadata of a Parquet file from CmdStan CSV metadata.
#'
#' @noRd
#' @param csv_metadata A list with one element of `read_csv_metadata()`
#'   output for each chain.
#' @return A JSON string.
#'
parquet_stan_metadata <- function(csv_metadata) {
  per_chain <- parquet_per_chain_fields
  metadata <- csv_metadata[[1]]
  out <- metadata[setdiff(names(metadata), per_chain)]
  # read_csv_metadata() turns the CSV values true and false into integers;
  # write them as JSON booleans
  is_flag <- vapply(
    out,
    function(x) is.integer(x) && length(x) == 1,
    logical(1)
  )
  out[is_flag] <- lapply(out[is_flag], as.logical)
  # I() keeps vectors of length 1 as JSON arrays
  variables <- repair_variable_names(metadata$variables)
  out$variables <- I(variables)
  out$sampler_diagnostics <- I(as.character(metadata$sampler_diagnostics))
  out$stan_variable_sizes <- lapply(variable_dims(variables), I)
  if (length(out$stan_variable_sizes) == 0) {
    # a named empty list gives a JSON object, not an array
    out$stan_variable_sizes <- stats::setNames(list(), character())
  }
  out$num_chains <- length(csv_metadata)
  out$chains <- lapply(csv_metadata, function(x) {
    chain <- x[intersect(per_chain, names(x))]
    if (!is.null(chain$inv_metric) && is.null(dim(chain$inv_metric))) {
      chain$inv_metric <- I(chain$inv_metric)
    }
    if (is.data.frame(chain$time)) {
      chain$time <- as.list(chain$time)
    }
    chain
  })
  # 17 significant digits keep the doubles exact; NaN, Inf, -Inf and NA are
  # JSON strings
  as.character(jsonlite::toJSON(
    out,
    auto_unbox = TRUE,
    digits = I(17),
    null = "null",
    na = "string"
  ))
}

# the metadata fields that can differ between chains
parquet_per_chain_fields <- c(
  "id",
  "seed",
  "init",
  "step_size",
  "step_size_adaptation",
  "fitted_params",
  "inv_metric",
  "time"
)

#' Read CmdStan output from a Parquet file
#'
#' Read the draws and the metadata from a Parquet file that
#' [cmdstan_csv_to_parquet()] wrote. The result is the same as the result of
#' [read_cmdstan_csv()] for the CSV files of the MCMC sampler. To make a fitted
#' model object from a Parquet file, use [as_cmdstan_fit()].
#'
#' @export
#' @inheritParams read_cmdstan_csv
#' @param file (string) Path of a Parquet file from [cmdstan_csv_to_parquet()].
#' @param warmup_file (string) Optional path of the Parquet file of the warmup
#'   draws (the `warmup_file` of [cmdstan_csv_to_parquet()]), from the same
#'   run as `file`. The default is `NULL`, which reads no warmup draws.
#'
#' @return A named list with the same elements as [read_cmdstan_csv()] gives
#'   for the MCMC sampler: `metadata`, `time`, `inv_metric`, `step_size`,
#'   `warmup_draws`, `post_warmup_draws`, `warmup_sampler_diagnostics` and
#'   `post_warmup_sampler_diagnostics`. The values are the same; the fields of
#'   `metadata` can be in a different order. Without `warmup_file`, the
#'   warmup elements are `NULL` also if `metadata$save_warmup` is 1.
#'
#' @details
#' This function requires the \pkg{nanoparquet} package. It reads only the
#' columns of the requested variables and sampler diagnostics. As in
#' [read_cmdstan_csv()], the draws contain `lp__` and the model variables, and
#' the sampler diagnostics are separate.
#'
#' @seealso [cmdstan_csv_to_parquet()], [read_cmdstan_csv()],
#'   [as_cmdstan_fit()]
#'
#' @examples
#' \dontrun{
#' fit <- cmdstanr_example("logistic", save_warmup = TRUE)
#' pq_file <- file.path(tempdir(), "logistic.parquet")
#' pq_warmup <- file.path(tempdir(), "logistic_warmup.parquet")
#' cmdstan_csv_to_parquet(fit$output_files(), pq_file, warmup_file = pq_warmup)
#'
#' x <- read_cmdstan_parquet(pq_file, variables = "beta", warmup_file = pq_warmup)
#' str(x$post_warmup_draws)
#' str(x$post_warmup_sampler_diagnostics)
#'
#' # a fitted model object
#' fit2 <- as_cmdstan_fit(pq_file, warmup_file = pq_warmup)
#' fit2$summary()
#' }
#'
read_cmdstan_parquet <- function(
  file,
  variables = NULL,
  sampler_diagnostics = NULL,
  format = getOption("cmdstanr_draws_format", NULL),
  warmup_file = NULL
) {
  require_suggested_package("nanoparquet")
  checkmate::assert_string(file)
  checkmate::assert_file_exists(file, access = "r")
  checkmate::assert_string(warmup_file, null.ok = TRUE)
  format <- assert_valid_draws_format(format)
  key_value <- parquet_key_value(file, "sampling")
  if (!is.null(warmup_file)) {
    checkmate::assert_file_exists(warmup_file, access = "r")
    warmup_key_value <- parquet_key_value(warmup_file, "warmup")
    if (!identical(warmup_key_value$metadata, key_value$metadata)) {
      stop(
        "'",
        warmup_file,
        "' and '",
        file,
        "' are not from the same run.",
        call. = FALSE
      )
    }
  }
  csv_metadata <- parquet_csv_metadata(key_value$metadata)
  merged <- merge_csv_metadata(csv_metadata)
  metadata <- merged$metadata
  if (metadata$method != "sample") {
    stop(
      "Only output of the MCMC sampler is supported, not method '",
      metadata$method,
      "'.",
      call. = FALSE
    )
  }
  selected <- select_csv_columns(metadata, variables, sampler_diagnostics)
  variables <- repair_variable_names(selected$variables)
  sampler_diagnostics <- selected$sampler_diagnostics
  post_warmup <- read_parquet_chains(
    file,
    variables,
    sampler_diagnostics,
    metadata$num_chains
  )
  warmup <- list()
  if (!is.null(warmup_file) && isTRUE(metadata$save_warmup == 1)) {
    warmup <- read_parquet_chains(
      warmup_file,
      variables,
      sampler_diagnostics,
      metadata$num_chains
    )
  }
  metadata$inv_metric <- NULL
  metadata$variables <- repair_variable_names(metadata$variables)
  metadata <- add_variable_metadata(metadata)
  if (is.null(format)) {
    format <- "draws_array"
  }
  as_draws_format <- as_draws_format_fun(format)
  as_draws_or_null <- function(x) if (length(x) > 0) as_draws_format(x)
  list(
    metadata = metadata,
    time = list(total = NA_integer_, chains = merged$time),
    inv_metric = merged$inv_metric,
    step_size = merged$step_size,
    warmup_draws = as_draws_or_null(warmup$draws),
    post_warmup_draws = as_draws_or_null(post_warmup$draws),
    warmup_sampler_diagnostics = as_draws_or_null(warmup$sampler_diagnostics),
    post_warmup_sampler_diagnostics = as_draws_or_null(
      post_warmup$sampler_diagnostics
    )
  )
}

#' Read and check the key-value metadata of a Parquet file.
#'
#' @noRd
#' @param file A Parquet file from `cmdstan_csv_to_parquet()`.
#' @param draws `"sampling"` or `"warmup"`, the expected `stan.draws` value.
#' @return A list with the JSON text `metadata`.
#'
parquet_key_value <- function(file, draws) {
  kv <- nanoparquet::read_parquet_metadata(
    file
  )$file_meta_data$key_value_metadata[[1]]
  value <- function(key) kv$value[kv$key == key]
  if (!identical(value("stan.format_version"), "1")) {
    stop(
      "'",
      file,
      "' has no CmdStan metadata of format version 1. ",
      "Use a file from cmdstan_csv_to_parquet().",
      call. = FALSE
    )
  }
  if (!identical(value("stan.draws"), draws)) {
    stop(
      "'",
      file,
      "' does not have the ",
      draws,
      " draws.",
      call. = FALSE
    )
  }
  list(metadata = value("stan.metadata"))
}

#' Rebuild the per-chain CSV metadata from the JSON of a Parquet file.
#'
#' @noRd
#' @param json The `stan.metadata` JSON text of a Parquet file.
#' @return A list with one element for each chain, in the form of
#'   `read_csv_metadata()` output, with the CSV variable names.
#'
parquet_csv_metadata <- function(json) {
  json <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  # merge_csv_metadata() sets num_chains, and add_variable_metadata() sets
  # the variable sizes
  shared <- json[setdiff(
    names(json),
    c("chains", "num_chains", "stan_variable_sizes")
  )]
  shared <- parquet_drop_null(lapply(shared, parquet_json_to_r))
  # the JSON has the repaired names; read_csv_metadata() gives the CSV names
  shared$variables <- unrepair_variable_names(shared$variables)
  lapply(json$chains, function(chain) {
    chain <- parquet_drop_null(lapply(chain, parquet_json_to_r))
    if (!is.null(chain$time)) {
      chain$time <- as.data.frame(chain$time)
    }
    c(shared, chain)
  })
}

parquet_drop_null <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

# the JSON strings of the doubles NA, NaN, Inf and -Inf
parquet_json_special <- c("NA", "NaN", "Inf", "-Inf")

#' Convert a value of `jsonlite::fromJSON(simplifyVector = FALSE)` to the
#' types of `read_csv_metadata()`.
#'
#' @noRd
#' @param x A parsed JSON value.
#' @return Numbers (also the strings `"NA"`, `"NaN"`, `"Inf"` and `"-Inf"`)
#'   as doubles, booleans as integers 0 and 1, arrays as vectors, arrays of
#'   arrays as matrices (rows), objects as named lists.
#'
parquet_json_to_r <- function(x) {
  is_number <- function(e) {
    is.numeric(e) || (is.character(e) && e %in% parquet_json_special)
  }
  to_numbers <- function(values) {
    vapply(
      values,
      function(e) suppressWarnings(as.numeric(e)),
      double(1),
      USE.NAMES = FALSE
    )
  }
  if (is.null(x)) {
    return(NULL)
  }
  if (is.character(x)) {
    if (x %in% parquet_json_special) {
      return(to_numbers(list(x)))
    }
    return(x)
  }
  if (is.logical(x)) {
    return(as.integer(x))
  }
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  if (!is.null(names(x))) {
    return(lapply(x, parquet_json_to_r))
  }
  if (length(x) == 0) {
    return(NULL)
  }
  if (all(vapply(x, is.list, logical(1)))) {
    return(do.call(rbind, lapply(x, to_numbers)))
  }
  if (all(vapply(x, is_number, logical(1)))) {
    return(to_numbers(x))
  }
  values <- unlist(x)
  if (is.logical(values)) as.integer(values) else values
}

#' Read columns of a Parquet file into arrays of iterations, chains and
#' variables.
#'
#' @noRd
#' @param path A Parquet file from `cmdstan_csv_to_parquet()`.
#' @param variables The repaired names of the variable columns.
#' @param sampler_diagnostics The names of the sampler diagnostic columns.
#' @param num_chains The number of chains.
#' @return A list with the arrays `draws` and `sampler_diagnostics`; an
#'   empty list if the file has no rows or no columns are requested.
#'
read_parquet_chains <- function(
  path,
  variables,
  sampler_diagnostics,
  num_chains
) {
  columns <- c(variables, sampler_diagnostics)
  if (length(columns) == 0) {
    return(list())
  }
  pq <- as.list(nanoparquet::read_parquet(
    path,
    col_select = c(columns, ".chain", ".iteration")
  ))
  if (length(pq$.chain) == 0) {
    return(list())
  }
  row_order <- order(pq$.chain, pq$.iteration)
  if (!is.unsorted(row_order)) {
    row_order <- NULL
  }
  n_rows <- tabulate(pq$.chain, nbins = num_chains)
  if (sum(n_rows) != length(pq$.chain) || any(n_rows != n_rows[1])) {
    stop(
      "The chains in '",
      path,
      "' have different numbers of draws.",
      call. = FALSE
    )
  }
  pq$.chain <- NULL
  pq$.iteration <- NULL
  # copy one column at a time into the array and free it
  to_array <- function(names) {
    if (length(names) == 0) {
      return(NULL)
    }
    out <- array(
      NA_real_,
      c(n_rows[1], num_chains, length(names)),
      dimnames = list(NULL, NULL, names)
    )
    for (j in seq_along(names)) {
      column <- pq[[names[j]]]
      out[,, j] <- if (is.null(row_order)) column else column[row_order]
      pq[[names[j]]] <<- NULL
    }
    out
  }
  list(
    draws = to_array(variables),
    sampler_diagnostics = to_array(sampler_diagnostics)
  )
}

#' A fitted model object made from a Parquet file
#'
#' Like `CmdStanMCMC_CSV`, but it reads further variables from the Parquet
#' file.
#'
#' @noRd
#'
CmdStanMCMC_Parquet <- R6::R6Class(
  classname = "CmdStanMCMC_Parquet",
  inherit = CmdStanMCMC_CSV,
  public = list(
    initialize = function(
      contents,
      file,
      warmup_file = NULL,
      check_diagnostics = TRUE
    ) {
      private$warmup_file_ <- warmup_file
      super$initialize(contents, file, check_diagnostics)
    },
    num_chains = function() {
      private$metadata_$num_chains
    },
    draws = function(
      variables = NULL,
      inc_warmup = FALSE,
      format = getOption("cmdstanr_draws_format", "draws_array")
    ) {
      private$check_warmup_file_(inc_warmup)
      super$draws(
        variables = variables,
        inc_warmup = inc_warmup,
        format = format
      )
    },
    sampler_diagnostics = function(
      inc_warmup = FALSE,
      format = getOption("cmdstanr_draws_format", "draws_array")
    ) {
      private$check_warmup_file_(inc_warmup)
      super$sampler_diagnostics(inc_warmup = inc_warmup, format = format)
    }
  ),
  private = list(
    warmup_file_ = NULL,
    check_warmup_file_ = function(inc_warmup) {
      if (
        inc_warmup &&
          is.null(private$warmup_file_) &&
          isTRUE(private$metadata_$save_warmup == 1)
      ) {
        stop(
          "The warmup draws are in a separate Parquet file. ",
          "Give it as 'warmup_file' to as_cmdstan_fit().",
          call. = FALSE
        )
      }
    },
    read_output_ = function(variables, sampler_diagnostics, format) {
      read_cmdstan_parquet(
        private$output_files_,
        variables = variables,
        sampler_diagnostics = sampler_diagnostics,
        format = format,
        warmup_file = private$warmup_file_
      )
    }
  )
)
