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
#' Thus `posterior::as_draws_df(nanoparquet::read_parquet(file))` gives the
#' draws, and the `col_select` argument of [nanoparquet::read_parquet()]
#' reads only some of the variables. Include `.chain`, `.iteration` and
#' `.draw` in `col_select` to keep the chain structure. Other Parquet
#' readers (for example \pkg{arrow}, \pkg{duckdb} and \pkg{polars}) can also
#' read the file.
#'
#' The key-value metadata of the Parquet file has two entries:
#' * `stan.format_version`: the version of this file layout, now `"1"`.
#' * `stan.metadata`: the CmdStan metadata as a JSON object. It has the
#'   values that [read_cmdstan_csv()] reads from the CSV comments, for example
#'   the CmdStan version and the sampler arguments, and `variables`,
#'   `sampler_diagnostics`, `stan_variable_sizes` and `num_chains`. Element
#'   `chains` is an array with one object for each chain: the chain `id`, the
#'   `seed`, the adapted step size `step_size_adaptation`, the inverse metric
#'   `inv_metric` (an array, or an array of rows for a dense metric) and the
#'   `time` in seconds. For the `fixed_param` sampler, `variables` and
#'   `sampler_diagnostics` do not list the columns `lp__` and
#'   `accept_stat__`. Use the Parquet schema to get all column names.
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
#' @seealso [read_cmdstan_csv()], [`$output_files()`][fit-method-output_files]
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
#' kv <- nanoparquet::read_parquet_metadata(pq_file)$file_meta_data$key_value_metadata[[1]]
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
      nanoparquet::write_parquet(
        x,
        path,
        compression = compression,
        metadata = key_value,
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
  per_chain <- c(
    "id",
    "seed",
    "init",
    "step_size",
    "step_size_adaptation",
    "fitted_params",
    "inv_metric",
    "time"
  )
  metadata <- csv_metadata[[1]]
  out <- metadata[setdiff(names(metadata), per_chain)]
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
  as.character(jsonlite::toJSON(
    out,
    auto_unbox = TRUE,
    digits = NA,
    null = "null",
    na = "null"
  ))
}
