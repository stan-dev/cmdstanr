#' Write Stan code to a file
#'
#' Convenience function for writing Stan code to a (possibly
#' [temporary][base::tempfile]) file with a `.stan` extension. By default, the
#' file name is chosen deterministically based on a [hash][rlang::hash()]
#' of the Stan code, and the file is not overwritten if it already has correct
#' contents. This means that calling this function multiple times with the same
#' Stan code will reuse the compiled model. This also however means that the
#' function is potentially not thread-safe. Using `hash_salt = Sys.getpid()`
#' should ensure thread-safety in the rare cases when it is needed.
#'
#' @export
#' @param code (character vector) The Stan code to write to the file. This can
#'   be a character vector of length one (a string) containing the entire Stan
#'   program or a character vector with each element containing one line of the
#'   Stan program.
#' @param dir (string) An optional path to the directory where the file will be
#'   written. If omitted, a global option `cmdstanr_write_stan_file_dir` is
#'   used. If the global options is not set, [temporary directory][base::tempdir]
#'   is used.
#' @param basename (string) If `dir` is specified, optionally the basename to
#'   use for the file created. If not specified a file name is generated
#'   from [hashing][rlang::hash()] the code.
#' @param force_overwrite (logical) If set to `TRUE` the file will always be
#'   overwritten and thus the resulting model will always be recompiled.
#' @param hash_salt (string) Text to add to the model code prior to hashing to
#'   determine the file name if `basename` is not set.
#' @return The path to the file.
#'
#' @examples
#' # stan program as a single string
#' stan_program <- "
#' data {
#'   int<lower=0> N;
#'   array[N] int<lower=0,upper=1> y;
#' }
#' parameters {
#'   real<lower=0,upper=1> theta;
#' }
#' model {
#'   y ~ bernoulli(theta);
#' }
#' "
#'
#' f <- write_stan_file(stan_program)
#' print(f)
#'
#' lines <- readLines(f)
#' print(lines)
#' cat(lines, sep = "\n")
#'
#' # stan program as character vector of lines
#' f2 <- write_stan_file(lines)
#' identical(readLines(f), readLines(f2))
#'
write_stan_file <- function(code,
                            dir = getOption("cmdstanr_write_stan_file_dir", tempdir()),
                            basename = NULL,
                            force_overwrite = FALSE,
                            hash_salt = "") {
  dir <- absolute_path(dir)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  collapsed_code <- paste0(code, collapse = "\n")

  if (!is.null(basename)) {
    if (!endsWith(basename, ".stan")) {
      basename <- paste0(basename, ".stan")
    }
    file <- file.path(dir, basename)
  } else {
    require_suggested_package("rlang")
    hash <- rlang::hash(paste0(hash_salt, collapsed_code))
    file <- file.path(dir, paste0("model_", hash, ".stan"))
  }
  overwrite <- TRUE
  # Do not overwrite file if it has the correct contents (to avoid recompilation)
  if (!force_overwrite && file.exists(file)) {
    tryCatch({
      file_contents <- paste0(readLines(file), collapse = "\n")
      if (gsub("\r|\n", "\n", file_contents) == gsub("\r|\n", "\n", collapsed_code)) {
        overwrite <- FALSE
      }
    },
    error = function(e) {
      warning("Error when checking old file contents", e)
    })
  }

  if (overwrite) {
    cat(code, file = file, sep = "\n")
  }
  file
}


#' Print a Stan file with syntax highlighting in Quarto
#'
#' Prints the contents of a Stan file, optionally with syntax highlighting
#' when used in a Quarto or R Markdown document. When called inside a
#' [knitr][knitr::knitr-package] code chunk with the chunk option
#' `output: asis` (or `results: asis` in R Markdown), the output is a
#' fenced Stan code block that Quarto renders with syntax highlighting.
#' When called interactively or without `output: asis`, the code is
#' printed as plain text via [writeLines()].
#'
#' @export
#' @param file (string) Path to a `.stan` file.
#' @param fold (logical) Whether to wrap the output in an HTML
#'   `<details>` block so that the code is collapsed (folded) by
#'   default. Only has an effect when rendering with `output: asis`.
#'   Defaults to `FALSE`.
#' @param summary (string) The summary text shown in the fold toggle
#'   when `fold = TRUE`. Defaults to `"Stan model code"`.
#' @return The file path (invisibly).
#'
#' @section Quarto usage:
#' Use in a Quarto code chunk with `output: asis` to get syntax
#' highlighting:
#'
#' ````
#' ```{r}
#' #| output: asis
#' print_stan_file("path/to/model.stan")
#' ```
#' ````
#'
#' To make the code block collapsible:
#'
#' ````
#' ```{r}
#' #| output: asis
#' print_stan_file("path/to/model.stan", fold = TRUE)
#' ```
#' ````
#'
#' @examples
#' stan_file <- write_stan_file("
#' parameters {
#'   real y;
#' }
#' model {
#'   y ~ std_normal();
#' }
#' ")
#'
#' # Prints plain code at the console
#' print_stan_file(stan_file)
#'
print_stan_file <- function(file, fold = FALSE, summary = "Stan model code") {
  code <- readLines(file)
  if (isTRUE(getOption("knitr.in.progress")) &
        identical(knitr::opts_current$get("results"), "asis")) {
    if (fold) {
      cat("<details><summary>", summary, "</summary>\n\n", sep = "")
    }
    cat("```stan\n")
    cat(code, sep = "\n")
    cat("\n```\n")
    if (fold) {
      cat("\n</details>\n")
    }
  } else {
    writeLines(code)
  }
  invisible(file)
}
