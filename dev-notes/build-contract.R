#!/usr/bin/env Rscript
#
# Generates compilation-state-contract.md from compilation-state.md.
#
# The design note marks its normative blocks with
#
#   <!-- contract --> ... <!-- /contract -->
#
# which render as nothing on GitHub. This script copies those blocks out, in
# document order, under the heading each one sits beneath, with a link back to
# that heading. Edit the blocks in compilation-state.md, never the generated
# file, then rerun:
#
#   Rscript dev-notes/build-contract.R          # rewrite the contract file
#   Rscript dev-notes/build-contract.R --check  # exit 1 if it is out of date
#
# Markers must alternate open/close; an unmatched or nested marker stops the
# script with the line number. Output depends only on the input file.

args <- commandArgs(trailingOnly = TRUE)
check_only <- identical(args, "--check")

script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
dir <- if (length(script)) dirname(normalizePath(script)) else "dev-notes"
src <- file.path(dir, "compilation-state.md")
out <- file.path(dir, "compilation-state-contract.md")

lines <- readLines(src, warn = FALSE, encoding = "UTF-8")
text <- paste(lines, collapse = "\n")
open_tag <- "<!-- contract -->"
close_tag <- "<!-- /contract -->"

find_all <- function(tag) {
  m <- gregexpr(tag, text, fixed = TRUE)[[1]]
  if (m[1] == -1) integer() else as.integer(m)
}
opens <- find_all(open_tag)
closes <- find_all(close_tag)
marks <- rbind(
  data.frame(pos = opens, open = TRUE),
  data.frame(pos = closes, open = FALSE)
)
marks <- marks[order(marks$pos), ]
line_of <- function(pos) sum(strsplit(substr(text, 1, pos), "")[[1]] == "\n") + 1
expected <- rep(c(TRUE, FALSE), length.out = nrow(marks))
if (nrow(marks) == 0) stop("no contract markers found in ", src)
if (nrow(marks) %% 2 == 1 || any(marks$open != expected)) {
  bad <- which(marks$open != expected)
  bad <- if (length(bad)) marks$pos[bad[1]] else marks$pos[nrow(marks)]
  stop("unmatched or nested contract marker at ", basename(src), ":", line_of(bad))
}

# Heading in force at each position: the nearest ## above, and the nearest ###
# above that is still under that ##.
line_starts <- c(1L, head(cumsum(nchar(lines, type = "chars") + 1L) + 1L, -1L))
is_h2 <- grepl("^## ", lines)
is_h3 <- grepl("^### ", lines)
heading_at <- function(pos) {
  ln <- max(which(line_starts <= pos))
  h2 <- max(which(is_h2 & seq_along(lines) <= ln))
  h3s <- which(is_h3 & seq_along(lines) <= ln & seq_along(lines) > h2)
  list(h2 = lines[h2], h3 = if (length(h3s)) lines[max(h3s)] else NA_character_)
}

# GitHub's heading anchors: strip formatting, lower-case, drop everything but
# letters, digits, spaces, hyphens and underscores, then spaces to hyphens.
slug <- function(h) {
  h <- sub("^#+\\s+", "", h)
  h <- gsub("[`*]", "", h)
  h <- tolower(h)
  h <- gsub("[^a-z0-9 _-]", "", h)
  gsub(" ", "-", h)
}
link_heading <- function(h) {
  level <- sub("^(#+).*$", "\\1", h)
  title <- sub("^#+\\s+", "", h)
  sprintf("%s [%s](%s#%s)", level, title, basename(src), slug(h))
}

body <- character()
cur_h2 <- cur_h3 <- NA_character_
for (i in seq(1, nrow(marks), by = 2)) {
  from <- marks$pos[i] + nchar(open_tag)
  to <- marks$pos[i + 1] - 1
  block <- substr(text, from, to)
  block <- gsub("^\\s+|\\s+$", "", block)
  block <- gsub(" +\n", "\n", block)
  h <- heading_at(marks$pos[i])
  if (!identical(h$h2, cur_h2)) {
    body <- c(body, "", link_heading(h$h2), "")
    cur_h2 <- h$h2
    cur_h3 <- NA_character_
  }
  if (!identical(h$h3, cur_h3) && !is.na(h$h3)) {
    body <- c(body, "", link_heading(h$h3), "")
    cur_h3 <- h$h3
  }
  body <- c(body, block, "")
}

header <- c(
  "# Compilation state and C++ options: the contract",
  "",
  paste0(
    "Generated from `", basename(src), "` by `build-contract.R`. Do not edit this",
    " file; edit the marked blocks there and rerun the script."
  ),
  "",
  "This is the short read: the rules the design note commits to, in the note's",
  "order and words, with none of the reasoning. Each heading links to the section",
  "that holds the rationale, the measurements and the rejected alternatives. Where",
  "this file and the note disagree the file is stale, not authoritative; regenerate",
  "it.",
  ""
)

result <- c(header, body[-1])
blank <- result == ""
result <- result[!(blank & c(FALSE, head(blank, -1)))]  # collapse doubled blanks
generated <- paste(result, collapse = "\n")

if (check_only) {
  current <- if (file.exists(out)) paste(readLines(out, warn = FALSE, encoding = "UTF-8"), collapse = "\n") else ""
  if (!identical(current, generated)) {
    stop(basename(out), " is out of date; rerun Rscript dev-notes/build-contract.R")
  }
  cat(basename(out), "is current\n")
} else {
  writeLines(generated, out, useBytes = TRUE)
  cat("wrote", out, "with", nrow(marks) / 2, "blocks\n")
}
