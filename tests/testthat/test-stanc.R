# Tests for the stanc helpers in R/stanc.R. Building a model with these options
# is covered in test-model-compile.R.

set_cmdstan_path()

test_that("stanc options without a dedicated argument are left alone", {
  options <- list("filename-in-msg" = "x.stan")
  expect_equal(assert_valid_stanc_options(options), options)
  expect_equal(assert_valid_stanc_options(list("O1")), list("O1"))
  expect_equal(
    assert_valid_stanc_options(list("warn-uninitialized" = TRUE)),
    list("warn-uninitialized" = TRUE)
  )
})

test_that("stanc_options_to_args() builds direct and Make-quoted arguments", {
  # Unnamed options are already flag names and are never quoted
  expect_equal(stanc_options_to_args(list("allow-undefined")), "--allow-undefined")
  expect_equal(
    stanc_options_to_args(list("allow-undefined"), quote_values = TRUE),
    "--allow-undefined"
  )

  # Logical values mark boolean flags
  expect_equal(stanc_options_to_args(list("warn-pedantic" = TRUE)), "--warn-pedantic")
  expect_equal(stanc_options_to_args(list("warn-pedantic" = FALSE)), NULL)
  expect_equal(
    stanc_options_to_args(list("O1")),
    stanc_options_to_args(list(O1 = TRUE))
  )

  # Values are quoted only for Make (#1227)
  expect_equal(
    stanc_options_to_args(list(canonicalize = "deprecations")),
    "--canonicalize=deprecations"
  )
  # make_shell_quote() quotes only arguments holding characters outside its
  # safe set, so a plain value comes back unquoted
  expect_equal(
    stanc_options_to_args(list(canonicalize = "deprecations"), quote_values = TRUE),
    "--canonicalize=deprecations"
  )

  # A value outside the safe set comes back as shQuote() writes it
  expect_equal(
    stanc_options_to_args(
      list("filename-in-msg" = "O'Brien's model.stan"), quote_values = TRUE
    ),
    shQuote("--filename-in-msg=O'Brien's model.stan", type = "sh")
  )

  # A `$` in a quoted argument is doubled, since make expands it
  expect_equal(
    stanc_options_to_args(list(canonicalize = "$HOME"), quote_values = TRUE),
    "'--canonicalize=$$HOME'"
  )

  expect_equal(
    stanc_options_to_args(list(name = "m_model"), quote_values = TRUE),
    "--name=m_model"
  )

  # Numeric values are kept rather than collapsed to a bare flag (#1233)
  expect_equal(
    stanc_options_to_args(list("max-line-length" = 78)),
    "--max-line-length=78"
  )

  expect_equal(stanc_options_to_args(list()), NULL)
  expect_equal(stanc_options_to_args(NULL), NULL)
})

test_that("a flag the call emits drops the make/local copy", {
  expect_equal(
    drop_overridden_stancflags(c("--warn-pedantic"), c("--warn-pedantic")),
    character(0)
  )
  expect_equal(drop_overridden_stancflags(c("--O1"), c("--O1")), character(0))

  # One hyphen, not two: the next word is a flag of its own
  expect_equal(
    drop_overridden_stancflags(c("--warn-pedantic", "-fno-soa"), c("--warn-pedantic")),
    "-fno-soa"
  )

  # A value given as a separate word goes with the flag
  expect_equal(
    drop_overridden_stancflags(
      c("--filename-in-msg", "published.stan"),
      c("--filename-in-msg=x.stan")
    ),
    character(0)
  )
  expect_equal(
    drop_overridden_stancflags(
      c("--filename-in-msg=published.stan"),
      c("--filename-in-msg=x.stan")
    ),
    character(0)
  )

  expect_equal(
    drop_overridden_stancflags(c("--O1", "--warn-pedantic"), c("--name=bernoulli_model")),
    c("--O1", "--warn-pedantic")
  )
  expect_equal(
    drop_overridden_stancflags(character(0), c("--name=bernoulli_model")),
    character(0)
  )
})

test_that("include_paths_stanc3_args() works", {
  expect_equal(include_paths_stanc3_args(), NULL)
  path_1 <- file.path(tempdir(), "folder1")
  if (!dir.exists(path_1)) {
    dir.create(path_1)
  }
  path_1 <- repair_path(path_1)
  path_1_compare <- ifelse(os_is_wsl(), wsl_safe_path(path_1), path_1)
  # tempdir() can hold characters that need quoting, such as the `~` in a
  # Windows short path. The quoting rule itself is pinned below.
  path_1_make <- make_shell_quote(path_1_compare)
  expect_equal(
    include_paths_stanc3_args(path_1),
    paste0("--include-paths=", path_1_make))
  path_2 <- file.path(tempdir(), "folder 2")
  if (!dir.exists(path_2)) {
    dir.create(path_2)
  }
  path_2 <- repair_path(path_2)
  path_2_compare <- ifelse(os_is_wsl(), wsl_safe_path(path_2), path_2)
  path_2_make <- paste0("'", path_2_compare, "'")
  expect_equal(
    include_paths_stanc3_args(c(path_1, path_2)),
    paste0("--include-paths=", path_1_make, ",", path_2_make)
  )
  expect_equal(
    include_paths_stanc3_args(
      c(path_1, path_2),
      direct_call = TRUE
    ),
    c("--include-paths", paste0(path_1_compare, ",", path_2_compare))
  )

  # Make expands the flag before the shell splits it, so a quote in the path is
  # quoted for the shell and a dollar sign is doubled for Make (#1230). Direct
  # calls still get the path as it is.
  path_3 <- file.path(tempdir(), "the model's includes")
  if (!dir.exists(path_3)) {
    dir.create(path_3)
  }
  path_3 <- repair_path(path_3)
  path_3_compare <- ifelse(os_is_wsl(), wsl_safe_path(path_3), path_3)
  expect_equal(
    include_paths_stanc3_args(path_3),
    paste0("--include-paths=", "\"", path_3_compare, "\"")
  )
  expect_equal(
    include_paths_stanc3_args(path_3, direct_call = TRUE),
    c("--include-paths", path_3_compare)
  )

  # The wsl launcher passes paths through a shell, so a `$` in the last path
  # component does not survive the directory check under WSL. That limit is
  # in the WSL path handling, not in the quoting tested here.
  if (!os_is_wsl()) {
    path_4 <- file.path(tempdir(), "costs $5")
    if (!dir.exists(path_4)) {
      dir.create(path_4)
    }
    path_4 <- repair_path(path_4)
    expect_equal(
      include_paths_stanc3_args(c(path_3, path_4)),
      paste0(
        "--include-paths=",
        "\"", path_3, "\"", ",",
        "'", sub("$5", "$$5", path_4, fixed = TRUE), "'"
      )
    )
    expect_equal(
      include_paths_stanc3_args(c(path_3, path_4), direct_call = TRUE),
      c("--include-paths", paste0(path_3, ",", path_4))
    )
  }
})

test_that("get_standalone_hpp() reports stanc failures", {
  model_dir <- withr::local_tempdir()
  stan_file <- file.path(model_dir, "model.stan")
  writeLines("parameters { real y; } model { y ~ std_normal(); }", stan_file)
  local_mocked_bindings(
    wsl_compatible_run = function(...) {
      list(
        status = 124L,
        stdout = "",
        stderr = "stanc: invalid canonicalize value"
      )
    }
  )

  expect_snapshot(
    error = TRUE,
    get_standalone_hpp(
      stan_file,
      "--canonicalize='deprecations'"
    )
  )
})

test_that("get_standalone_hpp() suggests formatting deprecated syntax", {
  stan_file <- withr::local_tempfile(fileext = ".stan")
  local_mocked_bindings(
    wsl_compatible_run = function(...) {
      list(
        status = 1L,
        stdout = "",
        stderr = "Syntax error: Use the auto-format flag to stanc"
      )
    }
  )

  expect_snapshot(
    error = TRUE,
    get_standalone_hpp(stan_file, character())
  )
})
