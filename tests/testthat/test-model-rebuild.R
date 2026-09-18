set_cmdstan_path()

# A fresh copy of a resources/stan/ program (with its include file, for
# bernoulli_include) in its own directory, so a build never touches the
# shared copy under resources/stan/.
local_program <- function(name = "bernoulli", .local_envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .local_envir)
  file.copy(testing_stan_file(name), dir)
  if (name == "bernoulli_include") {
    file.copy(testing_stan_file("divide_real_by_two"), dir)
  }
  file.path(dir, paste0(name, ".stan"))
}

# A copy of an already-built program's current executable and build record
# into `dir`, so a test that only needs a valid, current starting point does
# not pay for another real compile. assess_build() compares content hashes,
# never paths, so this is indistinguishable from a fresh build in `dir`.
seed_from <- function(canonical_stan_file, canonical_exe_file, dir) {
  file.copy(canonical_stan_file, dir)
  if (basename(canonical_stan_file) == "bernoulli_include.stan") {
    file.copy(testing_stan_file("divide_real_by_two"), dir)
  }
  file.copy(canonical_exe_file, dir)
  file.copy(build_record_path(canonical_exe_file), dir)
  file.path(dir, basename(canonical_stan_file))
}

# The selected installation pointed at a directory that does not exist,
# restored when envir exits. Mirrors test-installation-check.R.
local_gone_installation <- function(.local_envir = parent.frame()) {
  path <- cmdstan_path()
  gone <- repair_path(file.path(
    withr::local_tempdir(.local_envir = .local_envir), "cmdstan"
  ))
  .cmdstanr$PATH <- gone
  withr::defer(.cmdstanr$PATH <- path, envir = .local_envir)
  gone
}

canonical_bernoulli_stan <- local_program(
  "bernoulli", .local_envir = teardown_env()
)
canonical_bernoulli_exe <- cmdstan_model(canonical_bernoulli_stan)$exe_file()

canonical_include_stan <- local_program(
  "bernoulli_include", .local_envir = teardown_env()
)
canonical_include_exe <- cmdstan_model(canonical_include_stan)$exe_file()

# The first three tests share this pair: an object built from the program,
# and one that replaces its executable with force_recompile.
replaced_stan_file <- seed_from(
  canonical_bernoulli_stan, canonical_bernoulli_exe,
  withr::local_tempdir(.local_envir = teardown_env())
)
mod_a <- cmdstan_model(replaced_stan_file)
mod_b <- cmdstan_model(replaced_stan_file, force_recompile = TRUE)

test_that("a replaced executable is refused by the object that built it", {
  expect_error(
    mod_a$cmdstan_defaults(),
    "changed after this model was created", fixed = TRUE,
    class = "cmdstanr_stale_executable"
  )
  expect_no_error(mod_b$cmdstan_defaults())
})

test_that("adoption from a record verifies by hash alone", {
  mod_c <- cmdstan_model(exe_file = mod_b$exe_file())
  expect_no_error(mod_c$cmdstan_defaults())

  file.remove(build_record_path(mod_b$exe_file()))
  expect_no_error(mod_c$cmdstan_defaults())

  cmdstan_model(replaced_stan_file, force_recompile = TRUE)
  expect_error(
    mod_c$cmdstan_defaults(),
    "changed after this model was created", fixed = TRUE,
    class = "cmdstanr_stale_executable"
  )
})

test_that("an adopted executable that is deleted is refused", {
  dir <- withr::local_tempdir()
  seed_from(canonical_bernoulli_stan, canonical_bernoulli_exe, dir)
  exe <- file.path(dir, basename(canonical_bernoulli_exe))
  mod <- cmdstan_model(exe_file = exe)
  file.remove(exe)
  expect_error(
    mod$cmdstan_defaults(), "no longer exists", fixed = TRUE,
    class = "cmdstanr_stale_executable"
  )
})

test_that("adoption without a record verifies by the hash it captured", {
  exe <- mod_b$exe_file()
  record <- build_record_path(exe)
  kept <- withr::local_tempdir()
  file.copy(c(exe, record), kept)
  withr::defer(file.copy(
    file.path(kept, basename(c(exe, record))), dirname(exe), overwrite = TRUE
  ))

  file.remove(record)
  mod_d <- cmdstan_model(exe_file = exe)
  expect_no_error(mod_d$cmdstan_defaults())

  # Replaced without a build: another program's executable copied over it
  bernoulli_exe <- cmdstan_model(testing_stan_file("bernoulli"))$exe_file()
  file.copy(bernoulli_exe, exe, overwrite = TRUE)
  expect_error(
    mod_d$cmdstan_defaults(),
    "changed after this model was created", fixed = TRUE,
    class = "cmdstanr_stale_executable"
  )
})

test_that("adoption from a record needs no installation", {
  gone <- local_gone_installation()
  mod_e <- cmdstan_model(exe_file = mod_b$exe_file())
  expect_no_error(mod_e$cmdstan_defaults())
  expect_error(cmdstan_model(replaced_stan_file), gone, fixed = TRUE)
})

test_that("an edited program with an old mtime still triggers a rebuild", {
  dir <- withr::local_tempdir()
  stan_file <- seed_from(canonical_bernoulli_stan, canonical_bernoulli_exe, dir)
  mod <- cmdstan_model(stan_file)
  exe_mtime <- file.mtime(mod$exe_file())

  writeLines(
    c(readLines(stan_file), "generated quantities {", "  real dummy = 1;", "}"),
    stan_file
  )
  Sys.setFileTime(stan_file, exe_mtime - 10)

  expect_interactive_message(
    cmdstan_model(stan_file), "the Stan program changed"
  )
  expect_error(mod$cmdstan_defaults(), class = "cmdstanr_stale_executable")
})

test_that("a failure resolving again errors instead of producing a verdict", {
  dir <- withr::local_tempdir()
  stan_file <- seed_from(canonical_include_stan, canonical_include_exe, dir)
  mod <- cmdstan_model(stan_file)
  exe_mtime <- file.mtime(mod$exe_file())

  file.remove(file.path(dir, "divide_real_by_two.stan"))
  expect_error(
    cmdstan_model(stan_file), "divide_real_by_two.stan", fixed = TRUE
  )
  expect_equal(file.mtime(mod$exe_file()), exe_mtime)

  err <- tryCatch(mod$cmdstan_defaults(), error = function(e) e)
  expect_false(inherits(err, "cmdstanr_stale_executable"))
  expect_match(conditionMessage(err), "divide_real_by_two.stan", fixed = TRUE)
})

test_that("adoption from a record ignores a CmdStan it never selected", {
  record <- read_build_record(mod_b$exe_file())$record
  record$cmdstan$version <- "2.35.0"
  write_build_record(record, mod_b$exe_file())

  mod <- with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = stop("adoption from a usable record must not run info"),
    code = cmdstan_model(exe_file = mod_b$exe_file())
  )
  expect_equal(mod$cmdstan_version(), "2.35.0")
  expect_no_error(mod$cmdstan_defaults())

  expect_interactive_message(
    mock_cmdstan_model(replaced_stan_file), "selected CmdStan changed"
  )
})

test_that("identical included content reuses across different include paths", {
  base_dir <- withr::local_tempdir()
  stan_file <- write_stan_file(c(
    "functions {",
    "#include helper.stan",
    "}",
    "parameters {",
    "  real x;",
    "}",
    "model {",
    "  x ~ std_normal();",
    "}",
    "generated quantities {",
    "  real h = helper(x);",
    "}"
  ), dir = base_dir, basename = "with_helper")
  v1 <- file.path(base_dir, "v1")
  v2 <- file.path(base_dir, "v2")
  v1copy <- file.path(base_dir, "v1copy")
  dir.create(v1)
  dir.create(v2)
  dir.create(v1copy)
  writeLines(
    "real helper(real x) { return x + 1; }", file.path(v1, "helper.stan")
  )
  writeLines(
    "real helper(real x) { return x + 2; }", file.path(v2, "helper.stan")
  )
  file.copy(file.path(v1, "helper.stan"), file.path(v1copy, "helper.stan"))

  expect_compilation(cmdstan_model(stan_file, include_paths = v1))
  expect_no_recompilation(cmdstan_model(stan_file, include_paths = v1copy))
  expect_interactive_message(
    cmdstan_model(stan_file, include_paths = v2), "included files changed"
  )
  expect_no_recompilation(cmdstan_model(stan_file, include_paths = v2))
})

test_that("an edit to a nested include rebuilds", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "#include b.stan",
      "parameters {",
      "  real x;",
      "}",
      "model {",
      "  x ~ std_normal();",
      "}"
    ),
    file.path(dir, "a.stan")
  )
  writeLines("#include c.stan", file.path(dir, "b.stan"))
  writeLines("// c v1", file.path(dir, "c.stan"))
  stan_file <- file.path(dir, "a.stan")

  expect_mock_compile(mock_cmdstan_model(stan_file))

  writeLines("// c v2", file.path(dir, "c.stan"))
  expect_mock_compile(expect_interactive_message(
    mock_cmdstan_model(stan_file), "included files changed.*c\\.stan"
  ))
})

test_that("include paths reordered to the same resolution do not rebuild", {
  dir <- withr::local_tempdir()
  dir1 <- file.path(dir, "dir1")
  dir2 <- file.path(dir, "dir2")
  dir.create(dir1)
  dir.create(dir2)
  writeLines(
    "real helper(real x) { return x + 1; }", file.path(dir1, "helper.stan")
  )
  stan_file <- file.path(dir, "model.stan")
  writeLines(
    c(
      "functions {",
      "#include helper.stan",
      "}",
      "parameters { real x; }",
      "model { x ~ std_normal(); }",
      "generated quantities { real h = helper(x); }"
    ),
    stan_file
  )

  expect_mock_compile(
    mock_cmdstan_model(stan_file, include_paths = c(dir1, dir2))
  )
  expect_no_mock_compile(
    mock_cmdstan_model(stan_file, include_paths = c(dir2, dir1))
  )
})

test_that("formatting the program invalidates the executable that built it", {
  dir <- withr::local_tempdir()
  stan_file <- write_stan_file(c(
    "data {",
    "  int<lower=0>   N;",
    "  array[N] int<lower=0,upper=1>    y;",
    "}",
    "parameters {",
    "  real<lower=0,upper=1>    theta;",
    "}",
    "model {",
    "  theta ~ beta(1,1);",
    "  y ~ bernoulli(theta);",
    "}"
  ), dir = dir)
  mod <- expect_compilation(cmdstan_model(stan_file))

  mod$format(overwrite_file = TRUE, backup = FALSE)
  expect_error(mod$sample(), class = "cmdstanr_stale_executable")
  expect_interactive_message(
    cmdstan_model(stan_file), "the Stan program changed"
  )
  expect_true(any(grepl("   N", mod$code(), fixed = TRUE)))
})

test_that("pedantic mode runs on a current executable without rebuilding", {
  dir <- withr::local_tempdir()
  stan_file <- write_stan_file(c(
    "data {",
    "  int<lower=0> N;",
    "  vector[N] y;",
    "}",
    "parameters {",
    "  real mu;",
    "  real sigma;",
    "}",
    "model {",
    "  mu ~ normal(0, 1);",
    "  y ~ normal(mu, sigma);",
    "}"
  ), dir = dir)
  expect_compilation(cmdstan_model(stan_file))

  expect_no_recompilation(cmdstan_model(stan_file, pedantic = TRUE))
  expect_message(
    cmdstan_model(stan_file, pedantic = TRUE),
    "not constrained to be strictly positive", fixed = TRUE
  )
})

test_that("a warning flag in stanc_options is quiet on a reuse", {
  dir <- withr::local_tempdir()
  stan_file <- write_stan_file(c(
    "parameters { real y; }",
    "model {",
    "  real x;",
    "  target += x;",
    "  y ~ normal(0, 1);",
    "}"
  ), dir = dir)
  mock_cmdstan_model(stan_file, stanc_options = list("warn-uninitialized"))

  expect_no_mock_compile(expect_no_message(
    mock_cmdstan_model(stan_file, stanc_options = list("warn-uninitialized"))
  ))

  expect_message(
    mock_cmdstan_model(
      stan_file, stanc_options = list("warn-uninitialized"), pedantic = TRUE
    ),
    "may not have been assigned a value before its first use", fixed = TRUE
  )
})

test_that("an edited include is caught, then rebuilt", {
  dir <- withr::local_tempdir()
  stan_file <- seed_from(canonical_include_stan, canonical_include_exe, dir)
  mod <- cmdstan_model(stan_file)

  write("// edited", file.path(dir, "divide_real_by_two.stan"), append = TRUE)
  expect_error(mod$sample(), "divide_real_by_two.stan", fixed = TRUE)

  mod2 <- expect_interactive_message(
    cmdstan_model(stan_file), "included files changed"
  )
  utils::capture.output(fit <- mod2$sample(
    data = testing_data("bernoulli"), chains = 1,
    iter_warmup = 50, iter_sampling = 50, refresh = 0
  ))
  expect_s3_class(fit, "CmdStanMCMC")
})

test_that("log_prob after an edit still reflects the program that was built", {
  skip_if(os_is_wsl())
  dir <- withr::local_tempdir()
  stan_file <- seed_from(canonical_bernoulli_stan, canonical_bernoulli_exe, dir)
  mod <- cmdstan_model(stan_file)
  data <- testing_data("bernoulli")
  utils::capture.output(fit <- mod$sample(data = data, chains = 1, refresh = 0))

  edited <- gsub("beta(1, 1)", "beta(2, 2)", readLines(stan_file), fixed = TRUE)
  writeLines(edited, stan_file)
  fit$init_model_methods()
  theta <- plogis(0.3)
  expected_lp <- sum(dbinom(data$y, size = 1, prob = theta, log = TRUE)) +
    log(theta) + log(1 - theta)
  expect_equal(fit$log_prob(unconstrained_variables = 0.3), expected_lp)
})

test_that("a pedantic flag both added and in make/local is recorded once, as added", {
  local_cmdstan_make_local(cpp_options = list("STANCFLAGS += --warn-pedantic"))
  stan_file <- local_program("bernoulli")
  mod <- mock_cmdstan_model(stan_file, pedantic = TRUE)

  record <- read_build_record(mod$exe_file())$record
  added <- unlist(record$configuration$stanc_options_added)
  from_make <- unlist(record$configuration$stanc_options_from_make)
  expect_equal(sum(added == "--warn-pedantic"), 1)
  expect_false("--warn-pedantic" %in% from_make)
  expect_true(file.exists(mod$hpp_file()))
})

test_that("toggling cpp_options rebuilds and threading follows the exe", {
  dir <- withr::local_tempdir()
  stan_file <- seed_from(canonical_bernoulli_stan, canonical_bernoulli_exe, dir)
  data <- testing_data("bernoulli")

  mod_threaded <- expect_interactive_message(
    cmdstan_model(stan_file, cpp_options = list(stan_threads = TRUE)),
    "`cpp_options` changed"
  )
  utils::capture.output(fit <- mod_threaded$sample(
    data = data, chains = 1, threads_per_chain = 2, refresh = 0
  ))
  expect_equal(fit$metadata()$threads_per_chain, 2)

  mod_plain <- expect_interactive_message(
    cmdstan_model(stan_file), "`cpp_options` changed"
  )
  expect_length(mod_plain$cpp_options(), 0)
  expect_error(
    mod_plain$sample(data = data, threads_per_chain = 2),
    "does not report threading as enabled", fixed = TRUE
  )
})

test_that("a program in a directory with an apostrophe builds", {
  skip_if(os_is_wsl(), "the WSL file check cannot take an apostrophe")
  dir <- tempfile("O'Brien-")
  dir.create(dir)
  withr::defer(unlink(dir, recursive = TRUE))
  stan_file <- file.path(dir, "model.stan")
  writeLines(c("parameters { real y; }", "model { y ~ std_normal(); }"), stan_file)

  exe <- expect_interactive_message(
    compile_stan_file(stan_file), "Compiling Stan program..."
  )
  expect_true(file.exists(exe))

  before_mtime <- file.mtime(exe)
  expect_interactive_message(
    compile_stan_file(stan_file), "Model executable is up to date!"
  )
  expect_equal(file.mtime(exe), before_mtime)
})

test_that("a model whose Stan file is gone errors, and its executable can be adopted", {
  dir <- withr::local_tempdir()
  stan_file <- file.path(dir, "bernoulli.stan")
  file.copy(testing_stan_file("bernoulli"), stan_file)
  mod <- cmdstan_model(stan_file)
  data <- testing_data("bernoulli")

  file.remove(stan_file)
  expect_error(
    mod$sample(data = data), "this model was created from no longer exists",
    fixed = TRUE
  )
  adopted <- cmdstan_model(exe_file = mod$exe_file())
  expect_sample_output(adopted$sample(data = data, chains = 1), 1)
})

test_that("a build leaves only the model's C++ in the temporary directory", {
  stan_file <- local_program()
  before <- list.files(tempdir())
  mod <- cmdstan_model(stan_file)
  expect_equal(setdiff(list.files(tempdir()), before), basename(mod$hpp_file()))
})
