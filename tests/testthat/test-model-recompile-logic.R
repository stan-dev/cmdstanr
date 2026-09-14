set_cmdstan_path()

# The decision the constructor makes: build, reuse, or adopt. No C++ is
# compiled here. make is mocked and a text file stands in for the executable,
# while stanc, make/local and the build record are real.
mocked <- function(code, compile_ret = list(status = 0),
                   info_ret = default_info_ret) {
  with_mocked_cli(code = code, compile_ret = compile_ret, info_ret = info_ret)
}

# A copy of the program in its own directory, so a mocked build never leaves a
# text file where the other test files expect a real executable.
local_bernoulli <- function(.local_envir = parent.frame()) {
  stan_file <- file.path(
    withr::local_tempdir(.local_envir = .local_envir), "bernoulli.stan"
  )
  file.copy(testing_stan_file("bernoulli"), stan_file)
  stan_file
}

# Counts the launches of `<exe> info`, the only thing that runs the executable.
local_info_launches <- function(.local_envir = parent.frame()) {
  counter <- new.env()
  counter$n <- 0L
  real_run_info_cli <- run_info_cli
  local_mocked_bindings(
    run_info_cli = function(exe_file) {
      counter$n <- counter$n + 1L
      real_run_info_cli(exe_file)
    },
    .env = .local_envir
  )
  counter
}

test_that("a program with no executable is built", {
  stan_file <- local_bernoulli()

  mocked(expect_mock_compile(
    mod <- expect_interactive_message(
      cmdstan_model(stan_file), "Compiling Stan program..."
    )
  ))
  expect_true(file.exists(mod$exe_file()))
  expect_equal(read_build_record(mod$exe_file())$status, "available")
})

test_that("the same call again reuses the executable without launching it", {
  stan_file <- local_bernoulli()
  launches <- local_info_launches()

  mocked(expect_mock_compile(mod <- cmdstan_model(stan_file)))
  # The build asks the temporary executable what it reports, once, before it
  # is installed.
  expect_equal(launches$n, 1L)

  launches$n <- 0L
  mocked(expect_no_mock_compile(
    reused <- expect_interactive_message(
      cmdstan_model(stan_file), "Model executable is up to date!"
    )
  ))
  expect_equal(launches$n, 0L)
  expect_equal(reused$exe_file(), mod$exe_file())
})

test_that("a forced rebuild names whichever of the two asked for it", {
  stan_file <- local_bernoulli()
  mocked(expect_mock_compile(cmdstan_model(stan_file)))

  mocked(expect_mock_compile(expect_interactive_message(
    cmdstan_model(stan_file, force_recompile = TRUE),
    "Recompiling:\n  - `force_recompile = TRUE` was supplied"
  )))

  withr::local_options(cmdstanr_force_recompile = TRUE)
  mocked(expect_mock_compile(expect_interactive_message(
    cmdstan_model(stan_file),
    "Recompiling:\n  - the `cmdstanr_force_recompile` option is set"
  )))
})

test_that("cpp_options rebuild when they change, not when they are respelled", {
  stan_file <- local_bernoulli()
  mocked(expect_mock_compile(
    cmdstan_model(stan_file, cpp_options = list(stan_threads = TRUE))
  ))

  # The record holds the assignment make received, so these are one request.
  mocked(expect_no_mock_compile(
    mod <- cmdstan_model(stan_file, cpp_options = list(STAN_THREADS = "TRUE"))
  ))
  expect_equal(mod$cpp_options(), list(STAN_THREADS = "TRUE"))

  mocked(expect_mock_compile(expect_interactive_message(
    cmdstan_model(stan_file, cpp_options = list(stan_cpp_optims = TRUE)),
    "Recompiling:\n  - `cpp_options` changed"
  )))
})

test_that("stanc_options rebuild when they change, and pedantic never does", {
  stan_file <- local_bernoulli()
  mocked(expect_mock_compile(
    cmdstan_model(stan_file, stanc_options = list("O1"))
  ))

  # --warn-pedantic is injected rather than supplied, so it is not compared.
  mocked(expect_no_mock_compile(
    cmdstan_model(stan_file, stanc_options = list("O1"), pedantic = TRUE)
  ))

  mocked(expect_mock_compile(expect_interactive_message(
    cmdstan_model(stan_file, stanc_options = list("O0")),
    "Recompiling:\n  - `stanc_options` changed"
  )))
})

test_that("include_paths rebuild when they resolve a different file", {
  model_dir <- withr::local_tempdir()
  dir_a <- file.path(model_dir, "a")
  dir_b <- file.path(model_dir, "b")
  dir_copy <- file.path(model_dir, "copy")
  dir.create(dir_a)
  dir.create(dir_b)
  dir.create(dir_copy)
  # One directive, three directories, two different programs.
  writeLines("parameters { real alpha; }", file.path(dir_a, "params.stan"))
  writeLines("parameters { real beta; }", file.path(dir_b, "params.stan"))
  file.copy(file.path(dir_a, "params.stan"), dir_copy)
  stan_file <- file.path(model_dir, "included.stan")
  writeLines(c("#include params.stan", "model { target += 0; }"), stan_file)

  mocked(expect_mock_compile(cmdstan_model(stan_file, include_paths = dir_a)))

  # A byte-identical include somewhere else is the same build.
  mocked(expect_no_mock_compile(
    cmdstan_model(stan_file, include_paths = dir_copy)
  ))

  mocked(expect_mock_compile(expect_interactive_message(
    mod <- cmdstan_model(stan_file, include_paths = dir_b),
    paste0(
      "included files changed \\(",
      resolve_path(file.path(dir_b, "params.stan")), "\\)"
    )
  )))
  expect_equal(names(mod$variables()$parameters), "beta")
})

test_that("a changed program rebuilds and a newer mtime alone does not", {
  stan_file <- local_bernoulli()
  mocked(expect_mock_compile(cmdstan_model(stan_file)))

  Sys.setFileTime(stan_file, Sys.time() + 60)
  mocked(expect_no_mock_compile(cmdstan_model(stan_file)))

  writeLines(c("// a comment", readLines(stan_file)), stan_file)
  mocked(expect_mock_compile(expect_interactive_message(
    cmdstan_model(stan_file),
    "Recompiling:\n  - the Stan program changed"
  )))
})

test_that("a record that cannot be used rebuilds and says why", {
  stan_file <- local_bernoulli()
  mocked(expect_mock_compile(mod <- cmdstan_model(stan_file)))
  record_path <- build_record_path(mod$exe_file())

  file.remove(record_path)
  mocked(expect_mock_compile(expect_interactive_message(
    cmdstan_model(stan_file),
    "Recompiling:\n  - the executable has no build record"
  )))

  writeLines("{", record_path)
  mocked(expect_mock_compile(expect_interactive_message(
    cmdstan_model(stan_file),
    "Recompiling:\n  - the build record beside the executable could not be read"
  )))

  record <- jsonlite::fromJSON(record_path, simplifyVector = FALSE)
  record$format_version <- 2L
  jsonlite::write_json(record, record_path, auto_unbox = TRUE, digits = NA)
  mocked(expect_mock_compile(expect_interactive_message(
    cmdstan_model(stan_file),
    paste0(
      "written by a newer version of cmdstanr ",
      "\\(format 2; this version understands 1\\)"
    )
  )))
})

test_that("a record naming another installation rebuilds", {
  stan_file <- local_bernoulli()
  mocked(expect_mock_compile(mod <- cmdstan_model(stan_file)))

  # The executable is untouched, so its hash still matches the record and the
  # builder is the only row that differs.
  record <- read_build_record(mod$exe_file())$record
  record$builder$version <- "1.2.3"
  write_build_record(record, mod$exe_file())

  mocked(expect_mock_compile(expect_interactive_message(
    cmdstan_model(stan_file),
    "Recompiling:\n  - the selected CmdStan changed \\(built with 1\\.2\\.3"
  )))
})

test_that("dir puts the executable there and a second call reuses it", {
  stan_file <- local_bernoulli()
  exe_dir <- withr::local_tempdir()

  mocked(expect_mock_compile(mod <- cmdstan_model(stan_file, dir = exe_dir)))
  expect_true(same_path(
    mod$exe_file(), cmdstan_ext(file.path(exe_dir, "bernoulli"))
  ))
  expect_false(file.exists(cmdstan_ext(strip_ext(stan_file))))

  mocked(expect_no_mock_compile(
    again <- cmdstan_model(stan_file, dir = exe_dir)
  ))
  expect_equal(again$exe_file(), mod$exe_file())
})

test_that("a failed build leaves no executable and no record", {
  stan_file <- local_bernoulli()
  exe <- cmdstan_ext(strip_ext(stan_file))

  mocked(
    compile_ret = list(status = 1),
    code = expect_error(
      cmdstan_model(stan_file), "An error occurred during compilation"
    )
  )
  expect_false(file.exists(exe))
  expect_false(file.exists(build_record_path(exe)))
})

test_that("every reason that applies is listed, in table order", {
  stan_file <- local_bernoulli()
  mocked(expect_mock_compile(
    cmdstan_model(stan_file, cpp_options = list(stan_threads = TRUE))
  ))
  writeLines(c("// a comment", readLines(stan_file)), stan_file)

  mocked(expect_mock_compile(expect_interactive_message(
    cmdstan_model(stan_file, cpp_options = list(stan_cpp_optims = TRUE)),
    "Recompiling:\n  - `cpp_options` changed\n  - the Stan program changed"
  )))
})

test_that("an executable with a usable record is adopted from the record", {
  stan_file <- local_bernoulli()
  mod <- mock_cmdstan_model(stan_file, cpp_options = list(stan_threads = TRUE))
  launches <- local_info_launches()

  adopted <- cmdstan_model(exe_file = mod$exe_file())
  expect_equal(launches$n, 0L)
  expect_equal(adopted$cpp_options(), list(STAN_THREADS = "TRUE"))
  expect_equal(adopted$cmdstan_version(), cmdstan_version())
})

test_that("an executable with no record is asked to identify itself", {
  stan_file <- local_bernoulli()
  mod <- mock_cmdstan_model(stan_file, cpp_options = list(stan_threads = TRUE))
  file.remove(build_record_path(mod$exe_file()))
  launches <- local_info_launches()

  adopted <- mocked(cmdstan_model(exe_file = mod$exe_file()))
  expect_equal(launches$n, 1L)
  expect_equal(adopted$cpp_options(), structure(list(), names = character()))
  expect_equal(adopted$cmdstan_version(), "2.39.0")
})

test_that("an executable that reports no version is refused", {
  stan_file <- local_bernoulli()
  mod <- mock_cmdstan_model(stan_file)
  file.remove(build_record_path(mod$exe_file()))

  for (reported in c("", "stan_version_major = x\n")) {
    mocked(
      info_ret = list(status = 0, stdout = reported),
      code = expect_error(
        cmdstan_model(exe_file = mod$exe_file()),
        "did not identify itself as a CmdStan executable",
        fixed = TRUE
      )
    )
  }
})
