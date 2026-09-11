set_cmdstan_path()

# What the mocked executable reports when a test does not say otherwise.
default_info_ret <- list(
  status = 0,
  stdout = paste0(
    "stan_version_major = 2\n",
    "stan_version_minor = 39\n",
    "stan_version_patch = 0\n",
    "STAN_THREADS=true\n",
    "STAN_OPENCL=false\n"
  )
)

# No C++ is compiled here: the mock writes a text file where make would have
# left the executable. stanc, the tbb_dir query and make/local are real.
mock_compile <- function(stan_file, ..., info_ret = default_info_ret) {
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = info_ret,
    code = expect_mock_compile(
      mod <- cmdstan_model(stan_file, force_recompile = TRUE, ...)
    )
  )
  mod
}

# A copy of the program in its own directory. The mock writes a text file where
# the executable goes, which must never be the one the other test files share.
local_bernoulli <- function(.local_envir = parent.frame()) {
  stan_file <- file.path(
    withr::local_tempdir(.local_envir = .local_envir), "bernoulli.stan"
  )
  file.copy(testing_stan_file("bernoulli"), stan_file)
  stan_file
}

test_that("a build writes a record that reads back available", {
  stan_file <- local_bernoulli()
  mod <- mock_compile(stan_file)

  result <- read_build_record(mod$exe_file())
  expect_equal(result$status, "available")
  expect_equal(result$record$request$stanc_name, "bernoulli_model")
  expect_equal(
    result$record$builder,
    list(path = cmdstan_path(), version = cmdstan_version())
  )
  expect_equal(result$record$artifact, hash_file(mod$exe_file()))
  expect_false("user_header" %in% names(result$record$dependencies))
  if (!file.exists(file.path(cmdstan_path(), "make", "local"))) {
    expect_false("make_local" %in% names(result$record$dependencies))
  }
})

test_that("the recorded stanc name is the raw string stanc was passed", {
  stan_file <- file.path(withr::local_tempdir(), "my-model.stan")
  file.copy(testing_stan_file("bernoulli"), stan_file)
  mod <- mock_compile(stan_file)

  record <- read_build_record(mod$exe_file())$record
  expect_equal(record$request$stanc_name, "my-model_model")
  expect_true(
    "--name=my-model_model" %in% unlist(record$request$stanc_options_injected)
  )
})

test_that("supplied and injected stanc options are recorded apart", {
  stan_file <- local_bernoulli()
  mod <- mock_compile(
    stan_file,
    pedantic = TRUE,
    stanc_options = list("O1")
  )

  record <- read_build_record(mod$exe_file())$record
  expect_equal(record$request$stanc_options_supplied, list("--O1"))
  expect_equal(
    record$request$stanc_options_injected,
    list("--warn-pedantic", "--name=bernoulli_model")
  )
})

test_that("cpp_options_supplied holds what the caller passed", {
  stan_file <- local_bernoulli()
  user_header <- withr::local_tempfile(lines = "", fileext = ".hpp")
  local_mocked_bindings(
    get_standalone_hpp = function(stan_file, stancflags) ""
  )
  mod <- mock_compile(
    stan_file,
    cpp_options = list(stan_threads = TRUE),
    user_header = user_header
  )

  record <- read_build_record(mod$exe_file())$record
  expect_equal(record$request$cpp_options_supplied, list(STAN_THREADS = "TRUE"))
  expect_false("USER_HEADER" %in% names(record$request$cpp_options_supplied))
  expect_equal(
    record$dependencies$user_header,
    list(hash = hash_file(user_header), built_from = resolve_path(user_header))
  )
})

test_that("included files are recorded in stanc's order with content hashes", {
  dir <- withr::local_tempdir()
  note <- file.path(dir, "note.stan")
  zz <- file.path(dir, "zz.stan")
  aa <- file.path(dir, "aa.stan")
  writeLines("// nothing here", note)
  writeLines("real f_zz(real x) { return x; }", zz)
  writeLines("real f_aa(real x) { return x; }", aa)
  stan_file <- file.path(dir, "model.stan")
  writeLines(c(
    "functions {", "#include note.stan", "#include zz.stan",
    "#include note.stan", "#include aa.stan", "}",
    "parameters { real y; }",
    "model { y ~ normal(f_zz(0), f_aa(1)); }"
  ), stan_file)
  mod <- mock_compile(stan_file)

  record <- read_build_record(mod$exe_file())$record
  included <- record$dependencies$included_files
  expect_length(included, 4)
  expected <- list(note, zz, note, aa)
  for (i in seq_along(expected)) {
    expect_equal(included[[i]]$hash, hash_file(expected[[i]]))
    expect_true(
      same_path(included[[i]]$built_from, resolve_path(expected[[i]]))
    )
  }
  # The constructor defaults include_paths to the stan file's own directory.
  expect_equal(
    record$request$include_paths,
    list(resolve_path(dirname(stan_file)))
  )
})

test_that("an include on the WSL filesystem is recorded by its share path", {
  skip_if_not(os_is_wsl())
  # stanc reports this include as /tmp/..., a path only the distribution can
  # open, so the record has to hold the spelling Windows opens it by. No mock:
  # the point is that the host hashes the real file.
  include_dir <- repair_path(file.path(wsl_dir_prefix(), wsl_tempdir()))
  withr::defer(unlink(include_dir, recursive = TRUE))
  include <- file.path(include_dir, "f.stan")
  writeLines("real f(real x) { return x; }", include)
  stan_file <- file.path(withr::local_tempdir(), "model.stan")
  writeLines(c(
    "functions {", "#include f.stan", "}",
    "parameters { real y; }",
    "model { y ~ normal(f(0), 1); }"
  ), stan_file)
  mod <- cmdstan_model(
    stan_file, include_paths = include_dir, force_recompile = TRUE
  )

  record <- read_build_record(mod$exe_file())$record
  included <- record$dependencies$included_files
  expect_length(included, 1)
  expect_equal(included[[1]]$hash, hash_file(include))
  expect_equal(included[[1]]$built_from, resolve_path(include))
})

test_that("the other injection sites land in the injected list", {
  user_header <- withr::local_tempfile(lines = "", fileext = ".hpp")
  local_mocked_bindings(
    get_standalone_hpp = function(stan_file, stancflags) ""
  )
  stan_file <- local_bernoulli()
  mod <- mock_compile(
    stan_file,
    cpp_options = list(stan_opencl = TRUE),
    user_header = user_header
  )

  record <- read_build_record(mod$exe_file())$record
  expect_equal(
    record$request$stanc_options_injected,
    list("--use-opencl", "--allow-undefined", "--name=bernoulli_model")
  )
  expect_equal(record$request$stanc_options_supplied, list())
})

test_that("make/local is a dependency only when present", {
  local_cmdstan_make_local(cpp_options = list("CXXFLAGS += -O1"))
  make_local <- file.path(cmdstan_path(), "make", "local")
  stan_file <- local_bernoulli()
  mod <- mock_compile(stan_file)

  record <- read_build_record(mod$exe_file())$record
  expect_equal(record$dependencies$make_local$built_from, make_local)
  expect_equal(record$dependencies$make_local$hash, hash_file(make_local))
})

test_that("reported features are what the binary reports, by presence", {
  stan_file <- local_bernoulli()
  mod <- mock_compile(stan_file)

  record <- read_build_record(mod$exe_file())$record
  expect_mapequal(
    record$reported_features,
    list(stan_threads = TRUE, stan_opencl = FALSE, stan_version = "2.39.0")
  )

  # An executable that cannot report leaves every feature unknown.
  mod <- mock_compile(
    stan_file,
    info_ret = list(status = 1)
  )

  result <- read_build_record(mod$exe_file())
  expect_equal(result$status, "available")
  expect_equal(
    result$record$reported_features,
    structure(list(), names = character())
  )
})

test_that("tbb_dir is the directory this build resolved", {
  stan_file <- local_bernoulli()
  mod <- mock_compile(stan_file)
  record <- read_build_record(mod$exe_file())$record
  expect_true(same_path(
    record$tbb_dir,
    file.path(cmdstan_path(), "stan/lib/stan_math/lib/tbb")
  ))

  tbb <- withr::local_tempdir()
  mod <- mock_compile(
    stan_file,
    cpp_options = list(tbb_lib = tbb, tbb_inc = tbb)
  )
  record <- read_build_record(mod$exe_file())$record
  expect_equal(record$tbb_dir, tbb)

  # Make hands a relative TBB_LIB back unchanged, so the record resolves it.
  mod <- mock_compile(stan_file, cpp_options = list(tbb_lib = "relative-tbb"))
  record <- read_build_record(mod$exe_file())$record
  expect_equal(record$tbb_dir, file.path(cmdstan_path(), "relative-tbb"))
})

test_that("a build with an untracked dependency records it", {
  local_cmdstan_make_local(cpp_options = list("-include other.mk"))
  make_local <- file.path(cmdstan_path(), "make", "local")
  expect_true("-include other.mk" %in% readLines(make_local, warn = FALSE))
  stan_file <- local_bernoulli()
  mod <- mock_compile(stan_file)

  record <- read_build_record(mod$exe_file())$record
  expect_equal(
    record$known_untracked_dependencies,
    list(list(kind = "make_local_include", detected_in = make_local))
  )
})

test_that("the note fires on a write and not otherwise", {
  local_cmdstan_make_local(cpp_options = list("-include other.mk"))
  stan_file <- local_bernoulli()

  expect_message(mock_compile(stan_file), "does not track")
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = default_info_ret,
    code = expect_no_message(
      expect_no_mock_compile(cmdstan_model(stan_file)),
      message = "does not track"
    )
  )

  # Nothing is written on a dry run or a failed build, so nothing is said.
  expect_no_message(
    cmdstan_model(stan_file, dry_run = TRUE, force_recompile = TRUE),
    message = "does not track"
  )
  with_mocked_cli(
    compile_ret = list(status = 1),
    info_ret = default_info_ret,
    code = expect_no_message(
      expect_error(cmdstan_model(stan_file, force_recompile = TRUE)),
      message = "does not track"
    )
  )
})
