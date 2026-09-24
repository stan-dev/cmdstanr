skip_on_cran()

set_cmdstan_path()

# What R/model.R's private assert_current() and its callers say about every
# public member: whether it checks that the executable is still the one the
# object was built against before it does anything else. "disk" reads the
# executable and its record as they are now, on purpose.
member_class <- c(
  sample = "checked", sample_mpi = "checked", optimize = "checked",
  laplace = "checked", variational = "checked", pathfinder = "checked",
  generate_quantities = "checked", diagnose = "checked",
  cmdstan_defaults = "checked", expose_functions = "checked",
  code = "stored", variables = "stored", print = "stored",
  hpp_file = "stored", save_hpp_file = "stored", functions = "stored",
  stan_file = "accessor", has_stan_file = "accessor", model_name = "accessor",
  exe_file = "accessor", include_paths = "accessor",
  cmdstan_version = "accessor", cpp_options = "accessor",
  user_header = "accessor",
  check_syntax = "source", format = "source",
  build_info = "disk",
  initialize = "plumbing", clone = "plumbing"
)

is_stale_error <- function(x) inherits(x, "cmdstanr_stale_executable")
not_stale <- function(expr) !is_stale_error(tryCatch(expr, error = identity))

# A model whose executable make never built, made stale the only way a model
# with a source can be: a change to the program after construction. Nothing
# else changes, so the record still reads and the only reason is the source.
local_stale_model <- function(.local_envir = parent.frame()) {
  stan_file <- write_stan_file(
    "parameters { real y; } model { y ~ std_normal(); }",
    dir = withr::local_tempdir(.local_envir = .local_envir)
  )
  mod <- mock_cmdstan_model(stan_file, .local_envir = .local_envir)
  writeLines(c(readLines(stan_file), "// a change"), stan_file)
  mod
}

test_that("every public member is classified, and only those", {
  methods <- union(names(CmdStanModel$public_methods), "clone")
  classified_methods <- setdiff(names(member_class), "functions")

  unclassified <- setdiff(methods, classified_methods)
  expect_true(
    length(unclassified) == 0,
    info = paste("unclassified:", paste(unclassified, collapse = ", "))
  )
  gone <- setdiff(classified_methods, methods)
  expect_true(
    length(gone) == 0,
    info = paste("no longer exists:", paste(gone, collapse = ", "))
  )
  expect_equal(names(CmdStanModel$public_fields), "functions")
  expect_false("compile" %in% methods)
  expect_false("compile" %in% names(member_class))
})

test_that("every checked method raises the staleness error, bare", {
  mod <- local_stale_model()
  checked <- names(member_class)[member_class == "checked"]
  for (name in checked) {
    expect_error(
      mod[[name]](), class = "cmdstanr_stale_executable", info = name
    )
  }
})

test_that("every method that is not checked works on a stale build", {
  mod <- local_stale_model()
  unchecked <- setdiff(
    names(member_class)[member_class != "checked"],
    c("initialize", "clone", "functions", "print")
  )
  for (name in unchecked) {
    expect_true(not_stale(mod[[name]]()), info = name)
  }
  capture.output(expect_true(not_stale(mod$print())))
  expect_no_error(mod$clone())
  expect_true(is.environment(mod$functions))
})

test_that("assert_current() says what is stale and where to go", {
  mod <- local_stale_model()
  expect_error(mod$sample(), "the Stan program changed", fixed = TRUE)
  expect_error(mod$sample(), "Run cmdstan_model() to rebuild it.", fixed = TRUE)
})

test_that("assert_current() is not memoised", {
  mod <- local_stale_model()
  code <- readLines(mod$stan_file())
  writeLines(code[-length(code)], mod$stan_file())
  expect_true(not_stale(mod$cmdstan_defaults()))
})

test_that("a current model passes assert_current()", {
  stan_file <- write_stan_file(
    "parameters { real y; } model { y ~ std_normal(); }",
    dir = withr::local_tempdir()
  )
  mod <- mock_cmdstan_model(stan_file)
  expect_true(not_stale(mod$cmdstan_defaults()))
})

test_that("assert_current() names an altered or missing executable", {
  stan_file <- write_stan_file(
    "parameters { real y; } model { y ~ std_normal(); }",
    dir = withr::local_tempdir()
  )
  mod <- mock_cmdstan_model(stan_file)
  writeLines("altered", mod$exe_file())
  expect_error(
    mod$sample(), "the executable does not match its build record",
    fixed = TRUE, class = "cmdstanr_stale_executable"
  )
  file.remove(mod$exe_file())
  expect_error(
    mod$sample(), paste0("there is no executable at '", mod$exe_file(), "'"),
    fixed = TRUE, class = "cmdstanr_stale_executable"
  )
})
