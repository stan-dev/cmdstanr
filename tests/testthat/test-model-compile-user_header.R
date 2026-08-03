local_mocked_stanc <- function(.local_envir = parent.frame()) {
  local_mocked_bindings(
    get_cmdstan_flags = function(flag_name) character(),
    get_standalone_hpp = function(stan_file, stancflags) "",
    .env = .local_envir
  )
}

# A mocked compile installs an executable, so anything that compiles for real
# (even with a mocked compiler) works on a temporary copy rather than writing
# into the package's test resources.
local_external_model <- function(.local_envir = parent.frame()) {
  stan_file <- file.path(
    withr::local_tempdir(.local_envir = .local_envir),
    "bernoulli_external.stan"
  )
  file.copy(testing_stan_file("bernoulli_external"), stan_file)
  stan_file
}

user_header_routes <- function(header) {
  list(
    list(user_header = header),
    list(cpp_options = list(USER_HEADER = header)),
    list(cpp_options = list(user_header = header))
  )
}

# This test is deliberately placed above the file-level skip_if(os_is_macos())
# below: it mocks the stanc call and never compiles, so it needs no toolchain
# and should run on every platform.
test_that("cpp_options user headers allow undefined functions", {
  stan_file <- testing_stan_file("bernoulli_external")
  user_header <- withr::local_tempfile(lines = "", fileext = ".hpp")
  received_stancflags <- list()
  local_mocked_bindings(
    get_cmdstan_flags = function(flag_name) character(),
    get_standalone_hpp = function(stan_file, stancflags) {
      received_stancflags <<- append(received_stancflags, list(stancflags))
      ""
    }
  )

  for (option_name in c("USER_HEADER", "user_header")) {
    model <- cmdstan_model(stan_file, compile = FALSE)
    model$compile(
      cpp_options = setNames(list(user_header), option_name),
      force_recompile = TRUE,
      dry_run = TRUE
    )
  }

  expect_length(received_stancflags, 4)
  expect_equal(
    vapply(
      received_stancflags,
      function(x) "--allow-undefined" %in% x,
      logical(1)
    ),
    rep(TRUE, 4)
  )
})

# Also above the file-level skip_if() below: the compiler is mocked, so these
# need no toolchain either.
test_that("compile() reuses the user header from the previous compilation", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli_external.stan")
  file.copy(testing_stan_file("bernoulli_external"), stan_file)
  user_header <- withr::local_tempfile(lines = "", fileext = ".hpp")
  received_stancflags <- list()
  local_mocked_bindings(
    get_cmdstan_flags = function(flag_name) character(),
    get_standalone_hpp = function(stan_file, stancflags) {
      received_stancflags <<- append(received_stancflags, list(stancflags))
      ""
    }
  )
  model <- cmdstan_model(stan_file, compile = FALSE)
  expect_false(model$.__enclos_env__$private$using_user_header_)

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0),
    code = model$compile(user_header = user_header, force_recompile = TRUE)
  )
  expect_true(model$.__enclos_env__$private$using_user_header_)

  received_stancflags <- list()
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0),
    code = model$compile(force_recompile = TRUE)
  )
  expect_true(model$.__enclos_env__$private$using_user_header_)
  expect_equal(
    model$cpp_options()[["USER_HEADER"]],
    wsl_safe_path(absolute_path(user_header))
  )
  expect_equal(
    vapply(received_stancflags, function(x) "--allow-undefined" %in% x, logical(1)),
    rep(TRUE, 2)
  )
})

test_that("a no-op compile preserves a header supplied via cpp_options", {
  stan_file <- file.path(withr::local_tempdir(), "bernoulli_external.stan")
  file.copy(testing_stan_file("bernoulli_external"), stan_file)
  user_header <- withr::local_tempfile(lines = "", fileext = ".hpp")
  local_mocked_bindings(
    get_cmdstan_flags = function(flag_name) character(),
    get_standalone_hpp = function(stan_file, stancflags) ""
  )
  model <- cmdstan_model(stan_file, compile = FALSE)

  # The lowercase spelling is the telling one: a bare recompile re-derives the
  # header under the USER_HEADER spelling, so only this one shows whether the
  # no-op path rebuilt the recorded options or left them alone.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = model$compile(
      cpp_options = list(user_header = user_header),
      force_recompile = TRUE
    )
  )
  expect_equal(
    model$cpp_options()[["user_header"]],
    wsl_safe_path(absolute_path(user_header))
  )

  # The executable is up to date, so this call compiles nothing and must leave
  # the options describing it alone.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_no_mock_compile(model$compile())
  )
  expect_equal(
    model$cpp_options()[["user_header"]],
    wsl_safe_path(absolute_path(user_header))
  )
})

test_that("compile() uses a user header supplied to cmdstan_model()", {
  user_header <- withr::local_tempfile(lines = "", fileext = ".hpp")
  received_stancflags <- list()
  local_mocked_bindings(
    get_cmdstan_flags = function(flag_name) character(),
    get_standalone_hpp = function(stan_file, stancflags) {
      received_stancflags <<- append(received_stancflags, list(stancflags))
      ""
    }
  )

  model <- cmdstan_model(
    local_external_model(),
    user_header = user_header,
    compile = FALSE
  )
  # A mocked compile rather than a dry run: a dry run builds nothing, so it
  # records nothing about a compiled artifact.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = model$compile(force_recompile = TRUE)
  )

  expect_equal(
    model$cpp_options()[["USER_HEADER"]],
    wsl_safe_path(absolute_path(user_header))
  )
  expect_equal(
    vapply(received_stancflags, function(x) "--allow-undefined" %in% x, logical(1)),
    rep(TRUE, 2)
  )
})

test_that("a header configured over a current executable does not rebuild", {
  stan_file <- local_external_model()
  header <- withr::local_tempfile(lines = "", fileext = ".hpp")
  local_mocked_stanc()

  # An executable that already exists and is newer than both the program and
  # the header, built through a different object.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = cmdstan_model(stan_file, force_recompile = TRUE)
  )
  exe <- cmdstan_ext(strip_ext(stan_file))
  Sys.setFileTime(stan_file, Sys.time() - 60)
  Sys.setFileTime(header, Sys.time() - 60)
  Sys.setFileTime(exe, Sys.time())

  # Nothing records which header an executable was built with -- the binary
  # cannot report it and nothing is written alongside it -- so a fresh object
  # cannot tell a header it was configured with from the one already compiled
  # in. Rebuilding on the possibility would recompile in every new R session,
  # so the up-to-date executable is kept and $cpp_options() does not claim a
  # header it cannot vouch for. Documented under `force_recompile`.
  for (route in user_header_routes(header)) {
    model <- do.call(
      cmdstan_model,
      c(list(stan_file, compile = FALSE), route)
    )
    with_mocked_cli(
      compile_ret = list(status = 0),
      info_ret = list(status = 1),
      code = expect_no_mock_compile(model$compile())
    )
    expect_null(model$cpp_options()[["USER_HEADER"]])
    expect_null(model$cpp_options()[["user_header"]])
    # Source configuration is a separate axis and still reflects the request:
    # it is what makes stanc accept the undefined functions the header defines.
    expect_true(model$.__enclos_env__$private$using_user_header_)
  }
})

test_that("cmdstan_model() records a user header from every supply route", {
  header <- withr::local_tempfile(lines = "", fileext = ".hpp")

  for (route in user_header_routes(header)) {
    model <- do.call(
      cmdstan_model,
      c(list(testing_stan_file("bernoulli_external"), compile = FALSE), route)
    )
    private <- model$.__enclos_env__$private
    expect_equal(private$user_header_, resolve_path(header))
    expect_true(private$using_user_header_)
    expect_false(private$user_header_dirty_)
  }
})

test_that("cmdstan_model() honours an explicit user_header = NULL", {
  header <- withr::local_tempfile(lines = "", fileext = ".hpp")

  expect_warning(
    model <- cmdstan_model(
      testing_stan_file("bernoulli_external"),
      compile = FALSE,
      user_header = NULL,
      cpp_options = list(USER_HEADER = header)
    ),
    "User header specified both"
  )

  private <- model$.__enclos_env__$private
  expect_null(private$user_header_)
  expect_false(private$using_user_header_)
  expect_null(private$precompile_cpp_options_[["USER_HEADER"]])
  expect_null(private$precompile_cpp_options_[["user_header"]])
})

test_that("cmdstan_model() rejects an empty user header", {
  expect_error(
    cmdstan_model(
      testing_stan_file("bernoulli_external"),
      compile = FALSE,
      user_header = character(0)
    ),
    "user_header"
  )
  model <- cmdstan_model(testing_stan_file("bernoulli_external"), compile = FALSE)
  expect_error(model$compile(user_header = character(0)), "user_header")
})

test_that("a relative cpp_options user header survives a directory change", {
  model_dir <- withr::local_tempdir()
  file.copy(testing_stan_file("bernoulli_external"), model_dir)
  writeLines("", file.path(model_dir, "header.hpp"))
  local_mocked_stanc()

  model <- withr::with_dir(
    model_dir,
    cmdstan_model(
      "bernoulli_external.stan",
      compile = FALSE,
      cpp_options = list(USER_HEADER = "header.hpp")
    )
  )

  expect_equal(
    normalizePath(model$.__enclos_env__$private$user_header_),
    normalizePath(file.path(model_dir, "header.hpp"))
  )
  # The compile happens from the test's own working directory.
  expect_no_error(model$compile(force_recompile = TRUE, dry_run = TRUE))
})

test_that("a bare retry after a failed compile keeps the newly supplied header", {
  stan_file <- local_external_model()
  h1 <- withr::local_tempfile(lines = "", fileext = ".hpp")
  h2 <- withr::local_tempfile(lines = "", fileext = ".hpp")
  local_mocked_stanc()
  model <- cmdstan_model(stan_file, compile = FALSE)
  private <- model$.__enclos_env__$private

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = model$compile(user_header = h1, force_recompile = TRUE)
  )
  expect_equal(private$user_header_, resolve_path(h1))
  expect_false(private$user_header_dirty_)

  # The usual route to this is a bug in h2 itself, so the header the user just
  # supplied has to survive the failure.
  with_mocked_cli(
    compile_ret = list(status = 1),
    info_ret = list(status = 1),
    code = expect_error(model$compile(user_header = h2), "An error occurred")
  )
  expect_equal(private$user_header_, resolve_path(h2))
  expect_true(private$user_header_dirty_)

  # A bare retry must build h2 rather than reverting to h1 or no-op'ing: the
  # reuse branch resolves back to h2, so nothing here looks like a change.
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_mock_compile(model$compile())
  )
  expect_equal(private$user_header_, resolve_path(h2))
  expect_false(private$user_header_dirty_)
  expect_equal(
    model$cpp_options()[["USER_HEADER"]],
    wsl_safe_path(resolve_path(h2))
  )
})

test_that("changing the user header forces compilation", {
  stan_file <- local_external_model()
  h1 <- withr::local_tempfile(lines = "", fileext = ".hpp")
  h2 <- withr::local_tempfile(lines = "", fileext = ".hpp")
  local_mocked_stanc()
  model <- cmdstan_model(stan_file, compile = FALSE)

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = model$compile(user_header = h1, force_recompile = TRUE)
  )
  # Older than the executable, so only the change of header identity can force
  # a rebuild here (#813 only covers a header that was modified in place).
  Sys.setFileTime(h2, file.mtime(model$exe_file()) - 60)

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_mock_compile(model$compile(user_header = h2))
  )
  expect_equal(
    model$cpp_options()[["USER_HEADER"]],
    wsl_safe_path(resolve_path(h2))
  )
})

test_that("user_header = NULL clears a header from every supply route", {
  header <- withr::local_tempfile(lines = "", fileext = ".hpp")
  local_mocked_stanc()

  for (route in user_header_routes(header)) {
    model <- cmdstan_model(local_external_model(), compile = FALSE)
    private <- model$.__enclos_env__$private
    with_mocked_cli(
      compile_ret = list(status = 0),
      info_ret = list(status = 1),
      code = do.call(model$compile, c(route, list(force_recompile = TRUE)))
    )
    expect_true(private$using_user_header_)

    # The executable is up to date but was built against a header the model no
    # longer uses, so clearing has to force a rebuild rather than no-op.
    with_mocked_cli(
      compile_ret = list(status = 0),
      info_ret = list(status = 1),
      code = expect_mock_compile(model$compile(user_header = NULL))
    )
    expect_null(private$user_header_)
    expect_false(private$using_user_header_)
    expect_null(model$cpp_options()[["USER_HEADER"]])
    expect_null(model$cpp_options()[["user_header"]])
  }
})

test_that("duplicate headers of one spelling take the last, as make does", {
  first <- withr::local_tempfile(lines = "", fileext = ".hpp")
  second <- withr::local_tempfile(lines = "", fileext = ".hpp")

  # Every duplicate reaches make and a makefile takes the last, which is what
  # the cpp_options parser implements. Reading with [["USER_HEADER"]] took the
  # first instead, so the model compiled against a header make would not have
  # used -- and removing by name dropped only one occurrence, leaving the other
  # to reach make alongside the header selected here.
  for (spelling in c("USER_HEADER", "user_header")) {
    duplicated <- structure(
      list(first, second),
      names = c(spelling, spelling)
    )
    resolved <- resolve_user_header(NULL, FALSE, duplicated)
    expect_equal(resolved$user_header, second)
    expect_length(resolved$cpp_options, 0)
  }

  # Across spellings, the last of each is what make would have seen, and
  # precedence still picks USER_HEADER. Neither survives the strip.
  mixed <- structure(
    list(first, second, first),
    names = c("user_header", "USER_HEADER", "user_header")
  )
  resolved <- resolve_user_header(NULL, FALSE, mixed)
  expect_equal(resolved$user_header, second)
  expect_length(resolved$cpp_options, 0)
})

skip_if(os_is_macos())

w_path <- function(f) {
  x <- sapply(f, function(fi) wsl_safe_path(absolute_path(fi)))
  names(x) <- NULL
  x
}

local_cmdstan_make_local(cpp_options = list("PRECOMPILED_HEADERS" = "false"))

hpp <- "
#include <stan/math.hpp>
#include <boost/math/tools/promotion.hpp>
#include <ostream>

namespace bernoulli_external_model_namespace
{
    template <typename T0__,
          stan::require_all_t<stan::is_stan_scalar<T0__>>* = nullptr>
    inline typename boost::math::tools::promote_args<T0__>::type make_odds(
      const T0__ & theta,
      std::ostream *pstream__
    )
    {
        return theta / (1 - theta);
    }
}"

test_that("cmdstan_model works with user_header with mock", {
  file_that_exists <- withr::local_tempfile(pattern = "placeholder_exists")
  file_that_doesnt_exist <- withr::local_tempfile(pattern = "placeholder_doesnt_exist")
  tmpfile <- withr::local_tempfile(lines = hpp, fileext = ".hpp")
  file.create(file_that_exists)
  header_mtime <- Sys.time()
  # On GHA Windows/R 4.1 files created close together sometimes compared equal
  # and skipped the mocked recompile, so set the header mtime to be in the past
  # and ensure the exe mtime is newer
  Sys.setFileTime(file_that_exists, header_mtime - 10)
  Sys.setFileTime(tmpfile, header_mtime)

  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 0, stdout = "stan_version_major=2\nstan_version_minor=35\nstan_version_patch=0"),
    code = expect_mock_compile(
      mod <- cmdstan_model(
        stan_file = testing_stan_file("bernoulli_external"),
        exe_file = file_that_exists,
        user_header = tmpfile
      )
    )
  )

  with_mocked_cli(
    compile_ret = list(status = 0),
    # The mocked compile installs an executable, so the constructor queries it
    # for compilation info; report a failure rather than an empty list, which
    # model_compile_info() cannot interpret.
    info_ret = list(status = 1),
    code = expect_mock_compile({
      mod_2 <- cmdstan_model(
        stan_file = testing_stan_file("bernoulli_external"),
        exe_file = file_that_doesnt_exist,
        cpp_options = list(USER_HEADER = tmpfile),
        stanc_options = list("allow-undefined")
      )
    })
  )

  # Check recompilation upon changing header
  exe_mtime <- header_mtime + 10
  # The mocked compile above installed the executable with a fresh mtime; pin it
  # so the up-to-date check below compares against a known value.
  Sys.setFileTime(file_that_exists, exe_mtime)
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(),
    code = expect_no_mock_compile({
      mod$compile(quiet = TRUE, user_header = tmpfile)
    })
  )

  header_mtime <- exe_mtime + 10
  Sys.setFileTime(tmpfile, header_mtime) # touch file to trigger recompile
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(),
    code = expect_mock_compile({
      mod$compile(quiet = TRUE, user_header = tmpfile)
    })
  )

  Sys.setFileTime(mod$exe_file(), header_mtime + 10) # make exe newer than header

  # Alternative spec of user header
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(),
    code = expect_no_mock_compile({
      mod$compile(
        quiet = TRUE,
        cpp_options = list(user_header = tmpfile),
        dry_run = TRUE
      )
    })
  )

  # Error/warning messages
  with_mocked_cli(
    compile_ret = list(status = 1),
    info_ret = list(),
    code = expect_error(
      cmdstan_model(
        stan_file = testing_stan_file("bernoulli_external"),
        cpp_options = list(USER_HEADER = "non_existent.hpp"),
        stanc_options = list("allow-undefined")
      ),
      "header file '[^']*' does not exist"
    )
  )

  with_mocked_cli(
    compile_ret = list(status = 1),
    info_ret = list(),
    code = expect_warning(
      cmdstan_model(
        stan_file = testing_stan_file("bernoulli_external"),
        cpp_options = list(USER_HEADER = tmpfile, user_header = tmpfile),
        dry_run = TRUE
      ),
      "User header specified both"
    )
  )
  with_mocked_cli(
    compile_ret = list(status = 1),
    info_ret = list(),
    code = expect_warning(
      cmdstan_model(
        stan_file = testing_stan_file("bernoulli_external"),
        user_header = tmpfile,
        cpp_options = list(USER_HEADER = tmpfile),
        dry_run = TRUE
      ),
      "User header specified both"
    )
  )
})

test_that("wsl path conversion is done as expected", {
  tmp_file <- withr::local_tempfile(lines = hpp, fileext = ".hpp")
  local_mocked_stanc()
  # Mocked successful compiles rather than dry runs: only a compilation that
  # produced an executable records the options describing it.

 # Case 1: arg
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = {
      mod <- cmdstan_model(
        stan_file = local_external_model(),
        user_header = tmp_file
      )
    }
  )

  # USER_HEADER is converted
  # user_header is NULL
  expect_equal(mod$cpp_options()[['USER_HEADER']],  w_path(tmp_file))
  expect_true(is.null(mod$cpp_options()[['user_header']]))

  # Case 2: cpp opt USER_HEADER
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = {
      mod <- cmdstan_model(
        stan_file = local_external_model(),
        cpp_options = list(
          USER_HEADER = tmp_file
        )
      )
    }
  )

  # USER_HEADER is converted
  # user_header is unconverted
  expect_equal(mod$cpp_options()[['USER_HEADER']],  w_path(tmp_file))
  expect_true(is.null(mod$cpp_options()[['user_header']]))

  # Case # 3: only user_header opt
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = {
      mod <- cmdstan_model(
        stan_file = local_external_model(),
        cpp_options = list(
          user_header = tmp_file
        )
      )
    }
  )


  # In  other cases, in the *output* USER_HEADER is windows style user_header is not.
  # In this case, USER_HEADER is null.
  expect_true(is.null(mod$cpp_options()[['USER_HEADER']]))
  expect_equal(mod$cpp_options()[['user_header']],  w_path(tmp_file))
})

test_that("user_header precedence order is correct", {
  tmp_files <- sapply(1:3, function(n) withr::local_tempfile(
    lines = hpp,
    fileext = ".hpp",
    .local_envir = parent.frame(3)
  ))

  local_mocked_stanc()
  # Asserted after a mocked successful compile rather than a dry run: only a
  # compilation that produced an executable records the options describing it.
  # The ignored spelling is dropped in every case, so the next compile has a
  # single source for the header.

  # Case # 1: all 3 specified
  mod <- cmdstan_model(local_external_model(), compile = FALSE)
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_warning({
      mod$compile(
        user_header = tmp_files[1],
        cpp_options = list(
          USER_HEADER = tmp_files[2],
          user_header = tmp_files[3]
        ),
        force_recompile = TRUE
      )
    }, "User header specified both")
  )
  # In this case:
  # cpp_options[['USER_HEADER']] == tmp_files[1] <- actually used
  # tmp_files[2] and tmp_files[3] are not stored
  expect_equal(
    match(!!(mod$cpp_options()[['USER_HEADER']]), w_path(tmp_files)),
    1
  )
  expect_null(mod$cpp_options()[['user_header']])

  # Case # 2: Both opts, but no arg
  mod <- cmdstan_model(local_external_model(), compile = FALSE)
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_warning({
      mod$compile(
        cpp_options = list(
          USER_HEADER = tmp_files[2],
          user_header = tmp_files[3]
        ),
        force_recompile = TRUE
      )
    }, "User header specified both")
  )
  # In this case:
  # cpp_options[['USER_HEADER']] == tmp_files[2] <- actually used
  # tmp_files[3] is not stored
  expect_equal(
    match(!!(mod$cpp_options()[['USER_HEADER']]), w_path(tmp_files)),
    2
  )
  expect_null(mod$cpp_options()[['user_header']])

  # Case # 3: Both opts, other order
  mod <- cmdstan_model(local_external_model(), compile = FALSE)
  with_mocked_cli(
    compile_ret = list(status = 0),
    info_ret = list(status = 1),
    code = expect_warning({
      mod$compile(
        cpp_options = list(
          user_header = tmp_files[3],
          USER_HEADER = tmp_files[2]
        ),
        force_recompile = TRUE
      )
    }, "User header specified both")
  )
  # Same as Case #2: USER_HEADER wins whichever order the two appear in
  expect_equal(
    match(!!(mod$cpp_options()[['USER_HEADER']]), w_path(tmp_files)),
    2
  )
  expect_null(mod$cpp_options()[['user_header']])
})
