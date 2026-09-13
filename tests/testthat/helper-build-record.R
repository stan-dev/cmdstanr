local_fake_exe <- function(name = "bernoulli") {
  path <- file.path(
    withr::local_tempdir(.local_envir = parent.frame()),
    name
  )
  writeBin(as.raw(c(0x7f, 0x45, 0x4c, 0x46)), path)
  path
}

example_record <- function(exe_file) {
  new_build_record(
    request = list(
      cpp_options_supplied = list(STAN_THREADS = "true"),
      stanc_options_supplied = list("--O1"),
      stanc_options_injected = list("--name=bernoulli_model"),
      stanc_name = "bernoulli",
      include_paths = list(dirname(exe_file))
    ),
    reported_features = list(
      stan_threads = TRUE,
      stan_opencl = FALSE,
      stan_version = "2.39.0"
    ),
    dependencies = list(
      stan_file = list(hash = "0f1e", built_from = "bernoulli.stan"),
      included_files = list(
        list(hash = "2d3c", built_from = "helpers.stan")
      ),
      make_local = list(hash = "4b5a", built_from = "make/local")
    ),
    artifact = hash_file(exe_file),
    builder = list(path = "/opt/cmdstan-2.39.0", version = "2.39.0"),
    tbb_dir = "/opt/cmdstan-2.39.0/stan/lib/stan_math/lib/tbb",
    known_untracked_dependencies = list(
      list(kind = "make_local_include", detected_in = "make/local")
    )
  )
}
