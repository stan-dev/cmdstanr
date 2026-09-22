# print.stan_build_info() output for each record state

    Code
      print(full_x)
    Output
      Build record: available
      Reported features:
        stan_threads: TRUE
        stan_mpi: FALSE
        stan_opencl: FALSE
        stan_no_range_checks: unknown
        stan_version: 2.39.0
      Configuration:
        cpp_options: STAN_THREADS=true STAN_CPP_OPTIMS=true
        stanc_options: --O1
        stanc_options_from_make: --warn-pedantic
        include_paths: /proj/inc, /proj/shared
      Dependencies:
        stan_file: /proj/bernoulli.stan
        included_file: /proj/inc/half.stan
        included_file: /proj/shared/prior.stan (no longer exists)
        user_header: /proj/helpers.hpp
        make_local: /opt/cmdstan-2.39.0/make/local
      CmdStan 2.39.0 at /opt/cmdstan-2.39.0
      Dependencies CmdStanR does not track:
        make/local includes another makefile (/opt/cmdstan-2.39.0/make/local)
        the user header includes other headers (/proj/helpers.hpp)

---

    Code
      print(sparse_x)
    Output
      Build record: available
      Reported features:
        stan_threads: TRUE
        stan_mpi: unknown
        stan_opencl: FALSE
        stan_no_range_checks: unknown
        stan_version: 2.39.0
      Configuration:
        cpp_options: STAN_THREADS=true
        stanc_options: --O1
        stanc_options_from_make: none
        include_paths: none
      Dependencies:
        stan_file: bernoulli.stan (no longer exists)
      CmdStan 2.39.0 at /opt/cmdstan-2.39.0 (no longer exists)

---

    Code
      print(missing_x)
    Output
      Build record: not found. Only what the executable says about itself is known.
      Reported features:
        stan_threads: unknown
        stan_mpi: unknown
        stan_opencl: unknown
        stan_no_range_checks: unknown
        stan_version: unknown

---

    Code
      print(unreadable_x)
    Output
      Build record: could not be read. Rebuilding the executable writes a new one.
      Reported features:
        stan_threads: unknown
        stan_mpi: unknown
        stan_opencl: unknown
        stan_no_range_checks: unknown
        stan_version: unknown

---

    Code
      print(mismatch_x)
    Output
      Build record: does not match. Executable changed after the build.
      Reported features:
        stan_threads: unknown
        stan_mpi: unknown
        stan_opencl: unknown
        stan_no_range_checks: unknown
        stan_version: unknown

---

    Code
      print(newer_x)
    Output
      Build record: format 2 (newer CmdStanR). Upgrade CmdStanR to read it.
      Reported features:
        stan_threads: unknown
        stan_mpi: unknown
        stan_opencl: unknown
        stan_no_range_checks: unknown
        stan_version: unknown

---

    Code
      print(older_x)
    Output
      Build record: format 0 (older CmdStanR). Rebuild the executable to replace it.
      Reported features:
        stan_threads: unknown
        stan_mpi: unknown
        stan_opencl: unknown
        stan_no_range_checks: unknown
        stan_version: unknown

