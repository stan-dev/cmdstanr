# CmdStan internal chain threading: implementation writeup

## Document status

- **Date:** 2026-07-23
- **Branch:** `feature/cmstan-internal-parallel`
- **Feature head before merge:** `02c944af`
- **Merge target:** incoming `master` at `f65ee8db`
- **Reviewed state:** the conflict-free staged merge result; no merge commit has
  been created
- **Design source:** [`active/cmdstan-internal-chain-threading.md`](active/cmdstan-internal-chain-threading.md)
- **Scope:** sampling, fixed-parameter sampling, standalone generated
  quantities, OpenCL, WSL, and MPI

This document describes what the branch actually implements. It is not a
restatement of the original design proposal. Where the branch changes a public
contract, the behavior below is based on the current code and tests.

## Summary

The branch changes CmdStanR from assuming that every logical chain owns a
separate CmdStan process to explicitly modeling logical chains and physical
CmdStan invocations as different things.

For `sample()`, `sample_mpi()`, and `generate_quantities()`:

- If the executable reports `stan_threads = true`, all logical chains are sent
  to one CmdStan invocation through CmdStan's `num_chains` support.
- If the executable reports `stan_threads = false`, CmdStanR retains one
  invocation per logical chain.
- There is no execution-mode option. The topology is derived from executable
  metadata.
- A threaded invocation has one shared TBB pool controlled by CmdStan's total
  `num_threads`.
- Output CSVs, diagnostic CSVs, and adapted metrics remain chain-scoped.
- Return codes, raw console transcripts, profiles, and saved configurations are
  invocation-scoped.

The central architectural change is a run plan that carries the mapping between
logical chains and invocations through command composition, process execution,
file handling, failure handling, and fitted-object APIs. The plan is an
ordinary validated R list. `CmdStanRun`, not the caller or a second plan object,
is its sole long-lived owner.

## Before and after

Before this branch, the sampling and generated-quantities code relied on the
following equivalence:

```text
logical chain == CmdStan process == output index == return-code index
```

The new model is:

```text
validated run-plan list
├── logical chains
│   ├── output CSV
│   ├── diagnostic CSV
│   └── adapted metric
└── physical invocations
    ├── process and return code
    ├── raw stdout/stderr
    ├── profile CSV
    └── saved configuration
```

For four requested chains, the resulting cardinalities are:

| Executable | Logical chains | Physical invocations | Output CSVs | Return codes |
|---|---:|---:|---:|---:|
| Unthreaded | 4 | 4 | 4 | 4 |
| Threaded | 4 | 1 | 4 | 1 |

This distinction is the part that made internal chain threading possible
without fabricating per-chain process state.

## Public API and compatibility behavior

### Canonical thread control

`sample()`, `sample_mpi()`, and `generate_quantities()` now accept:

```r
threads = NULL
threads_per_chain = NULL
```

`threads` is the canonical total size of the CmdStan thread pool. Supplying
both arguments is an error.

For a threaded chain-oriented run, the effective total is:

```text
num_threads =
  threads
```

when `threads` is supplied, and otherwise:

```text
num_threads =
  min(chains, parallel_chains) * (threads_per_chain %||% 1)
```

This preserves calls that previously supplied only `threads_per_chain` or
`parallel_chains`. It also makes calls that omit both thread arguments valid:
the omitted `threads_per_chain` is treated as `1` for pool sizing.

For threaded single-invocation methods without chain topology, such as
optimization, variational inference, and Pathfinder, an omitted `threads`
value now defaults to `1` instead of producing an error.

For an unthreaded executable, either thread argument produces a warning and has
no effect; the effective thread count is one.

### Unthreaded compatibility contract

The capability split changes topology only when the executable reports
threading support. The unthreaded branch deliberately retains the previous
user-facing behavior:

- one process and return code per chain;
- `parallel_chains` limits concurrently running processes;
- no `num_chains=1` argument is added to the old single-chain command shape;
- per-chain seeds, step sizes, and arbitrary positive chain IDs are preserved;
- `fit$output(id)` returns the complete process transcript, including
  unprefixed informational lines;
- timing continues to use the existing per-process timers; and
- the positional `CmdStanRun$new(args, procs)` constructor remains valid for
  existing internal call sites and tests.

The new filtered chain-output and CSV-derived logical-chain timing paths are
used only when one physical invocation owns multiple logical chains. Focused
tests run the same public behavior with `STAN_THREADS` enabled and disabled to
lock this boundary.

### Meaning of `parallel_chains`

For an unthreaded executable, `parallel_chains` remains the maximum number of
chain processes that CmdStanR schedules concurrently.

For a threaded executable, there is only one physical invocation.
`parallel_chains` therefore contributes to the compatibility formula above but
is not a hard chain-concurrency limit. CmdStan and TBB schedule work inside the
shared pool.

### Vector arguments and chain IDs

An unthreaded run preserves existing per-chain vectors and arbitrary unique
positive chain IDs.

A threaded multi-chain invocation can pass only one seed and one step size to
CmdStan. The branch therefore:

- accepts vector `seed` values, warns, and uses the first value;
- accepts vector `step_size` values, warns, and uses the first value; and
- accepts nonconsecutive `chain_ids`, warns, and replaces them with a
  consecutive sequence beginning at the first supplied ID.

The warning messages include the effective values so the command that actually
runs is visible to the user.

## Implementation by layer

### 1. Executable capability detection

[`R/cpp_opts.R`](../R/cpp_opts.R) adds `read_exe_info()`, which runs the model
executable's `info` command and validates that the result includes both
`stan_threads` and the Stan version. Missing, malformed, or failed capability
inspection is an error rather than a silent fallback.

[`R/model.R`](../R/model.R) caches parsed executable metadata on
`CmdStanModel`. The cache is invalidated when the executable changes or the
model is recompiled. Threading and OpenCL option lookups are case-insensitive,
and the executable's reported capability takes precedence over the compile
options originally requested from R.

[`R/run-plan.R`](../R/run-plan.R) requires CmdStan 2.38 or newer before it
plans chain-oriented execution. The package-wide minimum supported CmdStan
version is correspondingly raised from 2.35 to 2.38 in
[`R/path.R`](../R/path.R), installation validation, documentation, and tests.

### 2. Input normalization and thread resolution

[`R/run-plan.R`](../R/run-plan.R) centralizes the topology-boundary rules:

- `resolve_num_threads()` validates the two thread inputs, implements the
  compatibility formula, and warns when thread controls are used with an
  unthreaded executable.
- `normalize_chain_scalar_args()` preserves unthreaded vector behavior and
  performs the documented seed, step-size, and chain-ID coercions only for
  threaded invocations.
- `cmdstan_thread_env()` constructs child-only `STAN_NUM_THREADS` and WSL
  environment values.

The parent R process is not mutated. Existing `STAN_NUM_THREADS` and `WSLENV`
values remain unchanged after success or failure.

### 3. Explicit run planning

The new [`R/run-plan.R`](../R/run-plan.R) introduces:

- `new_cmdstan_invocation_plan()` and
  `validate_cmdstan_invocation_plan()`, pure functions for one physical
  invocation;
- `new_cmdstan_run_plan()` and `validate_cmdstan_run_plan()`, pure functions
  that construct and validate the complete nested list; and
- `build_cmdstan_run_plan()`, which derives one invocation for a threaded
  executable or one invocation per chain for an unthreaded executable.

The plan has no R6 class or mutable plan object. Its top-level fields are
`method`, `chains`, `invocations`, `artifacts`, `requested`, and `stages`.
Every file-stage entry is also a plain validated list. Planning describes file
mappings but does not copy, restore, or delete files.

In abbreviated form, the value passed to `CmdStanRun` is:

```text
list(
  method,
  chains = list(ids),
  invocations = list(
    list(invocation_id, chain_indices, chain_ids, base_chain_id,
         num_chains, num_threads, command, command_args, env,
         mpi_cmd, mpi_args)
  ),
  artifacts = list(chain = ..., invocation = ...),
  requested = list(parallel_chains, threads, threads_per_chain),
  stages = list(
    list(public_paths, cmdstan_paths, direction)
  )
)
```

Validation checks that:

- every logical chain belongs to exactly one invocation;
- invocation IDs are sequential;
- multi-chain invocation IDs are consecutive;
- invocation chain IDs agree with the logical-chain registry;
- chain-scoped artifacts have one entry per logical chain; and
- invocation-scoped artifacts have one entry per physical invocation;
- request metadata contains valid positive integer values when present; and
- every file-stage mapping has matching public and CmdStan path cardinality.

The plan also records the requested `parallel_chains`, `threads`, and
`threads_per_chain` values so a live fit can report compatibility metadata
without pretending that reconstructed CSV-only fits know those R-side inputs.

`CmdStanRun` is the sole long-lived owner of the result. It stores the plan in
a private field, derives the appropriate process registry and concurrency from
the invocation list, and exposes only the existing run and fit APIs. Model
methods no longer construct a plan and a separate process registry that must be
kept synchronized.

#### Why this is a validated list rather than a class

The run plan has value semantics: it is constructed once, validated once, and
then consumed. It has no independent lifecycle, identity, mutation protocol, or
behavior that justifies an R6 object. The earlier `CmdStanRunPlan` class mostly
stored fields and duplicated ownership with `CmdStanRun`.

Two alternatives were considered:

1. Passing separate chain, invocation, artifact, and staging vectors between
   functions. This removes the class but scatters the cardinality and mapping
   invariants across call sites.
2. Keeping an R6 plan beside `CmdStanRun`. This groups the data, but creates two
   stateful objects and allows the process registry and plan to diverge.

The nested list keeps one explicit validation boundary without introducing a
second owner. Small pure constructors and validators enforce local invariants;
`CmdStanRun` owns all execution behavior and side effects.

### 4. Command composition and file-list staging

CmdStan 2.38 accepts exact comma-delimited file lists for internal chains.
`compose_invocation_args()` now creates one command per invocation and emits:

- the invocation's base `id`;
- `num_chains` only when the invocation owns more than one chain;
- total `num_threads` for threaded executables;
- one output path per logical chain;
- one diagnostic path per logical chain when requested;
- per-chain initialization and metric inputs;
- per-chain fitted-parameter inputs for standalone generated quantities; and
- one profile path per invocation.

The new [`R/file-stage.R`](../R/file-stage.R) handles the ambiguity between a
comma as a filename character and a comma as CmdStan's list separator:

- `new_cmdstan_file_stage()` purely plans public-to-CmdStan path mappings;
- only paths containing commas receive distinct staged paths;
- after taking ownership, `CmdStanRun` copies input files to comma-free
  temporary paths immediately before execution;
- `CmdStanRun` restores staged output files and cleans temporary inputs;
- restoration and cleanup are safe to call repeatedly; and
- every path is converted for WSL individually before the list is joined.

Public filenames therefore remain unchanged even though CmdStan receives a
comma-free exact list.

### 5. Shared invocation scheduler

[`R/run.R`](../R/run.R) replaces the separate chain-oriented scheduling loops
with one invocation scheduler used by sampling and standalone generated
quantities.

`CmdStanRun` privately owns the validated run-plan list and the process registry
derived from it. For each invocation, the scheduler reads the command,
arguments, environment, chain ownership, and optional MPI launcher information
from that private plan. It then:

- starts no more than the planned process concurrency;
- captures raw stdout and stderr per physical invocation;
- polls and finalizes processes through the existing processx lifecycle;
- kills the complete process tree during cleanup, including MPI launchers and
  ranks; and
- restores staged outputs before validating results.

Legacy methods that do not use a run plan continue through the existing
single-process paths, but now receive thread settings through child-scoped
environment values rather than parent-session mutation.

### 6. Chain-aware output and progress

`CmdStanProcs` now owns an explicit `invocation_chain_ids` mapping. Raw output
continues to be stored by invocation, while lines beginning with
`Chain [<id>]` are also routed to per-chain buffers.

This gives fitted objects two truthful views:

- `fit$output()` returns one raw transcript per physical invocation.
- For a multi-chain invocation, `fit$output(id)` prints only lines attributed
  to the selected logical chain.
- For an unthreaded one-chain invocation, `fit$output(id)` retains the previous
  behavior and prints the complete process transcript.

For a multi-chain invocation, unprefixed startup, metadata, launcher, and
shared error lines remain available only in raw invocation output. They are not
duplicated across chains.

The MCMC and generated-quantities output parsers strip the chain prefix before
matching progress, timing, rejection, and exception messages. This allows
interleaved internal-chain output to update the correct logical-chain progress
without changing the raw transcript.

### 7. Atomic failure and output validation

Exit status is invocation-scoped. A nonzero return code marks every logical
chain owned by that invocation as failed.

The branch also validates every expected output CSV from a successful
chain-oriented invocation. If any file is missing or its metadata cannot be
read, the invocation is changed to failed. Partial outputs are retained for
inspection through `include_failed = TRUE`, but they are excluded from normal
draw loading.

This preserves partial success for unthreaded execution, where separate chain
processes can succeed independently, while correctly making a one-invocation
threaded failure atomic.

### 8. Fit APIs and artifact cardinality

[`R/fit.R`](../R/fit.R) and [`R/run.R`](../R/run.R) now distinguish the two
cardinalities:

- `num_chains()` returns logical chains.
- `num_procs()` returns physical CmdStan invocations.
- `return_codes()` has one value per invocation.
- `output_files()`, latent-dynamics files, and metric files are chain-scoped.
- profile and configuration files are invocation-scoped.
- save/move methods use chain IDs for chain artifacts and invocation IDs for
  invocation artifacts.

For a true multi-chain invocation, sampling and generated-quantities timing
combines total wall time from the runner with logical-chain timing read from
the output CSVs. Unthreaded process-per-chain runs retain their previous timing
path. This avoids pretending that one process timer represents several
independent chain timers without changing old unthreaded results.

### 9. CSV metadata

[`R/csv.R`](../R/csv.R) preserves CmdStan's `num_threads` field as the
canonical thread metadata.

CmdStan internal-chain CSV headers repeat the invocation's base chain ID. When
the CSV metadata also reports the expected `num_chains`, `read_cmdstan_csv()`
expands those repeated IDs into the consecutive logical chain sequence and
updates timing rows accordingly.

CSV-only reconstructed fits expose what the files actually contain. They do
not invent `parallel_chains` or `threads_per_chain`. A live fit may supplement
those R-side values from its run plan:

- if the user supplied canonical `threads`, `threads_per_chain` is `NULL`;
- otherwise the requested compatibility value, defaulting to `1`, is retained.

### 10. Fixed-parameter, OpenCL, MPI, and WSL

Fixed-parameter sampling uses the same topology and planning path as ordinary
sampling.

OpenCL does not create a process-per-chain exception. A threaded OpenCL model
runs all chains in one invocation and shares the process-level OpenCL device
and context selected by `opencl_ids`.

`sample_mpi()` also uses the run plan:

- a threaded executable produces one MPI launcher invocation containing all
  logical chains;
- an unthreaded executable retains one launcher invocation per logical chain;
  and
- `threads` is passed as the TBB pool size per MPI rank, independently of the
  rank count in `mpi_args`.

The include-flag handling in [`R/utils.R`](../R/utils.R) was also changed to
leave absolute include paths absolute instead of prepending the CmdStan path.
This supports externally provided compiler, OpenCL, and MPI toolchains whose
module configuration emits absolute include directories.

For WSL, per-element path conversion happens before file-list joining, the
thread setting is forwarded through child `WSLENV`, and staged files use the
CmdStan temporary directory when available.

## Documentation changes

The branch updates:

- `NEWS.md` with topology, argument, metadata, and minimum-version changes;
- sampling and generated-quantities reference documentation;
- fit documentation for chain versus invocation cardinality;
- profiling documentation to describe aggregate invocation profiles;
- the getting-started vignette with canonical `threads` examples; and
- the OpenCL vignette with shared device/context behavior.

Generated `.Rd` files were refreshed from the roxygen sources.

## Test coverage added or changed

The new [`tests/testthat/test-run-plan.R`](../tests/testthat/test-run-plan.R)
provides focused unit coverage for:

- valid and invalid logical-chain mappings;
- a fully classless nested plan, including file-stage mappings;
- `CmdStanRun` ownership of the plan and derived process registry;
- the legacy `CmdStanRun(args, procs)` constructor;
- chain- and invocation-scoped artifact cardinality;
- canonical and compatibility thread resolution;
- threaded-only scalarization and warnings;
- exact sample and generated-quantities file lists;
- comma-path staging and idempotent restoration;
- threaded and unthreaded plan construction;
- unthreaded full-transcript and process-per-chain compatibility;
- child environment and inherited `PATH` handling;
- process-tree cleanup;
- fixed-parameter, OpenCL, and MPI topology;
- chain-prefixed output filtering and progress; and
- WSL environment composition without parent mutation.

Integration coverage was expanded across the existing suites to exercise:

- four threaded sampling chains in one invocation;
- threaded standalone generated quantities in one invocation;
- reproducibility with a scalar seed;
- vector seed and step-size coercion warnings;
- nonconsecutive chain-ID normalization;
- unthreaded compatibility behavior;
- executable metadata precedence;
- atomic threaded failures;
- logical-chain timing and file counts;
- invocation-scoped return codes and raw output;
- threaded OpenCL sampling and generated quantities; and
- threaded MPI sampling with one launcher invocation.

### Validation completed on the merged worktree

The merge-resolution and run-plan refactor passes completed the following
checks:

- all 19 R source files parsed;
- all 73 generated `.Rd` files parsed;
- the focused `cpp_opts`, `csv`, and `run-plan` tests passed;
- the public threading suite passed with `STAN_THREADS` enabled and disabled;
- the generated-quantities fit suite passed with `STAN_THREADS` enabled and
  disabled;
- model-level sampling and generated-quantities tests passed for both
  capabilities;
- the environment-gated OpenCL integration tests passed; and
- the environment-gated MPI integration test passed after supplying the
  Python runtime library required by this checkout's Boost.MPI build and
  permitting OpenMPI to create its local listener socket outside the restricted
  test sandbox.

A final full `devtools::test()` and `devtools::check()` have not been run on the
merged worktree, so this writeup does not claim full-suite or package-check
completion.

## Diff audit and non-core branch contents

At the time of this writeup, the staged feature result differs from incoming
`master` in 72 files, with approximately 5,400 inserted lines and 700 removed
lines. Most of that volume is the planner, runner refactor, tests, design
documents, and generated reference documentation.

The branch also contains changes that are not themselves part of the topology
model:

- `DESCRIPTION` records the roxygen version used to regenerate documentation.
- `R/utils.R` fixes absolute include-path handling, which is useful for the
  loaded OpenCL/MPI toolchains but is independently reviewable.
- Three compiled test executables are committed under
  `tests/testthat/resources/stan/`: `logistic_simple`, `loo_moment_match`, and
  `parameter_types`. They do not implement the feature and should be explicitly
  kept or removed before publication rather than being treated as required
  source changes.

The unstaged workflow edit and untracked benchmark result directories in the
working tree were not included in this implementation audit.

## Remaining release work

The implementation is functionally present, but release readiness still
requires:

1. running the full test suite and `devtools::check()` on the merged worktree;
2. validating Windows and WSL paths containing commas;
3. recording a fresh paired memory and wall-time benchmark against the
   process-per-chain baseline;
4. deciding whether the three compiled test executables belong in the branch;
   and
5. reviewing the generated documentation and minimum-version migration as a
   complete public API change.

The most important delivered contract is already in place: CmdStanR now keeps
logical-chain truth and physical-invocation truth separate, which allows a
threaded executable to run all chains internally without breaking chain-level
draws and files or inventing per-chain process state.
