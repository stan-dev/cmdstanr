# CmdStan internal chain threading in CmdStanR

## 1. Identity and lifecycle

- **Status:** Draft for maintainer review
- **Target release:** CmdStanR 1.0
- **Minimum CmdStan version:** 2.38
- **Tracking issue:** [stan-dev/cmdstanr#534](https://github.com/stan-dev/cmdstanr/issues/534)
- **Last updated:** 2026-07-16
- **Owners:** CmdStanR maintainers
- **Scope:** Sampling, fixed-parameter sampling, standalone generated quantities, OpenCL, WSL, and MPI execution

### Decision summary

The execution topology is derived from the executable and is not configurable:

- If the executable reports `stan_threads=true`, CmdStanR runs all logical chains in one CmdStan invocation using CmdStan's `num_chains` and `num_threads` arguments.
- Otherwise, CmdStanR retains the current one-CmdStan-process-per-chain execution and schedules those processes with `parallel_chains`.
- This rule also applies to fixed-parameter sampling, standalone generated quantities, OpenCL-enabled executables, and MPI-enabled executables.
- There is no user-facing or internal execution-mode flag. The run plan records logical chains and physical invocations; topology is derived from those values.

The public threading truth is CmdStan's total `num_threads`. `threads_per_chain` remains available as a heuristic compatibility input, not as a guaranteed per-chain allocation.

## 2. Context

CmdStanR currently launches one CmdStan process for every sampling chain and every generated-quantities input chain. The R process uses processx to keep at most `parallel_chains` child processes active. This architecture equates:

```text
logical chain == CmdStan process == process state == artifact index
```

That equivalence appears across the current implementation:

- `CmdStanModel$sample()` and `$generate_quantities()` create `num_procs` equal to the logical chain count.
- `CmdStanArgs` sizes seeds, initial values, validation, and filenames using `length(proc_ids)`.
- `CmdStanRun` composes one command per process and preallocates all artifacts per process.
- `CmdStanMCMCProcs` maintains one state machine, console buffer, timing record, and return code per chain process.
- `CmdStanMCMC$num_chains()` and `CmdStanGQ$num_chains()` derive their result from process count.
- Profiles and saved configuration files currently have one file per chain because they have one file per invocation.

CmdStan 2.38 can instead run multiple chains from one executable invocation. A threaded invocation loads one model and dataset, creates a shared TBB pool, and schedules both chains and within-chain work through that pool. It continues to produce one sample CSV, diagnostic CSV, and adapted metric file per chain, but produces only one process transcript, return code, profile, and saved configuration.

Pathfinder already provides a partial precedent inside CmdStanR: one CmdStan process receives `num_paths`, uses one thread pool, and internally manages several paths. Its implementation is method-specific and its output shape differs from MCMC, so this design generalizes the concept rather than reusing the Pathfinder special cases directly.

## 3. Problem

The change cannot be implemented safely by adding `num_chains` to the current command line. Doing so would break assumptions about:

- process and chain cardinality;
- per-chain versus per-invocation files;
- seed, step-size, and chain-ID vector handling;
- return codes and partial failures;
- console-output attribution and progress state;
- total versus per-chain thread metadata;
- output filtering and fit APIs;
- WSL path conversion and comma-delimited filename lists;
- OpenCL and MPI launcher ownership.

Without a first-class logical-chain-to-invocation mapping, fixes in one layer will cause incorrect indexing or fabricated per-chain state in another layer.

## 4. Goals and non-goals

### Goals

1. Use CmdStan's internal multi-chain execution automatically whenever the executable was compiled with `STAN_THREADS=true`.
2. Reduce peak memory by sharing the CmdStan model and data across chains.
3. Preserve one output CSV, diagnostic CSV, and adapted metric JSON per logical chain.
4. Represent logical chains and physical CmdStan invocations independently throughout argument composition, execution, file management, and fit APIs.
5. Make CmdStan's `num_threads` the canonical runtime and metadata value.
6. Retain `threads_per_chain` as a documented resource-sizing heuristic and compatibility shim.
7. Preserve current per-chain vector semantics when the executable is not threaded.
8. Define truthful invocation-scoped behavior for return codes, profiles, configurations, and raw console output.
9. Support fixed-parameter sampling, GQ, OpenCL, WSL, and MPI in the same architecture.
10. Preserve current public filenames, including targeted staging for paths containing commas.

### Non-goals

1. Providing a switch to force threaded or process-per-chain topology.
2. Guaranteeing a fixed or minimum number of TBB worker threads to any individual chain.
3. Implementing a second scheduler inside R for work already scheduled by CmdStan/TBB.
4. Recovering independent per-chain process exit codes from a single CmdStan invocation.
5. Salvaging individual chains from a failed internal invocation.
6. Supporting CmdStan versions older than 2.38.
7. Changing CmdStan's TBB, OpenCL, or MPI implementations.
8. Making `STAN_THREADS=true` the default model compilation option as part of this refactor.

## 5. Requirements

### Functional requirements

- **FR1: Deterministic topology.** CmdStanR must inspect executable compilation metadata once when planning the run. `stan_threads=true` produces one invocation containing all logical chains; false produces one invocation per chain.
- **FR2: No fallback flag.** Users and internal callers must not select an execution mode. A capability-detection failure must be reported rather than silently choosing a topology.
- **FR3: Explicit chain mapping.** Every physical invocation must declare the logical chain indices and chain IDs it owns.
- **FR4: Internal CmdStan arguments.** A threaded multi-chain invocation must pass `num_chains`, the first logical chain ID as `id`, and total `num_threads` explicitly.
- **FR5: Exact per-chain files.** CmdStanR must pass exact comma-delimited filename lists for per-chain outputs, diagnostics, initial values, metrics, and GQ fitted-parameter inputs.
- **FR6: Comma-path support.** If any exact path contains a comma, CmdStanR must stage that input or output under a comma-free temporary path and restore the requested public path after execution.
- **FR7: Scalarization warnings.** For a threaded invocation, vector `seed` and `step_size` values must remain accepted, but CmdStanR must use only the first value and issue a warning explaining that the value applies to all chains.
- **FR8: Consecutive chain IDs.** A threaded invocation must use consecutive IDs. Consecutive user input is accepted unchanged. For nonconsecutive input, the first supplied ID becomes the base and CmdStanR warns that the remaining IDs were ignored and replaced with a consecutive sequence.
- **FR9: Legacy vector behavior.** For an unthreaded executable, vector seeds, step sizes, and arbitrary unique chain IDs must retain their current per-process behavior.
- **FR10: Atomic failure.** A nonzero invocation return code marks every logical chain owned by that invocation failed. Partial CSVs must not be loaded as successful draws.
- **FR11: Truthful artifacts.** Sample, diagnostic, and metric files are chain-scoped. Console output, return codes, profiles, and configuration files are invocation-scoped.
- **FR12: Chain output filtering.** `$output()` returns raw output for all invocations. `$output(id)` returns only lines attributed to the requested chain by CmdStan's `Chain [id]` prefix. Unprefixed lines remain available only in the full invocation output.
- **FR13: Child-scoped threads.** CmdStanR must pass `STAN_NUM_THREADS` through the child environment and must not mutate the parent R session.
- **FR14: Unified runner.** Sampling and standalone GQ must use the same invocation scheduler and lifecycle machinery.
- **FR15: MPI launcher.** A threaded `sample_mpi()` run must launch one MPI command containing one CmdStan multi-chain invocation. An unthreaded MPI executable retains one launcher invocation per logical chain.
- **FR16: Fit cardinality.** `num_chains()` must return logical chain count; `num_procs()` and `return_codes()` must report physical invocation count.
- **FR17: CSV reconstruction.** `read_cmdstan_csv()` and `as_cmdstan_fit()` must remain independent of the original execution topology and treat CSV `num_threads` as the authoritative available thread metadata.

### Non-functional requirements

- **NFR1: Memory.** A threaded multi-chain run should hold one model and one copy of input data per invocation. Peak resident memory must be measurably lower than the process-per-chain baseline for a data-heavy model.
- **NFR2: Performance.** The refactor must not introduce material R-side overhead beyond path planning and finalization. Wall-time comparisons must separate process launch, CmdStan execution, and CSV parsing.
- **NFR3: Reproducibility.** Scalar seed plus consecutive chain IDs must remain reproducible across repeated internal runs with identical inputs. Vector scalarization must always warn.
- **NFR4: Cross-platform behavior.** Linux, macOS, Windows, WSL, OpenCL, and MPI path and process behavior must be covered proportionally to risk.
- **NFR5: Debuggability.** Verbose output must show the exact executable command, logical chain count, invocation count, and total CmdStan threads.
- **NFR6: No fabricated state.** CmdStanR must not duplicate return codes, profiles, configs, or unprefixed console lines to simulate per-chain artifacts.

## 6. Constraints and invariants

1. CmdStan 2.38 or newer is required.
2. Topology is derived solely from executable compilation metadata and the requested logical chains.
3. The run plan contains no configurable `execution_mode` or `internal_chains` flag. Code may derive whether an invocation contains several chains from `invocation$num_chains > 1`.
4. A threaded sampling or GQ plan contains one physical invocation, including when OpenCL or MPI is enabled.
5. An unthreaded plan contains one physical invocation per logical chain.
6. A logical chain belongs to exactly one invocation.
7. A successful invocation has return code zero and all required chain-scoped output files pass existing CSV validation.
8. A failed invocation has no successful logical chains, regardless of files left on disk.
9. `threads` and `threads_per_chain` cannot both be supplied.
10. `threads` maps directly to CmdStan `num_threads`.
11. When `threads` is absent, total threads are calculated as:

    ```text
    num_threads = min(chains, parallel_chains) * (threads_per_chain %||% 1)
    ```

    For single-invocation methods without chain topology, an absent `threads`
    value defaults to 1.

12. `threads_per_chain` is a heuristic used to size a shared pool. TBB may assign one, many, or temporarily no worker threads to a particular chain.
13. For an unthreaded executable, `parallel_chains` remains the actual process concurrency limit. For a threaded executable, it contributes to pool sizing but is not a hard chain-concurrency limit.
14. Under MPI, CmdStan's `num_threads` applies to each MPI rank that initializes a TBB pool. The total operating-system thread count therefore also depends on MPI rank count and rank placement.
15. Exact filename lists must be constructed only after applying WSL path conversion to each individual path.
16. No core execution or parsing behavior may be mocked in integration tests. Process creation may be mocked only in pure planner/command unit tests.
17. No implementation task may change an existing test merely to accommodate an accidental behavior change; user-visible contract changes must be tied to this design and documented.

## 7. Proposed design

### 7.1 Architecture

Introduce a run-planning layer between public method validation and process construction:

```text
CmdStanModel method
  -> normalize method inputs
  -> inspect executable capabilities
  -> build validated run-plan list
       -> logical chain registry
       -> invocation plans
       -> chain-scoped artifacts
       -> invocation-scoped artifacts
  -> compose one command per invocation
  -> execute invocation scheduler
  -> finalize staged files
  -> construct fit using the run plan
```

The cardinalities become explicit:

```text
validated run-plan list
├── logical chains (N)
│   ├── output CSV[N]
│   ├── diagnostic CSV[N]
│   └── metric JSON[N]
└── physical invocations (M)
    ├── command[M]
    ├── process/stdout[M]
    ├── return code[M]
    ├── profile CSV[M]
    └── config JSON[M]
```

For a threaded four-chain sample, `N=4` and `M=1`. For an unthreaded four-chain sample, `N=4` and `M=4`.

### 7.2 Run-plan contracts

Use pure constructor and validator functions that return a nested list:

```r
new_cmdstan_run_plan(
  method,                 # character(1)
  chain_ids,              # integer(N), positive and unique
  invocations,            # list(M) of validated invocation lists
  chain_artifacts,        # named list of character(N) vectors or NULL
  invocation_artifacts,   # named list of character(M) vectors or NULL
  requested_parallel_chains = NULL,
  requested_threads = NULL,
  requested_threads_per_chain = NULL,
  stages = list()         # validated plain file-mapping lists
)
```

The result has the following fixed top-level structure:

```r
list(
  method = method,
  chains = list(ids = chain_ids),
  invocations = invocations,
  artifacts = list(
    chain = chain_artifacts,
    invocation = invocation_artifacts
  ),
  requested = list(
    parallel_chains = requested_parallel_chains,
    threads = requested_threads,
    threads_per_chain = requested_threads_per_chain
  ),
  stages = stages
)
```

`validate_cmdstan_run_plan()` validates the complete result before it leaves
the planning layer. Neither the run plan nor its stage mappings use R6 classes
or perform file copies. `CmdStanRun` takes the validated list as its sole
long-lived owner, stores it privately, derives the process registry from it,
and performs staging during execution.

`NULL` is permitted only where an artifact or requested compatibility value
does not exist. Each nullable field has one meaning and is checked at
construction.

Each invocation is a validated internal list with:

```r
list(
  invocation_id = integer(1),
  chain_indices = integer(),
  chain_ids = integer(),
  base_chain_id = integer(1),
  num_chains = integer(1),
  num_threads = integer(1),
  command = character(1),
  command_args = character(),
  env = character(),
  mpi_cmd = character(1) | NULL,
  mpi_args = list() | NULL
)
```

Constructor invariants:

- `chain_indices` index the run plan's logical chain registry.
- `chain_ids` equal `run_plan$chain_ids[chain_indices]`.
- `num_chains == length(chain_indices) == length(chain_ids)`.
- `chain_ids` are consecutive when `num_chains > 1`.
- Every logical chain index occurs exactly once across all invocation plans.
- `num_threads >= 1`; an unthreaded invocation uses 1 and does not pass a value greater than 1.
- `invocation_id` is sequential from 1 to `M` and is unrelated to chain ID.

No topology boolean is stored. `length(invocations)` and each invocation's `num_chains` are sufficient.

### 7.3 Capability detection and planning

Planning uses executable information already exposed through `CmdStanModel$cpp_options()`/`$info()`. The detection result must be cached for the planning call so the executable is not repeatedly queried.

```r
stan_threads <- isTRUE(cpp_options[["stan_threads"]])
```

If executable information cannot be read or parsed, planning errors with an actionable message. It must not silently select process-per-chain execution because that could hide a threaded executable and materially change memory behavior.

When `stan_threads` is true:

- Normalize all logical-chain arguments.
- Build one invocation containing all logical chains.
- Pass one scalar seed and step size.
- Use the first chain ID as `id` and pass `num_chains=N`.
- Set explicit `num_threads` and matching child `STAN_NUM_THREADS`.

When `stan_threads` is false:

- Build one invocation per logical chain.
- Retain the original per-chain seeds, step sizes, and IDs.
- Schedule at most `parallel_chains` invocation processes concurrently.
- A supplied `threads` or `threads_per_chain` value warns that the executable was not compiled with threading and has no effect, preserving the current behavior class.

### 7.4 Public thread arguments

Add canonical `threads` to sampling and GQ methods while retaining `threads_per_chain`:

```r
sample(
  ...,
  chains = 4,
  parallel_chains = getOption("mc.cores", 1),
  chain_ids = seq_len(chains),
  threads = NULL,
  threads_per_chain = NULL,
  ...
)

generate_quantities(
  ...,
  parallel_chains = getOption("mc.cores", 1),
  threads = NULL,
  threads_per_chain = NULL,
  ...
)
```

Add the same thread controls to `sample_mpi()`. It also gains `parallel_chains` so the heuristic calculation is consistent across sampling entry points:

```r
sample_mpi(
  ...,
  chains = 1,
  parallel_chains = getOption("mc.cores", 1),
  chain_ids = seq_len(chains),
  threads = NULL,
  threads_per_chain = NULL,
  mpi_cmd = "mpiexec",
  mpi_args = NULL,
  ...
)
```

Validation and resolution:

```r
resolve_num_threads <- function(
  threads,
  threads_per_chain,
  chains,
  parallel_chains,
  stan_threads
)
```

- Both thread arguments supplied: error.
- `parallel_chains=NULL` normalizes to `chains`, preserving the current meaning of running all requested chains concurrently when resources permit.
- Threaded executable with `threads`: validate one positive integer and use it directly.
- Threaded executable without `threads`: validate/default `threads_per_chain` to 1 and calculate the shared pool size.
- Threaded single-invocation method without chain topology or `threads`: use 1.
- Unthreaded executable: return 1 and issue the existing no-effect warning if either argument was supplied.

The documentation must avoid claims that `threads_per_chain` is a reservation, minimum, maximum, or hard allocation.

### 7.5 Seed, step-size, and chain-ID normalization

Add a reusable normalization function at the planning boundary:

```r
normalize_chain_scalar_args <- function(
  seed,
  step_size,
  chain_ids,
  chains,
  stan_threads
)
```

For a threaded executable:

- `seed=NULL` generates one seed for the invocation.
- Scalar `seed` is used unchanged.
- Vector `seed` uses `seed[[1]]` and warns that the remaining values are ignored.
- Scalar or `NULL` `step_size` is used unchanged.
- Vector `step_size` uses `step_size[[1]]` and warns.
- Consecutive `chain_ids` are used without warning.
- Nonconsecutive `chain_ids` become `seq.int(chain_ids[[1]], length.out=chains)`, with a warning showing the effective IDs.

For an unthreaded executable, existing vector validation and selection remain unchanged.

Initial values and inverse metrics remain genuinely per-chain in both topologies because CmdStan 2.38 accepts exact filename lists. An initialization function continues to be evaluated once for each logical chain before planning files.

### 7.6 Command composition

Change `CmdStanArgs` from composing by process index to composing from an invocation plan:

```r
compose_invocation_args <- function(
  cmdstan_args,
  invocation,
  chain_artifacts,
  invocation_artifacts
)
```

A threaded sampling invocation contains:

```text
id=<base_chain_id>
num_threads=<num_threads>
random seed=<scalar_seed>
sample num_chains=<num_chains> ...
output file=<csv1,csv2,...>
output diagnostic_file=<diag1,diag2,...>
output profile_file=<one profile path>
init=<init1,init2,...>
sample algorithm=hmc metric_file=<metric1,metric2,...>
```

Standalone GQ similarly passes:

```text
generate_quantities num_chains=<num_chains>
generate_quantities fitted_params=<fit1,fit2,...>
output file=<gq1,gq2,...>
```

The file-list helper must:

1. stage paths containing commas;
2. apply `wsl_safe_path()` to each element;
3. verify that converted elements contain no commas;
4. join with commas only after conversion.

### 7.7 File planning and staging

Reuse existing filename generation, temporary-directory, WSL conversion, and copy helpers wherever possible. Add a small targeted staging utility rather than a parallel file-management subsystem.

```r
new_cmdstan_file_stage <- function(
  paths,
  direction = c("input", "output"),
  staging_dir,
  cmdstan_paths = NULL
)
```

Behavior:

- Planning returns a validated plain list and does not copy or delete files.
- Paths without commas map to themselves.
- Comma-containing inputs map to unique comma-free paths that `CmdStanRun`
  prepares before launch.
- Comma-containing outputs map to unique comma-free CmdStan targets and record
  their final destinations.
- After execution, every existing staged output is moved or copied to its public destination, including partial artifacts from a failed invocation for debugging.
- Atomic failure status is unaffected by the presence of a moved partial file.
- Finalization is idempotent: repeated calls do not duplicate, delete, or re-move already finalized files.
- Temporary staged inputs are cleaned through the runset finalizer/deferred cleanup.

CmdStan derives adapted metric names from sample output names. The planner must predict these names using CmdStan 2.38 rules and map them to the chain artifact registry before launch.

### 7.8 Artifact ownership

| Artifact | Scope | Successful count for N threaded chains | API behavior |
|---|---:|---:|---|
| Sample/GQ CSV | Chain | N | `output_files()` remains chain-ordered |
| Diagnostic CSV | Chain | N | `latent_dynamics_files()` remains chain-ordered |
| Adapted metric JSON | Chain | N | `metric_files()` remains chain-ordered |
| Stdout/stderr transcript | Invocation | 1 | `$output()` returns raw invocation output |
| Return code | Invocation | 1 | `$return_codes()` has length one |
| Profile CSV | Invocation | 1 | `profile_files()` and `profiles()` have length one |
| CmdStan config JSON | Invocation | 1 | `config_files()` has length one |
| Data file | Run | 1 | Existing behavior |

`save_*_files()` must select chain IDs for chain-scoped artifacts and invocation IDs for invocation-scoped artifacts. It must not reuse `proc_ids()` as a generic artifact identifier.

### 7.9 Invocation runner

Replace the duplicated sampling and GQ loops with a generic invocation scheduler:

```r
run_invocations <- function(runset, max_parallel_invocations)
```

- For a threaded sample/GQ plan, there is one invocation and `max_parallel_invocations=1`.
- For an unthreaded plan, the maximum is `min(parallel_chains, length(invocations))`.
- For threaded MPI, the one invocation command is the MPI launcher and its arguments include the executable followed by the multi-chain CmdStan arguments.
- For unthreaded MPI, each logical chain retains a separate launcher invocation; current sequential behavior is preserved unless separately changed.

Each process receives a child-scoped environment. For threaded invocations:

```r
env <- c(STAN_NUM_THREADS = as.character(invocation$num_threads))
```

WSL execution additionally merges the required `STAN_NUM_THREADS/u` entry into the child `WSLENV` value without replacing unrelated inherited entries. No `Sys.setenv()` call may be used for run-specific thread state.

Interruption and cleanup operate at invocation scope. Interrupting a threaded invocation terminates all its logical chains. For MPI, cleanup must terminate the launcher process tree rather than only the visible root process.

### 7.10 Progress and console output

Store one raw transcript per invocation. While reading stdout, parse lines matching:

```text
^Chain \[([0-9]+)\](?: |$)
```

Attributed lines update a logical-chain progress map and are also appended to that chain's filtered output buffer. Unprefixed stdout and all unattributed stderr remain only in the raw invocation transcript.

Public behavior:

```r
fit$output()       # list of raw transcripts, one per invocation
fit$output(id)     # prints chain-attributed lines for logical chain ID/index
```

The implementation must document whether `id` is positional or a CmdStan chain ID and retain the current choice consistently. The preferred behavior is the existing positional index, mapped through the run plan to its effective CmdStan chain ID.

Final chain timing must come from parsed CSV metadata. Interleaved timing lines in stdout are not used as authoritative chain timing.

### 7.11 Failure semantics

Invocation return code is authoritative:

- Return code zero: validate all required output CSVs; missing or invalid required files convert the invocation to failure.
- Nonzero return code: every owned logical chain is failed.
- Artifact validation does not rewrite the operating-system return code. `$return_codes()` remains the raw invocation code even when an internal invocation-status record is changed to failed because required output is missing or invalid.
- `output_files(include_failed=FALSE)` returns no files for chains in a failed invocation.
- `output_files(include_failed=TRUE)` returns the planned paths for debugging, whether or not every file exists.
- Draw, metadata, diagnostic, and summary methods reject an atomically failed fit as they do for an all-chain failure today.
- No `chain_status()` API is introduced in this design because it would imply a granularity CmdStan did not report.

### 7.12 Fit and metadata APIs

Change fit behavior as follows:

- `CmdStanFit$num_procs()` returns physical invocation count.
- `CmdStanMCMC$num_chains()` and `CmdStanGQ$num_chains()` return logical chain count from the run plan or parsed CSVs.
- `$return_codes()` returns one code per invocation.
- `$time()$total` remains run wall time; `$time()$chains` comes from CSV metadata.
- `$profiles()` parses invocation-scoped aggregate profiles.
- `$config_files()` and `$profile_files()` are invocation-scoped.
- Chain-scoped save and file methods retain logical chain order.

CSV metadata changes:

- Preserve CmdStan's `num_threads` name and value.
- Stop unconditionally renaming `num_threads` to `threads_per_chain` for sample and GQ.
- A live fit may also expose deprecated `threads_per_chain` compatibility metadata when the heuristic input/default was used and CmdStanR knows the requested value.
- If the user supplied canonical `threads`, `threads_per_chain` is `NULL` because no truthful per-chain value exists.
- A fit constructed only from CSV exposes `num_threads`; it does not invent `threads_per_chain` or `parallel_chains`.
- No execution-mode metadata field is added.

### 7.13 Fixed-parameter sampling

Fixed-parameter sampling follows the same topology rule as NUTS. Remove behavior that forces MPI fixed-parameter sampling to one chain. `num_chains`, chain output files, scalarization, atomic failure, and artifact scoping are identical to other sampling runs. Warmup remains disabled.

### 7.14 OpenCL

An executable compiled with both `STAN_THREADS=true` and `STAN_OPENCL=true` uses one internal multi-chain invocation. All chains share the process-level OpenCL selection and context. `opencl_ids` remain one platform/device pair per invocation. The implementation must not attempt per-chain OpenCL devices.

Tests must cover correctness, cleanup, and a representative memory/performance case. Performance results are informative rather than a correctness gate unless a clear regression threshold is agreed before implementation.

### 7.15 MPI

An executable compiled with both `STAN_MPI=true` and `STAN_THREADS=true` uses one MPI launcher invocation with `sample num_chains=N`. `mpi_args` controls MPI rank count; `num_threads` controls the TBB pool initialized by each CmdStan rank. These dimensions are independent and must be printed separately in startup messages.

Example shape:

```text
mpiexec -n <ranks> model sample num_chains=<chains> ... num_threads=<threads>
```

The launcher return code is the single invocation return code. A failure is atomic for all chains. Process-tree cleanup must cover all ranks.

Because rank placement and TBB pool creation are MPI-implementation dependent, the implementation task must verify behavior against supported OpenMPI/MPICH configurations rather than assuming total operating-system threads equal only `num_threads`.

### 7.16 WSL and Windows

- Convert each list element to a WSL-safe path before comma joining.
- Place staged files in the existing WSL-native temporary location to avoid cross-filesystem I/O regressions.
- Pass `STAN_NUM_THREADS` and `WSLENV` only to the child process.
- Use copies rather than symbolic links for targeted staging.
- Test output paths and basenames containing spaces and commas.

### 7.17 Idempotency and retries

CmdStan inference itself is not retried automatically. A failed invocation remains failed and visible to the caller.

Preparation and finalization must be idempotent:

- The run plan is immutable after command composition begins.
- Staged input preparation may be called again without changing effective inputs.
- Artifact finalization records completed moves and safely skips them on repeated cleanup.
- Finalizers tolerate missing partial files.
- No failed chain is automatically rerun in a new invocation because that would violate atomic failure semantics and change RNG behavior.

## 8. Project common utilities that will be used

- `assert_valid_threads()` and executable option normalization in `R/cpp_opts.R`, refactored to validate canonical total threads and the heuristic shim.
- `process_init()` and inverse-metric serialization in `R/args.R`.
- `process_fitted_params()` and draws-to-CSV helpers in `R/data.R`.
- `generate_file_names()`, `copy_temp_files()`, and related path helpers in `R/file.R`.
- `wsl_safe_path()`, `wsl_tempdir()`, and WSL-compatible process helpers.
- `read_cmdstan_csv()` and `read_csv_metadata()` in `R/csv.R` as the source of per-chain output truth.
- Existing processx construction and cleanup in `CmdStanProcs`.
- Test fixtures and builders in `tests/testthat/helper-models.R` and `tests/testthat/resources/`.

Before adding a helper during implementation, each subtask must search these modules and record whether an existing utility was reused or why a new utility was required.

## 9. Project common utilities that will be created

Names may be adjusted to surrounding conventions, but responsibilities must remain separate:

- `new_cmdstan_run_plan()` / `validate_cmdstan_run_plan()`: construct and
  validate the classless logical-to-physical mapping and scoped artifacts.
- `new_cmdstan_invocation_plan()`: validates a single invocation description.
- `resolve_num_threads()`: resolves canonical `threads` and heuristic `threads_per_chain` inputs.
- `normalize_chain_scalar_args()`: implements threaded scalarization and warning behavior.
- `new_cmdstan_file_stage()`: purely plans comma-path staging mappings.
- `compose_cmdstan_file_list()`: per-element WSL conversion and safe comma joining.
- `run_invocations()`: shared process scheduler for sample and GQ.
- `parse_chain_output_id()`: extracts effective chain ID from CmdStan-prefixed progress/log lines.

Each new helper requires focused unit tests. File and scheduling helpers must not duplicate existing generic utilities.

## 10. Interface contracts

### Public method additions and changes

```r
# sample(), sample_mpi(), and generate_quantities()
threads = NULL
threads_per_chain = NULL
```

- `threads`: `NULL` or positive integer scalar. Canonical total CmdStan `num_threads`. Under MPI this value is passed to every rank.
- `threads_per_chain`: `NULL` or positive integer scalar. Heuristic compatibility input used only when `threads` is `NULL`; defaults conceptually to 1.
- Supplying both is an error.
- `parallel_chains`: positive integer scalar. Actual process-concurrency bound for unthreaded execution; pool-sizing input for threaded execution.

### Return and file cardinality

```r
fit$num_chains()       # integer(1), logical chains
fit$num_procs()        # integer(1), physical CmdStan invocations
fit$return_codes()     # integer(fit$num_procs())
fit$output_files()     # character(fit$num_chains()) on success
fit$profile_files()    # character(fit$num_procs()) when profiling exists
fit$config_files()     # character(fit$num_procs()) when requested
fit$metric_files()     # character(fit$num_chains()) when requested
```

### Console output

```r
fit$output()           # list(fit$num_procs()) of raw character vectors
fit$output(id)         # prints only lines attributed to logical chain position id
```

`id=NULL` is justified as the existing selector convention: absence requests all invocation output. A supplied ID must be a valid logical chain position. Unattributed shared lines are intentionally excluded from chain-filtered output.

### Warnings

Warnings must identify the effective value, not merely say that an argument was ignored. Required warning classes/messages cover:

- vector seed collapsed to its first value;
- vector step size collapsed to its first value;
- nonconsecutive chain IDs replaced by the consecutive effective sequence;
- thread arguments supplied for an unthreaded executable;
- deprecated `threads_per_chain` metadata where applicable.

## 11. Alternatives considered

### Alternative A: Add `num_chains` directly to current `CmdStanArgs`

Rejected. The runner, fit classes, file APIs, failure logic, and timing would continue to assume process count equals chain count. This would produce incorrect indexing and fabricated per-chain artifacts.

### Alternative B: Add separate threaded sample and GQ runner classes

Rejected. This duplicates scheduling, output processing, cleanup, and file logic while leaving the underlying cardinality model unresolved. A run plan lets both topologies use the same machinery.

### Alternative C: Group chains by compatible seed, step size, or ID sequence

Rejected for the initial refactor. It would create several internal invocations, preserve only some vector semantics, and complicate atomic failure, artifact cardinality, and user expectations. The approved behavior is to use the first vector value with a warning.

### Alternative D: Expose an execution-mode switch

Rejected. The topology is an executable capability consequence, not a tuning choice. A flag would multiply testing combinations and allow avoidable high-memory behavior.

### Alternative E: Retain process-per-chain execution for OpenCL or MPI

Rejected. The automatic threaded-executable rule applies consistently to these compiled capabilities. Their distinct resource semantics are handled in planning and tests.

### Alternative F: Do nothing

Rejected. Process-per-chain execution loads redundant model/data copies, uses more memory, and does not take advantage of CmdStan's supported internal chain scheduler.

## 12. Test data acquisition plan

This refactor has no external HTTP, WebSocket, database, or ETL data source. Tests must not download live payloads.

- Reuse local Stan programs and JSON/CSV fixtures under `tests/testthat/resources/`.
- Add the smallest local Stan fixtures necessary to trigger chain-prefixed output, deterministic failure, profiling, OpenCL, or MPI behavior.
- Use a real CmdStan 2.38+ installation for integration tests.
- Generated output fixtures may be committed only when they test parsing independent of execution and are stable across supported platforms.
- Performance tests generate data locally with a fixed seed and record model/data dimensions in the benchmark script.

## 13. Testing and verification strategy

### Test integrity rules

- Write or update a failing focused test before each behavior change where practical.
- Mock only the external process boundary in pure planner/command tests.
- Do not mock CSV validation, artifact finalization, or fit cardinality in integration tests.
- Do not weaken existing tests without tying the changed expectation to a requirement in this document.
- Test warnings and effective values together so warning-only compatibility does not hide incorrect commands.

### Planner and command unit tests

Add focused coverage for:

1. Threaded versus unthreaded executable plans.
2. One and several logical chains.
3. Canonical `threads`, heuristic `threads_per_chain`, defaults, and mutual exclusion.
4. `parallel_chains > chains` clamping for heuristic calculation.
5. Scalar and vector seeds/step sizes, including warning content.
6. Consecutive and nonconsecutive chain IDs.
7. Per-chain init and metric exact lists.
8. GQ fitted-parameter exact lists.
9. Comma paths, spaces, WSL conversion order, and staging idempotency.
10. Chain-scoped versus invocation-scoped artifact counts.
11. Standard, OpenCL, WSL, and MPI command shapes.

### Integration tests

Extend or add tests adjacent to:

- `tests/testthat/test-threads.R`
- `tests/testthat/test-model-sample.R`
- `tests/testthat/test-model-generate_quantities.R`
- `tests/testthat/test-model-sample_mpi.R`
- `tests/testthat/test-opencl.R`
- `tests/testthat/test-failed-chains.R`
- `tests/testthat/test-profiling.R`
- `tests/testthat/test-model-output_dir.R`
- `tests/testthat/test-fit-mcmc.R`
- `tests/testthat/test-fit-gq.R`
- `tests/testthat/test-csv.R`

Required scenarios:

1. Four threaded chains use one physical invocation and produce four valid sample CSVs.
2. Four unthreaded chains retain four invocations and current parallel scheduling.
3. Threaded fixed-parameter sampling produces the requested chain count.
4. Threaded GQ uses one invocation and one output per fitted-parameter chain.
5. Vector seed and step size use their first values and warn; unthreaded runs preserve vectors.
6. Nonconsecutive IDs warn and produce the documented consecutive IDs.
7. A forced internal failure yields one nonzero return code and no successful chain outputs.
8. Profile/config counts are one while metric/diagnostic/output counts equal chains.
9. `$output(id)` returns only the requested `Chain [id]` lines; `$output()` retains unprefixed content.
10. Parent `STAN_NUM_THREADS` and `WSLENV` values are unchanged after success, failure, and interruption.
11. Fits reconstructed from CSV expose `num_threads` without inventing per-chain thread metadata.
12. Threaded OpenCL sampling produces correct draws and cleanup behavior.
13. Threaded MPI launches one command, produces all chain outputs, and terminates the rank process tree on interruption.
14. Output/input paths containing commas round-trip to the requested public names.

### CI matrix

- Add an explicit minimum supported CmdStan 2.38 job rather than testing only the latest release.
- Retain current Linux, macOS, Windows, and WSL coverage.
- Run MPI integration where the existing MPI toolchain is available.
- Run OpenCL integration where existing OpenCL CI support is available.
- Conditional platform skips must state the missing capability; they must not convert real failures into skips.

### Performance verification

Provide a reproducible benchmark comparing threaded internal chains with the pre-refactor process-per-chain baseline on a data-heavy model:

- four chains;
- identical scalar seed and sampling configuration;
- peak resident memory for the entire process tree;
- wall time split into launch/execution/read phases;
- at least three paired repetitions;
- median summary and raw results.

The acceptance criterion is a clear peak-memory reduction consistent with one model/data instance. Wall time must not regress materially without an explained upstream scheduling reason.

### Commands

```sh
Rscript -e 'devtools::test(filter = "threads")'
Rscript -e 'devtools::test(filter = "model-sample")'
Rscript -e 'devtools::test(filter = "model-generate_quantities")'
Rscript -e 'devtools::test(filter = "failed-chains")'
Rscript -e 'devtools::test()'
Rscript -e 'devtools::check()'
```

## 14. Rollout, migration, and operations

### Implementation sequence

The code may land in several reviewable changes, but the released behavior is automatic and has no feature flag.

1. Introduce the run plan and scoped artifact model while preserving current process-per-chain behavior.
2. Refactor the existing sample and GQ runners onto the shared invocation scheduler.
3. Add canonical `threads`, heuristic resolution, and child-scoped environments.
4. Enable internal sampling and fixed-parameter chains for threaded executables.
5. Enable internal standalone GQ.
6. Update fit APIs, metadata, profile/config handling, and output filtering.
7. Add WSL/comma staging and OpenCL coverage.
8. Enable and validate the threaded MPI launcher topology.
9. Complete migration documentation, NEWS, vignettes, and performance evidence.

### User migration

- Existing ordinary scalar-seed calls continue to work.
- Threaded models no longer require explicit `threads_per_chain`; it defaults heuristically to 1.
- Threaded optimize, variational, Pathfinder, and Laplace calls no longer
  require explicit `threads`; an omitted value preserves the historical
  single-threaded behavior.
- `threads` is the preferred direct control for total CmdStan threads.
- Vector seed/step-size and nonconsecutive ID calls remain accepted for threaded models but warn and use the documented effective values.
- `return_codes()`, profiles, configs, and raw output change from chain to invocation cardinality for threaded multi-chain runs.
- Metadata consumers should migrate from `threads_per_chain` to `num_threads`.

Update:

- `NEWS.md` with the breaking cardinality and metadata changes;
- sampling, GQ, fixed-parameter, profiling, MPI, OpenCL, and thread argument documentation;
- examples that imply `parallel_chains * threads_per_chain` is a hard allocation;
- fit method documentation for `num_procs()`, `return_codes()`, `output()`, profiles, configs, and timings.

### Observability

Startup output should report facts rather than an execution-mode label. Example:

```text
Running 4 chains in 1 CmdStan invocation with 8 total threads...
```

MPI startup additionally reports ranks:

```text
Running 4 chains in 1 CmdStan invocation using MPI with 3 ranks and 8 threads per rank...
```

Verbose mode prints the exact command and child thread environment. Fit objects expose logical chain count and physical invocation count through existing APIs.

### Rollback

There is no runtime flag. Before release, rollback is a code revert. After release, defects must be fixed in the automatic planner rather than instructing users to choose a hidden legacy topology.

## 15. Open questions and follow-ups

There are no blocking product decisions remaining. Implementation must confirm:

1. The exact set of CmdStan 2.38 stdout/stderr lines that carry `Chain [id]`, especially initialization and exception paths. Unattributed lines remain invocation-only regardless of the result.
2. TBB pool initialization and total resource reporting for the supported MPI implementations and rank-placement configurations.
3. Whether `threads_per_chain` compatibility metadata should be removed in CmdStanR 2.0 or retained indefinitely as a live-fit-only field.
4. The performance acceptance threshold after the first paired benchmark establishes normal platform variance.

These follow-ups do not change the approved topology or public contracts in this design.

## 16. Subtasks

### T1: Add failing run-plan cardinality tests
- **Summary:** Specify logical chain, invocation, and artifact cardinalities before implementation.
- **Scope:** Pure R planner tests; no process execution.
- **Acceptance:** Tests cover threaded/unthreaded sample, GQ, fixed-parameter, OpenCL, and MPI plans and fail against the current implementation.
- **Status:** [ ] Not started

### T2: Implement classless run-plan and invocation contracts
- **Summary:** Add the validated logical-to-physical mapping used by all later work.
- **Scope:** Pure nested-list planning functions and focused tests; existing
  execution remains process-per-chain.
- **Acceptance:** Every logical chain maps exactly once, artifact scopes validate, and existing sample/GQ suites remain green.
- **Status:** [ ] Not started

### T3: Refactor argument normalization and thread resolution
- **Summary:** Add canonical `threads`, heuristic `threads_per_chain`, scalarization, and consecutive-ID rules.
- **Scope:** Public argument validation, warnings, and plan inputs.
- **Acceptance:** Focused tests prove exact effective values and preserve unthreaded vector behavior.
- **Status:** [ ] Not started

### T4: Add exact file-list composition and comma staging
- **Summary:** Compose CmdStan 2.38 filename lists safely across POSIX, Windows, and WSL.
- **Scope:** Inputs, outputs, diagnostics, metrics, GQ fitted parameters, staging, and cleanup.
- **Acceptance:** Spaces and commas round-trip correctly; staging/finalization is idempotent; no parent paths are mutated.
- **Status:** [ ] Not started

### T5: Implement the shared invocation scheduler
- **Summary:** Replace sample/GQ scheduling duplication with invocation-scoped process tracking.
- **Scope:** Process creation, polling, interruption, return codes, child environments, and raw output.
- **Acceptance:** Legacy unthreaded behavior remains green and parent environment variables never change.
- **Status:** [ ] Not started

### T6: Enable internal sample and fixed-parameter chains
- **Summary:** Compose and run one CmdStan invocation for every threaded sampling request.
- **Scope:** Standard, WSL, and fixed-parameter sampling; MPI handled separately.
- **Acceptance:** N logical chains produce N valid chain artifacts, one return code/profile/config, and atomic failures.
- **Status:** [ ] Not started

### T7: Enable internal standalone generated quantities
- **Summary:** Apply the same run plan to per-chain fitted-parameter inputs and GQ outputs.
- **Scope:** `generate_quantities()`, `CmdStanGQ`, and GQ failure behavior.
- **Acceptance:** Threaded GQ uses one invocation and produces one valid output per input chain; unthreaded behavior is preserved.
- **Status:** [ ] Not started

### T8: Refactor fit APIs and metadata
- **Summary:** Make chain and invocation cardinality truthful in fits and CSV metadata.
- **Scope:** `num_chains()`, `num_procs()`, files, profiles, configs, output, return codes, timings, and `num_threads`.
- **Acceptance:** All public methods return the cardinalities specified in this document and CSV-only fits do not invent unavailable metadata.
- **Status:** [ ] Not started

### T9: Add chain-aware console parsing
- **Summary:** Track progress and filtered output by effective CmdStan chain ID while retaining raw invocation transcripts.
- **Scope:** MCMC/GQ process output parsing and `$output(id)`.
- **Acceptance:** Interleaved output is routed correctly, shared lines remain invocation-scoped, and timing comes from CSV.
- **Status:** [ ] Not started

### T10: Validate threaded OpenCL execution
- **Summary:** Exercise internal chains with one process-level OpenCL device/context.
- **Scope:** OpenCL correctness, cleanup, memory, and representative performance.
- **Acceptance:** Existing OpenCL results remain correct and multi-chain artifact/failure contracts hold.
- **Status:** [ ] Not started

### T11: Implement and validate threaded MPI execution
- **Summary:** Run one MPI launcher invocation containing all internal chains when both MPI and threads are compiled.
- **Scope:** Launcher arguments, rank/thread reporting, fixed-parameter behavior, interruption, and cleanup.
- **Acceptance:** Supported MPI CI produces all chain outputs, reports one return code, and leaves no ranks after interruption.
- **Status:** [ ] Not started

### T12: Complete cross-platform, performance, and migration work
- **Summary:** Finish the release evidence and user-facing transition for CmdStanR 1.0.
- **Scope:** CmdStan 2.38 CI, Windows/WSL, paired memory benchmarks, NEWS, reference docs, and vignettes.
- **Acceptance:** Full tests and `devtools::check()` pass; memory evidence is recorded; all breaking cardinality and metadata changes are documented.
- **Status:** [ ] Not started
