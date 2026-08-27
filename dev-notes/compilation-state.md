# Compilation state and C++ options — intended behaviour

Status: draft for discussion. Describes the target behaviour of `cmdstan_model()`,
the build record kept beside an executable, and when that record is validated.

## Purpose and scope

This document exists because the defects in this area have not been independent.
#1228, #1234, the residual warning gaps, #1019 and #1237 all reduce to a small
number of contracts that were never written down, so each was rediscovered by
being violated. The intent is to state those contracts once, and to sequence the
remaining work off them rather than off the issue list.

**This note is deliberately not the specification.** Each decision below lives on
its issue in more detail than is repeated here — that is where an implementer
should read, and it is the copy to trust if the two disagree. What this note adds
is the part no issue can carry: why the contracts are what they are, why the work
is ordered this way, and which tempting alternatives were rejected.

The work is tracked in #1238 (the record), #1255 (rebuild decisions), #1256
(removing deferred compilation), #1257 (untracked dependencies), #1237 (includes),
#1250 (option classification) and #1251 (logical `FALSE`).

**In scope:** what is recorded about an executable and when; what a configuration
means when it reaches `make`; when the record is validated; what can and cannot be
known about an executable cmdstanr did not build.

**Out of scope:** the toolchain itself — compiler version, system libraries — which
is deliberately untracked (§6). The *selected CmdStan installation* is in scope and
is part of `builder` identity; an earlier draft put installation paths out of scope
and then relied on them, which was a contradiction.

**Relation to the 1.0 milestone.** Every issue in this area is milestoned
`v1.0.0 - release`, so the milestone is mostly this work — but not only: it also
holds linting (#1172), formatting (#1153), interactive installation (#605) and the
`num_chains` argument (#534), none of which touch these contracts.

---

## At a glance

Orientation, not specification — the sections below are the contract, and they are
where the reasoning lives.

**The API**

- `cmdstan_model()` is the only place a build is *configured*. It ensures a current
  executable: reusing one that matches the requested source and options, rebuilding
  when it does not.
- `compile = FALSE` and `$compile()` are removed. **`cmdstan_model(exe_file =)`
  stays** — you can still hand cmdstanr an executable it did not build, and it says
  up front whether it knows how that executable was built.
- Introspection and compilation both get standalone functions:
  `format_stan_file()`, `check_syntax_stan_file()`, `stan_variables()`,
  `stan_build_info()` and `compile_stan_file()`. `cmdstan_model()` and
  `compile_stan_file()` share one implementation — the object is a wrapper around
  the build, not a second path to it.
- `$code()` and `$variables()` describe the source the executable was built from,
  not the file as it is now.
- Anything that runs the binary checks that it is current and **errors** — it never
  compiles. Only `cmdstan_model()` builds.

**When it rebuilds** — the Stan program, an include or how one resolves,
`include_paths`, the user header or its path, `make/local`, `cpp_options` or
`stanc_options`, the CmdStan installation, a replaced or corrupt executable, a
missing record, an executable predating records, or `force_recompile = TRUE`. Every
applicable reason is reported, not only the first.

**What the record holds** — a JSON file beside the executable,
`<model>.cmdstanr.json`, holding the request; what the binary reports as enabled
(tri-state, and absence never means disabled); source paths and hashes, including
the include list from `stanc --info`; the `make/local` hash; the CmdStan
installation; a hash of the executable; known-untracked dependencies; and a format
version.

---

## 1. Vocabulary: what the record is, and what it is not

The record is a **provenance manifest**: a description of how an artifact came to
exist. It is not a configuration store, not an authority that can authorise
whatever happens to be at the executable path, and not a substitute for observing
the binary itself.

That distinction is load-bearing because these are genuinely different facts, and
an earlier draft collapsed them:

- **`request`** — what the user asked for on the call that built it.
- **`reported_features`** — what the binary itself reports as enabled. Distinct
  from `request` because `make/local` can enable threading or OpenCL that the user
  never mentioned.
- **`dependencies`** — the sources consumed, and enough about how they were
  resolved to re-resolve them.
- **`artifact`** — which executable this record describes, by hash (§4).
- **`builder`** — which CmdStan installation produced it.
- **`known_untracked_dependencies`** — dependencies we can see exist but cannot
  resolve (§6). Named for what it is: an empty list means nothing was *detected*,
  never that the record is complete.
- **`format_version`** — the record schema's own version, without which the
  forward-compatibility rule in §4 has nothing to check.

`reported_features` is **tri-state and best-effort**: each feature is *known
enabled*, *known disabled*, or *unknown*. `<exe> info` reports what CmdStan chooses
to report — threading, OpenCL, Stan version — not arbitrary flags. **Absence must
never be read as disabled.** `$cpp_options()` merges `request` with
`reported_features` — structurally like `merge_exe_info_cpp_options()`
(`R/cpp_opts.R:78`), though that function does not implement the tri-state
contract today and will need changing — without claiming completeness.

### What consumers do with each state

Recording three states is useless unless downstream code acts on three. The policy:

**This table covers one case only: a runtime argument asks for a build feature.**
The converse — an artifact that has a feature nobody asked to use — is a separate
policy below, and conflating the two is easy.

| State, *when the feature was requested at runtime* | Behaviour |
|---|---|
| known enabled | proceed |
| known disabled | **error** |
| **unknown** | **error** — never silently read as disabled, never discard the user's runtime option |

`assert_valid_threads()` (`R/cpp_opts.R:157`) is the case to fix. Asking for threads
on an unthreaded binary warns and then *discards the argument*:

```r
stan_threads <- cpp_option_value(cpp_options, "stan_threads")
if (is.null(stan_threads) || !isTRUE(stan_threads)) {
  if (!is.null(threads)) {
    warning(...)
    threads <- NULL          # the user asked for threads and silently got none
```

That is a genuine request-exceeds-artifact mismatch, which §5 says operations error
on, and it is the expensive half: a silently single-threaded run is the four-hour
job that should have taken one, discovered afterwards. Erroring is a deliberate
behaviour change rather than preservation, and it costs a user who wants to run
unthreaded nothing — they stop passing `threads_per_chain`.

### The converse case: a capability nobody asked to use

**Additional policy, not an instance of the table above.** A threading-enabled
binary run with no `threads` argument is not a mismatch — the artifact simply
exceeds the request. cmdstanr errors today (`R/cpp_opts.R:174`), and **that is
kept**, on the grounds that building with threading and then not using it is much
more likely a mistake than an intention.

Two things make keeping it the conservative choice rather than a new imposition.
It is well established — five assertion sites in `test-threads.R` plus snapshots.
And it is already reachable for threading inherited from `make/local`, because
`$cpp_options()` has merged executable metadata on the construction and no-op paths
for some time; #1235 extends that merge to the fresh-compile path, making the
behaviour uniform rather than introducing it.

The cost is real and should be stated: a user with `STAN_THREADS=true` in
`make/local` must pass a threads argument on every run. That is defensible — a
threaded binary should be told how many threads to use — but it is a policy choice,
not a consequence of the tri-state contract.

**Scope this to features an operation actually requires.** For arbitrary options —
`CXXFLAGS`, a user header — status is *permanently* unknown, because CmdStan never
reports them; erroring on those would error on everything. It applies where a
runtime argument depends on a build feature: `threads_per_chain` on `STAN_THREADS`,
OpenCL device selection on `STAN_OPENCL`.

In practice the error will rarely fire. CmdStan 2.39 reports all four flags
explicitly, including negatives:

```
STAN_THREADS=false
STAN_MPI=false
STAN_OPENCL=false
STAN_NO_RANGE_CHECKS=false
```

so exactly the features with runtime checks are the ones whose status is known.
Unknown arises when `<exe> info` cannot be run at all, or on a CmdStan old enough
not to report a flag — which is when erroring is most warranted.

**A model object is a handle on an executable plus its record.** It holds no
durable configuration of its own.

---

## 2. Contract: options are specified once, at `cmdstan_model()`

> Every call that builds specifies the configuration it wants. Omitting an option
> means you are not asking for it.

**Persistent options were proposed and rejected (#1248).** Recorded here because it
is the most tempting alternative in this design and will be proposed again. The
case for it was not convenience: once cmdstanr rebuilds on its own initiative it
has to build with *something*, and under one-shot semantics the previous compile
consumed the options, so an automatic rebuild would silently drop the user's
threading.

That is answered by removing the premise rather than by persisting. An unrequested
rebuild never replays stored configuration — the constructor builds from the
request in front of it, and anything that would run a stale executable errors
instead (§5). There is no point at which cmdstanr needs configuration it was not
just given.

`cmdstan_model()` **ensures a current compiled executable** when given a Stan file
— it reuses one that is up to date and builds when it is not; it does not compile
unconditionally. There is no `compile = FALSE` (§8), and **`$compile()` is removed** — once deferred compilation
is gone it has no unique public purpose, and `cmdstan_model(file,
force_recompile = TRUE, ...)` covers every remaining use.

Removing it rather than narrowing it also avoids a trust problem. A `$compile()`
that rebuilds "as recorded" has to replay build arguments from a file, including
opaque Make arguments, which means it needs a strictly validated record schema
before it can be safe. Nothing replaces it internally either: the assessment never
rebuilds (§5), and constructor compilation uses the explicit current request.

Nothing structural blocks removal: fits do not hold model references
(`R/fit.R:20-26` copies the model-methods environment rather than pointing at the
object).

### What this costs, stated accurately

**The executable cache is single-configuration.** One binary per Stan file per
`dir`, and the most recent compile owns it. Two call sites wanting different
options will each rebuild to evict the other.

An earlier draft claimed this "fails loudly." **That was wrong**, and only true for
threading: `assert_valid_threads()` warns when `threads_per_chain` is set on an
unthreaded model, but nothing warns about changed optimisation flags, range checks,
a different user header, or opaque Make options. Most configuration changes have no
runtime check at all.

Single-configuration caching is an acceptable simplicity tradeoff for v1, but it
needs a guard rather than a hope. The model object records the artifact identity it
was constructed against, and §5's assessment detects that another call or process
replaced it.

**Report what actually differs.** If the replacement carries a different
configuration, name it. If another process installed an *equivalent* configuration
as a different artifact, the honest message is that the executable was replaced —
promising a changed option name when none changed would be a lie the user cannot
act on.

A content-addressed executable cache would remove the problem rather than detect
it. That is deferred, not rejected.

---

## 3. Contract: what a configuration means to `make`

### Logical `FALSE` must disable (#1251)

`cpp_options = list(stan_threads = FALSE)` currently *enables* threading. Verified:
`ifdef` is true for any non-empty value.

```
STAN_THREADS=FALSE  ->  ifdef: ENABLED
STAN_THREADS=       ->  ifdef: disabled
```

A logical `FALSE` must emit the empty assignment. Note this is a *value*, not an
absence — it overrides `make/local`, which is the point. `NULL` already means
exactly this and must keep meaning it.

### The command line is not `make/local`

Different languages, and the difference is not cosmetic. Verified with a recursive
Make test:

- On the command line, `+=`, `?=` and `:=` **all collapse to `=`**.
- A command-line assignment has command-line *origin*, which **blocks** any
  makefile-side `+=` to the same variable.
- Both properties propagate into sub-makes via `MAKEFLAGS`.

In `make/local` — a file — `+=` is real and appends. cmdstanr's own `+=` usage
(`CXXFLAGS` at `R/install.R:92`, `:258`, `:269`; `CPPFLAGS_SUNDIALS` at
`R/utils.R:932`) is all `make/local`, and is not evidence that `+=` works in
`cpp_options`. It does not. `::=` errors on Make 3.81, still the macOS default.

### Assignments are named; everything else is opaque (#1250)

Two verified defects. **Unnamed raw entries reach `make` but are invisible to
everything keyed on names**, because an unnamed list entry's name is `""` while
`cpp_options_to_compile_flags()` passes it straight through (`R/cpp_opts.R:139`):

```r
mod <- cmdstan_model(f, cpp_options = list("STAN_THREADS=TRUE"))
mod$sample(threads_per_chain = 4)
#> Warning: ... not compiled with 'cpp_options = list(stan_threads = TRUE)'
```

The model *is* threaded — #765's symptom via a different spelling. Raw
`USER_HEADER=my.hpp` is worse: it lands in **neither** bucket, so the header
compiles in while `resolve_user_header()` never sees it.

**Names are lower-cased at `R/cpp_opts.R:100`**, so `foo=1` and `FOO=1` compare
equal despite being different Make variables.

**Raw `NAME+=`, `NAME?=` and `NAME:=` are assignments, not opaque arguments.**
`parsed_cpp_options()`'s `^[A-Za-z_][A-Za-z0-9_]*=` does not match them, so they
fall through to `opaque` — but on the command line they *are* plain assignments:

```
make 'FOO=x'   -> FOO=[x]  origin=command line
make 'FOO+=x'  -> FOO=[x]  origin=command line
make 'FOO?=x'  -> FOO=[x]  origin=command line
make 'FOO:=x'  -> FOO=[x]  origin=command line
```

So `list("FOO+=x")` and `list(foo = "x")` produce identical builds and must
classify identically. An earlier draft argued against widening the regex on the
grounds that `+=` appends; that was written before the Make test above and is
wrong for the command line.

**This is a prerequisite for the record, not adjacent to it.** Per-field
canonicalization (§4) cannot be implemented without a correct named/opaque
classification, because the two kinds get different treatment.

---

## 4. Contract: the build record (#1238)

A file beside the executable describing how it was built. JSON, named
`<model>.cmdstanr.json` — so `bernoulli.stan` compiled to `bernoulli` is described
by `bernoulli.cmdstanr.json` in the same directory. `jsonlite` is already an
import, the format is readable and diffable by hand, and the name stays clear of
`.dep` and `.d`, which `make` and the C++ toolchain already claim in that
directory. Both remain revisable up to the release, so nothing outside the reader
and the writer should depend on either.

### Binding the record to its executable

**The record must contain a hash of the executable it describes.** Atomic
replacement of the record alone does not bind the pair:

```
process A installs executable A
process B installs executable B
process B writes record B
process A writes record A
final state: executable B + record A
```

A crash between the two installations produces the same result; the current code
already acknowledges a crash window when replacing an executable — *"A crash
between renames may leave only the backup"* (`R/utils.R:267-272`). Hashing a CmdStan binary measured **8.5 ms**, against 28.6 ms
merely to spawn `<exe> info`, so cost is not an argument against it.

The hash also detects manual replacement and ordinary corruption — which retires
an earlier claim in this document that distrust of the artifact is "never
detectable." It is detectable, cheaply.

**Installation is a transaction:** build and stage both artifacts, install the
executable, install the record, then verify the pair. **Any failure — including a
successful record install whose pair verification then fails — restores both the
previous executable and the previous record**, leaving a consistent pair rather
than a new artifact with old provenance. An earlier draft called record-write
failure "non-fatal but visible"; that is incompatible with §5, since an executable
without a valid record is immediately unusable.

**Concurrency is out of scope for v1.** The hash fixes crash-created and sequential
mismatches. It does **not** fix active concurrency: process A can validate
executable A, process B can replace it, and A then launches B. That is a TOCTOU
race, and no claim of correctness under concurrency is made here. Concurrent
compilation or use of one destination is unsupported; locking is tracked
separately. Note the project's existing answer to this shape of problem is
workspace isolation (#1025), so a lock would be a second concurrency strategy
alongside an existing one — worth deciding deliberately.

### The bug this fixes

`R/model.R:732-733` asks Make's question:

```r
file.mtime(exe) < file.mtime(self$stan_file())
```

*Exe versus source*, correct only if mtimes move monotonically forward. Extracting
a project tarball over an existing build:

```
source content is now DIFFERENT
exe mtime : 2026-08-27 10:19:38.476
stan mtime: 2026-03-15 09:00:00.000
exe < stan (rebuild?): FALSE   <-- no rebuild
```

cmdstanr reports "up to date" and runs the **old binary against the new source**.
`tar -x`, `unzip`, `cp -p`, `rsync -a` and backup restores all reach this.

The fix is not "hash instead of mtime" — it is **compare against the record instead
of against the artifact.**

### Hashes rather than mtimes

Once the comparison is against the record, a stored mtime is defensible and fixes
the case above. What hashing adds is the false-positive column:

| | false negative (stale binary) | false positive (spurious rebuild) |
|---|---|---|
| exe vs. source mtime (today) | **easy** | rare |
| recorded vs. source mtime | needs mtime restored to the exact recorded value | branch round-trip, `touch`, cloud sync |
| recorded vs. source hash | none *within tracked files* | none |

The bottom row is bounded by what is tracked. Toolchain drift and the untracked
dependencies in §6 are false negatives by construction, not defects in the
comparison.

The false-positive case is ordinary — the Stan file is in git, the executable is a
gitignored artifact beside it, and a branch round-trip leaves identical content
with a fresh mtime. Measured on six realistic source files: **0.04 ms** to stat,
**0.31 ms** to hash. 0.27 ms against a 30–90 second rebuild.

Ninja uses mtime because it stats tens of thousands of files per invocation. We
stat about six.

### Canonicalization is per-field

A single "sort and last-wins-deduplicate" rule is wrong. The correct rules differ:

- **Named Make assignments** — case-sensitive names, last assignment wins, then
  sort by name for comparison.
- **Opaque Make arguments** — preserve order exactly; later arguments can override
  earlier ones.
- **Include paths** — preserve order; it controls shadowing.
- **Stanc options** — a **conservative comparison** for v1: compare the resolved
  set literally and accept that equivalent spellings may occasionally trigger an
  unnecessary rebuild. "Canonicalize per option semantics" appeared in an earlier
  draft and is not implementable guidance — it would require enumerating the
  semantics of every stanc option. Enumerate them later if the spurious rebuilds
  turn out to matter.
- **User-header paths** — normalise without erasing meaningful distinctions.
- **`NULL` / `FALSE`** — preserve the explicit empty-assignment meaning (§3).

### Forward-version records

A record written by a newer cmdstanr must not be silently replaced. Since a
rebuild would have to install a replacement record, "preserve it" and "rebuild"
cannot both happen — **refuse automatic replacement and require explicit
force/migration.**

---

## 5. Contract: when validation happens

Specifying rebuild triggers is not enough; the design must say *when* they are
checked and what each caller does about them. Today `$format(overwrite_file = TRUE)`
rewrites the Stan file (`R/model.R:1293`) and updates cached source and variables
(`:1309-1311`) but leaves the executable untouched, and an external editor does the
same. The object can then show new Stan code while `$sample()` runs the old binary.

### Assessment is separate from what the caller does about it

One operation answers one question — *is this executable current?* — and it
**never compiles and never mutates state.** Callers differ:

| Caller | On a trigger |
|---|---|
| `cmdstan_model()` | **rebuilds**, printing every reason (§6) |
| any operation that runs or derives state from the binary | **errors** |

An earlier draft stated both behaviours as if they were one contract, which read as
a contradiction between §5 and §6. They are one assessment with two responses.

### What the error says

**Not `force_recompile = TRUE`.** An earlier draft told users to reach for it after
a source or configuration change, which is wrong: the constructor detects those on
its own, so a plain `cmdstan_model(...)` is the fix. Reserve `force_recompile` for
what it is actually for — a corrupt or missing record, an executable/record
mismatch, or explicit distrust of the artifact.

Erroring rather than rebuilding is deliberate. A 30–90 second compile appearing
inside `$sample()` is worse than an actionable message, and it is exactly the kind
of latency users cannot predict or plan around.

### Scope and cost

**Guard every operation that executes or derives state from the binary** — not only
the fitting methods. At least: `$sample()`, `$optimize()`, `$variational()`,
`$laplace()`, `$pathfinder()`, `$generate_quantities()`, `$diagnose()`,
`$cmdstan_defaults()`, and the exposure methods.

Cost is **~8.8 ms**: source hashes plus the executable hash. An earlier draft cited
0.3 ms, which counted only the sources and predates the artifact hash in §4. If
include re-resolution runs here too (§6) add ~30 ms. Still negligible against a
sampling run, and the accurate number belongs in the document.

### Introspection is a construction-time snapshot

Pre-operation validation does **not** make cached introspection safe: `$code()` and
`$variables()` can still be stale after an external edit. The rule is that they
describe **the source the executable was built from**, not the file as it is now.

That is the more correct answer rather than a compromise — `$code()` returning the
current file would show code the binary does not have. It also does not undo #1228,
which was staleness after a **recompile**; a snapshot as of the last compile fixes
exactly that case.

**The snapshot must be captured eagerly, or it is not a snapshot.** `$variables()`
parses from disk on first call (`R/model.R:874`), so an edit made before that first
call would return information about the *new* source while claiming to describe the
built one — the contract violated by the mechanism meant to implement it.

Capture costs nothing extra: the assessment already invokes `stanc --info` for
include resolution (§6), and the same output carries the variables. The assessment
returns parsed source information; the constructor commits it as the object's
snapshot after a successful validation or rebuild.

**`$format(overwrite_file = TRUE)` must not replace the snapshot.** Today it
rewrites the file and refreshes the caches from it (`R/model.R:1309-1311`). Under
this contract that is backwards: formatting changes the source and makes the object
stale, so `$code()` and `$variables()` must go on describing the binary until a new
model is constructed. Refreshing them would leave the object describing source that
was never compiled.

---

## 6. Contract: when a rebuild happens (#1019)

A rebuild is triggered by any of:

- the Stan program changed
- a resolved include changed, or resolves differently now (#1237)
- the user header changed
- `make/local` changed
- the supplied `cpp_options` or `stanc_options` differ from `request`
- the supplied `include_paths` or `user_header` path differ from `request`
- the selected CmdStan installation differs from `builder`
- the executable does not match `artifact` — replaced by another process, or corrupt
- the record is missing or unreadable
- the executable predates build records, so there is nothing to compare
- `force_recompile = TRUE`

The middle three are *artifact-side*: reasons the recorded facts cannot be trusted,
rather than reasons the inputs changed. They belong in the same list because the
constructor's response is identical — rebuild, and say why.

**One case is deliberately not in that list.** A record whose `format_version` is
*newer* than this cmdstanr understands does **not** trigger a rebuild, because
rebuilding would install a replacement record over one written by a version that
knows more than we do. It errors and requires explicit force or migration (§4).
"Unreadable" and "readable but from the future" look alike and must not be
conflated — the first has nothing to preserve, the second does.

**The comparison is request identity, not effective-source identity.** A changed
include-path *order*, or a user header at a different path with identical content,
triggers a rebuild even though the resolved sources happen to match. That is the
conservative rule and the simple one: it can rebuild unnecessarily, but it cannot
miss a change. Effective-source identity would be tighter and is not worth the
specification cost for v1.

**CmdStan identity rules.** A different selected installation is a different
requested build environment and triggers a rebuild at `cmdstan_model()`. The same
version at a different path also counts — the path is part of the identity, since
two installations at the same version can differ in `make/local` and in patches.
If the recorded installation no longer exists, the record's `builder` is
unverifiable and the model must be rebuilt against the current one.

**Report every applicable trigger, not whichever branch is checked first.** Today's
`if`/`else if` chain (`R/model.R:726-739`) reports one. A user who changed both the
source and `make/local` should be told both.

```
#> Recompiling:
#>   - the Stan program changed
#>   - make/local changed (/path/to/cmdstan-2.39.0/make/local)
```

`make/local` is per-installation, so editing it invalidates every model built
against that installation. That is correct — `cmdstan_make_local()`,
`install_cmdstan(overwrite = TRUE)` and the `-fPIC` auto-fix at `R/utils.R:932` all
genuinely stale those executables, and the last of those fixes a bug for free
(today nothing notices). It is narrower than it first appears: an *upgrade*
installs to a new directory, so `builder` already differs.

### Include shadowing is detected, not accepted

An earlier draft accepted this as an undetectable limitation. It is tractable, and
accepting it contradicts the premise that we never silently run the wrong binary —
a branch switch adding a higher-priority include is the same workflow used to
justify hashing.

**`stanc --info` already answers this.** It returns the resolved include set
directly:

```json
{
  "parameters": { "y": { "type": "real", "dimensions": 0 } },
  "included_files": [
    "/abs/path/to/inc/half.stan"
  ]
}
```

So: **store the normalised `included_files` vector at build time and compare it
against fresh `stanc --info` output.** The `include_paths` already in `request`
supplies the search configuration. An earlier draft proposed recording each
include's spelling, its ordered search roots and the selected path, then
re-resolving that mapping — unnecessary, and it would need parsing stanc does for
us.

**Re-resolve by invoking stanc, never by reimplementing its rules.** Reproducing
stanc's resolution semantics in R is a correctness hazard, and getting it subtly
wrong reintroduces the silent-stale-binary class this design exists to remove.
There is no performance argument for the risk:

```
stanc --info      : 29.9 ms
exe info          : 32.2 ms
```

Against ~8.8 ms of hashing and a 30–90 second compile, a stanc call is free.

**Invoke stanc from the recorded `builder`, not from whichever installation is
selected now**, or a different stanc's resolution rules get applied to a model this
one did not build. **Check builder identity first**: if the selected installation
differs from `builder`, or the recorded installation no longer exists, that is
already a rebuild trigger (above) and should be reported without attempting
re-resolution at all.

**Normalisation: normalised absolute paths, and relocation rebuilds.**
`included_files` comes back as absolute paths, so moving a project changes every
recorded entry and triggers a rebuild. That is the v1 rule, and it applies equally
to the recorded Stan file and `include_paths`.

Relocatable records were considered and rejected: they would require defining
roots, symlink behaviour and paths that fall outside the project, for little
benefit against a conservative rule that is easy to explain. It is also consistent
with treating the CmdStan installation path as part of `builder` identity. And the
case where rebuilding is genuinely impossible — no source — is already covered by
executable-only models (§7).

### Provenance we cannot complete

Two dependencies cannot be tracked in v1, and both are **detected and recorded even
though they are not resolved**, so the limitation reaches the users who actually
hit it rather than only the documentation:

- **`make/local` including another makefile.** `make/local.example:36` ships with
  `# -include $(HOME)/.config/stan/make.local`, so it is a suggested pattern.
  Parsing arbitrary Make syntax is not justified for v1.
- **Headers transitively included by `USER_HEADER`.** Hashing the top-level header
  misses them.

In both cases a regex — `^\s*-?include\b` for `make/local`, `^\s*#\s*include\s*"`
for the user header — tells us there *is* an untracked dependency, without
resolving anything.

**The field is `known_untracked_dependencies`, not `provenance_complete`.** A regex
can establish that a gap exists; it cannot establish that none does. Make also has
`sinclude`, variable expansion and `eval`; C++ has angle-bracket local headers,
macro-expanded includes, line continuations and conditional inclusion. **No match
means "no known gap," never "complete."** Until compiler depfiles exist, *any* user
header potentially carries untracked transitive dependencies — the regex improves
the message, not the guarantee.

(An earlier draft named this `provenance_complete`, which is the same error §10
warns about for `reported_features`: treating absence of evidence as evidence of
absence, in the same document.)

**Surface it at construction and through `stan_build_info()` — not in
pre-operation validation.** It is a standing property of the model, not a change,
and validation reports only what changed. A warning on every `$sample()` call is
noise that trains people to ignore warnings.

```
#> Note: this model has dependencies cmdstanr does not track — make/local
#>   includes another makefile. If you change it, rebuild with
#>   force_recompile = TRUE.
```

**Compiler-generated dependency files are the right long-term mechanism for
transitive C++ headers, and CmdStan does not currently provide one.** The machinery
appears to exist — `make/program:106-108` sets up `-include` for a model depfile
and `make/program:68` has a `%.d: %.hpp` rule — but building a model produces no
`.d`, because the model's `.hpp` is an intermediate that CmdStan deletes after
linking. Asking for it directly fails: `make m.d` → `No rule to make target`.
Verified against CmdStan 2.39. So this needs a CmdStan-side change or a different
compilation driver, not merely reading a file that is already there.

### What no record can fix

- **Toolchain drift.** A compiler upgrade, a changed system library. Deliberately
  outside the recorded set rather than chasing completeness.
- **Distrust of a *source*.** The artifact is now verifiable (§4); its inputs are
  only as trustworthy as the filesystem.

`R/model.R:799` currently tells users to use `force_recompile = TRUE` to apply
options. Under this design options apply on their own, so that message needs
rewriting.

---

## 7. Executable-only models

`cmdstan_model(exe_file = ...)` is a first-class workflow today, including with no
Stan source at all (`R/model.R:156`). Earlier drafts omitted it entirely, and three
of their statements are impossible for it: that `cmdstan_model()` always compiles,
that a missing record causes a rebuild, and that pre-record executables get a
one-time rebuild.

**They are preserved, and they split into two cases.** An earlier draft called them
all unprovenanced, which discards information we may have written ourselves —
`compile_stan_file()` followed by `cmdstan_model(exe_file = path)` is a first-class
flow under this design, and it produces an executable *with* a record.

**Executable plus a valid hash-bound record.** Artifact provenance is *known*: the
record describes this binary, verified by the hash in §4. Report it.
`stan_build_info()` returns the build information, not "unavailable." Source
freshness may still be unverifiable — if the recorded sources are absent or the
paths no longer resolve, say so specifically rather than collapsing it to unknown
provenance.

**Executable without a usable record** — missing, corrupt, or hash mismatch.
Explicitly unprovenanced: read whatever metadata the executable reports as today,
and have `stan_build_info()` return an explicit *unavailable* result rather than an
empty one that could be mistaken for "nothing was configured."

In both cases: permit fitting, and **never attempt an automatic rebuild** — there
is no source to build from. They are the deliberate exception to §5's requirement
that a model have a valid record before running.

Rejecting executable-only models would also be coherent for v1, but it is a
substantial capability removal and would need its own argument. None is made here.

**Pre-record executables** — anything built before this work — are a separate case:
they *have* source, so they can be rebuilt. **Migration happens during the
explicitly requested `cmdstan_model()` call, with the reason printed**, rather than
erroring and demanding `force_recompile`. The user asked for a model; a one-time
rebuild with a stated cause is the least surprising way to give them one. This is
the same rule as §5 — the constructor rebuilds, operations error.

```
#> Recompiling: this executable predates build records, so what it was
#>   built with cannot be determined.
```

---

## 8. Removing deferred compilation

`cmdstan_model(compile = FALSE)` goes. 96 uses in tests, 10 in `R/`, 9 in `man/`,
5 in vignettes.

The use cases it served were introspection, and they are better served by functions
that never needed an object. Names follow cmdstanpy where a counterpart exists, and
cmdstanr's existing `write_stan_file()`, rather than inventing a third convention:

```r
compile_stan_file(file, cpp_options = NULL, stanc_options = NULL, ...)  -> exe path
format_stan_file(file, ...)
check_syntax_stan_file(file, include_paths = NULL, ...)
stan_variables(file, include_paths = NULL, ...)
stan_build_info(exe)
```

`compile_stan_file()` and `format_stan_file()` match cmdstanpy exactly.
cmdstanpy has no standalone syntax check, and its `src_info()` is lower-level than
`$variables()`, so those two are cmdstanr-only and named to fit R. **Where
cmdstanpy already has a name we copy it; where it does not, we pick one and they
can copy it if they add a counterpart.** The two APIs are taught together, so
parity matters, but nothing here waits on a joint naming decision.

`model_variables()` at `R/model.R:2657` is already this shape internally.

### `compile_stan_file()` is exported, and shares one implementation

An earlier draft said to export it only if a consumer committed to it, on the
grounds that citing `instantiate` was speculative. That was the wrong bar. The
better argument is parity: cmdstanpy already has `compile_stan_file`, and having
`format_stan_file()` and `check_syntax_stan_file()` public while the compile step
is not is arbitrary — with `compile = FALSE` gone there would be no way to build
without constructing an R6 object.

**One implementation, two entry points**, so nothing is duplicated:

```
compile_impl(stan_file, cpp_options, stanc_options, include_paths,
             user_header, dir, force_recompile, quiet, dry_run)
    -> list(path =, record =, src_info =, hpp_code =)

compile_stan_file(...)   # exported: compile_impl(...)$path
cmdstan_model(...)       # exported: R6 object built from all four
```

This is a lift of today's `$compile()` rather than a rewrite — the stanc and make
invocation moves unchanged, and 12 of its 31 `private$` touches are `precompile_*`
fields this issue deletes anyway. What remains resolves into arguments in and
values out.

Four constraints:

- **The internal returns more than a path.** Otherwise `cmdstan_model()` re-reads
  the record and re-runs stanc, which is duplication in its most wasteful form. It
  needs `record` for `$cpp_options()` and `$cmdstan_version()`, and `src_info` for
  the eager `$code()`/`$variables()` snapshot (§5) — which is the `stanc --info`
  call the build already makes.
- **`hpp_code` answers #1245's discriminator.** Populated when a build ran, absent
  when an executable was reused, which is exactly the "is there generated C++?"
  half of the two independent questions that issue needs.
- **`dry_run` lives on the internal only.** The single argument the public wrapper
  omits, which makes `compile_stan_file()` a genuine wrapper rather than a
  re-export — but a three-line one.
- **`compile_stan_file()` performs the same up-to-date check**, reusing a current
  executable rather than always compiling. The verb suggests otherwise, so this
  needs documenting; one operation behaving two ways depending on entry point is
  the inconsistency this design exists to remove. It writes the record too, so a
  later `cmdstan_model(exe_file = path)` finds it and knows provenance (#1238).

`force_recompile` keeps cmdstanr's spelling rather than cmdstanpy's `force`.
Matching on the function *name* is what buys cross-implementation teachability;
matching every argument at the cost of internal consistency is not worth it.

**`stan_build_info()` returns a parsed object**, not the file. Users should not be
encouraged to depend on the record's on-disk format.

**`$format()` gets a standalone plus a method wrapper.** An earlier draft argued it
must stay a method because it invalidates `stan_code_` and `variables_`
(`R/model.R:1309-1311`). That argument is weak: another model object or an external
editor already bypasses that invalidation, so it was never a guarantee. §5's
pre-run validation is what actually makes it safe.

**`dry_run` demotes to internal.** Its documentation says *"Used to speedup tests"*
(`R/model.R:558-559`); 22 test uses, zero vignette uses. It stays as an argument to
the internal compile machinery that the public entry points wrap.

### What this dissolves

**#1252** closes outright — it exists because `precompile_stanc_options_` leaks into
file operations, and with deferred compilation gone there is no precompile store.
Also removed: `precompile_include_paths_`, `precompile_cpp_options_`, the `%||%`
include-path baseline and `include_paths_dirty_` (`R/model.R:642-656`).

**Note on #1235.** That PR's include-path fix at `R/model.R:646-656` *is* the
persistence mechanism this design rejects. It should still merge — it fixes a live
bug against today's API, and this work deletes the mechanism rather than correcting
it.

**#1234's guarantee does not move into the record.** An earlier draft said it did,
which was true of a draft where the record was replayed and is false now (§2). What
actually happens is that the *lifecycle disappears*: #1234 exists because a second
build call — `$compile()` — could drop the include paths and user header the first
one supplied. Once deferred compilation and `$compile()` are gone there is no
second call, every build carries its own complete configuration, and there is
nothing left to drop.

---

## 9. Order of work

Three constraints shape this. The Make-option fixes come first, because per-field
canonicalization depends on them. **The API change and the decision engine ship as
one stage** — separating them leaves a window where the new promise is broken
whichever way the cut is made (Stage 4). And Stages 0–4 must all be in the release
candidate, because the API removal is the breaking change downstream packages need
to see; Stage 5 adds a function and breaks nothing, so it need not be.

### Stage 0 — landing in #1235

#1228, #1234 and the double metadata query (#1236). Independent of everything below.

### Stage 1 — Make-option correctness

**#1251** (logical `FALSE`), **#1250** (casing and raw assignments, including the
`+=` / `?=` / `:=` classification in §3), **#1230** and **#1232** (quoting and
escaping). All independent of the record, all prerequisites for comparing
configurations correctly.

### Stage 2 — schema and helper tests

Field separation (§1), executable hash, corruption, forward versions,
executable-only models. Parser, writer and comparison helpers tested against
fixtures.

**This stage is behaviour-free**, and should be kept that way: nothing writes a
record beside a user's Stan program until Stage 3. Name and format are settled
(§4); portability and the git-ignore story are not, and both need an answer *here*,
before anything creates a file.

### Stage 3 — transactional record writing

Executable-plus-record staged and committed together, with verification and the
rollback in §4. Locking deliberately excluded. This is where the file first
appears on disk; it is written but does not yet drive decisions.

### Stage 4 — the API change and the decision engine, together

Removing deferred compilation and `$compile()`, adding the standalone family (§8),
**and** the constructor decision engine: **#1019** and **#1237**, the triggers in
§6, include re-resolution, and §5's assessment with its two caller behaviours.
Closes **#1252**, likely closes **#1253**.

**These cannot ship separately, in either order.** An earlier draft split them and
said combining was optional. It is not — the intermediate state is broken whichever
way it is cut:

```r
mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE))
```

If `$compile()` is removed before configuration mismatches trigger rebuilds, an
existing unthreaded executable is still reused under today's decision logic while
the only escape route is gone. That breaks the central promise of the new API —
that supplied options apply — in the window between the two stages.

### Stage 5 — public build-record inspection

`stan_build_info()` last, once the schema has stabilised under real use — and the
release-candidate period is that use. Purely additive, so it can land after the
candidate, or after 1.0.

### The release candidate

A 1.0 candidate ships after Stage 4, so packages built around precompiled models —
`instantiate` most directly — have something to migrate against rather than a
release note. That is what makes §8's breaking change affordable.

One scheduling constraint follows. The repo-wide formatting and linting work
(#1153, #1172) lands either before Stage 1 or after 1.0, never between Stage 4 and
the candidate: a reformatting diff on top of the API removal leaves a downstream
maintainer unable to see what actually broke.

### How the stages are executed

One pull request per stage, merged to master, green and revertable on its own.
Long-lived integration branches are the wrong unit here — #1235 alone ran to sixty
commits across weeks of review, and a branch held open across two stages spends
more time being rebased than reviewed.

Stage 4 ships as one pull request but is built in two parts: the assessment engine
as a pure function with its full decision table, tested and unwired; then the wiring
and the API removal. Most of the risk is retired before anything user-facing moves.

Parallel work is bounded by what the tests contend for, not by what a checkout
isolates. `make/local` lives in the CmdStan installation, the precompiled headers
are keyed by `STAN_FLAGS` in that same installation, and parts of the suite reach
`rebuild_cmdstan()`. Separate checkouts separate none of that, so **only one
compiling task runs at a time** — and a run killed part-way leaves residue that the
next run snapshots as its baseline. What does parallelise is the work that never
compiles: record fixtures, the downstream-usage inventory, documentation and test
migration once the API commit exists, and adversarial review of a finished stage.
That last is worth a reviewer rather than another implementer; this document
reached its current form through five review rounds.

### Independent, can land any time

**#1245**, **#1246** (error message quality) and **#1249** (`$cmdstan_version()`
reports the installed CmdStan, not the one that built the executable — caused by
`R/model.R:318`, not `dry_run`). Small, user-visible, and the natural work to pick
up while Stage 0 is in review.

### Issue consolidation

Done. #1247, #1248 and #1252 are closed. #1237, #1238, #1245, #1246, #1249, #1250
and #1253 have been rewritten against this document. #1255 (rebuild decisions),
#1256 (removing deferred compilation, adding the standalone family) and #1257
(untracked dependencies) are new. #1019 closes when #1255 does.

---

## 10. Notes for whoever implements this

**The record describes; it does not authorise.** It is not permission to run
whatever is at the executable path — that is what the artifact hash is for. Code
that reads the record and proceeds without verifying the pair reintroduces exactly
the class of bug this design exists to remove.

**Two checks that look alike and are not.** "The record disagrees with what was
asked for" means rebuild. "The record cannot be read" means there is nothing to
disagree with. Conflating them makes an unreadable record silently equivalent to a
matching one.

**Absence of evidence is not evidence of absence — twice.** `reported_features` is
tri-state (§1): a feature CmdStan does not report is *unknown*, and treating
unknown as disabled reproduces #765 in a new place. `known_untracked_dependencies`
(§6) is the same shape: an empty list means nothing was *detected*, never that the
record is complete. Both drafts of this document got one of these wrong, so it is
worth checking for deliberately rather than trusting the field names. JSON adds a
third way to get it wrong: a tri-state field has to round-trip *unknown* as
distinct from both absent and `false`, which is a property to test rather than
assume of the serializer.

**The assessment is pure.** The operation that answers "is this executable
current?" must not compile, install, or mutate object state (§5). Callers decide
what to do about the answer — the constructor rebuilds, everything else errors. A
convenience rebuild tucked inside the assessment reintroduces the hidden
recompilation this design removed.

**Naming is open.** `stan_build_info()` and `check_syntax_stan_file()` are
placeholders, to be settled in the stage that implements each (§8). So is
`<model>.cmdstanr.json` (§4), with the difference that changing it after the
release means migrating records that already exist.
