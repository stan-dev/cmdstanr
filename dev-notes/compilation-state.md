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
should read once the issues have been brought into line. What this note adds is
the part no issue can carry: why the contracts are what they are, why the work is
ordered this way, and which tempting alternatives were rejected.

> **The issues have not been updated yet, and several now contradict this draft.**
> Until they are, **this note is the current copy** — the reverse of the usual
> rule. See "Issues that will mislead you right now" below before reading any of
> them.

**In scope:** what is recorded about an executable and when; what a configuration
means when it reaches `make`; when the record is validated; what can and cannot be
known about an executable cmdstanr did not build.

**Out of scope:** toolchain and installation paths.

**Relation to the 1.0 milestone.** Every issue in this area is milestoned
`v1.0.0 - release`, so the milestone is mostly this work — but not only: it also
holds linting (#1172), formatting (#1153), interactive installation (#605) and the
`num_chains` argument (#534), none of which touch these contracts.

---

## What changed from the previous draft

Two drafts have been superseded. The first was built on **persistent options**
(#1248) and **deferred compilation**; the second replaced those with a one-shot
rule and a build record, but treated that record as authoritative in ways it
cannot support. Both revisions came from review, and both are recorded here rather
than quietly folded in.

**On persistence.** #1248's case was not "persistence is nicer." It was that
one-shot semantics force #1019 to invent a third lifecycle: once cmdstanr rebuilds
on its own initiative, it has to build with *something*, and under one-shot the
honest answer is "nothing — the previous compile consumed the options."

That objection was correct, and it is answered rather than dismissed: **rebuilds
the user did not request are no longer replayed from stored configuration at all.**
They error and ask for an explicit rebuild (§5). Nothing needs to persist, in the
object or in the record, for that to be safe.

**On the record.** The second draft claimed "the record is the state" and treated
it as the single answer to what a binary is. It cannot be, for two reasons that
review made plain: a record does not prove *which* executable it describes, and a
record of resolved call arguments is not the same fact as what a binary actually
has enabled. §1 and §4 are rewritten accordingly. The record is a provenance
manifest — it describes, it does not authorise.

| | First draft | Second draft | This draft |
|---|---|---|---|
| Configuration between builds | object fields | the record | not replayed at all |
| Unrequested rebuild | persisted config | recorded config | errors, asks the user |
| Binds record to executable | — | atomic rename (wrong) | executable hash |
| What a binary has enabled | merged into one list | resolved arguments | separate reported field |
| `$compile()` | takes options | takes none | removed |

### Issues that will mislead you right now

None of the issues have been updated — that work is deliberately held until the
design settles, so it does not have to be redone. Read them knowing:

- **#1248** is the sharpest trap. Its *title* asserts the decision this draft
  reverses, and its comments carry a written decision to persist options for 1.0.
  That argument is answered above, not ignored, but nothing on the issue says so.
  It will close as won't-do.
- **#1252** will close. Its premise — stanc options meant for compilation leaking
  into `$check_syntax()` and `$format()` — disappears with the precompile store.
- **#1253** will probably close, once `dry_run` is internal (§8).
- **#1247** folds into #1238; canonical option spelling becomes part of the record.
- **#1250 grows in priority.** An earlier draft said it shrinks. That was wrong:
  per-field canonicalization (§4) needs the named/opaque classification to know
  which treatment applies, so it is a **prerequisite** for the record, not a
  nice-to-have.
- **#1019** grows, to the triggers in §6 including `make/local` and CmdStan
  identity.
- **#1238** will absorb the exe-vs-record bug (§4), the executable hash, and
  transactional installation. Its current text describes only a passive record.
- **#1249** is substantively unchanged, but this draft corrects its attribution:
  the cause is `R/model.R:318` clobbering unconditionally on construction, not
  `dry_run`.

Unaffected and safe to read as written: **#1251**, **#1230**, **#1232**, **#1245**,
**#1246**, **#1237**, **#1025**.

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
- **`artifact`** — which executable this record describes.
- **`builder`** — which CmdStan installation produced it.

`reported_features` is **tri-state and best-effort**: each feature is *known
enabled*, *known disabled*, or *unknown*. `<exe> info` reports what CmdStan chooses
to report — threading, OpenCL, Stan version — not arbitrary flags. **Absence must
never be read as disabled.** `$cpp_options()` merges `request` with
`reported_features` as the current code already does (`R/cpp_opts.R:78`), without
claiming completeness.

**A model object is a handle on an executable plus its record.** It holds no
durable configuration of its own.

---

## 2. Contract: options are specified once, at `cmdstan_model()`

> Every call that builds specifies the configuration it wants. Omitting an option
> means you are not asking for it.

`cmdstan_model()` always compiles when given a Stan file. There is no
`compile = FALSE` (§8), and **`$compile()` is removed** — once deferred compilation
is gone it has no unique public purpose, and `cmdstan_model(file,
force_recompile = TRUE, ...)` covers every remaining use.

Removing it rather than narrowing it also avoids a trust problem. A `$compile()`
that rebuilds "as recorded" has to replay build arguments from a file, including
opaque Make arguments, which means it needs a strictly validated record schema
before it can be safe. An internal exact-rebuild operation may still be needed for
freshness checks, but it should not be a second public configuration lifecycle.

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
was constructed against, and §5's pre-run validation detects that another call or
process replaced it — reporting the changed configuration by name.

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

**This is a prerequisite for the record, not adjacent to it.** Per-field
canonicalization (§4) cannot be implemented without a correct named/opaque
classification, because the two kinds get different treatment.

---

## 4. Contract: the build record (#1238)

A file beside the executable describing how it was built.

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
executable, install the record, and verify the pair before reuse. If record
installation fails, restore the previous executable and fail the compile. An
earlier draft called record-write failure "non-fatal but visible" — that is
incompatible with §5, since an executable without a valid record is immediately
unusable.

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
| recorded vs. source hash | none | none |

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
- **Stanc options** — canonicalize per option semantics.
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
checked. Today `$format(overwrite_file = TRUE)` rewrites the Stan file (`R/model.R:1293`)
and updates cached source and variables (`:1309-1311`) but leaves the executable
untouched, and an external editor does the same. The object can then show new Stan code while `$sample()` runs the old
binary.

**Validation runs at construction and before every fitting operation.** It is cheap
— roughly 0.3 ms for a realistic source set — so there is no reason to skip it.

**Validation never compiles.** On stale source, changed configuration, a mismatched
executable/record pair, or a missing or corrupt record, it **errors** and tells the
user to create a new model with `cmdstan_model(..., force_recompile = TRUE)`.

Erroring rather than rebuilding is deliberate. A silent 30–90 second compile
appearing inside `$sample()` is worse than an actionable message, and it is exactly
the kind of latency users cannot predict or plan around.

---

## 6. Contract: when a rebuild happens (#1019)

A rebuild is triggered by any of:

- the Stan program changed
- a resolved include changed, or resolves differently now (#1237)
- the user header changed
- `make/local` changed
- the supplied `cpp_options` or `stanc_options` differ from `request`
- the selected CmdStan installation differs from `builder`
- `force_recompile = TRUE`

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

**Record, for each include: its spelling, its ordered search roots, and the path
selected.** Validation re-resolves that mapping. If a higher-priority root now
holds a candidate that was not selected before, the resolution changed and the
model rebuilds. This is stat calls, not a stanc invocation.

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
for the user header — tells us the record *cannot* be complete without resolving
anything. Set `provenance_complete: false` with the reason, and say so when
validation runs:

```
#> This model's build record is incomplete: make/local includes another
#>   makefile, which is not tracked. If you changed it, use force_recompile.
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

**Executable-only models are preserved, with provenance explicitly unknown:**

- read whatever metadata the executable reports, as today
- permit fitting
- never claim a record describes them
- never attempt an automatic rebuild — there is no source to build from
- `stan_build_info()` returns an explicit *unavailable / unprovenanced* result,
  not an empty one that could be mistaken for "nothing was configured"

They are the deliberate exception to §5's requirement that a model have a valid
record before fitting.

Rejecting executable-only models would also be coherent for v1, but it is a
substantial capability removal and would need its own argument. None is made here.

**Pre-record executables** — anything built before this work — are a separate case
from executable-only models: they *have* source, so they can be rebuilt. The
migration is a one-time rebuild, and it must be a deliberate decision with a
message rather than a silent 90-second surprise on first use after upgrade.

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
`$variables()`, so those two are cmdstanr-only and named to fit R. **The names
should be agreed with the cmdstanpy developers rather than each side guessing**,
since the two APIs are taught together.

`model_variables()` at `R/model.R:2657` is already this shape internally.

**Export `compile_stan_file()` only if there is a committed consumer.** An earlier
draft cited `instantiate` as motivation; that was speculative and should not be the
basis for public API. Keep the helper internal until demand is demonstrated.

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
it. #1234 stays fixed throughout; the guarantee moves from a private field to the
record. There is no window where it regresses.

---

## 9. Order of work

Reordered from earlier drafts: the Make-option fixes come first, because
per-field canonicalization depends on them.

### Stage 0 — landing in #1235

#1228, #1234 and the double metadata query (#1236). Independent of everything below.

### Stage 1 — Make-option correctness

**#1251** (logical `FALSE`), **#1250** (casing and raw assignments), **#1230** and
**#1232** (quoting and escaping). All independent of the record, all prerequisites
for comparing configurations correctly.

### Stage 2 — specify and test the record schema

Field separation (§1), executable hash, corruption, forward versions, executable-only
models. Specification and tests before behaviour.

**This stage is not behaviour-free.** It creates a user-visible file beside every
Stan program. Settle its name, location, portability and git-ignore story before
shipping — `.dep` is a placeholder and must not collide with anything `make` or
another build system claims in the same directory.

### Stage 3 — transactional installation

Executable-plus-record staged and committed together, with verification on reuse.
Locking deliberately excluded (§4).

### Stage 4 — the record drives decisions

**#1019** and **#1237**: constructor reuse, the triggers in §6, include-shadowing
re-resolution, and §5's pre-run validation.

### Stage 5 — the API change

Removing deferred compilation and `$compile()`, adding the standalone family.
Closes **#1252**, likely closes **#1253**.

### Stage 6 — public build-record inspection

`stan_build_info()` last, once the schema has stabilised under real use.

### Independent, can land any time

**#1245**, **#1246** (error message quality) and **#1249** (`$cmdstan_version()`
reports the installed CmdStan, not the one that built the executable — caused by
`R/model.R:318`, not `dry_run`).

### Issue consolidation

Held until this draft settles. The full list is in "Issues that will mislead you
right now" above. Net: #1248 and #1252 close outright and #1253 probably joins them;
#1247 folds into #1238; #1250 grows in priority; #1019 grows in scope. New issues
for removing deferred compilation, the standalone family, and the unsupported-
dependency documentation.

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

**Absence is not disabled.** `reported_features` is tri-state (§1). A feature that
CmdStan does not report is *unknown*, and code that treats unknown as disabled will
reproduce #765 in a new place.

**Naming is open.** `stan_build_info()`, `check_syntax_stan_file()` and `.dep` are
placeholders, and the standalone names should be settled jointly with cmdstanpy.
