# Compilation state and C++ options — intended behaviour

Status: draft for discussion. Describes the target behaviour of `cmdstan_model()`,
the build record kept beside an executable, and when that record is validated.

## Purpose and scope

This document exists because the defects in this area have not been independent.
#1228, #1234, the residual warning gaps, #1019 and #1237 all reduce to a small
number of contracts that were never written down, so each was rediscovered by
being violated. The intent is to state those contracts once, and to sequence the
remaining work off them rather than off the issue list.

**This note is the specification for these contracts, and is the copy to trust if
it and an issue disagree.** The issues carry implementation detail, reproductions
and progress, and link back to the sections here; they are not a second source of
truth. An earlier version of this paragraph said the opposite, which produced
exactly the drift it invited — §3 and #1250 came to contradict each other on raw
Make assignments, and the *document* was the stale copy.

What this note adds beyond the contracts themselves is the part no issue can carry:
why they are what they are, why the work is ordered this way, and which tempting
alternatives were rejected. Those rejected-alternative passages are kept
deliberately. They are longer than a summary would be, and they are what stops the
same rejected idea being proposed again each round.

The work is tracked in #1238 (the record), #1255 (rebuild decisions), #1256
(removing deferred compilation), #1257 (untracked dependencies), #1237 (includes),
#1250 (option classification) and #1251 (logical `FALSE`).

**In scope:** what is recorded about an executable and when; what a configuration
means when it reaches `make`; when the record is validated; what can and cannot be
known about an executable cmdstanr did not build.

**Out of scope:** the toolchain itself — compiler version, system libraries — which
is deliberately untracked (§6). The *CmdStan version* is in scope as `builder`
identity, as is whether the recorded installation still exists; the installation's
path is not compared (§6).

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
missing record, an executable predating records, a record from a newer cmdstanr, or
`force_recompile = TRUE`. Every applicable reason is reported, not only the first.

**What the record holds** — a hidden JSON file beside the executable,
`.<model>.cmdstanr.json`, holding the request; what the binary reports as enabled
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
- **`format_version`** — the record schema's own version, so a record written by
  a newer cmdstanr can be recognised as such and reported (§4).

`reported_features` is **tri-state and best-effort**: each feature is *known
enabled*, *known disabled*, or *unknown*. `<exe> info` reports what CmdStan chooses
to report — threading, OpenCL, Stan version — not arbitrary flags. **Absence must
never be read as disabled.**

**The two are never merged into one accessor.** `$cpp_options()` reports the
request. `stan_build_info()` reports what the binary says, with its provenance. And
**runtime validators read `reported_features` directly, never a merged convenience
list.** A merged structure answers neither question: it looks complete but is not,
and it cannot represent *unknown*, so a requested `TRUE` sitting over an unknown
reading survives as a plain `TRUE` and the table below is silently bypassed.

`merge_exe_info_cpp_options()` (`R/cpp_opts.R:78`) is what this replaces, and it
already demonstrates the failure. It **drops FALSE values** — the comment explains
that passing `FLAG=FALSE` back to CmdStan can enable the flag, which is #1251 — so a
binary reporting `STAN_THREADS=false` contributes nothing, and known-disabled is
already indistinguishable from never-reported. Fixing #1251 removes the reason for
dropping FALSE but not the representational gap: `cpp_options` is a *request*
structure, and has nowhere to put *unknown*.

Little is given up by separating them. `<exe> info` reports four `STAN_*` booleans
and the Stan version; `CXX`, `CXXFLAGS`, `PRECOMPILED_HEADERS` and everything else
have no reported counterpart and are request-only regardless.

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

`assert_valid_threads()` (`R/cpp_opts.R:282`) is the case to fix. Asking for threads
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
exceeds the request, and the run is correct — merely serial. cmdstanr errors on this
today (`R/cpp_opts.R:297-303`), and **that error is removed.**

The rule is asymmetric, because the two directions are not equally expensive:

| Runtime request | Feature enabled | Feature disabled or unknown |
|---|---|---|
| `threads > 1` | proceed | **error** |
| `threads == 1` | proceed | proceed |
| no `threads` argument | proceed | proceed |

Only the top-right cell is the expensive silent failure — parallelism asked for and
not delivered, discovered after a run that took four hours instead of one. Omitting
the argument, or asking for a single thread, requests no parallelism at all, so
there is nothing to fail to deliver.

The current `stop()` is wrong in three ordinary situations. A model with no
`reduce_sum` or `map_rect` cannot use threads at all, yet is required to supply the
argument. Running `parallel_chains = 4` with one thread each is a sensible
configuration that must currently be spelled `threads_per_chain = 1` to avoid an
error. And `STAN_THREADS=true` in `make/local` is a single global setting that makes
every model on the machine error by default.

Its message also gives the wrong advice:

```
The model executable was built with threading enabled but 'threads_per_chain'
was not set!
```

If what concerns the user is paying for threading they do not use, the remedy is to
build without it — not to start passing `threads_per_chain`.

**Threading is not free when unused, and that is a documentation point rather than a
runtime one.** With `STAN_THREADS` defined, Stan Math makes the autodiff stack
pointer thread-local, on the hottest path in the program. The cost has been
engineered down deliberately — a pointer is used so the TLS can be
constant-initialized, and the `__thread` extension is preferred over `thread_local`
because only it "guarantees that constant initialization and its implied speedup"
(`stan/math/rev/core/autodiffstackstorage.hpp`). Small and constant, not structural.
Worth saying where users choose whether to enable threading globally; not worth a
runtime message, since anyone who put it in `make/local` chose it deliberately, and
`stan_build_info()` reports it on demand.

Five assertion sites in `test-threads.R`, plus snapshots, encode the old behaviour
and will change.

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

- On the command line, a **lone** `+=`, `?=` or `:=` collapses to `=`. Two
  assignments to the same variable do **not** — see the rejection rule below.
- A command-line assignment has command-line *origin*, which **blocks** any
  makefile-side `+=` to the same variable.
- Both properties propagate into sub-makes via `MAKEFLAGS`.

In `make/local` — a file — `+=` is real and appends. cmdstanr's own `+=` usage
(`CXXFLAGS` at `R/install.R:258` and `:269`; `CPPFLAGS_SUNDIALS` at
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
fall through to `opaque`. But the collapse is only true of an assignment appearing
**alone**. Two assignments to the same variable retain operator semantics
(GNU Make 3.81):

```
make 'FOO=x'                -> FOO=[x]       # alone: all four collapse
make 'FOO+=x'               -> FOO=[x]
make 'FOO?=x'               -> FOO=[x]
make 'FOO:=x'               -> FOO=[x]

make 'FOO=base' 'FOO+=x'    -> FOO=[base x]  # in combination: they do not
make 'FOO=base' 'FOO?=x'    -> FOO=[base]
make 'FOO+=x'   'FOO+=y'    -> FOO=[x y]
make 'FOO+=x'   'FOO:=y'    -> FOO=[y]
make 'FOO=a'    'FOO=b'     -> FOO=[b]
```

**So raw assignment-shaped entries are rejected, not reclassified.** A previous
draft of this section said `list("FOO+=x")` and `list(foo = "x")` "must classify
identically", reasoning from the single-assignment collapse alone. That is false
whenever a second assignment to the same variable exists, and supporting these
correctly would mean preserving and interpreting an ordered assignment program —
real complexity for no user benefit. #1250 already specifies the rejection; this
section was the stale half.

**Rejection is what makes §4's canonicalization sound.** With only named entries
reachable, only `=` is ever emitted, so "last assignment wins" is correct. The two
rules are not independent: accepting raw operators would invalidate the
canonicalization rule as well.

**Plain `NAME=value` is rejected too**, though it is unambiguous, because it is the
spelling that causes the worst live bug: raw `USER_HEADER=my.hpp` compiles the
header in while `resolve_user_header()` never sees it. Accepting raw `=` means
re-implementing every special-cased variable on the raw path. Nothing is lost —
named entries already emit `NAME=value` (`R/cpp_opts.R:141`), so the migration is a
spelling change and the error can name the form to use.

**This applies to `cmdstan_model(cpp_options = )` only.** `cmdstan_make_local()`
passes unnamed entries through verbatim (`R/install.R:332-333`) into a *file*, where
`+=` is real and is not otherwise expressible — cmdstanr's own documented example
(`man/install_cmdstan.Rd:137-142`) is exactly this:

```r
cpp_options <- list("CXX" = "clang++", "CXXFLAGS+= -march=native", ...)
```

That must keep working. `+=` there preserves an environment-provided `CXXFLAGS`
where `=` discards it, and since `append = TRUE` is the default, repeated calls
accumulate rather than clobber. The asymmetry is principled: on the command line
named entries cover every case one-to-one, and in a makefile they do not.

**This is a prerequisite for the record, not adjacent to it.** Per-field
canonicalization (§4) cannot be implemented without a correct named/opaque
classification, because the two kinds get different treatment.

---

## 4. Contract: the build record (#1238)

A file beside the executable describing how it was built. JSON, named
`.<model>.cmdstanr.json` — so `bernoulli.stan` compiled to `bernoulli` is described
by `.bernoulli.cmdstanr.json` in the same directory.

**The leading dot is deliberate: this is not a file users are expected to open.**
`stan_build_info()` is the supported way to ask what an executable is (§1), so the
record is an implementation detail rather than a user-facing artifact, and hiding it
on macOS and Linux keeps a project directory from filling with files nobody needs to
read. Windows does not hide by naming convention, but it shows the same file either
way, so the dot costs nothing there.

JSON still earns its place, just not for the reason an earlier draft gave. Not
because users read it — they should not have to — but because `jsonlite` is already
an import, a parse failure hands us §4's "unreadable" case for free, and a
text format makes bug reports, golden fixtures and our own debugging tractable. The
name also stays clear of `.dep` and `.d`, which `make` and the C++ toolchain already
claim in that directory.

**Implementation trap:** `list.files()` defaults to `all.files = FALSE` and will not
see the record. Anything enumerating files beside a model must opt in.

Both name and format remain revisable up to the release, so nothing outside the
reader and the writer should depend on either.

### Keeping the record out of version control

**The record should be ignored, and cmdstanr has to say so** — the leading dot
hides it from `ls`, not from git, so `git add -A` commits it silently.

A committed record is worse than no record for whoever checks it out. It holds a
hash of an executable that does not exist on their machine and an absolute path to
the CmdStan installation that built it, so it fails validation and rebuilds anyway,
having produced a diff on every rebuild in the meantime. Nothing is lost by ignoring
it: the record is derived data, and a missing one costs a single rebuild (§6). The
pattern to document is `.*.cmdstanr.json`.

**`.Rbuildignore` needs the same entry**, which is easy to miss. `R CMD build`
excludes hidden files by a fixed list (`tools:::.hidden_file_exclusions`), not by
leading dot, so this name is not covered — a package author who compiles in a source
tree ships records inside the tarball describing their own machine. That is the case
that reaches instantiate-style packages.

**cmdstanr does not write either file itself.** Compiling a model should not modify a
user's repository configuration; the recommendation belongs in documentation.

**This repository needs the patterns too, before Stage 3 writes anything.**
`tests/testthat/resources/stan/.gitignore` enumerates fifteen compiled binaries by
hand and is already behind — four models there have no entry, and two generated
`.hpp` files sit untracked beside them. Every compiled test model will add a record,
so replace the enumeration with patterns rather than extending it.

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

### The record is parsed into an object, and compared as one

JSON is how the record is *stored*, not how it is reasoned about. Reading one yields
a structured R object, and the request assembled at `cmdstan_model()` is built into
the same shape, so deciding whether to rebuild is a field-by-field comparison of two
objects rather than text matching or a single equality test. That is what lets §6
report every applicable reason instead of the first, and it is why the rules below
are per field.

The two are separable concerns, worth keeping separable. The object model is what
delivers the comparison and the reasons; the format is only how bytes reach disk.
JSON earns its place there on different grounds — a parse failure is exactly the
"unreadable" case §4 needs, and getting it from `jsonlite` leaves only the
`format_version` check to write by hand. RDS would round-trip R types exactly, which
would remove the tri-state hazard in §10 for free, but at the cost of an opaque
binary beside the user's model and a second forward-compatibility surface under our
own. Not worth it while the tri-state property is testable.

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

A record written by a newer cmdstanr **rebuilds, and says so**, exactly like an
executable that predates records (§7). It is not refused and does not require
`force_recompile`.

An earlier draft refused automatic replacement on the grounds that overwriting a
record from a version that knows more than we do destroys something. It does not:
the record is entirely *derived* data, and every field in it can be regenerated by
rebuilding. What that rule protected cost thirty to ninety seconds, and it bought
that protection with an error in the one situation where it actually arises.

Which is worth naming, because it is not the one the rule was written for. Passing
an executable between machines is barely possible — the binary links TBB at an
absolute path inside the CmdStan installation that built it (§6), so it may not
launch elsewhere at all. The realistic case is **one person on one machine
downgrading cmdstanr**: install the development version, build some models, revert
to the release. Same CmdStan, same `make/local`, same paths, so nothing else
triggers and this rule is the only thing in the way. Erroring there is hostile for
no gain.

The real hazard was never the record. It is that a newer cmdstanr may have built the
executable under option semantics this version no longer implements, so rebuilding
can produce a *differently built* binary. That is an argument for saying loudly what
happened, not for refusing:

```
#> Recompiling: this model's build record was written by a newer version of
#>   cmdstanr (format 3; this version understands 2), so how the existing
#>   executable was built cannot be verified.
```

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
the fitting methods. "At least" is not implementable, so the full public surface is
classified here: `CmdStanModel` carries **twenty-seven public methods and one public
field**, and every one appears below. `cmdstan_model()` is listed as the builder but
is not itself a member, so it does not count toward either total.

| Behaviour | Members |
|---|---|
| **Validate, and error on any trigger** | `$sample()`, `$sample_mpi()`, `$optimize()`, `$laplace()`, `$variational()`, `$pathfinder()`, `$generate_quantities()`, `$diagnose()`, `$cmdstan_defaults()`, `$expose_functions()` |
| **Rebuild, printing every reason** | `cmdstan_model()` itself — the constructor, and the only builder |
| **Snapshot of the built model; no validation** | `$code()`, `$variables()`, `$print()`, `$functions` |
| **Accessor; no validation, never errors** | `$stan_file()`, `$has_stan_file()`, `$model_name()`, `$exe_file()`, `$include_paths()`, `$cmdstan_version()`, `$cpp_options()` |
| **Operates on source, not the binary; no validation** | `$check_syntax()`, `$format()` |
| **Generated C++; no validation** | `$hpp_file()`, `$save_hpp_file()` |
| **R6 plumbing; no validation** | `$initialize()`, `$clone()` |
| **Removed** | `$compile()` (§8) |

Two entries need their reasoning stated rather than inferred.

**`$check_syntax()` and `$format()` never touch the executable** — they run `stanc`
against source. Validating there would demand a current binary in order to answer a
question the binary is irrelevant to, and would make a syntax check on a model whose
executable is stale fail for the wrong reason.

**Functions exposed by `$expose_functions()` are a snapshot, like `$code()`.** The
validation happens at exposure; the resulting entries in `$functions` are plain R
bindings over separately compiled code, and re-validating on every call is neither
practical nor meaningful — they are not the CmdStan binary. A recompile drops them
and they must be exposed again, which is already the behaviour #1228 established.
The `$functions` field itself is classified with them, for the same reason.

**`$initialize()` and `$clone()` are R6 plumbing**, listed because an unlisted member
is indistinguishable from an overlooked one. Neither is guarded. `$initialize()` is
the constructor: it is reached through `cmdstan_model()`, nothing in cmdstanr calls
it on a live object, and no supported workflow calls it directly. Invoking it a
second time would retarget private state, but writing a guard for that is error
handling for a case nobody arrives at.

`$clone()` copies state and derives nothing from the binary. It needs one note,
because it looks like a defect until it doesn't: `functions` is an environment
(`R/model.R:264`) and there is no `deep_clone` method, so a clone *shares* that
environment with the original and exposing functions on either is visible on both.
That is safe precisely because §8 makes a model immutable after construction. The
two objects describe the same executable permanently and cannot diverge, so a shared
exposure is valid for both.

**The `$exe_file(path)` setter is removed** (`R/model.R:365-370`). It assigns
`private$exe_file_` with no validation, no snapshot refresh and no provenance
update, so under this design it would leave an object holding a record that
describes a *different* binary — the exact pairing §4 exists to prevent. The getter
stays and must keep not erroring (§1). Retargeting is done by constructing a new
object, which is the same answer §8 gives to the rest of the mutable-configuration
surface. Its only call site is `test-model-compile.R:1535`, in a test built on
`compile = FALSE`, so it retires with that. Folded into #1253.

Cost is **~8.8 ms**: source hashes plus the executable hash. An earlier draft cited
0.3 ms, which counted only the sources and predates the artifact hash in §4. If
include re-resolution runs here too (§6) add ~30 ms. Still negligible against a
sampling run, and the accurate number belongs in the document.

### The verdict is not state, and must not be stored as state

Assessment reads the filesystem each time it is asked, and its answer is good only
for that moment. That matters most where a model object outlives the machine that
built it, which is not hypothetical: brms keeps a whole `CmdStanModel` in
`attributes(fit$fit)`, so a saved `brmsfit` carries our object — and its recorded
absolute paths — to wherever the fit is next opened.

That case needs nothing extra. Where the executable is absent the assessment fails
and the operation errors; where a *different* executable sits at the same path, the
artifact hash (§4) catches it rather than running the wrong binary. Both fall out of
the contract above.

What would break it is memoization. **~8.8 ms per operation is exactly the number
someone later decides to cache on the object**, and a cached verdict is right within
a session and wrong the moment the object is deserialized somewhere else. That
failure is silent and looks like a caching bug rather than a violated contract,
which is why it is worth prohibiting here while the reason is still visible.

One knock-on for consumers, stated at the right level. brms currently decides
whether to rebuild by reading `$exe_file()` and calling `file.exists()` on the
result (`brms/R/backends.R:377-383`), which is how a relocated fit gets recompiled
before anything asks us for a verdict at all. That code is ours to change, so it is
not a constraint on #1253. The **capability** is: a caller must be able to ask
whether a usable executable exists **without triggering an error**. Whether that
stays `$exe_file()` returning something benign, or becomes a public form of the
assessment above, is open. Leaving no way to ask is not.

### Introspection is a construction-time snapshot

Pre-operation validation does **not** make cached introspection safe: `$code()` and
`$variables()` can still be stale after an external edit. The rule is that they
describe **the source the executable was built from**, not the file as it is now.

That is the more correct answer rather than a compromise — `$code()` returning the
current file would show code the binary does not have. It also does not undo #1228,
which was staleness after a **recompile**; a snapshot as of the last compile fixes
exactly that case.

**The snapshot must be captured eagerly, or it is not a snapshot.** `$variables()`
parses from disk on first call (`R/model.R:1032`), so an edit made before that first
call would return information about the *new* source while claiming to describe the
built one — the contract violated by the mechanism meant to implement it.

**`$format(overwrite_file = TRUE)` must stop refreshing the cache**
(`R/model.R:1308-1312`). It rewrites the Stan file and then reassigns `stan_code_`
and clears `variables_`, which makes both accessors describe a source the executable
was *never* built from — #1228's failure in the opposite direction. #1235 added that
refresh deliberately, under the older contract where `$code()` meant "the file as it
is now"; the accessors have since been redefined and this has not caught up.

Removing those lines is the whole fix. **`$format()` is kept, overwriting included** —
a reviewer proposed removing the method, but rewriting the file is the useful part
and is not what breaks anything. With the refresh gone: the file changes, the
snapshot keeps describing the built source, the Stan file's content hash no longer
matches, and the next operation that runs the binary errors and points at
`cmdstan_model()`. So **reformatting forces a recompile**, which is correct rather
than unfortunate — the bytes changed, and whether the build is unaffected cannot be
known without doing it. No warning is needed, because §5 already says this about
external edits and formatting is only an edit cmdstanr performs on the user's behalf.

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
- the CmdStan version differs from `builder`, or the recorded installation is gone
- the executable does not match `artifact` — replaced by another process, or corrupt
- the record is missing, unreadable, or written by a newer cmdstanr
- the executable predates build records, so there is nothing to compare
- `force_recompile = TRUE`

The middle three are *artifact-side*: reasons the recorded facts cannot be trusted,
rather than reasons the inputs changed. They belong in the same list because the
constructor's response is identical — rebuild, and say why.

**A record from a newer cmdstanr is in that list too** — it rebuilds, with the
reason stated (§4). It is still worth *distinguishing* from an unreadable record,
because the two warrant different messages: one says the file could not be parsed,
the other says which format version was found and which is understood. Same
behaviour, different diagnosis.

**Dependencies are identified by path and content together.** A dependency matches
only when its normalised absolute path *and* its content hash both match what the
record holds. Moving a project therefore rebuilds it, once, and the rebuild reason
names the cause: the recorded build location differs from the current one.

**Content-only identity was tried and rejected**, because the executable is not
location-independent. stanc embeds the absolute path of the Stan file and of every
resolved include into the generated C++ `locations_array__` — verified against 2.39,
six occurrences for a twenty-line model plus one per included file — and those
strings are compiled in and surface in every runtime exception:

```
Exception: normal_lpdf: Scale parameter is 0, but must be positive!
  (in '/old/path/model.stan', line 12, column 2 to column 30)
```

So a relocated executable is not equivalent to a rebuilt one. It computes the same
answers and reports them against a location that no longer exists, indefinitely,
until something else triggers a rebuild.

The variant that keeps relocation free — compare content, but store an immutable
`built_from` path so the manifest still records where the artifact was actually
built — is coherent, and was rejected on cost rather than correctness. It removes
one field comparison and adds six things to explain: the built-from versus
current-resolution split, why the record is not rewritten after a move, stale paths
in exception messages, the copy-and-run trap, how `stan_build_info()` presents a
build location that is gone, and an explicit rule that request paths are never
compared. Five of those exist only to explain why something that looks wrong is
fine.

What this costs is narrow. Normal development, branch switching, CI checkouts and
container builds all keep stable paths and are unaffected; moving a project costs a
single compile. Defining project roots, symlink behaviour and out-of-project paths
stays rejected, and under path identity there is nothing left to define.

**Moving everything except the record is fine.** The record is hidden (§4), so a
`cp *` or a drag-select will leave it behind. Where source is available that is a
missing record, which rebuilds once and writes a new one — a single compile, never a
wrong answer. For an executable-only model (§7) there is nothing to rebuild from, so
a lost record costs provenance rather than time.

**CmdStan identity rules.** The comparison is the **normalised installation path and
the version**, plus the `make/local` hash, which is its own dependency with its own
trigger rather than part of `builder`.

**A different path at the same version is a rebuild reason.** Selecting another
installation with `set_cmdstan_path()` is a deliberate act, and under a version-only
rule it would have no effect at all: the executable would still be the one the old
installation built, still linked against its TBB at an absolute path inside it
(below), while validation ran the new installation's `stanc`. Rebuilding is what
makes the selection mean something.

Version-only identity was tried and rejected. The argument for it was that nothing
nameable distinguishes two same-version installations — which is nearly true, since
`make/local` lives inside the installation and is hashed separately, so switching
usually rebuilds on that alone. But that makes path identity nearly free rather than
unnecessary. It adds a rebuild only when both installations are configured
identically, which is precisely the case where the user's explicit choice would
otherwise be ignored silently. "The installation that built it" is also the simpler
rule to state.

**The recorded installation must still exist**, which is a separate rule with a
concrete mechanism behind it. Stan Math links model executables against TBB at an
absolute path inside the installation (`TBB_BIN_ABSOLUTE_PATH`,
`stan/lib/stan_math/make/compiler_flags:279`), so a binary carries a hard reference
to the installation that built it. If that installation is gone, the executable may
not launch at all, and the model is rebuilt against the current one.

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

**Normalisation: normalised absolute paths, compared as identity.** `included_files`
comes back from `stanc --info` as absolute paths. Each entry is compared by path and
content together (§6), so a moved project rebuilds and the record is rewritten with
the new locations. Normalising first is what keeps an unchanged project from
rebuilding because a path was merely spelled differently.

Relocatable *records* remain a separate and rejected idea: storing paths relative to
some root would require defining that root, symlink behaviour, and what to do with
paths outside the project. Content comparison delivers what motivated it without any
of that. The case where rebuilding is genuinely impossible — no source — is covered
by executable-only models (§7).

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
- **CmdStan or Stan Math modified in place.** A patch applied, or a checkout
  updated, at the same path and version. The version is unchanged, `make/local` is
  unchanged, and nothing else is recorded, so this is invisible and needs
  `force_recompile = TRUE` (#1257). Tracking a git identity was considered and
  rejected: `install_cmdstan()` unpacks a tarball, so a typical installation is not
  a repository at all, and a check that works only for developers running from a
  checkout would report "unchanged" for everyone else — the absence-of-evidence
  failure §10 warns about, in a new place.
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

`$cpp_options()` returns the **recorded** request here. That is consistent with §1
rather than an exception to it: the accessor always answers "what was this build
asked for", and adoption simply sources that answer from the record instead of from
the current call. Since adoption hydrates from a hash-matched record without
launching the binary (§4), this costs nothing.

**Executable without a usable record** — missing, corrupt, or hash mismatch.
Explicitly unprovenanced: read whatever metadata the executable reports as today,
and have `stan_build_info()` return an explicit *unavailable* result rather than an
empty one that could be mistaken for "nothing was configured."

Here `$cpp_options()` **is** empty, and that is the honest answer — no request is
known, and inventing one from the four flags the binary happens to report is exactly
the merge §1 rejects. This is the one case where a user must call
`stan_build_info()` to learn anything, and error messages about options should say
so rather than leaving them at an empty list.

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
to see; Stage 5 may land during the candidate period rather than before it, but it
cannot be deferred past 1.0.

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
record beside a user's Stan program until Stage 3. Name, format and the
version-control story are decided (§4) — settled enough to build against, and
revisable until the release. What this stage owes is the groundwork that has to be
in place before Stage 3 creates a file: ignore patterns in this repository, and the
user-facing wording to ship with the writer.

### Stage 3 — transactional record writing

Executable-plus-record staged and committed together, with verification and the
rollback in §4. Locking deliberately excluded. This is where the file first
appears on disk; it is written but does not yet drive decisions.

### Stage 4 — the API change and the decision engine, together

Removing deferred compilation and `$compile()` and adding the standalone family
(§8) is **#1256**. The constructor decision engine is **#1255**, superseding
**#1019**, and brings with it **#1237**, the triggers in §6, include re-resolution,
and §5's assessment with its two caller behaviours. Likely closes **#1253**.

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

**One thing possibly worth automating here.** §6 treats an executable predating
records as a rebuild trigger, and that transition can be tested rather than merely
waited for: 0.9 stays installable from GitHub, so a job could install it, build an
executable, then install this branch and assert the adoption path — no record,
provenance unknown, rebuild triggered. Worth weighing when the stage is planned,
not a commitment. Building such an executable by hand when it is needed may well be
enough, and is one fewer moving part in CI.

### Stage 5 — public build-record inspection

`stan_build_info()` last, once the schema has stabilised under real use — and the
release-candidate period is that use.

**It must ship in 1.0.** An earlier ordering treated it as purely additive and
therefore optional. It is not: because §1 keeps the request separate from what the
binary reports, this is the only way to ask what an executable actually is, and the
only answer available at all for an unprovenanced one (§7). Landing it during the
candidate period is fine; landing it after the release is not.

### Reconciling NEWS before the candidate

`NEWS.md` accumulates entries describing behaviour that later stages remove, and by
1.0 it would read as a changelog for methods that no longer exist. The unreleased
section already carries fifteen-plus entries about `$compile()`, `compile = FALSE`
and `dry_run`, all of which Stage 4 deletes, plus at least one — the
`$format(overwrite_file = TRUE)` cache refresh (`NEWS.md:94-96`) — describing
behaviour this document now reverses.

**Entries are removed, not annotated.** An entry that no longer applies at 1.0 is
noise for the reader it was written for, whichever release introduced it: a user
upgrading from 0.9 to 1.0 never saw the intermediate behaviour and does not need to
know it existed. This is a pass before the candidate, once the stages have settled,
not something to do incrementally while the picture is still moving.

### The release candidate

A 1.0 candidate ships after Stage 4, so downstream packages have something to
migrate against rather than a release note. That is what makes §8's breaking change
affordable.

**brms**, **instantiate** and **rethinking** are the priorities. The first two are
chokepoints rather than merely important packages: instantiate's own dependents call
`instantiate::stan_package_model()` rather than cmdstanr directly, so fixing
instantiate carries its dependency tree with it. rethinking is here for reach rather
than fan-out — it is how most people first meet cmdstanr. Everyone else has the
candidate period to adapt on their own.

brms uses `cmdstan_model(compile = FALSE)` in `.parse_model_cmdstanr()`
(`brms/R/backends.R:23-34`) to build a throwaway object solely for `$check_syntax()`
and `$code()` — precisely the case §8's standalone family replaces, and the clearest
confirmation so far that the family is the right shape; the replacement removes
lines rather than adding them. `.compile_model_cmdstanr()` needs no change at all,
since it already supplies options on every construction, which is what §2 asks of
every caller. brms does set `cpp_options$stan_threads` only when threading is
requested, so its users meet §1's threading policy as a rebuild when they toggle it
off.

instantiate is smaller still. `stan_package_model()` adopts an existing executable
with `cmdstan_model(exe_file = , compile = FALSE)`, and dropping the argument is the
whole fix, since adoption never compiles. Its other branch — `stan_file = ` with
`compile = FALSE` and the executable *missing* — has no successor, but that state
means a package was installed without its binary, so erroring is defensible.
`stan_package_compile()` maps onto `compile_stan_file()` directly.

rethinking is the smallest of the three. It reaches cmdstanr at four places — three
in `ulam()` (`R/ulam-function.R:1424`, `:1455`, `:1493`) and one in `cstan()`
(`R/cmdstan_support.r:32`), all of the form `cmdstan_model(stan_file, compile = ,
cpp_options = , stanc_options = )` — and never reads build state back. No
`$compile()`, no `dry_run`, no `exe_file` (commented out at all three sites), none of
the §5 accessors. Everything else it touches is fit-side.

§8 is therefore the only thing that reaches it. In `ulam()` the argument is
`compile = filex[[3]]`, and `filex[[3]]` is hardcoded `TRUE` (`:1399`), so deleting
the line is the whole fix. `cstan()` is the one place across all three packages where
the removal propagates to end users: `compile` is rethinking's own documented
argument (`R/cmdstan_support.r:17`), passed straight through.

§1's threading policy leaves it unchanged: `ulam()` enables `stan_threads` and
always supplies `threads_per_chain`, so it satisfies the rule both before and after.

rethinking has no version-control exposure either: `tempdir()` means records never
reach a repository.

No survey proves there is no further caller, and instantiate reaches us through
`eval(parse(text = paste0("cmdstanr::", name)))`, so no static check will find a
break in it — ours or theirs. Run all three packages' test suites against the candidate
rather than trusting a search.

**These are signals, not constraints.** brms internals are ours to change, and the
survey above is worth having because it shows what real callers need to express —
not because their current code has to keep working. Build what §8 describes, then
make brms use it. The only genuine obligation is negative: nothing here may leave a
downstream package unable to express something it legitimately needs.

We open those pull requests ourselves rather than waiting to be asked; the candidate
is what they are written and tested against. They need to work against both the old
and the new cmdstanr — brms is on CRAN and cmdstanr is not — so a version guard
rather than a clean switch. rethinking is the exception: it is distributed from
GitHub and already has cmdstanr in `Depends`, so it can require the new version
outright.

The formatting and linting work is scheduled around this, and the formatter and the
linter go to different places.

Air's one-time whole-repo format (#1153) is the **last** change before 1.0. It is
whitespace-only and deterministic, so shipping it after the candidate is cheap, and
by then there is no branch left for it to conflict with — which there would be
today, with #1235 and #1254 both open. Its PR-review action is a separate thing:
additive, conflicting with nothing, and most useful *during* the stages, since
Stages 2 to 4 write a good deal of new code that would otherwise be formatted after
the fact. Check first whether it comments on changed lines or on whole files; if the
latter, it waits for the format.

Jarl (#1172) does not travel with it. Adopting the linter is additive, but acting on
its findings is semantic editing, and that must not land after the candidate — 1.0
would then ship code in a form nobody tested. Those findings are ordinary reviewed
changes, taken whenever, not a sweep.

Neither may land between Stage 4 and the candidate, where a reformatting diff on top
of the API removal leaves a downstream maintainer unable to see what actually broke.

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
asked for" and "the record cannot be read" both mean rebuild, but they are not the
same check. Conflating them makes an unreadable record silently equivalent to a
matching one — the failure is treating *no answer* as *the answer agreed*. The same
applies to a record from a newer cmdstanr: identical behaviour, different diagnosis,
and the message has to say which happened.

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

**Naming.** `stan_build_info()` and `check_syntax_stan_file()` are still
placeholders, to be settled in the stage that implements each (§8).
`.<model>.cmdstanr.json` is *decided* rather than open (§4) — build against it — but
it stays revisable until the release, after which changing it means migrating
records that already exist.
