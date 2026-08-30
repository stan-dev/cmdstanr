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
truth. §3 and #1250 have already contradicted each other on raw Make assignments,
with the *document* the stale copy — which is what a second source of truth costs.

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
is deliberately untracked (§6). The CmdStan installation *is* in scope: its
normalised path and version together are `builder` identity, and whether that
installation still exists is checked as well (§6).

**Relation to the 1.0 milestone.** Every issue in this area is milestoned
`v1.0.0 - release`, so the milestone is mostly this work — but not only: it also
holds linting (#1172), formatting (#1153), interactive installation (#605) and the
`num_chains` argument (#534), none of which touch these contracts.

---

## At a glance

**A map, not a summary.** Every rule is stated once, in the section that owns it,
and this section only says which section that is.

Restating a rule here would make it a second thing to keep correct, and the copy
loses — the same argument this document makes for being canonical over the issues,
applied internally.

| Section | Answers |
|---|---|
| §1 | What the record is; what `$cpp_options()` and `stan_build_info()` each report, and why they are never merged |
| §2 | Where a build is configured, and why nothing on an existing model object reconfigures it |
| §3 | What a `cpp_options` entry means to `make`, which spellings are accepted, and why the rest are rejected |
| §4 | What the record holds, where it lives, how it is bound to its executable, and who ignores it. **Contains the recorded/compared table, which is the authority on both** |
| §5 | When an operation validates, what it does on failure, and the full classification of the public surface |
| §6 | Every rebuild trigger, what identity means for each kind of dependency, and what is deliberately untracked |
| §7 | Executable-only models: adoption, provenance, and what cannot be configured |
| §8 | Removing deferred compilation, and the standalone functions that replace it |
| §9 | Why the work is ordered as it is, the downstream packages, and the release candidate. **#1258 holds the work list itself** |
| §10 | Traps for whoever implements this |

**Two things worth knowing before reading any of it.** Options are supplied on every
build call and never accumulate on the object (§2). And a model either has a current
executable or errors — nothing compiles behind your back, and the only two things
that build are `cmdstan_model()` and `compile_stan_file()` (§8).

---

## 1. Vocabulary: what the record is, and what it is not

The record is a **provenance manifest**: a description of how an artifact came to
exist. It is not a configuration store, not an authority that can authorise
whatever happens to be at the executable path, and not a substitute for observing
the binary itself.

That distinction is load-bearing because these are genuinely different facts:

- **`request`** — the build configuration, with `cpp_options` and `stanc_options`
  each stored twice: what the caller **supplied**, and what cmdstanr **injected**.
  The two are disjoint, because cmdstanr injects only what the caller did not
  supply, and origin is what decides whether an option can force a rebuild (§4).
  Everything else is stored once, in the form the build used: a caller who passes
  no `include_paths` on a program with `#include` has `dirname(stan_file)` recorded
  (§6). These fields **explain** a build rather than replay one — `make/local`
  contributes to the same stanc invocation and appears in none of them, being
  covered by its own hash. Feeding them back is not a supported operation and no
  rule here depends on it.
- **`reported_features`** — what the binary itself reports as enabled. Distinct
  from `request` because `make/local` can enable threading or OpenCL that the user
  never mentioned. The line between the two is *when the fact was known*: `request`
  is everything settled before the build ran, `reported_features` is what could only
  be discovered afterward.
- **`dependencies`** — the sources consumed, and enough about how they were
  resolved to re-resolve them. Identified by content; each also records the
  `built_from` path it had at build time, which is provenance. That path is not
  compared, with one deliberate exception — the user header, whose directory is an
  input to a C++ include closure we cannot enumerate (§6). §4's table is the
  authority on which fields are compared.
- **`artifact`** — which executable this record describes, by hash (§4).
- **`builder`** — which CmdStan installation produced it.
- **`known_untracked_dependencies`** — dependencies we can see exist but cannot
  resolve (§6). Named for what it is: an empty list means nothing was *detected*,
  never that the record is complete.
- **`format_version`** — the version of the *build interpretation contract*, not of
  the JSON shape. A version this cmdstanr does not support, in either direction, is
  reported and rebuilt; a change in option or build semantics obliges a bump (§4).

`reported_features` is **tri-state and best-effort**: each feature is *known
enabled*, *known disabled*, or *unknown*. `<exe> info` reports what CmdStan chooses
to report — threading, OpenCL, Stan version — not arbitrary flags. **Absence must
never be read as disabled.**

**The two are never merged into one accessor.** `$cpp_options()` reports
`cpp_options_supplied`: what the caller asked for, not what cmdstanr added on top.
The user header has its own accessor, `$user_header()`, matching its own argument —
it is not readable through `$cpp_options()` because it is no longer settable there
(§3). `stan_build_info()` reports what the binary says, with its provenance. And
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

## 2. Contract: options are specified once, on the build call

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

**This does not fail loudly**, except for threading: `assert_valid_threads()` warns when `threads_per_chain` is set on an
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

### The user header is not a `cpp_options` entry

`user_header` is currently reachable three ways: the dedicated argument,
`cpp_options[["USER_HEADER"]]` and `cpp_options[["user_header"]]`, with the argument
taking precedence over both (`R/model.R:512-513`). **Only the argument survives.**
The two `cpp_options` spellings are rejected, with an error naming it.

This is the same rule as `include_paths`, `warn-pedantic` and `STANCFLAGS` (§6, §4):
one setting, one channel. It is worth stating separately because of what it deletes.
`resolve_user_header()` (`R/cpp_opts.R:189-245`) exists almost entirely to reconcile
the three, tracking positions of both casings so duplicates follow make's last-wins
rule, walking a four-level precedence chain, and raising two distinct conflict
warnings. §8 removes its `previous` parameter along with deferred compilation, and
what remains is resolving a path and setting `USER_HEADER` for `make`.

The codebase already treats the header as not belonging here: `parsed_cpp_options()`
skips `user_header` and `stan_version` when canonicalizing (`R/cpp_opts.R:101`),
because neither is an ordinary Make assignment to compare.

**`--allow-undefined` is not separately settable either.** It is the flag the header
implies, so `stanc_options = list("allow-undefined")` and its named form are rejected with
the same error. Builds derive it from `user_header`; the source-only operations always set
it (§8). Nothing is left for a caller to decide.

**`$user_header()` is added**, so the dedicated argument has a dedicated accessor.
Today the only way to read the header back is `$cpp_options()[["USER_HEADER"]]`, which
is why §1 can have `$cpp_options()` report `cpp_options_supplied` without losing
anything: the header was never really a `cpp_option`, and now it is not one at all.

### Assignments are named; everything else is opaque (#1250)

Two verified defects. **Unnamed raw entries reach `make` but are invisible to
everything keyed on names**, because an unnamed list entry's name is `""` while
`cpp_options_to_compile_flags()` passes it straight through (`R/cpp_opts.R:139`):

```r
mod <- cmdstan_model(f, cpp_options = list("STAN_THREADS=TRUE"))
mod$sample(threads_per_chain = 4)
#> Warning: ... not compiled with 'cpp_options = list(stan_threads = TRUE)'
```

The model *is* threaded — #765's symptom via a different spelling.

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

**Plain `NAME=value` is rejected too**, though it is unambiguous, because accepting
raw `=` means re-implementing every special-cased variable on the raw path: a raw
`USER_HEADER=my.hpp` reaches `make` while everything keyed on names looks straight
past it. Nothing is lost —
named entries already emit `NAME=value` (`R/cpp_opts.R:141`), so the migration is a
spelling change and the error can name the form to use.

**This applies to `cpp_options` on a build call** — `cmdstan_model()` or
`compile_stan_file()`, which share one implementation (§8), so a rule that held for
only one of them would be bypassable by choosing the other. The distinction is
between configuring a build and writing `make/local`, not between the two build
entry points. `cmdstan_make_local()`
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

JSON earns its place, though not because users read it — they should not have to.
Rather: `jsonlite` is already
an import, a parse failure hands us §4's "unreadable" case for free, and a
text format makes bug reports, golden fixtures and our own debugging tractable. The
name also stays clear of `.dep` and `.d`, which `make` and the C++ toolchain already
claim in that directory.

**Implementation trap:** `list.files()` defaults to `all.files = FALSE` and will not
see the record. Anything enumerating files beside a model must opt in.

**Scope: the record describes how the artifact was built, not what the model
contains.** That boundary is what keeps `format_version` tractable — storing derived
model metadata such as stanc's variable output would make every change in that output
a record format change. It also settles proposals to hydrate `$variables()` or
similar onto adopted models from the record: they are out of scope by construction,
not merely unimplemented.

Both name and format remain revisable up to the release, so nothing outside the
reader and the writer should depend on either.

### What is recorded, and what is compared

These are two different questions, and treating them as one is what made several
rules in earlier versions of this document inconsistent with each other.
**Recording** serves provenance and diagnosis: the record should describe the build
completely enough to explain it, which is a weaker bar than reproducing it (§1). **Comparison** serves the rebuild verdict (§6), and
a field belongs there only if it determines the artifact's observable behaviour and
is not already determined by another compared field.

**This table is the single statement of both.** Prose elsewhere refers to it and
must not restate it — a rule written in two places is a future inconsistency.

| Field | Recorded | Compared | Notes |
|---|---|---|---|
| `request.cpp_options_supplied` | yes | yes | what the caller passed; canonicalized per field (§3, #1250) |
| `request.stanc_options_supplied` | yes | yes | as above |
| `request.cpp_options_injected` | yes | **no** | what cmdstanr added, disjoint from `_supplied` by construction |
| `request.stanc_options_injected` | yes | **no** | as above. `make/local`'s `STANCFLAGS` reach the same stanc invocation and appear in neither field, being covered by `make/local`'s hash |
| `request.include_paths`, effective | yes | **no** | the *current* call's paths drive re-resolution (§6); the recorded value is provenance |
| `request.user_header` path | yes | **yes** | the one *dependency* path compared, because the C++ closure beneath it cannot be enumerated. `-I` flags decide the same resolution and are compared inside `cpp_options` above (§6) |
| `reported_features` | yes | no | describes the binary; never a trigger (§1) |
| `dependencies[].hash` | yes | yes | content hash — this is what identity means |
| `dependencies[].built_from` | yes | no | where the file was at build time; provenance. The user header is the exception above |
| `dependencies.included_files` | yes | yes | **ordered sequence**, duplicates preserved (§6) |
| `artifact` | yes | yes | hash of the executable this record describes |
| `builder` | yes | yes | normalized installation path and version |
| `known_untracked_dependencies` | yes | no | reported (§6), never a trigger |
| `format_version` | yes | yes | unsupported in either direction rebuilds and says so |

Three consequences, each of which has been got wrong at least once:

**Recorded-but-not-compared is the ordinary case, not a list of exceptions.** Six of
the fourteen rows are in it. The default is *not* "everything in `request` is
compared," and reasoning from that default is what produced the errors.

**Origin is stored, not inferred.** The verdict compares only what the caller supplied,
and a merged list cannot be split back apart without knowing this version's injection
rules — which is the reconstruct-after-the-fact fragility this design removes
everywhere else. That is the whole reason, and it holds even if no option can arrive
by both routes.

Options that *can* make the consequence visible: cmdstanr injects `--filename-in-msg`
as the real source path, and a caller may supply their own value, which wins untouched
(§9). The flag is identical and the two must compare differently — a path-derived
injection would reintroduce path sensitivity, a user-typed string is a fixed value like
any other. `--name` is the same shape. Neither is load-bearing: the rule does not depend
on them existing, and a future rejection that removes one takes nothing with it.

The two fields are built side by side rather than one recovered from the other:
`R/model.R:673`, `:677`, `:693` and `:835` currently write into a single
`stanc_options` variable, which would force the record to reconstruct the caller's
input, and accumulating injections into their own list instead makes both fields
fall out of the code (§10).

**What cmdstanr injects is itself build semantics.** The set is recorded rather than
described here, so changing it does not change what is compared, and no list of
injected options has to be kept correct in this document. It still obliges a
`format_version` bump (below): an option a later cmdstanr injects can change the
artifact while an old record's `_supplied` goes on matching, so nothing would
rebuild. The bump is what forces it.

**An injected option still applies; not comparing it only means it cannot force a
rebuild.** Where the option's whole purpose is to produce output — `--warn-pedantic`
is the only current case — the operation still has to happen on a model that is
already up to date, or the user's request silently evaporates (§5).

**Every `stanc_options` spelling of `--warn-pedantic` is therefore rejected**, with an
error naming `pedantic = TRUE` — `list("warn-pedantic")`, `list("warn-pedantic" = TRUE)`
and `list("warn-pedantic" = FALSE)` alike. The named `FALSE` is refused too because it
emits nothing today (logical `FALSE` leaves a flag out, #1251) while `pedantic = TRUE`
still injects, so it reads as a way to switch pedantic off and is not one.

Supplied rather than injected, the flag would be *compared*: the first build warns, the
second construction matches the record, nothing rebuilds, and the warnings never appear
again — the same evaporation through the door the rule above does not cover. One
spelling that is already handled is cheaper than a second rule. `name` and
`filename-in-msg` stay supplyable: neither has a dedicated argument, and
`filename-in-msg` is deliberately caller-overridable (§9). `allow-undefined` does not,
being the flag `user_header` implies (§3).

### The record's lifecycle follows the executable's

Whatever ignores the executable ignores the record; wherever the executable goes,
the record goes with it. One rule, which answers the cases below and the ones nobody
has thought of yet. It is also why the pair is hash-bound in the first place (§4): a
record without its executable describes nothing, and an executable without its
record is unprovenanced.

**In practice that means `.gitignore`**, because a Stan executable is a
platform-specific build artifact that almost nobody commits. Add `.*.cmdstanr.json`
beside whatever already excludes the binary. The leading dot makes this easy to
forget — it hides the file from `ls`, not from git, so `git add -A` commits it
silently.

Committing a record beside a *disposable* executable is worse than having none. It
holds a hash of a binary that does not exist on the machine checking it out and an
absolute path to the CmdStan installation that built it, so it fails validation and
rebuilds anyway — having produced a diff on every rebuild in the meantime.

**`.Rbuildignore` follows the same rule**, and is the easier one to miss. `R CMD
build` excludes hidden files by a fixed list (`tools:::.hidden_file_exclusions`), not
by leading dot, so this name is not covered: a package author who compiles in a
source tree ships records inside the tarball describing their own machine. Exclude
the executable and the record together.

**Where the executable is deliberately kept, keep the record with it** — a CI job
passing a built binary to a later job, a container layer, a shared build directory.
Any staging step that copies one must copy both. These cases are real but uncommon,
and instantiate is specifically *not* one of them: it compiles on the user's machine
at install time, so the record is generated locally beside a binary that was never in
git or in the tarball.

**Losing a record never corrupts the executable**, which is worth saying so nobody
engineers around it — but it is not harmless, and the three cases differ:

- **Source-backed construction** rebuilds and writes a new record (§6). One compile.
- **An already-constructed source-backed object** errors on any guarded operation
  (§5), because assessment reads the filesystem each time and the record is gone.
  The remedy is `cmdstan_model()` again, not `force_recompile`.
- **Executable-only adoption** continues with reduced provenance (§7). It is the
  only case that keeps working without a record, and the only one where the loss is
  not recovered by rebuilding.

What is never lost is the binary itself, and `<exe> info` still reports its flags in
all three.

**cmdstanr writes neither ignore file itself.** Compiling a model should not modify a
user's repository configuration; the recommendation belongs in documentation.

**This repository needs the patterns too, before Stage 3 writes anything.**
`tests/testthat/resources/stan/.gitignore` enumerates fifteen compiled binaries by
hand and is already behind — four models there have no entry. Every compiled test
model will add a record, so replace the enumeration with patterns rather than
extending it.

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

The hash also detects manual replacement and ordinary corruption, so distrust of the
artifact is not the undetectable case it might look like. It is detectable, cheaply.

**Installation is a transaction:** build and stage both artifacts, install the
executable, install the record, then verify the pair. **Any failure — including a
successful record install whose pair verification then fails — restores both the
previous executable and the previous record**, leaving a consistent pair rather
than a new artifact with old provenance. Treating a record-write failure as
non-fatal but visible would be incompatible with §5, since an executable without a
valid record is immediately unusable.

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

The hash row is bounded by what is tracked. Toolchain drift and the untracked
dependencies in §6 are false negatives by construction, not defects in the
comparison.

The false-positive case is ordinary — the Stan file is in git, the executable is a
gitignored artifact beside it, and a branch round-trip leaves identical content
with a fresh mtime. Measured on six realistic source files: **0.04 ms** to stat,
**0.31 ms** to hash. 0.27 ms against a rebuild measured at **6.7 s** for
`bernoulli.stan` with precompiled headers enabled and **13.8 s** without.

**Treat that as a floor, not a typical figure.** It is a trivial model on one fast
machine; a program with many user functions, ODE solves or a large generated
quantities block is substantially slower, and Windows differs. Every argument in this
document that weighs a check against a rebuild is of the form "the check is free
next to the compile," so anchoring low is deliberate — those arguments hold *a
fortiori* at larger values, and none of them depends on the number.

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
- **Include paths** — preserve order. Order controls shadowing, so a record that
  reordered them would misdescribe the build. A recording rule only; see §4's table
  for whether it is compared, and §6 for what the verdict resolves with.
- **Stanc options** — a **conservative comparison** for v1: compare the resolved
  set literally and accept that equivalent spellings may occasionally trigger an
  unnecessary rebuild. "Canonicalize per option semantics" is not implementable
  guidance — it would require enumerating the semantics of every stanc option.
  Enumerate them later if the spurious rebuilds turn out to matter.
- **User-header paths** — normalise. This is the one path that is *also* compared,
  because the C++ include closure beneath the header cannot be enumerated (§6), so
  normalisation here affects the verdict rather than only the record's readability.
- **`NULL` / `FALSE`** — preserve the explicit empty-assignment meaning (§3).

### Format versions, in both directions

A record whose `format_version` this cmdstanr does not read **rebuilds, and says
so**, exactly like an executable that predates records (§7). It is not refused and
does not require `force_recompile`. At 1.0 the readable set is exactly `{1}`, so in
practice any mismatch rebuilds; a later release may widen the set, which changes what
is readable without changing this rule.

**`format_version` versions the build interpretation contract, not the JSON shape.**
If a later cmdstanr changes how `cpp_options` are canonicalised, identical bytes in a
`request` field mean something different under the new rules, and reading an old
record with them could conclude "unchanged" when the build would in fact differ —
the silent wrong answer this design exists to remove. So **a change in option or
build semantics obliges a version bump**, not only a change to the fields.

Both directions occur, and only one is obvious. Forward is familiar: a newer
cmdstanr wrote something this one cannot interpret. Backward arises because a
downgrade *regenerates* old-format records — v3 writes format 3, the user reverts to
v2 which rebuilds and writes format 2, and v3 then meets a format-2 record it may no
longer read.

Refusing to replace a forward-version record was considered and rejected. The record
is entirely *derived* data and every field regenerates on a rebuild, so refusing
protects nothing while costing one recompile (measured above) — paid with an error in
the one situation where the rule actually arises. That situation is also not the one it
was written for: passing an executable between machines barely works, since the
binary links TBB at an absolute path inside the CmdStan installation that built it
(§6). The realistic case is **one person on one machine downgrading cmdstanr**,
where the installation, `make/local` and every path are unchanged and nothing else
would trigger.

The real hazard was never the record. It is that the executable may have been built
under option semantics this version does not implement, so rebuilding can produce a
*differently built* binary. That is an argument for saying loudly what happened, not
for refusing:

```
#> Recompiling: this model's build record was written by a newer version of
#>   cmdstanr (format 3; this version understands 2), so how the existing
#>   executable was built cannot be verified.
```

**Executable-only models are the exception**, because §7 forbids rebuilding them.
The record is not replaced there: an unreadable version joins missing, corrupt and
hash-mismatched in leaving the executable unprovenanced, and it stays on disk.

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

These are one assessment with two responses, not two contracts — stating them as one
contract is what makes §5 and §6 look like they disagree.

### What the error says

**Not `force_recompile = TRUE`.** Everything the assessment detects, the constructor
already fixes — a changed source or configuration, a corrupt or missing record, an
executable that does not match the record it sits beside. Naming any of those as a
reason to reach for the flag is circular: a plain `cmdstan_model(...)` is the fix.
For an executable-only model it cannot help at all, because there is no source to
rebuild from.

Reserve `force_recompile` for what nothing we compare can see: **explicit distrust of
the artifact**, and **a change to one of the untracked dependencies** below —
toolchain drift, CmdStan or Stan Math modified in place, headers reached
transitively through `USER_HEADER`, a `make/local` that includes another makefile.
Those are the cases where the assessment is right that nothing it tracks has changed
and wrong about the conclusion.

Erroring rather than rebuilding is deliberate. A compile appearing unexpectedly
inside `$sample()` is worse than an actionable message *whatever it costs* — the
objection is that the caller asked to sample and did not ask to build, and that the
latency is unpredictable, not that the number is large.

### Scope and cost

**Guard every operation that executes or derives state from the binary** — not only
the fitting methods. "At least" is not implementable, so the full public surface is
classified here: `CmdStanModel` carries **twenty-seven public methods and one public
field**, and every one appears below. `cmdstan_model()` is listed as the builder but
is not itself a member, so it does not count toward either total.

| Behaviour | Members |
|---|---|
| **Validate, and error on any trigger** | `$sample()`, `$sample_mpi()`, `$optimize()`, `$laplace()`, `$variational()`, `$pathfinder()`, `$generate_quantities()`, `$diagnose()`, `$cmdstan_defaults()`, `$expose_functions()` |
| **Rebuild, printing every reason** | `cmdstan_model()` itself — the constructor. `compile_stan_file()` is the other build entry point, but returns a path rather than a model |
| **Snapshot of the built model; no validation** | `$code()`, `$variables()`, `$print()`, `$functions` |
| **Accessor; no validation, never errors** | `$stan_file()`, `$has_stan_file()`, `$model_name()`, `$exe_file()`, `$include_paths()`, `$cmdstan_version()`, `$cpp_options()`, `$user_header()` |
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
That is safe precisely because §8 makes a model immutable after construction. Through
the supported API the two objects describe the same executable permanently and cannot
diverge, so a shared exposure is valid for both. A direct `$initialize()` call could
retarget one of them — that is the unguarded case above, and it is not defended
against here either.

**The `$exe_file(path)` setter is removed** (`R/model.R:365-370`). It assigns
`private$exe_file_` with no validation, no snapshot refresh and no provenance
update, so under this design it would leave an object holding a record that
describes a *different* binary — the exact pairing §4 exists to prevent. The getter
stays and must keep not erroring (§1). Retargeting is done by constructing a new
object, which is the same answer §8 gives to the rest of the mutable-configuration
surface. Its only call site is `test-model-compile.R:1535`, in a test built on
`compile = FALSE`, so it retires with that. Folded into #1253.

Cost is **~8.8 ms**: source hashes plus the executable hash, which dominates — a
figure counting only the sources comes out at 0.3 ms and understates it. If
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
whether a usable executable exists **without triggering an error**. `$exe_file()`
provides it, and the table above settles that it stays a plain accessor that never
errors. A public form of the assessment may be added later for callers wanting a
fuller answer; that would not change the accessor's contract, and the two are not
alternatives. What is ruled out is leaving no way to ask.

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
built one — the contract violated by the mechanism meant to implement it. Worse, the
result depends on call history: the same object, after the same edit, answers
differently depending on whether anyone happened to ask before the edit. `$code()` is
already eager (`R/model.R:272`), so today the two accessors can describe two
different versions of the program.

**Construction is the right moment, and the call is already being made.** It is the
one instant when the source and the executable are guaranteed to agree — the
constructor has just either rebuilt or verified the hashes — and §8's immutability
means the snapshot cannot go stale relative to the object afterwards. §6 also already
invokes `stanc --info` there to re-resolve includes, and a single call returns both
things:

```
inputs, parameters, transformed parameters, generated quantities,
functions, distributions, included_files
```

So for a model with includes this is one call doing two jobs rather than a new cost.
A model without includes pays one ~30 ms stanc call at construction that it would
otherwise have paid on first `$variables()` — earlier, not extra, and anyone who
samples pays it regardless, since `$sample()` calls `$variables()` to validate data
(`R/model.R:1409-1412`).

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

---

## 6. Contract: when a rebuild happens (#1019)

A rebuild is triggered by `force_recompile = TRUE`, or when any of these differs from
the record:

- the Stan program changed
- a resolved include changed, or resolves differently now (#1237)
- the user header changed, or its path differs from the one recorded
- `make/local` changed
- the `cpp_options` or `stanc_options` **the user supplied** differ from `request`
  (options cmdstanr injects are recorded but never compared — §4)
- the CmdStan installation path or version differs from `builder`, or the recorded
  installation is gone

Or when the recorded facts cannot be trusted at all:

- the executable does not match `artifact` — replaced by another process, or corrupt
- the record is missing, unreadable, or written in a format version this cmdstanr
  does not read
- the executable predates build records, so there is nothing to compare

The second group is *artifact-side*: reasons the record cannot be relied on, rather
than reasons the inputs changed. It belongs in the same contract because the
constructor's response is identical — rebuild, and say why.

**`include_paths` is absent from that list deliberately.** Comparing it *as a
spelling* would reintroduce path sensitivity for every model that has an include —
renaming a working directory would rebuild through a different field, for exactly
the population content identity exists to serve.

It does not need comparing, because its whole effect is *which files it resolves*,
and that is checked directly: re-resolution runs `stanc --info` and compares the
resulting sequence of content hashes against the record. A change to `include_paths`
that alters what resolves therefore rebuilds; one that alters nothing does not.

**That argument requires `include_paths` to be the only way paths reach stanc, so
it is made the only way.** stanc accepts `--include-paths` repeatedly and
accumulates them, and *four* cmdstanr channels reach the build's stanc invocation:
the `include_paths` argument (`R/model.R:832`), `stanc_options` (`:837`),
`make/local`'s `STANCFLAGS` (`:839`), and `STANCFLAGS` set through `cpp_options`,
which arrives as a `make` command-line assignment that cmdstanr's own `STANCFLAGS +=`
appends to rather than replaces — confirmed in a built binary's embedded `stancflags`
string. Only the first reaches `model_variables()` (`:2668`), the `stanc --info` call
re-resolution is built on, so a path supplied through any of the others resolves for
the build and nowhere else. That is a live defect in released cmdstanr, independent
of this design: a model built through `stanc_options` compiles and then fails on
`$sample()`, which calls `$variables()` unconditionally (`:1410`).

**`include_paths` is therefore the only accepted channel**, and the rest are rejected
with an error naming it — `--include-paths` in `stanc_options` under either spelling,
`list("include-paths" = p)` and `list("include-paths=p")`; `--include-paths` in
`make/local`'s `STANCFLAGS`, where the message names the file; and `STANCFLAGS` in
`cpp_options` outright, since `stanc_options` is the channel for stanc flags and a raw
make-variable passthrough only duplicates it. Detection is a substring test on the
flag, not a parse: we never interpret `--include-paths`'s comma lists, quoting or
separator forms, only refuse them.

**The two rejections are deliberately different in scope, and should not be unified.**
`cpp_options` is a cmdstanr argument, so the whole variable goes. `make/local` is
CmdStan's own configuration file — `make/local.example:20` ships
`STANCFLAGS+= --warn-pedantic` as a suggested line — so only the include-path flag is
refused there, not the variable. The check on `cpp_options` belongs in
`validate_cpp_options()`, which `cmdstan_make_local()` does not call
(`R/install.R:324-338` builds its flags inline), so writing `STANCFLAGS` *into*
`make/local` through the supported function stays possible. A `--warn-pedantic` left
there is fine and is not the case §4 rejects: it asks to warn whenever CmdStan builds,
and that is what it does. §4 refuses the *per-call* spelling, where silence on an
up-to-date model would contradict the request.

This narrows what CmdStan accepts, deliberately. cmdstanr owns source resolution
because it re-runs it, and a second spelling of the same configuration buys nothing
and costs the guarantee above. The cost is a `make/local` set for command-line
CmdStan use now erroring even for programs with no `#include` — a configuration
that is already broken for every program that has one.

**Re-resolution uses the include paths supplied on the current call, not the
recorded ones.** This is the single easiest thing in this document to get backwards,
and backwards it is inert. Resolving with the *recorded* paths can only ever confirm
that the previously resolved files are unchanged — it can never reveal that the
caller asked for different ones. A user moving `include_paths` from `v1` to `v2`
would silently keep the `v1` binary, which is the same class of failure as the user
header below.

**Order matters on the current call, for the same reason.** Include paths control
shadowing, so re-resolution must be given them in the order supplied. The recorded
order is provenance (§4).

**The user header's path *is* compared**, as one instance of a general rule:
directories that participate in C++ include resolution are compared as spellings,
and nothing beneath them is tracked. Stated with its reasoning, and with the `-I`
flags that are the rule's other instance, in "Identity for C++ include resolution"
below.

**An unsupported format version is in that list too** — it rebuilds, with the reason
stated (§4), whether the version is newer or older than what this cmdstanr supports.
It is still worth *distinguishing* from an unreadable record, because the two warrant
different messages: unreadable means the file could not be parsed, unsupported means
a version was found that this cmdstanr does not interpret. Same behaviour, different
diagnosis — and the vocabulary is kept apart deliberately, since a supported record
can also be corrupt.

**Dependencies are identified by content.** A dependency matches when its content hash
matches what the record holds, wherever it now lives. Moving a project does not
rebuild it.

The record still stores the absolute path each dependency had **at build time**, as
`built_from`. That field is provenance, not identity: it records where the artifact
was actually built, and is never rewritten. Its immutability needs no mechanism, since
records are replaced whole on every build (§4). It is not compared either, with the
single exception of the user header below.

**Path-and-content identity was considered and rejected. This is the closest call in
the document**, so the reasoning is recorded in full rather than summarised.

The argument for including path is real and survives scrutiny: stanc compiles absolute
source paths into the generated C++ `locations_array__` — every resolved include
today, and the program itself once Stage 4 passes `--filename-in-msg` (§9). Two builds
at two paths produce genuinely different binaries, so a cache reporting "up to date"
is reporting on an artifact that differs from what a fresh build would make. On that
view the path is a build input like any other and needs no special rule.

**It was rejected because the difference is one string, and no user benefits from
correcting it.** What a relocated executable actually costs is a stale directory
prefix in an exception message:

```
Exception: normal_lpdf: Scale parameter is 0, but must be positive!
  (in '/old/path/sub/helper.stan', line 12, column 2 to column 30)
```

Correct line, correct column, and whoever moved the project knows where it went.
Against that, the everyday case — renaming a working directory from `docs/A` to
`docs/B` — pays a full recompile for a benefit its user never receives. Nobody models
a directory name as a compiler input, so the rebuild does not follow from the action,
which is the test §7 applies to messages and which applies at least as strongly to
something that costs a full recompile.

Nor is it rescued by the exotic cases. Cross-machine scenarios almost always rebuild
on **builder** identity anyway, since CmdStan is identified by installation path and
version, so path identity is the sole trigger only when the project moved and the
installation did not. That is the folder rename, and nothing else of consequence.

**This is the rule the rest of the section already uses:** rebuild when the artifact
would differ in a way the caller can observe and care about. It is why dependencies
are hashed rather than trusted by mtime, and why `make/local` rebuilds even on an edit
that changes nothing — there we cannot tell whether it matters. Here we can.

Outside precedent agrees. ccache meets this exactly, with absolute paths baked into
debug information, and ships `base_dir` and `-fdebug-prefix-map` so users can defeat
path-induced cache misses. The mature tools in this space treat "moved the tree, lost
the cache" as a defect to work around rather than a property to preserve.

**Provenance argues for neither side**, though it has been cited for both. A manifest
recording where equivalent inputs are *now* rather than where the artifact was built
is not a provenance manifest — but that is a constraint on what the record *stores*,
which `built_from` satisfies, and no rebuild policy either repairs or requires it.

**Comparison is positional.** The verdict compares the recorded sequence of content
hashes against a freshly resolved one, element by element, preserving order and
duplicates. `built_from` is no part of the test — it only *names* the file once a
mismatch is found, since position *i* in the sequence has a `built_from` at position
*i*. A whole project moving changes every path and no hash, so the comparison passes
with no special handling at all.

**Order and multiplicity are compared because stanc reports them faithfully.**
`stanc --info` returns `included_files` in source-inclusion order rather than sorted,
and repeats a file that is included twice. Comparing the ordered sequence is both
exact and the *simpler* implementation — a set comparison requires sorting or
multiset bookkeeping in order to arrive at a weaker answer, and would miss two files
exchanging contents in place.

One reporting note for the implementer: when an include is added or removed the
sequences differ in length and every later position shifts, so walking positions
would name every subsequent file as changed. Report a length change as "the
included-file sequence changed" and walk positions only when the lengths match. The verdict
is correct either way; this is about the message.

Defining project roots, symlink behaviour and out-of-project paths stays rejected, and
under content identity there is nothing left to define — normalisation now matters
only for `built_from`, which is recorded rather than compared.

**Moving everything except the record is fine.** The record is hidden (§4), so a
`cp *` or a drag-select will leave it behind. Where source is available that is a
missing record, which rebuilds once and writes a new one — a single compile, never a
wrong answer. For an executable-only model (§7) there is nothing to rebuild from, so
a lost record costs provenance rather than time.

### Identity for C++ include resolution

**Every directory that participates in C++ include resolution is compared as a
spelling, and nothing beneath it is tracked.** The `-I` flags a user puts in
`cpp_options` are the explicit members of that set; the user header's own directory
is the implicit one. The two are compared through different fields — the flags as
part of `request.cpp_options_supplied`, the header's path as the one *dependency* whose
recorded path is compared (§4) — but this is one rule with two instances, not a rule
plus an exception.

**The user header is therefore matched on normalised path *and* content**, and the
reason is not that C++ headers are special — it is that our information about them is
incomplete in a way it is not for Stan files.

For a Stan program, `stanc --info` hands back the *entire* include closure, so
hashing it covers every byte that went into the artifact and the paths are genuinely
irrelevant. For a C++ header we hash the top of the tree and nothing beneath it.
CmdStan feeds the header in as `-include $(USER_HEADER)` (`make/program:41`), and a
quoted `#include` inside it resolves **relative to the directory of the file
containing the directive** — so the header's location is an input to the compilation,
not a label for it.

Two byte-identical headers in different directories are therefore different
translation units:

```
cb942ef4…  exact/user_header.hpp     #include "odds_impl.hpp"
cb942ef4…  approx/user_header.hpp    #include "odds_impl.hpp"     same hash

exact/odds_impl.hpp    return theta / (1 - theta);
approx/odds_impl.hpp   return -1.0;
```

Built through cmdstanr, those two produce `mean(odds) = 0.3261` and `-1.0000`. Under
content-only identity every compared field matches and the second build is skipped,
so a caller who explicitly selected the second header runs the first.

**So this is the same rule under incomplete information, not an exception to it.**
Identity is the content of the whole input closure. Where the closure can be
enumerated we compare it exactly; where it cannot, we compare what identifies the
closure's *root* and accept that we see nothing below it — for a header, the
directory it hangs from. §6 already does this once, for `make/local`: the whole file
is hashed, so any edit rebuilds, including edits that change nothing; equally, an
edit to a makefile it includes rebuilds nothing (#1257). This is the second
instance, not a new principle, and a `-I` directory is the third: we compare the
flag as the user wrote it and track nothing in the directory it names.

**What the comparison does and does not guarantee.** It detects *re-rooting*: a
model whose header moves, or whose caller selects a different header directory,
rebuilds. On that class of change it errs only in the safe direction — an
unnecessary rebuild costs one compile, and a binary built from a different root is
never reused. It is not a conservative approximation of every change beneath the
root, and the next paragraph is the case it misses.

**What it does not catch, and what happens instead.** An in-place edit to a file the
header includes changes nothing we compare, so the stale binary is reused and the
numbers are silently wrong. It makes no difference whether the included file sits
beside the header or arrives through a `-I` directory; the gap and the remedy are the
same. That gap exists today and is documented in #1257 with `force_recompile = TRUE`
as the remedy, plus a single message at compile time for models the regex detects —
triggered by the `#include` directive in the header, not by where it resolves, so a
`-I` supplied without a user header raises nothing and correctly so, having no
user-controlled `#include` to satisfy. Closing it properly means asking the compiler for the
closure — `c++ -MM -MG` returns it in ~150 ms and `-MG` conveniently leaves
CmdStan's own headers as unresolved names to discard. That is recorded on #1257 with
the measurements, and deliberately not in v1: the cost is not the 150 ms but coupling
record validation to compiler flag construction, which can drift quietly. When it
does land, this path comparison is what it replaces.

### Identity for the CmdStan installation

The comparison is the **normalised installation path and the version**, plus the
`make/local` hash, which is its own dependency with its own trigger rather than part
of `builder`.

**A different path at the same version is a rebuild reason.** Selecting another
installation with `set_cmdstan_path()` is a deliberate act, and under a version-only
rule it would have no effect at all: the executable would still be the one the old
installation built, still linked against its TBB at an absolute path inside it
(below), while validation ran the new installation's `stanc`. Rebuilding is what
makes the selection mean something.

Version-only identity was tried and rejected. The argument for it was that nothing
nameable distinguishes two same-version installations — which is nearly true, since
`make/local` lives inside the installation and is hashed separately, so switching
usually rebuilds on that alone. But that makes comparing the installation path nearly
free rather than unnecessary. It adds a rebuild only when both installations are
configured identically, which is precisely the case where the user's explicit choice
would otherwise be ignored silently. "The installation that built it" is also the
simpler rule to state.

**Why this is path-sensitive when dependencies are not.** The asymmetry is deliberate.
A dependency is a file whose *content* is the input; its location is incidental, and moving it
changes nothing anyone can act on. An installation is not a file — the executable is
linked against its TBB at an absolute path inside it, so where it lives is a standing
runtime dependency of the binary rather than a build-time locator. And
`set_cmdstan_path()` is an explicit act of selecting a different toolchain, where
renaming a working directory is not an act of selecting anything.

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

Accepting this as an undetectable limitation would contradict the premise that we
never silently run the wrong binary — a branch switch adding a higher-priority
include is the same workflow used to justify hashing. It is tractable.

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
against fresh `stanc --info` output.** Recording each include's spelling, its ordered
search roots and the selected path, then re-resolving that mapping, is unnecessary —
and it would need parsing that stanc does for us.

**The search configuration for that fresh call is the effective `include_paths` of
the current call, not the one in `request`.** Using the recorded value would make
changing `include_paths` a no-op: stanc would be pointed at the old directories,
find the old files, report matching hashes, and reuse a binary the caller did not
ask for. The recorded value exists for provenance (§4); the verdict is
about what *this* call would build.

**Re-resolve by invoking stanc, never by reimplementing its rules.** Reproducing
stanc's resolution semantics in R is a correctness hazard, and getting it subtly
wrong reintroduces the silent-stale-binary class this design exists to remove.
There is no performance argument for the risk:

```
stanc --info      : 29.9 ms
exe info          : 32.2 ms
```

Against ~8.8 ms of hashing and a compile measured in seconds (§4), a stanc call is
free.

**Invoke stanc from the recorded `builder`, not from whichever installation is
selected now**, or a different stanc's resolution rules get applied to a model this
one did not build. **Check builder identity first**: if the selected installation
differs from `builder`, or the recorded installation no longer exists, that is
already a rebuild trigger (above) and should be reported without attempting
re-resolution at all.

**Normalisation: normalised absolute paths, recorded but not compared.**
`included_files` comes back from `stanc --info` as absolute paths. Each entry is
compared by content (§6); the normalised path is stored as its `built_from` so the
record still says where the artifact was built. Normalising is therefore about the
record reading consistently rather than about avoiding spurious rebuilds, which
content identity already avoids.

Relocatable *records* remain a separate and rejected idea: storing paths relative to
some root would require defining that root, symlink behaviour, and what to do with
paths outside the project. Content identity removes the reason anyone wanted them —
a moved project does not rebuild (§6) — and the case where rebuilding is genuinely
impossible, no source at all, is covered by executable-only models (§7).

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

Angle brackets are the blind spot most worth naming, because a user-supplied `-I` is
exactly what makes `#include <helpers.hpp>` work for a user's own files, so the form
is realistic rather than exotic once `cpp_options` carries include directories. The
regex is still not widened to `[<"]`: it would then fire on `<vector>`, which is to
say on nearly every real user header, and separating `<helpers.hpp>` from `<vector>`
means resolving against the search path — the work the regex exists to avoid, and
what depfiles would do properly.

(Naming it `provenance_complete` would repeat the error §10 warns about for
`reported_features`: treating absence of evidence as evidence of absence.)

**Surface it when the record is written, and through `stan_build_info()` — not in
pre-operation validation, and not on every construction.** It is a standing property
of the model, not a change, and validation reports only what changed. A warning on
every `$sample()` call is noise that trains people to ignore warnings.

Writing is the right trigger because it is the moment the information is new, and
because it is stateless. "Once per session" would work too, but it needs a cache of
what has already been said, and §5 spends a subsection prohibiting stored verdicts
for reasons that apply to any sibling cache. Keying on the write also settles the
case that raised this: adoption never writes a record, so
`instantiate::stan_package_model()` — which constructs on every fit (§9) — stays
silent, while the note still reaches whoever ran the build that could not be fully
tracked.

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
Stan source at all (`R/model.R:156`). Three otherwise-general statements do not hold
for it: that `cmdstan_model()` always compiles, that a missing record causes a
rebuild, and that pre-record executables get a one-time rebuild.

**A whole class of package belongs here by design, not by accident.** Anything that
compiles at install time and ships the binary inside itself — instantiate and its
dependents most directly — ships the source beside the binary and *could* register
it; under content identity that would not even rebuild (§9). It should not, because
the package owns when its model is built, and registering source hands that decision
to the session. This section is their normal case rather than their fallback, and §9
carries the argument.

**They are preserved, and they split into two cases.** Calling them all unprovenanced
would discard information we may have written ourselves —
`compile_stan_file()` followed by `cmdstan_model(exe_file = path)` is a first-class
flow under this design, and it produces an executable *with* a record.

**Executable plus a valid hash-bound record.** Artifact provenance is *known*: the
record describes this binary, verified by the hash in §4. Report it.
`stan_build_info()` returns the build information, not "unavailable." Source
freshness may still be unverifiable — if the recorded sources are absent or the
paths no longer resolve, say so specifically rather than collapsing it to unknown
provenance.

`$cpp_options()` returns the **recorded** `cpp_options_supplied` here, and
`$user_header()` the recorded header path. That is consistent with §1
rather than an exception to it: the accessor always answers "what was this build
asked for", and adoption simply sources that answer from the record instead of from
the current call. Since adoption hydrates from a hash-matched record without
launching the binary (§4), this costs nothing.

**Executable without a usable record** — missing, corrupt, hash mismatch, or written
in a format version this cmdstanr does not read.
Explicitly unprovenanced, which is a statement about *provenance* and must not
suppress what the binary does report. `stan_build_info()` returns an explicit
unavailable provenance — `provenance = "unavailable"` or equivalent, never an empty
result that could be mistaken for "nothing was configured" — **together with the
`reported_features` the executable supplies**. The four `STAN_*` flags and the
version from `<exe> info` are real information, and §1's separation is exactly what
makes reporting them here consistent rather than contradictory: the request is
unknown, the reported features are not.

Here `$cpp_options()` **is** empty, and that is the honest answer — no request is
known, and inventing one from the four flags the binary happens to report is exactly
the merge §1 rejects. This is the one case where a user must call
`stan_build_info()` to learn anything, and error messages about options should say
so rather than leaving them at an empty list.

In both cases: permit fitting, and **never attempt an automatic rebuild** — there
is no source to build from. They are the deliberate exception to §5's requirement
that a model have a valid record before running.

**Adoption is silent.** Unknown provenance is a standing property of the executable,
not a change, and cmdstanr must not announce it on every construction. The reason is
concrete rather than stylistic: `instantiate::stan_package_model()` adopts on *every
fit* rather than once at install (§9), so a message here reaches every user of every
package built that way, on every call, and cannot be silenced by fixing anything.
`stan_build_info()` is how a caller asks.

The line is not "standing properties are silent" — §6 *does* surface
`known_untracked_dependencies` at construction, and should. Nor is it whether the
user chose the thing: including another makefile from `make/local` is entirely
deliberate, and `make/local.example:36` ships it as a suggestion. The line is
**whether the consequence follows from the action.**

Passing `exe_file =` is itself a statement that cmdstanr did not build this, so
answering that we do not know how it was built repeats what the caller just told us.
The threading policy (§1) is the same: `STAN_THREADS` in `make/local` produces a
threaded binary, which is the thing that was asked for. But writing an `-include`
line says nothing about cmdstanr's staleness detection stopping at the first file.
That is a limitation of ours, invisible from where the user stands, and no amount of
deliberateness on their part reveals it.

**Say something when the user asks for what we cannot deliver.** That is about their
argument rather than the artifact, and nothing they did implies it.

### Build configuration cannot accompany an adopted executable

**With no `stan_file`, explicitly supplied build configuration is an error** —
`cpp_options`, `stanc_options`, `include_paths`, `user_header`, `force_recompile`.
None of them can configure an artifact that will not be rebuilt, and a valid record
is there to be *inspected*, not overridden. Silently ignoring them is the failure
mode this design exists to remove: the user believes they asked for something.

`include_paths` is rejected for a slightly different reason worth stating, because it
is not really build configuration: it configures **source resolution**, and every
stanc invocation needs it — compiling, `$check_syntax()`, and the `$variables()` call
`$sample()` makes to validate data (`R/model.R:1410`). It is therefore meaningful
whenever a source is registered, whether or not anything is compiled. With no source
there is nothing to resolve against, so the rejection stands; but the reason is the
missing source, not the missing build.

**The check is on whether the argument was supplied, not on what it resolves to**,
and `force_recompile` is why. Its default is `getOption("cmdstanr_force_recompile")`
(`R/model.R:621`), so a check written as `isTRUE(force_recompile)` would error for
every adoption performed by anyone who has that option set — including every
`instantiate` fit (§9), from inside a package the user never chose to look at. The
option was set for *their* models; adoption is a third party's implementation
detail.

That difference is the §7 line applied to provenance rather than to content. Writing
`force_recompile = TRUE` beside `exe_file =` is a per-call request we cannot honour,
so it errors. A session-wide option is not a statement about this model, so it is
ignored. Document that on the option's help page — it has no effect on
executable-only models — so the advice arrives as documentation rather than as a
runtime failure in somebody else's code.

Prefer a `NULL` sentinel to `missing()`: resolve the option inside the body after the
check, so omission survives both public build entry points and the shared
implementation without a separate `force_recompile_supplied` flag.

The fragility is ours, not a caller's. `missing()` survives dynamic dispatch and `...`
forwarding — including instantiate's `eval(parse(text = paste0("cmdstanr::", name)))`,
measured — and breaks only when an intermediate layer declares its own default and
forwards it. That is exactly the shape §8 introduces: `cmdstan_model()` and
`compile_stan_file()` each declare `force_recompile = getOption(...)` and hand it to
one shared implementation, at which point `missing()` is `FALSE` on every call.

**`force_recompile` never enters the record.** It changes *whether* we build, never
*what* we build, so it is a decision override rather than configuration. Two
identical builds must produce identical records whether or not one of them was
forced; recording it would leave a forced rebuild permanently marked and every later
comparison seeing a difference that means nothing. For the same reason the rebuild
*reason* should name the option when the value came from there — "`force_recompile =
TRUE`" is baffling to someone who set it in `.Rprofile` months ago and passed
nothing.

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
compile_stan_file(file, include_paths = NULL, cpp_options = NULL, stanc_options = NULL, ...)  -> exe path
format_stan_file(file, include_paths = NULL, ...)
check_syntax_stan_file(file, include_paths = NULL, ...)
stan_variables(file, include_paths = NULL, ...)
stan_build_info(exe)
```

**Every function that hands a source file to stanc takes `include_paths`**, and
`format_stan_file()` is the one that makes this a correctness requirement rather than
symmetry. `$format()` has no such parameter today and does not need one, because it
reads `self$include_paths()` internally (`R/model.R:1252`) — which carries the
`dirname(stan_file)` default. A standalone replacement has no object to read from, so
without the parameter it cannot format any program containing `#include` at all:

```
Syntax error in 'model.stan', line 3, column 0 to column 15, include error:
```

That would be a regression on behaviour `$format()` has today, not merely a gap.

`compile_stan_file()` and `format_stan_file()` match cmdstanpy exactly.
cmdstanpy has no standalone syntax check, and its `src_info()` is lower-level than
`$variables()`, so those two are cmdstanr-only and named to fit R. **Where
cmdstanpy already has a name we copy it; where it does not, we pick one and they
can copy it if they add a counterpart.** The two APIs are taught together, so
parity matters, but nothing here waits on a joint naming decision.

`model_variables()` at `R/model.R:2657` is already this shape internally.

### One resolver produces the effective include paths

The `dirname(stan_file)` default currently lives in the constructor
(`R/model.R:293-297`) and is shared through object state — `$format()`,
`$variables()` and `$check_syntax()` all reach it via `self$include_paths()`. Three
of the five entry points above have no object, so the default has to move into a
plain function that all of them call:

```
cmdstan_model()          ─┐
compile_stan_file()       │
format_stan_file()        ├─→  effective include paths  ─→  stanc
check_syntax_stan_file()  │
stan_variables()         ─┘
```

Mostly a move rather than new logic. Two constraints on it:

**It runs before the request is recorded**, so the record holds the effective value
(§4). Recording the caller's `NULL` would store something true about the user and
useless to the machine.

**It is not what the verdict re-resolves with.** The verdict uses the effective paths
of the *current* call (§6); the recorded ones are provenance. Those
are the same value on the call that builds and can differ on any later one, which is
the entire point.

The default is user-visible behaviour and belongs in the public documentation for
`include_paths`, not only in §10's implementation notes.

### An option that only produces output still has to produce it

`pedantic = TRUE` becomes `--warn-pedantic` (`R/model.R:672-673`). §4 records it and
does not compare it, so it cannot force a rebuild — correct, since the only
difference it makes to the artifact is one embedded flag string:

```
< "stancflags = --name=bernoulli_model"
> "stancflags = --warn-pedantic --name=bernoulli_model"
```

But *not rebuilding* must not become *doing nothing*. Pedantic warnings are produced
by stanc during the build, so a build entry point that skips the build has to run the
check anyway — around 30 ms — or the caller asks to be warned about their model and
receives silence. That is worse than an unnecessary recompile, because nothing
indicates the request was dropped.

Asking for the same flag through `stanc_options` is refused in every spelling (§4), so
the two routes cannot diverge: `pedantic` owns the concept and is the only way to ask
for it.

### Only a build cares whether a function has a definition

`--allow-undefined` suppresses exactly one error: a function declared and never
defined. A call to something never declared at all is still *not in scope* and still
fails, flag or no flag. Verified on CmdStan 2.39.

**So the source-only operations always set it**, whether reached as a method or as a
standalone function — `$format()`, `$check_syntax()`, `$variables()`,
`format_stan_file()`, `check_syntax_stan_file()` and `stan_variables()`. Only the build
entry points derive it from `user_header`, because only a build has to link. One rule,
by operation rather than by entry point, so a retained method and its standalone twin
cannot disagree.

This finishes `eeed5baf` rather than reverting it. That commit fixed `$check_syntax()`
and `$format()` reporting a syntax error on a program whose functions are defined in a
user header, by having them consult `using_user_header_`. Under one rule those
conditionals become unconditional and the dependency on `using_user_header_` leaves all
three, which is less code than the conditional version and removes the divergence the
retained methods would otherwise have: `mod$check_syntax()` erroring where
`check_syntax_stan_file()` passes on the same file.

**The accepted cost, recorded so it is not filed as a bug.** `check_syntax_stan_file()`
reports success on a program that `compile_stan_file()` then rejects, in the one case
where a function is declared, never defined, and no header is supplied. The build is
where that surfaces, with a message naming the function. Everywhere else the flag is
inert: `--auto-format` and `--info` return byte-identical output with and without it on
a program that defines everything it declares.

### `compile_stan_file()` is exported, and shares one implementation

Exporting it only once a consumer commits to it would be the wrong bar. The argument
is parity: cmdstanpy already has `compile_stan_file`, and having
`format_stan_file()` and `check_syntax_stan_file()` public while the compile step
is not is arbitrary — with `compile = FALSE` gone there would be no way to build
without constructing an R6 object.

**One implementation, two entry points**, so nothing is duplicated:

```
compile_impl(stan_file, cpp_options, stanc_options, include_paths,
             user_header, pedantic, dir, force_recompile, quiet, dry_run)
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

**`$format()` gets a standalone plus a method wrapper.** The case for keeping it
method-only — that it invalidates `stan_code_` and `variables_`
(`R/model.R:1309-1311`) — is weak: another model object or an external
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

**#1234's guarantee does not move into the record.** It would if the record were
replayed on a later build, but it is not (§2). What happens instead is that the
*lifecycle disappears*: #1234 exists because a second
build call — `$compile()` — could drop the include paths and user header the first
one supplied. Once deferred compilation and `$compile()` are gone there is no
second call, every build carries its own complete configuration, and there is
nothing left to drop.

---

## 9. Order of work

**#1258 is the work list; this section is only the reasoning behind the order.** The
items belonging to each stage are enumerated there, with the file and line references
implementation needs, and where the two disagree #1258 is right. What lives here is
what an issue checklist cannot carry: why the sequence is what it is, and what breaks
under the orderings that were rejected.

Three constraints shape it. The Make-option fixes come first, because per-field
canonicalization depends on them. **The API change and the *live* decision engine
ship as one stage** — separating those leaves a window where the new promise is
broken whichever way the cut is made (Stage 4), though the engine's pure, unwired
half separates cleanly and is Stage 3b. And Stages 0–4 must all be in the release
candidate, because the API removal is the breaking change downstream packages need
to see; Stage 5 may land during the candidate period rather than before it, but it
cannot be deferred past 1.0.

### Stages 0 and 1 — landing in #1235, then Make-option correctness

Both are independent of the record, which is why they can go first. Stage 1 must go
before everything below because per-field canonicalization (§3) is meaningless while
options are still parsed and spelled inconsistently — comparing two configurations
presupposes knowing what each one means, so the `+=` / `?=` / `:=` classification,
casing, quoting and escaping all have to be settled before any comparison is written.

### Stage 2 — schema and helper tests

Field separation (§1), executable hash, corruption, unsupported format versions in
both directions, executable-only models. Parser, writer and comparison helpers
tested against fixtures.

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

### Stage 3b — why the engine separates from the API change

The assessment is built and tested as a pure function before anything calls it,
which is a stage of its own rather than the first half of Stage 4. It depends only
on Stage 2's fixtures, compiles nothing, and can be worked alongside Stage 3.

The reason to separate it is that **it is what makes this section's contract
checkable**. §6's triggers are prose, and prose gives consistency only if a reader
notices two statements disagreeing. As a decision table with a fixture per row, a
contradiction is a red suite instead. This document has already paid for the
difference: §6 held "`include_paths` is not compared as a spelling" and
"re-resolution uses the recorded paths" eight lines apart for a full review round,
and a single test asserting that `v1/` → `v2/` rebuilds would have failed against
the second the day it was written. Landing the engine early moves that check ahead
of Stage 4 rather than arriving with it.

It does not weaken the argument below that the API change and the decision engine
ship together. That argument is about the engine being *live* while `$compile()` is
gone; an unwired function changes nothing observable, so it carries none of the
risk the combined stage exists to contain.

### Stage 4 — the API change and the decision engine, together

Removing deferred compilation and `$compile()` and adding the standalone family
(§8) is **#1256**. The constructor decision engine is **#1255**, superseding
**#1019**, and brings with it **#1237**, the triggers in §6, include re-resolution,
and §5's assessment with its two caller behaviours. Likely closes **#1253**.

**These cannot ship separately, in either order.** Combining them is not optional —
the intermediate state is broken whichever way it is cut:

```r
mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE))
```

If `$compile()` is removed before configuration mismatches trigger rebuilds, an
existing unthreaded executable is still reused under today's decision logic while
the only escape route is gone. That breaks the central promise of the new API —
that supplied options apply — in the window between the two stages.

**The main file's compiled-in path is wrong today, and Stage 4 fixes it.** stanc bakes
source paths into `locations_array__`, which holds for resolved includes but not for
the program itself: `R/model.R:823-824` copies it to a `tempfile()` and compiles the
copy. This is independent of §6 — content identity means the path never decides a
rebuild — and is worth fixing purely on its own merits. Measured on 2.39 for a model with one include —
ten occurrences of `/var/folders/…/RtmpLEUpO4/model-92f518cac7e5.stan`, two of the
include's real absolute path. What a user sees is a location that was deleted before
they could reach it:

```
Chain 1 Exception: normal_lpdf: Location parameter is nan, but must be finite!
  (in '/var/folders/…/RtmpxvEWEP/model-950f21e1e16b.stan', line 6, column 2 to column 38)
```

Correct line and column, useless filename. This is a live defect in released
cmdstanr, independent of everything else here, and it has never been filed. The fix
is `--filename-in-msg=<normalised original path>`, which stanc has already
(`absent=MODEL_FILE`). Verified accepted and effective on every CmdStan from 2.27 to
2.39, and `cmdstan_min_version()` is 2.35 (`R/path.R:145`), so it can be injected
unconditionally with no version guard.

**Precedence is settled, not left to implementation.** Absent, cmdstanr injects the
real source path. Supplied by the caller in `stanc_options`, that value wins
untouched — existing cmdstanr already accepts it there, so overriding it would break
§2's rule that supplied options apply, and would do so silently. The injected value
is recorded and not compared; a caller-supplied one is compared like anything else
the user puts in `stanc_options` (§4). A user-typed value is a fixed string, so it
introduces no path sensitivity.

**Only the two build entry points inject it.** `check_syntax_stan_file()`,
`format_stan_file()` and `stan_variables()` run stanc against the real file already —
`$check_syntax()` writes its *output* to a tempfile but reads `self$stan_file()`
(`R/model.R:1126-1150`) — so their messages name the right file and injecting there
would be noise.

**The cost, so it is not discovered instead.** The injected value is path-derived and
not compared, so a project that moves keeps the old embedded path in its binary until
something else triggers a rebuild. That is the conceded price of content identity
(§6), not an oversight.

Its own NEWS entry and its own test; it is small in code but it changes what every
user reads in every runtime error.

For a model built at install time the recovered path is the staged build location
(§9), so this makes the artifact describe where it was built — which is all it
claims to do — rather than making that location reachable.

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

**It must ship in 1.0**, despite looking purely additive. Because §1 keeps the
request separate from what the binary reports, this is the only way to ask what an
executable actually is, and the
only answer available at all for an unprovenanced one (§7). Landing it during the
candidate period is fine; landing it after the release is not.

**It reports each dependency's `built_from`, and whether that path still exists.**
It cannot report where the file resolves *now*, and must not try: it receives an
executable and nothing else, so for a relocated project it has no source to resolve
against, and for an executable-only model (§7) there is no registered source at all.
Deriving one would mean inferring a project root or searching the filesystem —
the relocatable-record idea rejected in §4 and §6, arriving through a different door.
Current resolution belongs to a source-backed model, which already has
`$stan_file()` and `$include_paths()` to answer it.

**The existence flag is a neutral fact, not a warning.** For every package that
builds at install time — a whole class by construction (§7) — `built_from` points
into R's staging tree and is *expected* to be gone, because R deletes it on success.
Rendering that as a problem would mean a maintainer asking a user to run
`stan_build_info()` and getting back a healthy installation with every dependency
flagged. The documentation should say plainly that a missing recorded path is normal
for install-time builds.

Deliberately not a message either — a relocation that costs nothing should not
narrate itself on every construction, and this is reported only when asked.

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
with `cmdstan_model(exe_file = , compile = FALSE)`; dropping `compile` *and*
`include_paths` from that call is the whole of the runtime change, since adoption
never compiles. Its other branch fires on `!file.exists(exe_file) || isTRUE(compile)`
and does supply `stan_file`, so it survives §8 unchanged for the explicit-compile
case; the missing-executable case has no successor, but that state means a package
was installed without its binary, so erroring is defensible.
`stan_package_compile()` maps onto `compile_stan_file()` directly.

**Its runtime model stays executable-only, and that is a choice — the staged build
path is not the reason.** R installs packages in staged mode by default:
`src/install.libs.R` runs against an `R_PACKAGE_DIR` under `00LOCK-<pkg>/00new/<pkg>`,
and R moves the tree to its final location afterwards. instantiate compiles there, so
the path the executable was *built from* stops existing the moment installation
finishes.
Verified with an ordinary `R CMD INSTALL` of a probe package built to the same shape:

```
build time (R_PACKAGE_DIR): …/lib/00LOCK-pkgstage/00new/pkgstage/bin/stan/bernoulli.stan
runtime (system.file):      …/lib/pkgstage/bin/stan/bernoulli.stan
```

Identical content, different normalised path. **The source itself is still there**, at
its final location, and could be registered — under §6's content identity that would
not even rebuild. So the recommendation needs a reason that does not depend on the
identity rule.

**The reason is that registering source hands the rebuild decision to the session,
and `builder` guarantees it fires.** instantiate's defining promise is that models
compile at installation and never during use. §6 compares the CmdStan installation
path and version, and `install_cmdstan()` puts every version in its own directory, so
an upgrade changes both halves:

```
Monday     install.packages("somepkg")  -> built against cmdstan-2.39.0
Tuesday    install_cmdstan()            -> cmdstan-2.40.0
Wednesday  somepkg::fit(...)            -> builder mismatch, rebuild inside the fit
                                           call, into the package library
```

`stan_package_model()` adopts on *every fit*, so this is the next fit rather than an
eventual one. A `format_version` bump does the same for a cmdstanr upgrade.

**And that rebuild would be unnecessary**, which is what makes this decisive rather
than merely awkward. The executable is self-contained apart from TBB, which it loads
through an absolute rpath baked in at link time, and `install_cmdstan()` leaves the
old tree in place:

```
$ otool -l <model> | grep LC_RPATH -A2
  path /Users/jgabry/.cmdstan/cmdstan-2.39.0/stan/lib/stan_math/lib/tbb
$ ./<model> info      # no cmdstanr, no CmdStan on any path
stan_version_minor = 39
```

`tbb_path()` is non-`NULL` only on Windows (`R/run.R:1238-1247`), so on macOS and Linux
which CmdStan cmdstanr points at has no bearing on whether the binary loads. A new
CmdStan release therefore costs a source-backed instantiate model a full recompile and
buys it nothing anyone asked for.

For an ordinary model, rebuilding when CmdStan changes is correct — you want the new
Stan. For a model built at package-install time it is not: the *package* owns when
that model is built, and the user asks for a rebuild by reinstalling the package.
Executable-only adoption is what keeps that true, because §7 has no path that rebuilds
a model with no source.

A secondary case, weaker but real: somebody edits the installed `.stan` in the package
library to debug something, and the content hash differs. A third candidate does not
exist — two `.libPaths()` entries resolving source and binary to different package
versions cannot happen, because `stan_package_model()` derives the executable from the
source it just found, `exe_file <- file.path(dirname(stan_file), name)`, so there is
only ever one library resolution.

**This is not an instantiate quirk.** Any package that compiles at install time and
ships the binary inside itself is in the same position, because staged installation is
R's default. The general statement: an install-time-built executable should be adopted
executable-only, and §7 is the correct mode for that whole class of package rather
than a degraded fallback.

Note that the conclusion does not rest on the staged path, which is what keeps it
stable under §6's content identity. The measurement above still earns its place, for a
narrower purpose: it establishes that `built_from` genuinely points somewhere that
stops existing — which is what `stan_build_info()` reports, and why its existence flag
has to read as normal rather than as a fault.

**`include_paths` is settled at installation, and inert at adoption.** instantiate
currently forwards it into a branch supplying no source, which §7 rejects — for the
reason given there: with no source there is nothing to resolve, not because nothing
will be compiled.

#1094 is that distinction being missed. A model built with `stan_file`, `exe_file`,
`compile = FALSE` *and* `include_paths` failed at **fit** time, not compile time:
with a source registered, `$sample()` reached `$variables()`, stanc ran with no
include paths, and the include did not resolve. Dropping `stan_file` was the
workaround, and instantiate's present shape follows from it — losing `stan_file`
(instantiate #28) and gaining an `include_paths` argument (#33) as separate changes.

1.0 dissolves this structurally rather than by patching storage. With `compile = FALSE`
gone there is exactly one build call per model, it carries `include_paths`, and they
are therefore stored once and available to every later stanc invocation.
`precompile_include_paths_` exists only to bridge construction and a later
`$compile()`; with no second call there is nothing to bridge.

So the two calls separate cleanly — installation resolves includes, adoption has no
source to resolve them against:

```r
# installation
compile_stan_file(stan_file, include_paths = , cpp_options = , stanc_options = )
# runtime
cmdstan_model(exe_file = exe_file)
```

Nothing about include resolution is lost at the second line, because all of it was
settled at the first; the record carries the requested include paths, the resolved
files, their hashes and any known untracked dependencies, to be read rather than
recomputed. `include_paths` survives in instantiate's own signature, narrowed to their
explicit `compile = TRUE` branch, which does supply a source.

**No instantiate user's model changes behaviour.** On the adoption path
`include_paths` is stored and never consumed *today* — no source means no
`$variables()`, so no stanc runs — so making it an error rather than a silent no-op
changes what the argument does, not what any model does.

**Executable-only adoption does cost data validation, which belongs in the trade.**
With no source, `is_variables_method_supported()` is false, so `$sample()` skips the
variable-based check of supplied data and `process_data()` serialises with
`always_decimal = FALSE` (`R/data.R:281`). Both are already true of instantiate today,
so neither is a regression — but unlike an accurate path in an exception message this
is a real capability, and declining to add it is a trade rather than a free choice.

Questions about the installed source go to §8's standalone family against that source
directly — `stan_variables(file, include_paths = )`, `check_syntax_stan_file(file,
include_paths = )` — which is also honest about what it describes: the source
currently installed, not the file the executable was built from.

**cmdstanr cannot repair an install-time-built model.** It is executable-only, so §7
forbids rebuilding it, and no CmdStan upgrade or cmdstanr update changes that. The
remedy is reinstalling the package, so any message here points at the package and
never at `force_recompile`. That is today's behaviour too; what changes is that it
follows from a decision rather than from an accident. Expect one cosmetic consequence
rather than discovering it: `stan_build_info()` on such a model reports a
`00LOCK-…/00new/…` build path, which is accurate provenance and will look like a bug
to whoever sees it first.

**Adoption happens on every fit, not once at install.** `stan_package_compile()` runs
from `src/install.libs.R` — instantiate ships no `configure` template at all — but
`stan_package_model()` is called inside the user-facing model function — instantiate's own example package wraps it and `$sample()` together in
`run_bernoulli_model()`. So anything cmdstanr prints at adoption prints on every fit,
in every package built this way, through a function that does not look like it
touches a compiler. That is the concrete case behind the silence rule in §7.

**Its `.gitignore` template needs the record, and will not get it by accident.** The
example package ships:

```
inst/stan/**
!inst/stan/**/*.*
inst/stan/**/*.exe
inst/stan/**/*.EXE
```

Ignore everything, re-include anything with a dot so `.stan` files survive, re-ignore
Windows binaries. The rule is built on "extensionless means binary", and the record
has an extension, so it is re-included. Verified with `git check-ignore`: the
executable is ignored, `.bernoulli.cmdstanr.json` is not. Telling users to "ignore the
record alongside the binary" does not help against a pattern that un-ignores it by
construction, which is why §4 asks for an explicit `.*.cmdstanr.json` line. Updating
this template is part of the instantiate pull request, alongside dropping
`compile = FALSE` and deciding the missing-executable branch.

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

Stage 4 ships as one pull request, but with the engine already built and tested in
Stage 3b what remains is the wiring and the API removal. Most of the risk is retired
before anything user-facing moves, and it is retired in a separate reviewable unit.

Parallel work is bounded by what the tests contend for, not by what a checkout
isolates. `make/local` lives in the CmdStan installation, the precompiled headers
are keyed by `STAN_FLAGS` in that same installation, and parts of the suite reach
`rebuild_cmdstan()`. Separate checkouts separate none of that, so **only one
compiling task runs at a time** — and a run killed part-way leaves residue that the
next run snapshots as its baseline. What does parallelise is the work that never
compiles: Stages 2 and 3b in their entirety, the downstream-usage inventory,
documentation and test migration once the API commit exists, and adversarial review
of a finished stage.
That last is worth a reviewer rather than another implementer; this document
reached its current form through five review rounds.

### Independent, can land any time

**#1245**, **#1246** (error message quality) and **#1249** (`$cmdstan_version()`
reports the installed CmdStan, not the one that built the executable — caused by
`R/model.R:318`, not `dry_run`). Small, user-visible, and the natural work to pick
up while Stage 0 is in review.

---

## 10. Notes for whoever implements this

**The record describes; it does not authorise.** It is not permission to run
whatever is at the executable path — that is what the artifact hash is for. Code
that reads the record and proceeds without verifying the pair reintroduces exactly
the class of bug this design exists to remove.

**The `dirname(stan_file)` include default is behaviour, not scaffolding.** When a
program contains `#include` and no `include_paths` are supplied, cmdstanr defaults
them to the model's own directory (`R/model.R:293-294`). stanc does *not* do this
itself — invoked directly on a model with `#include utils/silly.stan` and no
`--include-paths`, 2.39 fails with a syntax error at the include line — so the
default is the only reason such models build. It is also load-bearing downstream:
`instantiate::stan_package_compile()` defaults `include_paths` to `NULL` and the
example `install.libs.R` does not pass it, so **every instantiate package with a
multi-file model depends on this default**, and dropping it would turn their
installs into build failures rather than degraded messages.

§8 removes the reason `precompile_include_paths_` exists as a *separate variable* —
there is no second build call left to bridge — but the default it carries has to
survive in whatever replaces it. With one build call the natural form is to resolve
the default where include paths are resolved for that build, with no second variable.
Requirement, not mechanism; it needs a test either way.

**Whatever resolves that default must do so before the request is recorded**, so the
record holds the effective value rather than the caller's empty one (§4).
`$include_paths()` already reports the defaulted value today, so this is consistent
rather than a new disclosure. Note the verdict does *not* depend on it — §6
re-resolves with the current call's paths — but provenance does.

**Stop merging the injected options into the user's list in place.** `R/model.R:673`,
`:677`, `:693` and `:835` all write into the same `stanc_options` variable, so by the
time a record could be written the user's entries and cmdstanr's additions are
indistinguishable. Do not fix that with a snapshot taken before line 673; it only
works until someone adds a fifth injection site above it. Accumulate injections in
their own list and merge the two when converting to arguments, so `_supplied` and
`_injected` are both values the code already holds and neither is reconstructed.
Getting this wrong is silent: it turns every injection into a compared option, and
toggling `pedantic` starts recompiling.

**For the user header, the *directory* is the load-bearing part.** §6 compares the
whole normalised path because that is what `built_from` already holds and the only
extra rebuild it buys is renaming a header in place with identical contents, which
nobody does. If anyone later wants to loosen it, `dirname` is the loosening —
removing the comparison is not.

**Two checks that look alike and are not.** "The record disagrees with what was
asked for" and "the record cannot be read" both mean rebuild, but they are not the
same check. Conflating them makes an unreadable record silently equivalent to a
matching one — the failure is treating *no answer* as *the answer agreed*. The same
applies to a record whose format version this cmdstanr does not support, in either
direction: identical behaviour, different diagnosis, and the message has to say
which happened.

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
