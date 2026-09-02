# Compilation state and C++ options — intended behaviour

Status: draft for discussion. Describes the target behaviour of `cmdstan_model()`,
the build record kept beside an executable, and when that record is validated.

## Purpose and scope

This document exists because the defects in this area have not been independent.
#1228, #1234, the residual warning gaps, #1019 and #1237 all reduce to a small
number of contracts that were never written down, so each was rediscovered by
being violated. The intent is to state those contracts once, and to sequence the
remaining work off them rather than off the issue list.

**This note owns behaviour and contracts, and is the copy to trust if it and an
issue disagree about those.** The issues own what a specification cannot carry:
staging, checklist detail, reproductions and progress. #1258 is therefore the
authority on the order of work and on what belongs to each stage, which is what §9
says. Neither is the authority on *code locations* — the `file:line` references
throughout both are navigation aids that go stale on the next edit, and the source
settles them.

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

- **`request`** — the build configuration, with `stanc_options` stored twice: what
  the caller **supplied**, and what cmdstanr **injected**. The two are disjoint,
  because cmdstanr injects only what the caller did not supply, and origin is what
  decides whether an option can force a rebuild (§4). Everything else is stored
  once, in the form the build used: `cpp_options`, which cmdstanr adds nothing to
  (§4), and the effective `include_paths`, so a caller who passes none on a program
  with `#include` has `dirname(stan_file)` recorded (§6). These fields **explain** a
  build rather than replay one — `make/local`
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
- **`format_version`** — which fields the record carries and how they read, not the
  JSON shape. A version this cmdstanr does not support, in either direction, is
  reported and rebuilt. 1.0 reads one format, and what a later release can still read
  is that pair of versions' question (§4).

`reported_features` is **tri-state and best-effort**: each feature is *known
enabled*, *known disabled*, or *unknown*. `<exe> info` reports what CmdStan chooses
to report — threading, OpenCL, Stan version — not arbitrary flags. **Absence must
never be read as disabled.**

**Encode it by presence, not by a third value, in the record**: write a key only when
the state is known, and let an absent key mean unknown. The rule is about what
survives a file; §8 settles the public result separately and reaches the other
answer. The obvious alternative, `NA` for unknown,
does not survive the record: `jsonlite` writes `NA` as `null` and reads it back as
`NULL`, so the R type is gone after one trip through a file and `is.na()` on the result
returns `logical(0)`, which errors inside an `if`. The two states do remain
*recoverable* — `names()` distinguishes an explicit null from a missing key — but only
through a two-step check at every lookup, while the one-step idiom anyone actually
writes collapses them: `x[["stan_threads"]]` is `NULL` either way and `!isTRUE(x)` is
`TRUE` either way, which is the disabled-versus-unknown collapse this rule exists to
prevent. Encoding by presence leaves only `true` and `false` in the JSON, so the naive
access is the correct access and no reader has a null branch to get wrong. §4 carries the schema; this is the constraint it satisfies.

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
have no reported counterpart and are request-only regardless. `STAN_CPP_OPTIMS` is
worth naming there, because it is a `STAN_*` flag that is *not* one of the four: it
only adds compiler flags (`makefile:66`), leaving no macro for
`write_stan_flags.hpp`'s `#ifdef`s to test, so CmdStan could not report it without a
change of its own. Verified on 2.39.

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

so exactly the features with runtime checks are the ones whose status is known, and
every supported CmdStan reports the same four — `cmdstan_min_version()` is 2.35
(`R/path.R:144`), and every version from 2.27 up reports them.

**Unknown is not expected from a supported binary, not impossible from one.** §7 admits
an executable on a valid version from `<exe> info` rather than on a full set of flags,
so a future or custom CmdStan that reports its version and omits `STAN_THREADS`
constructs with threading unknown, from a live call with no record in sight. An
`<exe> info` that fails outright is a different thing and not a source at all, since §7
errors on that instead of constructing. Records are the other source for a checked
feature, which is what the encoding rule above is for — a key absent because the
CmdStan that built the artifact did not report that feature. The set does move:
`STAN_CPP_OPTIMS` was reported from 2.27 and dropped at 2.38, so both sources are
standing cases rather than hypotheticals.

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
skips `user_header` when canonicalizing (`R/cpp_opts.R:101`), because it is not an
ordinary Make assignment to compare.

**A flag cmdstanr derives from another argument is not separately settable.**
`--allow-undefined` is what `user_header` implies, `--use-opencl` is what
`cpp_options$stan_opencl` implies (`R/model.R:676-678`), and `--name` is what
`stan_file` implies (`R/model.R:273`, `:835`). All are rejected in
`stanc_options` by the occurrence rule below, with an error naming the argument that
owns them.
For `allow-undefined` the builds derive it and the source-only operations always set it
(§8), so a caller has nothing left to decide.

`--use-opencl` is worth rejecting rather than guarding, because supplying it alone never
produces an OpenCL-enabled executable. It produces one of two other things, and which one
depends on the model. stanc emits `matrix_cl` members and `to_matrix_cl` calls only where
a GLM-family function takes data it can move to the device, and `STAN_OPENCL` is what
makes those types exist (`stan/math/prim.hpp:6`). A model with such a call dies at compile
time on `no template named 'matrix_cl'`. A model without one — `bernoulli.stan`, measured
on 2.39 — emits C++ identical but for the embedded `stancflags` string, builds, and
reports `STAN_OPENCL=false`. The second is the worse outcome: nothing tells the caller
their request did nothing. One sentence naming `cpp_options` beats either.

`--name` is the one whose owning argument is the source file itself. `R/model.R:273`
takes the model name from the file's basename and nothing else writes it, so
`stanc_options = list(name = "foo")` leaves `$model_name()` answering `bernoulli` while
every CSV the binary writes stamps `foo`. That much could be reconciled by writing
`model_name_` too. The reason to reject rather than reconcile is that nothing asks for
it: a model compiled from a string is named through `write_stan_file(basename =)`
(`R/file.R:61`), and two models that need telling apart in a CSV header need distinct
file names anyway. The error says the name comes from the file name, since there is no
other argument to redirect to.

**`$user_header()` is added**, so the dedicated argument has a dedicated accessor.
Today the only way to read the header back is `$cpp_options()[["USER_HEADER"]]`, which
is why §1 can have `$cpp_options()` report `cpp_options_supplied` without losing
anything: the header was never really a `cpp_option`, and now it is not one at all.

**`R/model.R:709` is what makes it one, and it goes.** After resolution, `compile()`
writes the header back into `cpp_options` under whichever spelling was used, and
`:941` then stores that list, so a caller who passed `user_header = "inc/mine.hpp"` as
an argument and never touched `cpp_options` gets `USER_HEADER = "/abs/path/inc/mine.hpp"`
back from `$cpp_options()`. Wired to `cpp_options_supplied` with that line still in
place, the accessor would report a field the caller never passed, holding a path they
never wrote. What survives is only the Make flag: CmdStan reads `-include $(USER_HEADER)`
(`make/program:41`), so `USER_HEADER=` still has to reach `make`.
**It reaches it as a flag built with the others, not as a recorded `cpp_options` entry.**
A recorded option would put the header's path in `request` as well as in
`dependencies` (§8) — and under WSL not even in the same
spelling, since the Make side is `wsl_safe_path()`-transformed (`R/utils.R:627`) and
`built_from` is not. `resolved_header$spelling` goes with the line — `:709` is its only
consumer, and one channel has one spelling — alongside the `previous` parameter §8
removes.

### One canonical spelling, established on entry

`cpp_options_to_compile_flags()` (`R/cpp_opts.R:129`) uppercases every named entry, so
`list(USER_HEADER = h)`, `list(user_header = h)` and `list(User_Header = h)` are one
variable to `make` and three values to R. The codebase reconciles that three times in two
directions today: `toupper()` on the way out to `make`, `tolower()` in
`parsed_cpp_options()` (`:100`) on the way into comparison, and `tolower()` again in the
dormant `validate_cpp_options()` (`:165`).

**Named `cpp_options` entries are normalized to their `make` spelling once, on entry to
the build call, ahead of validation.** Uppercase is the canonical direction because it is
what `make` receives and what a compile log shows; lowercase is an artifact of R naming
convention. After that point one spelling is in play, and validation, comparison, the
record and `$cpp_options()` all use it. `list(stan_threads = TRUE)` keeps working — it is
normalized immediately instead of at three later points.

This is what makes the reserved-variable rejection below exact rather than approximate,
but the rejection is not the reason to canonicalize. A matcher that folded case itself
would get that one case right and leave every other consumer seeing whatever the caller
typed, each folding again or not at all. Doing it once on entry is what gives validation,
comparison, the record and `$cpp_options()` one representation, and it is the one `make`
receives.

**It changes what `$cpp_options()` returns, and that is a break worth naming.** Today the
accessor reports the caller's spelling *and* the binary's, because
`merge_exe_info_cpp_options()` writes reported names in upper case over the request
(`R/cpp_opts.R:83`); `list(stan_threads = TRUE)` comes back as `stan_threads` and
`STAN_THREADS` both. After this it is one entry, `STAN_THREADS`. Indexing the lower-case
name stops working, which is loud. Indexing the upper-case name keeps working and quietly
changes meaning, from a value the binary confirmed to one the caller asked for —
`stan_build_info()` is where that meaning went (§1). Its own NEWS entry, tested on
ordinary construction and on record-backed adoption.

It also retires `parsed_cpp_options()`'s exclusion list (`:101`), for a different reason
on each entry. Both are there because that function is fed a *merged* list of request and
binary report (`R/model.R:774`); §1 stops merging those, so the only input left to parse
is a supplied list.

`user_header` cannot appear in one — the named spelling is rejected above, and cmdstanr's
own injection is not supplied. `stan_version` can, and should. Nothing in CmdStan reads
`STAN_VERSION`: cmdstanr synthesizes the name at `R/cpp_opts.R:68` from the three
`stan_version_*` fields `<exe> info` prints, and CmdStan's own version variable is
`CMDSTAN_VERSION` (`makefile:151`). A supplied one is still an ordinary Make variable.
`cpp_options_to_compile_flags()` puts it on the make command line (`R/cpp_opts.R:141`,
`R/model.R:864`) and `make/local` is `-include`d before anything else runs
(`makefile:20`), so a user's own file can read `$(STAN_VERSION)` and change `CXXFLAGS`
with it — verified, not hypothetical. That is exactly the position `FOO` is in, which is
why the treatment is the same: recorded, compared, and able to trigger a rebuild.
Excluding it would silently drop a supplied entry that can change the artifact, which is
the failure this section removes rather than one to keep.

That leaves the parser with one caller, because the other is **deleted**.
`exe_info_reflects_cpp_options()` (`:327`) exists to diff supplied `cpp_options` against an
adopted binary, and §7 makes supplying them there an error, so it has no input left. The
question it asks survives where it belongs: `assert_valid_threads()` and
`assert_valid_opencl()` put it to `reported_features` at the moment the feature is used,
rather than at construction against a request that could never have been applied.

**Its deletion is sequenced with this rule, not after it.** The function matches the
parser's names against `tolower(names(exe_info))` — one side folded, the other inherited
from the parser. Canonicalizing while it still exists empties that intersection for every
option, and the check silently stops checking: no error, no mismatch ever reported, just a
validator that always agrees.

**One function owns normalize-then-check.** `assert_valid_cpp_options()` (#1250) does
both, called from the single build implementation §8 leaves behind, so the ordering is
structural rather than a convention a later contributor has to know. It pairs with
`assert_valid_stanc_options()` (`R/model.R:2562`), which already does this job for the
other list. `validate_cpp_options()` (`R/cpp_opts.R:151`) is **deleted** rather than left
dormant: its one piece of substantive behaviour is a warning that a logical `FALSE` will
turn an option *on*, which #1251 reverses, so leaving it in the file would document the
opposite of v1.0's semantics to whoever reads it next. Its tests go with it
(`test-cpp_opts.R:24-37`).

**The `stanc_options` side is deliberately not symmetric.** `stanc_options_to_args()`
passes names through unchanged and stanc is case-sensitive, so a miscased flag fails at
the build with `unknown option --Warn-Pedantic. Did you mean --warn-pedantic?` — better
than anything we would write. Canonicalizing there would add a transformation to solve a
problem that does not exist.

### Rejection matches the option, not the spelling

These entries are rejected from an R option list that currently accepts them:
`include-paths`, `warn-pedantic`, `allow-undefined`, `use-opencl` and `name` from
`stanc_options`, and `USER_HEADER` / `user_header` and `STANCFLAGS` from `cpp_options`.
**Every one is matched by where the option name occurs, never by enumerating accepted
values.** (`--include-paths` inside `make/local`'s `STANCFLAGS` is rejected too, but
that is text in CmdStan's own file rather than a list entry, so §6 gives it its own
detection rule.)

`stanc_options_to_args()` (`R/model.R:2598`) puts the flag name in a different slot
depending on the entry's shape, so the rule has two arms:

- **Named entry** — reject if the *name* is the flag, whatever the value is.
- **Unnamed entry** — reject if the *value* is the flag, or begins with the flag
  followed by `=`.

Enumerating values does not terminate. `warn-pedantic` alone has six spellings that
`stanc_options_to_args()` treats differently, three of which nothing in this document
had considered:

| spelling | emits |
|---|---|
| `list("warn-pedantic")` | `--warn-pedantic` |
| `list("warn-pedantic" = TRUE)` | `--warn-pedantic` |
| `list("warn-pedantic" = FALSE)` | *nothing* |
| `list("warn-pedantic" = NA)` | *nothing* |
| `list("warn-pedantic" = NULL)` | `--warn-pedantic=` |
| `list("warn-pedantic" = "yes")` | `--warn-pedantic=yes` |

**The two that emit nothing are rejected as well.** A validator keyed on the emitted
arguments would pass them, and the caller who wrote `list("warn-pedantic" = FALSE)`
believing it disables pedantic mode gets silence rather than the error naming the
`pedantic` argument. Rejecting on occurrence catches the whole column; §4 gives the
`FALSE` case a second reason on top of this one.

The `cpp_options` rejections are the same rule against `make` variable names, applied
after the normalization above, so `USER_HEADER` and `STANCFLAGS` are matched as literals
with no case folding inside the matcher.

Unnamed entries need a message rather than a matcher. They skip the uppercasing and reach
`make` verbatim, so `list("User_Header=h")` assigns an unrelated variable and sets no
header at all; only an exact-case raw assignment would do anything, and #1250 already
rejects every assignment-shaped raw entry. What that rejection owes the reserved
variables is guidance naming the argument that owns them, not a second occurrence test.

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
an import, a parse failure hands us one of §4's "unreadable" cases for free, and a
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
| `request.stanc_options_injected` | yes | **no** | what cmdstanr added, disjoint from `_supplied` by construction. Never compared as a list; whether an injection's *effect* is compared is decided per field like every other row, and the model name is the one that earns its own, below. `make/local`'s `STANCFLAGS` reach the same stanc invocation and appear in neither field, being covered by `make/local`'s hash |
| `request.stanc_name` | yes | **yes** | the `--name` stanc receives, which `R/model.R:835` derives from the file name — §3 rejects the `stanc_options` spelling, so this is the only source. It meets this column's own criterion: the build bakes it into the binary, and no other compared field pins it down, since content hashes are compared and paths are not. Its visible effect is the CSV header (`R/csv.R:873`), which carries both the raw value stanc was passed and the mangled one stanc compiled |
| `request.include_paths`, effective | yes | **no** | the *current* call's paths drive re-resolution (§6); the recorded value is provenance |
| `reported_features` | yes | no | describes the binary; never a trigger (§1) |
| `dependencies[].hash` | yes | yes | content hash — this is what identity means |
| `dependencies[].built_from` | yes | no | where the file was at build time; provenance. The user header is the exception below |
| `dependencies.user_header.built_from` | yes | **yes** | the one *dependency* path compared, because the C++ closure beneath it cannot be enumerated. `-I` flags decide the same resolution and are compared inside `cpp_options` above (§6) |
| `dependencies.included_files` | yes | yes | **ordered sequence**, duplicates preserved (§6) |
| `artifact` | yes | yes | hash of the executable this record describes |
| `builder` | yes | yes | normalized installation path and version |
| `known_untracked_dependencies` | yes | no | reported (§6), never a trigger |
| `format_version` | yes | **no** | not a comparison: this call computes no format version to compare against. The reader either understands the record's version or does not, which is an artifact-side reason like unreadable JSON (§6), and rebuilds in either direction. Equality would be the wrong test — a release that widens the readable set still reads records it no longer writes |

Three consequences, each of which has been got wrong at least once:

**Recorded-but-not-compared is the ordinary case, not a list of exceptions.** The
column says which, and it is not a short list. The default is *not* "everything in
`request` is compared," and reasoning from that default is what produced the errors.

**Origin is stored, not inferred.** The verdict compares only what the caller supplied,
and a merged list cannot be split back apart without knowing this version's injection
rules — which is the reconstruct-after-the-fact fragility this design removes
everywhere else. That is the whole reason, and it holds even if no option can arrive
by both routes.

Options that *can* make the consequence visible: cmdstanr injects `--filename-in-msg`
as the real source path, and a caller may supply their own value, which wins untouched
(§9). The flag is identical and the two must compare differently — a path-derived
injection would reintroduce path sensitivity, a user-typed string is a fixed value like
any other. It is not load-bearing: the rule does not depend on it existing, and a future
rejection that removes it takes nothing with it.

**`--name` had the same shape and now has its own compared row**, because leaving it
inside the injected field was unsound. `R/model.R:835` derives it from the file name, so
move a source, its executable and its record together under a new name and nothing
compared changes: content hash, artifact hash and builder all match, and the supplied
options are empty on both sides. No rebuild. The
object's `$model_name()` then reads `survival` while the binary goes on stamping
`bernoulli_model` into every CSV it writes. Unlike `--filename-in-msg`, which describes a
build that really happened, this is two live answers to one question, and both are
visible inside R, since `R/csv.R:873` maps the CSV header onto
`fit$metadata()$model_name`.

**Comparing the name does not avoid a CSV boundary; it decides where the boundary falls.**
Uncompared, the binary goes on stamping `bernoulli_model`, so runs from either side of
the rename still combine — until some later unrelated rebuild makes the stamp
`survival_model`, and `check_csv_metadata_matches()` (`:948-951`) rejects the mixture as
"not generated with the same model". A CmdStan upgrade is enough to trigger that, and
nothing about upgrading CmdStan explains it. Compared, the rename is the rebuild and the
same rejection lands on the act that caused it. The whole chain reproduces in released
cmdstanr, whose mtime check (`R/model.R:733`) a coordinated rename passes untouched.

**The recorded value is what stanc is passed, not what stanc compiles.** A name that is
not a legal C++ identifier is mangled, and not by a rule worth reimplementing: measured
identical at 2.35, 2.36 and 2.39, `--name=my-model_model` compiles to `my_model_model`,
while `my.model_model` becomes `myx46model_model` and `my+model_model` becomes
`myx43model_model` — hex escapes rather than substitutions. Comparing the raw value costs
two things and is still right. A punctuation-only rename rebuilds although the compiled
name is unchanged, and even there the artifact differs: the raw string is embedded
verbatim, and CmdStan writes it onto a `stancflags` line in each sampler output CSV
(`command.hpp:294` through `write_config.hpp`) that cmdstanr does not parse. Diagnostic
CSVs get the compiled name and not that line, their writers being given `write_model`
without `write_config` (`command.hpp:295-297`).
Second, `$model_name()` still will not match the compiled name for a file stanc mangles,
which is true today and is not something this comparison sets out to fix. The alternative
is reproducing a compiler's internal mangling in R, where drift fails silently in the
direction that does not rebuild.

Two limits belong here rather than waiting to be rediscovered. The rename has to carry
the executable and the record with it, since otherwise cmdstanr looks for an executable
that is not there and builds one. And it does nothing for executable-only models, which
§7 forbids from rebuilding at all. So the row covers one route rather than a class of
them, and what earns it a place is the criterion rather than how often it fires:
dropping it would put an exception inside the rule that decides every other row.

The two fields are built side by side rather than one recovered from the other:
`R/model.R:673`, `:677`, `:693` and `:835` currently write into a single
`stanc_options` variable, which would force the record to reconstruct the caller's
input, and accumulating injections into their own list instead makes both fields
fall out of the code (§10).

**What cmdstanr injects is itself build semantics.** The set is recorded rather than
described here, so no list of injected options has to be kept correct in this document.
That is a statement about the `_injected` *field*, which is never compared, and not about
the injections themselves: whether the value an injection determines is compared is this
table's question, decided per field on the criterion above, and `--name` is the one that
currently earns a row.

**A change to which options cmdstanr injects does not rebuild anything already built.**
An option a later cmdstanr adds can change the artifact while an old record's
`_supplied` goes on matching, and nothing rebuilds. That is the intended answer rather than a gap
to close: the caller asked for the same build they asked for before, and the new binary
is theirs to ask for with `force_recompile = TRUE`. The scheduled case is
`--filename-in-msg` (§9), which makes runtime exceptions name the real source rather
than a deleted tempfile copy; an executable built before it goes on naming the tempfile
until something else rebuilds it, which is the price of leaving working models alone.

That is the general rule for cmdstanr's own changes, and CmdStan is not an exception to
it. A CmdStan upgrade *does* rebuild, through `builder`, because the installation is a
standing runtime dependency of the artifact rather than a fact about how it was built:
the binary loads its TBB through an absolute rpath into that tree, and re-resolution
invokes that installation's stanc (§6). cmdstanr appears nowhere in the executable. It
drives builds rather than forming part of one, so changing it changes what happens next
rather than invalidating what already exists.

Only `stanc_options` has an injected row, and the reason stands on its own. With the
user header built as a flag (§3), cmdstanr injects no C++ option at all, so a
`cpp_options_injected` would be empty in every record 1.0 writes. If one ever appears
it arrives with the field.

**An injection nothing compares still applies.** The qualifier earns its place:
`--name` is injected too, and its value *is* compared through `stanc_name` (above), so
renaming a source does rebuild. `--warn-pedantic` is what is left, and for it the
invariant this obliges is narrower than "a diagnostic always re-emits": *asking for
something you did not have before never yields nothing.* A compared option satisfies
it for free, since turning it on mismatches the record and rebuilds. An uncompared one
cannot, and needs a mechanism of its own — §8 runs the check on a model that is
already up to date.

`pedantic = TRUE` and a supplied `--warn-uninitialized` therefore behave differently on an
identical repeat, and that is right rather than a compromise, because they are different
kinds of thing. `pedantic` is a request scoped to the call, like `quiet`: it runs whenever
it is asked for, and the way to stop the warnings is to stop asking for them.
`--warn-uninitialized` is part of the build configuration, so it applies when a build
happens and is otherwise quiet, which is what a C compiler does with `-W` flags against an
up-to-date object file.

The record makes a third policy available — run the check only when the recorded
injections show pedantic was not already applied — and it is rejected. It would make two
identical calls behave differently on the strength of a file the caller cannot see, with
no obvious way to ask for the output back, and it would defeat the main reason to put
`pedantic` in a script, which is to have the check run on every execution. Suppressing a
request because it was satisfied once is not a saving when the request costs 30 ms.
Output the caller asked for runs whenever they ask, while output they did not ask for
picks its moment — which is why §6's untracked-provenance note fires on writing a record
rather than on every construction, and is not this decision taken the other way.

**A general diagnostic classifier is refused.** Rerunning supplied `stanc_options`
diagnostics on an up-to-date model would need cmdstanr to know which stanc flags are
diagnostic-only, per CmdStan version — the per-option semantics this section declines
below for canonicalization, arrived at from the other direction. What it buys is
re-emitting warnings for a build nobody asked to repeat.

**`--warn-pedantic` is rejected from `stanc_options`**, with an error naming
`pedantic = TRUE`, matched on occurrence rather than by value (§3). This is the
one-channel rule of §3 and §6, and pedantic is the case where two channels would differ
in *kind* rather than in spelling: `pedantic = TRUE` is injected and not compared, the
same flag through `stanc_options` is supplied and compared, so one warns on every call
and the other only on a build. The named `FALSE` has a second reason of its own: it emits
nothing today (logical `FALSE` leaves a flag out, #1251) while `pedantic = TRUE` still
injects, so it reads as a way to switch pedantic off and is not one.

`filename-in-msg` stays supplyable: it has no dedicated argument and is deliberately
caller-overridable (§9). `name` and `allow-undefined` do not, each being what another
argument implies — the source file and `user_header` respectively (§3).

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
a structured R object, and the call assembles a second one from what it was asked
for, so deciding whether to rebuild is a field-by-field comparison over the fields §4
marks compared rather than text matching or a single equality test. The two are not
the same shape and do not need to be: the record carries `format_version`, which the
call has no counterpart for and never computes (§4). That is what lets §6 report
every applicable reason instead of the first, wherever there is a usable record to
compare against (§6), and it is why the rules below are per field.

The two are separable concerns, worth keeping separable. The object model is what
delivers the comparison and the reasons; the format is only how bytes reach disk.
JSON earns its place there on different grounds — a parse failure is one of the
"unreadable" cases §4 needs, and getting it from `jsonlite` leaves the
`format_version` check and the reader's own field checks to write by hand. RDS would
round-trip R types exactly, which would remove the tri-state hazard in §10 for free, but
at the cost of an opaque binary beside the user's model and a second
forward-compatibility surface under our own. Not worth it while the tri-state property
is testable.

**What the reader cannot use, it rejects as unreadable rather than working with** — a
file that parses as JSON is not yet a record (§6). Every field the format requires is
checked for type and shape before the record is accepted — `builder`'s version against
the grammar §7 defines, since a string that is not a CmdStan version is the wrong shape
rather than an odd value — and a record failing any of those checks is unreadable whole:
nothing in it is used and nothing in it is reported, including a `format_version` that
parsed perfectly well. How that checking is written is the implementation's to choose;
how far it reaches is not. Checking only the fields the caller at hand happens to need is
what leaves one record adoptable by `cmdstan_model()` and unavailable to
`stan_build_info()`.

**`reported_features` is checked for shape and never for membership.** §1 makes an
absent key mean unknown, so requiring the four flags CmdStan reports today would make a
record written by a CmdStan that stopped reporting one unreadable, and rebuild every
model it built. The set has already moved once (§1), and it is the one field whose
contents are the binary's to decide rather than the format's.

**The version is checked first, and on its own.** Reading a record has two steps, and
the first decides whether the second means anything: `format_version` is validated by
itself, and only a version this cmdstanr reads sends the reader on to the remaining
fields. A record written in a format we do not read is never measured against the
current schema, because we do not know what that version required — it reports
`unsupported_format` and the version it read, and nothing else *from the record*. The
executable's own `reported_features` still come back, as §8 requires of every unavailable
provenance. The rule above governs the records we claim to understand.

### Canonicalization is per-field

A single "sort and last-wins-deduplicate" rule is wrong. The correct rules differ:

- **Named Make assignments** — canonical by the time anything compares them (§3), so
  comparison is a literal match rather than a case-folding one; last assignment wins,
  then sort by name.
- **Opaque Make arguments** — preserve order exactly; later arguments can override
  earlier ones.
- **Include paths** — preserve order. Order controls shadowing, so a record that
  reordered them would misdescribe the build. A recording rule only; see §4's table
  for whether it is compared, and §6 for what the verdict resolves with.
- **Stanc options** — compare the **sorted argument vector the options emit**, not the
  R list. `stanc_options_to_args()` already computes it, and it collapses the two
  accepted spellings at no cost: `list("O1")` and `list("O1" = TRUE)` both become
  `--O1`, so a model built one way is not rebuilt by the other. Sorting is safe
  only because `--include-paths` — the one order-sensitive flag that could appear here
  — is rejected from `stanc_options` (§6). What stays uncanonicalized is *semantic*
  equivalence, two different flags meaning the same thing, and that would need the
  semantics of every stanc option. Leave it: a spurious rebuild is the safe direction
  and no one has reported hitting one.
- **User-header paths** — normalise. This is the one path that is *also* compared,
  because the C++ include closure beneath the header cannot be enumerated (§6), so
  normalisation here affects the verdict rather than only the record's readability.
- **`NULL` / `FALSE`** — preserve the explicit empty-assignment meaning (§3).

### Format versions, in both directions

A record whose `format_version` this cmdstanr does not read **rebuilds, and says
so**, exactly like an executable that predates records (§7). It is not refused and
does not require `force_recompile`. **1.0 reads exactly the format it writes and
nothing else**, so in practice any mismatch rebuilds; a later release may widen the set,
which changes what is readable without changing this rule.

The number is deliberately not written down here. Stage 3 starts writing records, so a
literal in this section would have to be kept in step with a value only the merging
pull request knows — the two-owner failure that §9 exists to avoid. Nothing later in
the staged rollout moves it: the one bump once scheduled there was for
`--filename-in-msg`, which the rule above no longer counts as a reason to bump.

**`format_version` says which fields a record carries and how they read**, not what
the JSON looks like. Whether a later cmdstanr can still read a record it did not write
is then a question about that pair of versions rather than a property of the scheme,
and 1.0 has no older format to face (above), so what this document owes is the test a
later release applies rather than a verdict of its own.

**An older record is read while it justifies reuse, and refused when it does not.**
Two things stop a record justifying reuse, and the paragraphs below take them in turn:
its fields stop meaning what they said, or the verdict stops following from what it
carries.

The second is the one that gets assumed away. A field an older record never carried is
not compared for it, and not comparing is indistinguishable from agreeing, since
either way the verdict is that nothing compared differs. So a record that cannot say
whether its binary still matches the inputs this cmdstanr checks does not justify
reusing that binary, however much of what it does carry still matches.

The first is plainer, and hashing is the case §8 already leaves open: the algorithm is
free to change at a bump, so across one, hashes computed under the two rules never
match, and what is left is not a disagreement about the model but a comparison that
cannot be run. It is worth pricing correctly, because the obvious argument for the
bump is wrong. Without one the model rebuilds, the rebuild writes a record under the
new algorithm, and every construction after that matches — one rebuild per model
rather than an endless loop, and repeated only for someone alternating cmdstanr
versions (below). What the bump buys is the diagnosis. Unbumped, the mismatch surfaces
as "the Stan program changed" against a file nobody touched, and one `format_version`
ends up naming two incompatible meanings for the same field.

Canonicalization is not a change of that kind, which is worth saying because it looks
like one. The canonical form of an option *is* what the compiler receives (above), so a
rules change makes an old record's value differ from a freshly computed one and you get
a spurious rebuild, which is the safe direction. Getting a false *match* would need the
canonical form to stop being the compiler's input and become a token standing for it,
which is the semantic-equivalence canonicalization this section already declines.

**Bumping is not how a change reaches an executable that already exists.** Rebuilding
follows from a record this cmdstanr cannot use, never from wanting a binary to be
different, and the line between those is the one the injection rule above draws. A
record that still answers whether the binary matches its inputs is used and the binary
is left alone, however differently a newer cmdstanr would have built it. A record that
can no longer answer that is refused.

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
The record is not replaced there: an unsupported version joins missing, unreadable and
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

It has one other legitimate use, and it is not of that kind. A cmdstanr release that
changes what it injects deliberately leaves existing executables alone (§4), so the
flag is how a caller opts into the new build. The assessment is not wrong there —
not rebuilding is the intended answer — which is why this is stated separately rather
than added to the list above.

Erroring rather than rebuilding is deliberate. A compile appearing unexpectedly
inside `$sample()` is worse than an actionable message *whatever it costs* — the
objection is that the caller asked to sample and did not ask to build, and that the
latency is unpredictable, not that the number is large.

### Scope and cost

**Guard every operation that executes or derives state from the binary** — not only
the fitting methods. "At least" is not implementable, so the full public surface is
classified here, and three counts are involved because the surface moves underneath the
table. `CmdStanModel` carries **twenty-seven public methods and one public field** today
and **twenty-seven** at 1.0 — §3 adds `$user_header()`, §8 removes `$compile()` — while
the table below has **twenty-eight** method rows, being the union of both. It classifies
the removed member rather than omitting it, so no count is wrong; they answer different
questions and the test below depends on which one it asks. `cmdstan_model()` is listed as
the builder but is not itself a member, so it counts toward none of them.

The completeness claim is enforceable rather than merely asserted, and should be
enforced: `CmdStanModel$public_methods` and `$public_fields` enumerate the live surface,
so a test can compare it against the twenty-seven non-removed rows and fail on any member
that appears without a classification, asserting `$compile()`'s absence separately rather
than as an exception to the comparison. Otherwise this table decays the first time someone
adds a method, which is the failure the `$initialize()` and `$clone()` entries below
already guard against by hand.

**Every member is exercised, not one per class.** A representative passing proves nothing
about the other nine guarded methods, each of which can be classified correctly here and
still run a stale executable. Call each guarded method against a stale model with **no
other arguments**: if validation runs first every one of them fails with the staleness
error, while a method that validates late fails with a missing-argument complaint instead
— so the matrix checks ordering rather than only presence, and none of the ten get far
enough to need MPI, data or an algorithm. The must-nots carry equal weight and cost the
same, since guarding `$format()` or `$code()` is the regression this section argues
against. `$initialize()` is excluded for the reason given below; `$clone()` is called and
asserted not to error.

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
was *never* built from — #1228's failure in the opposite direction. Both lines were
written under the older contract where `$code()` meant "the file as it is now"; the
accessors have since been redefined and this has not caught up.

**The two lines are not the same age, and the difference decides a NEWS entry.** The
`variables_` clear is #1235's, written and reverted inside the unreleased window, so
`NEWS.md:94` goes with it and no user upgrading from 0.9 ever saw it. The `stan_code_`
reassignment is commit `1719851e` from April 2022 and shipped in 0.7.0 through 0.9.0,
so removing it changes *released* behaviour and owes an entry of its own. `NEWS.md:94`
describes both in one sentence, which is how deleting it as a stale unreleased entry
would silently take the released half down with it.

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

A rebuild is triggered by `force_recompile = TRUE`, or when any field §4's table marks
**compared** differs from what this call computes. Which fields those are is §4's to say
and is not restated here, under its own rule that a rule written in two places is a
future inconsistency. The dependencies among them are the Stan program, its resolved
includes, the user header and `make/local` (§8 carries the shape), named here only so
this section can be read without holding the table open.

Or when the record cannot be used at all:

- the executable does not match `artifact` — replaced by another process, or corrupt
- the record is missing, unreadable, or written in a format version this cmdstanr
  does not read
- the executable predates build records, so there is nothing to compare

These are *artifact-side*: reasons the record cannot be relied on, rather than
reasons the inputs changed. They belong in the same contract because the
constructor's response is identical — rebuild, and say why.

**`include_paths` is absent from §4's compared column deliberately.** Comparing it *as a
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
with an error naming it — `--include-paths` in `stanc_options`, matched on occurrence
(§3); `--include-paths` in `make/local`'s `STANCFLAGS`, where the message names the
file; and `STANCFLAGS` in `cpp_options` outright, since `stanc_options` is the channel
for stanc flags and a raw make-variable passthrough only duplicates it. In the
`make/local` case detection is a substring test on the flag, not a parse: we never
interpret `--include-paths`'s comma lists, quoting or separator forms, only refuse them.

**The two rejections are deliberately different in scope, and should not be unified.**
`cpp_options` is a cmdstanr argument, so the whole variable goes. `make/local` is
CmdStan's own configuration file — `make/local.example:20` ships
`STANCFLAGS+= --warn-pedantic` as a suggested line — so only the include-path flag is
refused there, not the variable. The check on `cpp_options` belongs in
`assert_valid_cpp_options()` (§3, #1250), which `cmdstan_make_local()` does not call
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

**An unsupported format version is among those artifact-side reasons** — it rebuilds,
with the reason stated (§4), whether the version is newer or older than what this
cmdstanr supports.
It is still worth *distinguishing* from an unreadable record, because the two warrant
different messages.
**Unreadable means the record could not be accepted as a record at all** — invalid JSON,
a missing or unusable `format_version`, or a required field that is absent or the
wrong shape. *Required* is relative to the record's own `format_version`: a field added
in a later version is not required of a record written before it.
Unsupported means a version was found, and read, and this cmdstanr does not interpret
it. Same behaviour, different diagnosis — and the vocabulary is kept
apart deliberately, since a record whose version we do support can still be corrupt.

**Dependencies are identified by content.** A dependency matches when its content hash
matches what the record holds, wherever it now lives. Moving a project does not
rebuild it.

**Renaming the source file does rebuild, and that is not an exception to it.** A Stan
source is identified by content and its directory is not compared; its basename is a
compiler input, since `R/model.R:835` passes it to stanc as `--name` and §3 makes the
file name the only way to set that flag. So moving a project costs nothing while renaming
`bernoulli.stan` to `survival.stan` costs a compile, for the same reason a changed
`stanc_options` entry does. §4's `request.stanc_name` row carries the argument. The user
header is the one dependency whose directory *is* compared, for an unrelated reason
given below, so neither statement is the general rule about paths.

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
not launch at all.

**Which is why an executable is launched with its builder's TBB rather than the
session's.** The two platforms fail in opposite directions, so it takes one rule to
cover both. `compiler_flags:329` bakes an absolute `-rpath` into the binary and `:327`
guards that out on Windows. Measured on a compiled model, `otool` reports a single
`LC_RPATH` into the builder's `stan/lib/stan_math/lib/tbb`, with no fallback entry. So
on macOS and Linux `tbb_path()` returns `NULL` (`R/run.R:1238-1248`), cmdstanr supplies
nothing, and a missing builder means the loader refuses the binary. On Windows there is
no rpath, `tbb_path()` defaults `dir` to `cmdstan_path()`, and every runtime call site
takes it bare (`R/run.R:336`, `:422`, `:660`, `:782`), so the session's current
installation supplies the TBB whatever built the binary.

That second half is not an adoption problem, which is what makes this a general rule
rather than an adoption one. Build a model, call `set_cmdstan_path()`, then sample: on
Windows a 2.39 binary runs against 2.40's TBB, in released cmdstanr, with no record
involved. `instantiate` reaches the same state by design rather than by accident —
`stan_package_model()` sets the CmdStan path, constructs the object, and restores the
previous path `on.exit`, so by the time the user samples the session points at a third
installation.

So **cmdstanr supplies the TBB directory of the installation recorded as the builder**,
and `cmdstan_path()` becomes the fallback for a model with no record rather than the
default for every model. `tbb_path()` already takes `dir` and `R/install.R:485` already
calls it that way, so nothing is needed but passing it.

**A missing builder is reported, and is not itself a rebuild trigger.** It is not the
record going bad: the record parses, it hash-binds to this binary, and
`stan_build_info()` answers out of it. What it says is that the executable may not run,
and whether anything can be done about that turns on the *selected* installation rather
than the recorded one. Select a different one and `builder` differs, so the
ordinary trigger above has already fired and the model rebuilds against a CmdStan that
exists. Leave the selection alone and the gone installation is also the one a rebuild
would run in, so making this a trigger buys a doomed compile in place of the honest
failure below. With only an executable (§7) there is nothing to rebuild from at all.

So the record is kept and reported in every case, and refusing it would break a working
setup: a model built with `cpp_options = list(tbb_lib =, tbb_inc =)` against a system
TBB has an rpath outside CmdStan entirely and runs with the builder tree deleted. On
Windows the fallback applies for a second reason: putting a directory that is gone on
`PATH` is worse than today's wrong-but-present one. `stan_build_info()` reports the
builder with `exists = FALSE` — the treatment §7 already gives recorded sources that
are gone, for the same reason — and a launch failure becomes an error naming the
recorded installation, with reinstalling it or rebuilding from source as the two
remedies.

**A selected installation that is gone is its own error**, with or without a record,
because both halves of the work are inside it: re-resolution invokes its stanc
(below) and `make` runs in it. `set_cmdstan_path()` checks the directory once and
caches the path and version (`R/path.R:69-77`), and `cmdstan_path()` hands back the
cached value without rechecking (`R/path.R:93-100`), so a directory deleted or
unmounted mid-session goes on being handed out. Measured, what that reaches is
`cannot start processx process 'make' (system error 2, No such file or directory)`,
which reads as a missing toolchain. Check the selected installation before using it,
and name it.

**Report every applicable trigger, not whichever branch is checked first.** Today's
`if`/`else if` chain (`R/model.R:726-739`) reports one. A user who changed both the
source and `make/local` should be told both.

**The rule needs a usable record to be about anything.** With one, report every
compared field that differs. Without one — the artifact-side reasons above — report
why the record could not be used, and stop there: missing, unreadable and unsupported
leave no baseline to measure the inputs against, and one bound to a different
executable would supply the wrong baseline. This is the rule's precondition rather
than an exception to it.

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
differs from `builder`, that is already a rebuild trigger (above) and should be
reported without attempting re-resolution at all. That leaves one way for the
recorded stanc to be missing — the selection is the recorded installation and it is
gone — and there the model can be neither assessed nor rebuilt, which is the error
above rather than a reason to re-resolve with some other installation's stanc.

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

- **`make/local` including another makefile** (`make_local_include`). `make/local.example:36` ships with
  `# -include $(HOME)/.config/stan/make.local`, so it is a suggested pattern.
  Parsing arbitrary Make syntax is not justified for v1.
- **Headers transitively included by `USER_HEADER`** (`user_header_include`). Hashing
  the top-level header misses them.

In both cases a regex — `^\s*(?:-?include|sinclude)\b` for `make/local`,
`^\s*#\s*include\s*"` for the user header — tells us there *is* an untracked
dependency, without resolving anything. `sinclude` is GNU Make's silent-include
spelling and costs one alternation rather than any Make parsing; verified on Make
3.81, it loads the named file and stays quiet when that file is missing, exactly
like `-include`.

**The field is `known_untracked_dependencies`, not `provenance_complete`.** A regex
can establish that a gap exists; it cannot establish that none does. Make also has
variable expansion and `eval`; C++ has angle-bracket local headers,
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

**They are preserved, and adoption has three outcomes.** Calling them all unprovenanced
would discard information we may have written ourselves —
`compile_stan_file()` followed by `cmdstan_model(exe_file = path)` is a first-class
flow under this design, and it produces an executable *with* a record.

| the artifact arrives with | adoption | provenance |
|---|---|---|
| a usable hash-bound record | succeeds, without launching the binary | known |
| no usable record, and `<exe> info` reporting a valid version | succeeds | unavailable |
| no usable record and no valid version from `<exe> info` | **errors** | — |

The first two are what the rest of this section describes. The third is the only state
here that refuses an executable, and the version rule below is why it refuses nothing
that could have sampled.

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

**Adoption establishes what the artifact is, not that it runs.** A hash-matched record
is read and nothing executes the binary, so an executable that cannot run on this
machine adopts successfully and fails when something first runs it. The alternative is
spawning a process on every adoption to learn something the first fit establishes
anyway — which is the cost `instantiate` pays per fit rather than once at install (§9).

**Executable without a usable record** — missing, unreadable (an unparseable `builder`
version among the field checks that decide it, §4), hash mismatch, or written in a format
version this cmdstanr does not read.
Explicitly unprovenanced, which is a statement about *provenance* and must not
suppress what the binary does report. `stan_build_info()` returns an explicit
unavailable provenance, never an empty result that could be mistaken for "nothing was
configured", **together with the `reported_features` the executable supplies**. §8 carries
the shape, and the four forms named above are the four reasons it reports. The four `STAN_*` flags and the
version from `<exe> info` are real information, and §1's separation is exactly what
makes reporting them here consistent rather than contradictory: the request is
unknown, the reported features are not.

Here `$cpp_options()` **is** empty, and that is the honest answer — no request is
known, and inventing one from the four flags the binary happens to report is exactly
the merge §1 rejects. This is the one case where a user must call
`stan_build_info()` to learn anything, and error messages about options should say
so rather than leaving them at an empty list.

**Both paths must yield a syntactically valid version, and adoption fails if neither does.**
Adoption is the one place a version arrives from an artifact nobody vouched for;
everywhere else it comes from the CmdStan installation the session validated at install
time. So this is the only place the invariant §10 relies on can be established. A usable
record's half is enforced where the record's other field checks are (§4), so a record
carrying an unparseable `builder` version is not a usable record; on the fallback,
`<exe> info` must report complete version fields.

Failing both means the executable did not identify itself as a supported CmdStan
executable, which is a weaker claim than "it did not run" and is the one to make. A
two-line shell script that exits 0 reaches the same place, since `model_compile_info()`
then synthesises `".."` from three absent fields. What it does not reach is a CmdStan
binary that could have sampled: `info` has printed `stan_version_*` unconditionally
since CmdStan 2.27 (`write_stan.hpp`), eight releases below cmdstanr's own floor of 2.35
(`R/path.R:145`). The error refuses artifacts that could not have sampled either, not
ones we merely cannot identify.

**"Syntactically valid" means the grammar cmdstanr already uses**: three numeric
components with an optional release-candidate suffix, anchored at both ends —
`^[0-9]+\.[0-9]+\.[0-9]+(-rc[0-9]+)?$`. The grammar is not a new contract invented here.
It is the version half of `R/path.R:298`'s `^cmdstan-[0-9]+\.[0-9]+\.[0-9]+(-rc[0-9]+)?$`,
which decides what counts as a CmdStan installation directory, and of the
trailing-component match at `:337`. What differs is where the anchors sit, and that
follows from the job: there the string is a directory name and the anchors bind the
`cmdstan-` prefix along with the version, while here the whole string is the version.
Unanchored, `grepl()` accepts `cmdstan 2.36.0 (broken)`.
Note for whoever implements it that `cmdstan_version_for_comparison()` strips `-rc[0-9]+$`
(`R/path.R:156`), so the check runs on the reported string before that stripping.

**`utils::compareVersion()` must not be the validator.** "It did not complain" is a
weaker property than "this is a CmdStan version": `compareVersion("2.36", "2.35.0")` and
`compareVersion("2.36.0.1", "2.35.0")` both return `1` with no warning and no error, and
neither input is a version CmdStan reports. Only the grammar rejects them.

It is a syntactic check and nothing more. **Rejecting a version for being old would
defeat this whole section**, whose purpose is that a binary built by an older CmdStan
keeps working. Without the check, `model_compile_info()` synthesises `".."` from three
absent fields (`R/cpp_opts.R:68`) — a string that passes every guard
`cmdstan_version_compare()` has, so construction succeeds silently and the failure
surfaces later inside a version gate, complaining about `TRUE/FALSE` values in code the
user never called. An executable that cannot say what built it is not a CmdStan
executable, and the error says so, sharing #1246's message rather than inventing a
second one. Tests: an `info` result missing the version fields, and one printing a
malformed value.

Both rows above that succeed — the usable hash-bound record, and the executable
admitted on a valid version alone — permit fitting and **never attempt an automatic
rebuild**, there being no source to build from. The second of them is the deliberate
exception to §5's requirement that a model have a valid record before running. The
first has one.

**That exception is also who pays when a `format_version` stops being readable** (§4),
which is worth pricing before treating a bump as routine. An ordinary model reads a
version it does not support, rebuilds once, and is current again. An adopted one
cannot rebuild, so it drops to the unprovenanced path above and stays there until
whoever produced the executable rebuilds it — for a package that compiles at install
time, until the user reinstalls it. Nothing breaks: fitting, the runtime validators
and `reported_features` are all unaffected, and what remains is exactly the pre-record
behaviour. What is lost is provenance, and the saving of not launching the binary to
get it.

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

**With no `stan_file`, an explicitly supplied argument that can only be honoured by
building or by reading the source is an error** — `cpp_options`, `stanc_options`,
`include_paths`, `user_header`, `force_recompile`, `pedantic`. Silently ignoring any of
them is the failure mode this design exists to remove: the user believes they asked for
something.

The rule is framed on both halves rather than on build configuration alone, because two
of the six fail on the source rather than on the build, and a frame that named only the
build would need an exception written for each. `cpp_options`, `stanc_options`,
`user_header` and `force_recompile` cannot configure an artifact that will not be
rebuilt, and a valid record is there to be *inspected*, not overridden. `include_paths`
configures **source resolution**, and every stanc invocation needs it — compiling,
`$check_syntax()`, and the `$variables()` call `$sample()` makes to validate data
(`R/model.R:1410`) — so it is meaningful whenever a source is registered, whether or not
anything is compiled. `pedantic` is a request scoped to the call (§4), and what it asks
for is a stanc run over the program; with no program there is nothing to run and nothing
to report, so the guarantee that it produces diagnostics on every call cannot be kept
quietly. For both, the reason is the missing source, not the missing build.

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
stan_build_info(exe_file)
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

`stan_variables()` is the one name to revisit before it ships. `metadata()$stan_variables`
already exists on fit objects (`R/csv.R:362`) and means something else — the variable
names in the output, not the declarations in the program. That is not a collision in R
and not a reason to hold the design; the name is provisional and the decision belongs
with the implementation.

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

Asking for the same flag through `stanc_options` is refused however it is spelled (§3), so
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

**`stan_build_info()` returns a public result, not the parsed record.** Saying only
that it returns "a parsed object" does not settle this: the obvious implementation is
`jsonlite::fromJSON()` on the record, and then the returned object *is* the on-disk
schema one deserialisation removed. That contradicts §4, which makes the format
private and versions it so that it can change — a later cmdstanr that stores
`cpp_options_supplied` in a different shape bumps `format_version` and reshapes the
field, and every script reading `info$request$cpp_options_supplied` breaks on a change
§4 says is ours to make. So the
reader **translates**: the public field names are the promise, and the record's layout
is free underneath them.

**The result is a list with class `"stan_build_info"`.** That is what the print
method below dispatches on. One class name rather than a vector: `is.list()` is already
`TRUE` without adding `"list"`, and a second name only offers someone a wrong target
to write a method against. The class name follows the function name, so §10's note
that `stan_build_info()` is still a placeholder covers both.

| field | present when | holds |
|---|---|---|
| `provenance` | always | `status` and `reason`, both always present |
| `reported_features` | always | one entry per feature: four logical flags, each `TRUE`, `FALSE` or `NA`, and `stan_version`, a character scalar or `NA_character_` |
| `format_version` | depends on the reason, not the status (below) | which contract wrote it: the fields it carries and how they read |
| `request` | provenance available | the recorded build configuration of §1 |
| `dependencies` | provenance available | every file whose content the build consumed |
| `artifact` | provenance available | a hash of the executable, only useful for comparing against another one |
| `builder` | provenance available | installation path, version, `exists` |
| `known_untracked_dependencies` | provenance available | §6's list; empty means nothing was detected |

`artifact` and `format_version` are both public, and `format_version` is not the
coupling this rule exists to prevent: exposing the *value* is not exposing the
*layout*. The promise is one integer naming which contract wrote the record, and it
survives any rearrangement of the JSON beneath it.

**Every hash in the result is a token to compare, and nothing else** — `artifact`, and
every `hash` under `dependencies`. No algorithm is named, because §4 lets the hashing
change at a `format_version` bump, and naming one would put a guarantee users can act
on behind a signal this section keeps private. That bound also limits the comparison:
two hashes are comparable when they are the same field taken from records at the same
`format_version`, and across two versions they say nothing, since the algorithm may
have changed in between.

**The nested names are settled here rather than by whoever implements it**, because a
test for the public shape cannot be written from a list of eight top-level fields.
A source-backed model with one include, no user header, and a `make/local` that
includes another makefile:

```r
list(
  provenance = list(status = "available", reason = NULL),
  reported_features = list(
    stan_threads = TRUE, stan_mpi = FALSE, stan_opencl = FALSE,
    stan_no_range_checks = FALSE, stan_version = "2.39.0"
  ),
  format_version = 1L,                    # a stand-in; §4 does not fix this number
  request = list(
    cpp_options_supplied   = list(STAN_THREADS = TRUE),
    stanc_options_supplied = list(),
    stanc_options_injected = list(name = "bernoulli_model"),
    stanc_name             = "bernoulli_model",
    include_paths          = "/proj"
  ),
  dependencies = list(
    stan_file      = list(hash = "a1b2…", built_from = "/proj/bernoulli.stan", exists = TRUE),
    included_files = list(
      list(hash = "c3d4…", built_from = "/proj/inc/half.stan", exists = TRUE)
    ),
    user_header    = NULL,
    make_local     = list(hash = "e5f6…", built_from = "/cmdstan-2.39.0/make/local", exists = TRUE)
  ),
  artifact = "9a8b…",
  builder  = list(path = "/cmdstan-2.39.0", version = "2.39.0", exists = TRUE),
  known_untracked_dependencies = list(
    list(kind = "make_local_include", detected_in = "/cmdstan-2.39.0/make/local")
  )
)
```

Each of the rules below explains a place in that sketch where the obvious shape is not
the one chosen.

**The user header appears once, under `dependencies`.** It is a file the build read,
with a content hash, so it belongs beside the other files the build read rather than in
`request` with the options. §4 compares it as `dependencies.user_header.built_from` and
the record holds it in that one place too, so nothing is translated here. The rule
exists because carrying it in both is the natural thing to write, and it puts one
normalised path in two public places that can never disagree — a second owner, in the
API rather than in the prose.

**The effective name is `stanc_name`, not `model_name`.** It earns a field of its own
even though `--name` also appears in `stanc_options_injected`: that list says who asked
for the flag and is not compared, this says what stanc received and is, and a field is
one or the other rather than both. `request.include_paths` is already the same kind of
field, recording what the build resolved rather than what the caller typed. What the
name avoids is a trap, since the value carries the `_model` suffix
`R/model.R:835` appends: `mod$model_name()` returns `bernoulli` where this returns
`bernoulli_model`, and two fields spelled alike with different values is worse than one
named after the flag it holds. Each sampler output CSV carries it verbatim on a
`stancflags` line, beside the `model =` line holding what stanc compiled — the same
string unless the name needed mangling, which §4 prices along with why this is the one
to compare.

**`make_local` is `NULL` when the installation had none**, and that has to be
distinguishable from a `make/local` that hashes to something, or creating the file
after a build would not trigger §6's rebuild. No unknown case arises: a record always
knows whether the file was there.

**A known untracked dependency says which gap and where it was found, never what it points at.**
Each entry is `list(kind, detected_in)`. `kind` is one of §6's two cases,
`make_local_include` or `user_header_include`, and `detected_in` is the file the regex
matched in — the `make/local` or the user header itself, not the thing it includes. The
unresolved target is deliberately absent, because resolving it is the work §6 declines
to do and a field for it would end up holding a path sometimes and a guess the rest of
the time.

That does repeat a path `dependencies` already holds, which the header rule above
forbids for `request`. The difference is that this field is never compared (§4), so a stale
copy cannot move a verdict, and an entry that names its own file is what lets the
printer build its message from the entry alone.

**One entry per distinct `(kind, detected_in)` pair, ordered by `kind` then `detected_in`.**
Nothing in an entry says how many times the regex matched, so a header with two quoted
includes would otherwise contribute two entries equal byte for byte. With one file per
kind today the list is at most two entries long, but the rule is written on the pair
rather than the kind, so a later kind that can match in more than one file needs no
amendment. Fixing the order costs a line and makes the list comparable as a value, where
leaving it unspecified pushes a sort into every test that touches it.

**`reported_features` has fixed names, and unknown is `NA`.** §1 encodes unknown by
omitting the key, which is right for the record and wrong here. Nothing in this result
round-trips through a file, so `NA` keeps its type, and that is the whole of the
difference: `NA` is a state you can ask about, where a missing member gives back an
answer that is neither yes nor no.

```r
is.na(NA)     # TRUE
is.na(NULL)   # logical(0)
```

Neither encoding buys a loud failure on its own. `isTRUE()` reads both as `FALSE` and
`if ()` errors on both, so a caller who writes `isTRUE(x$stan_threads)` collapses
unknown into disabled whichever way the field is encoded. What fixed names and `NA` buy
is that the collapse becomes the caller's to avoid rather than invisible to them, and
that `names(reported_features)` is a fixed set a test can hold — the same property this
section relies on for `names(provenance)`. The names are the four booleans
`<exe> info` prints — `stan_threads`, `stan_mpi`, `stan_opencl`,
`stan_no_range_checks` — plus `stan_version`, and the table above has their types. They
are always all present. A flag CmdStan starts reporting gets added here deliberately
rather than appearing on its own.

**`provenance` carries why, not only whether.** §7 already enumerates four ways a
record fails to describe an executable, and collapsing them to a bare "unavailable"
throws the distinction away at the only point a user can act on it:

```r
provenance = list(status = "available",   reason = NULL)
provenance = list(status = "unavailable", reason = "record_missing")
                                        # "record_unreadable"
                                        # "artifact_mismatch"
                                        # "unsupported_format"
```

Both names are always there. `available` requires `reason = NULL`, `unavailable`
requires exactly one code, and `names(provenance)` is the same pair either way, which
is what a test can hold. The enum is machine-readable and no free-form message is
stored — the printer derives its prose from the reason, so the wording stays revisable
and never becomes contract.

Which of the four leaves a `format_version` behind follows from whether the reader
established one it can trust:

| reason | `format_version` | |
|---|---|---|
| `record_missing` | absent | nothing to read it from |
| `record_unreadable` | absent | an unreadable record is withheld whole (§4), even where its version parsed |
| `unsupported_format` | present | a version was read and this cmdstanr does not interpret it |
| `artifact_mismatch` | present | the record parsed and its version is one we read; only its binding to this executable is wrong |

**`unsupported_format` is not evidence that cmdstanr is old.** §4's format-version
rule runs in both directions, and a downgrade regenerates old-format records, so the
record may be either side of the readable set. The printer reads the direction off the
reported `format_version` and says the corresponding thing: upgrade cmdstanr for a
newer record, rebuild or obtain a newly produced artifact for an older one.

**Unknown and empty must never render alike, anywhere in the result.** This is §6's
`known_untracked_dependencies` rule and §1's absence-of-evidence rule stated once as a
property of the whole object rather than per field. An empty
`known_untracked_dependencies` means the scan detected nothing; no record to scan means
the field is absent. A recorded builder whose path is gone is `exists = FALSE`; an
absent `builder` means there was no usable record to read one from, which is the only
way it can be missing. An unknown request is absent, not
`list()`.

That last one is where the two accessors answer the same executable differently, and
both are right. §7 keeps `$cpp_options()` empty for an unprovenanced executable because
that accessor reports what the caller asked for and nobody asked for anything.
`stan_build_info()` reports what is known about the build, so it must say unknown.

**A readable record whose hash does not match is read only to say why.**
`artifact_mismatch` returns no record-derived field but the reason and
`format_version`, even though `request`, `dependencies` and `builder` all parsed.
`reported_features` still comes back, read off the executable rather than the record,
which is the general rule below and not an exception to this one. This is the one
place the reader holds back data it can see, so it is worth saying why to whoever
implements it: reporting the fields with a caveat is the natural instinct and it is
wrong, because
`reported_features` from that record is what §1 sends the runtime validators to. A
helpful partial report puts `stan_threads: true` from some other build in front of a
validator guarding this one, which is the silently single-threaded run this design
exists to remove, arriving through a new door. Note that relocation never reaches here:
a moved executable keeps its bytes and its hash, and §6 compares dependencies by
content, so `artifact_mismatch` means the executable at this path was replaced without
cmdstanr writing a new record.

**Unavailable provenance still reports the binary's own features.** All four reasons
are §7's "executable without a usable record", so all four read `reported_features`
off the executable; an available one reads them from the record. That is one rule covering five
states with nothing carved out of it, and it is what keeps `artifact_mismatch` narrow:
what a mismatched record loses is everything it *claims about the build*, not what the
binary says about itself.

A test has to prove where the features came from, not that the field is populated.
Reporting them with a caveat from the rejected record fills the field, so an assertion
that it is non-empty passes. Make the record say `stan_threads = TRUE`, make `<exe>
info` say `false`, and require `FALSE`.

**The `exe_file` argument has two failure modes and both are errors.** The name
matches `cmdstan_model(exe_file = )` and `$exe_file()` rather than inventing a third
spelling for the same thing. A path that is missing or is not a file errors.
Otherwise, a valid hash-matched record is answered from the record alone, without
launching the binary — §4's adoption rule already works
this way, and it is what keeps a cross-platform artifact fully reported. Only an
executable with no usable record is launched for `<exe> info`, and if that fails the
function errors rather than returning an all-unknown result: with neither a record nor
an info response there is nothing to report, and an object of unknowns would be zero
information wearing the shape of an answer. Feature-level *unknown* stays for a feature
genuinely absent from a valid record or a successful info response, and never stands in
for total inspection failure.

**`$format()` gets a standalone plus a method wrapper.** The case for keeping it
method-only — that it invalidates `stan_code_` and `variables_`
(`R/model.R:1309-1311`) — is weak: another model object or an external
editor already bypasses that invalidation, so it was never a guarantee. §5's
pre-run validation is what actually makes it safe.

**`dry_run` demotes to internal.** Its documentation says *"Used to speedup tests"*
(`R/model.R:558-559`); 22 test uses, zero vignette uses. It stays as an argument to
the internal compile machinery that the public entry points wrap.

### `compile_model_methods` and `compile_standalone` are removed

Neither is build configuration. `compile_standalone = TRUE` runs
`expose_stan_functions()` into `self$functions` after make finishes (`R/model.R:963`),
and `compile_model_methods = TRUE` runs `expose_model_methods()` into the environment
fit objects copy (`:966`). Neither sets a make flag or changes a byte of the executable,
which is why neither appears in `compile_impl()` above: that signature was written from
what the build consumes.

**They are already broken on the reuse path, in released code.** `$compile()` returns at
`R/model.R:804`, inside the `if (!force_recompile)` branch that opens at `:741`, and
both exposure calls sit past that return. The same call therefore populates `functions`
or does not, depending on whether a rebuild happened to be needed:

```r
mod <- cmdstan_model("bernoulli.stan", compile_standalone = TRUE)
# fresh session, executable still current
mod <- cmdstan_model("bernoulli.stan", compile_standalone = TRUE)
mod$functions$foo   # not there
```

That is the failure this design exists to remove: a request the call cannot honour and
does not report. Keeping them would mean writing a reuse-path rule for a case that has
been wrong since before 0.9.0.

**The replacements are the ones their own documentation already names.**
`R/model.R:551` tells the caller to use `fit$init_model_methods()` instead when the
model will be saved, and `:556` says `$expose_functions()` does the same job after
compilation. Both are public, both are tested, and neither depends on the reuse path,
because they run when they are called.

**`$expose_functions()` has to be fixed in the same change, since removal makes it the
only route.** `expose_stan_functions()` refuses whenever `function_env$existing_exe` is
`TRUE` (`R/utils.R:1217`), and the no-op path sets exactly that: `:267` initialises it
`TRUE`, `:299` sets `exe_file_` only when the caller passed `exe_file`, and `:786`
branches on `length(private$exe_file_) == 0`, still true for a source-only construction.
So `cmdstan_model("m.stan")` on an up-to-date executable, followed by
`mod$expose_functions()`, errors with *"Exporting standalone functions is not possible
with a pre-compiled Stan model!"* about a model that has a source sitting beside it.
`existing_exe` should mean "this model has no source" rather than "this object did not
personally run make", and the hpp should be generated on demand from the registered
source the way `pedantic` re-runs stanc. The error stays for models that genuinely have
no source (§7).

Taken together, nothing is lost: on the reuse path today neither route works, one in
silence and one with a message describing a different model.

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
implementation needs. Staging is the issue's to own, so where the two disagree about
what lands when, #1258 is right — the contracts being staged are still this
document's, and the boundary is stated under Purpose and scope. What lives here is
what an issue checklist cannot carry: why the sequence is what it is, and what breaks
under the orderings that were rejected.

Two constraints shape it. The Make-option fixes come first, because per-field
canonicalization depends on them. And **the API change and the *live* decision engine
ship as one stage** — separating those leaves a window where the new promise is broken
whichever way the cut is made (Stage 4), though the engine's pure, unwired half
separates cleanly and is Stage 3b.

**#1258 states the release order; this section states the constraints behind it.** The
split follows the boundary already set under Purpose and scope, and it has a test: if
the order changes, the work list is what you edit; if the *reason* changes, this is.
Both printing it is how four different statements of it came to exist in an earlier
draft.

The candidate ships when everything is ready rather than at the earliest defensible
moment, and that is a structural choice rather than a preference. The moment an item can
be adjudicated safe-before or safe-after the tag, every item needs adjudicating, and the
answers settle into separate paragraphs that drift apart — which is exactly how four
different statements of this order came to exist in an earlier draft of this section.
One definition of ready removes the adjudication instead of getting it right each time.

What downstream gives up by waiting is a tag, not a start. We open their pull requests
ourselves (below), and the dev version supplies the boundary in the meantime: bumped **in
the same pull request as each break**, it lets brms guard on `packageVersion("cmdstanr")`
against master the day the break lands, which is finer-grained than a tag rather than a
substitute for one. Bumping in a follow-up commit is worse than not bumping at all, since
a guard written against the new number then takes the old branch and calls a method that
has already gone.

**Each guard names the stage it needs, not a number chosen now.** brms moves onto the
standalone family, which arrives in Stage 4, so its boundary is *the Stage 4 dev
version*, whatever that pull request assigns. Writing a literal here picks the wrong
stage as soon as the arithmetic moves: from `0.9.0.9002`, Stage 1's bump is `.9003`, so
a guard written against `.9003` today would switch brms to `compile_stan_file()` three
stages before it exists — the partial migration this paragraph exists to prevent. The
downstream pull requests we open carry the real numbers.

**A bump is owed for a public contract downstream has to branch on, not for anything
observable.** Stages 1, 4 and 5 owe one; Stages 2, 3 and 3b do not. Stage 3 is the case
that makes the distinction necessary rather than pedantic: it creates a sidecar beside
every executable and can print the untracked-dependency note, both plainly observable,
and neither is something brms or instantiate could write an `if` against. A version
number exists so someone can branch on it, and there is nothing there to branch on. What
the sidecar does raise is whether instantiate carries the record into the package library
alongside the staged executable, which is install mechanics and belongs to #1238; no
version guard would answer it.

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

Its own NEWS entry and its own test; it is small in code but it changes what a user
reads in every runtime error from a model built after it lands. Executables built
before it keep the tempfile path, since a change to which options cmdstanr injects
rebuilds nothing already built (§4), and the NEWS entry should say so.

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

`stan_build_info()` last, because **it publishes answers Stage 4 settles**. Its inputs
exist a stage earlier — Stage 3 writes the record and captures `reported_features` — so
the ordering is not about availability. It is about meaning. Until Stage 4 deletes the
merge, `$cpp_options()` still answers "what is this binary" by mixing the report into
the request, so a function published before then would arrive into a world where its
own purpose is not yet true, and Stage 4 would change what it reports. It must also
answer for an unprovenanced executable (§7), and record-aware adoption is Stage 4, so
before that the case it most needs to cover does not exist. That is an ordering fact
rather than a scheduling preference, so it cannot drift. It ships before the candidate,
per #1258.

**It must ship in 1.0**, despite looking purely additive. Because §1 keeps the
request separate from what the binary reports, this is the only way to ask what an
executable actually is, and the
only answer available at all for an unprovenanced one (§7). From the candidate onward
its output may gain fields — the dependency reporting is expected to — but may not
rename or remove one. That is ordinary candidate discipline for any published API, not
a dispensation this function needs arguing for.

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

**The pass covers inherited gaps, not only this design's.** Release notes belong to the
release rather than to whoever caused each line, and the unreleased window already
carries user-facing changes that were never written up: measured against merge subjects
since `v0.9.0`, roughly a quarter of the merged pull requests that touch behaviour have
no entry, among them a new exported function, two new public methods and a changed
default on `loo()`. #1258 owns the method and the list.

### The release candidate

The candidate ships after the NEWS reconciliation, per #1258, so downstream
packages have something to migrate against rather than a release note. That is what
makes §8's breaking change affordable.

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
eventual one.

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
the line is the whole fix. `cstan()` propagates to end users, because `compile` is
rethinking's own documented argument (`R/cmdstan_support.r:17`), passed straight
through.

**brms and instantiate propagate too, through `...` rather than through a named
argument.** `brm(stan_model_args = list(...))` becomes `compile_args` and reaches
`do_call(cmdstanr::cmdstan_model, args)` inside `.compile_model_cmdstanr()`, and
`instantiate::stan_compile_model()` and `stan_package_model()` both end their signature
with `...` and forward it verbatim. So any argument removed from `cmdstan_model()`
reaches users who never call cmdstanr directly, while the packages themselves need no
change for it — which is what the migration note has to say, since "grep your own code"
is easy advice to skip when you only ever call `brm()`. Neither package names
`compile_model_methods` or `compile_standalone` anywhere, measured across both installed
trees; rethinking cannot be reached this way at all, because its four call sites name
every argument and forward no dots. brms already uses the replacement:
`.expose_functions_cmdstanr()` calls `stanmodel$expose_functions()`, and
`expose_functions.brmsfit` tests `"expose_functions" %in% names(stanmodel)`, so a
downstream package inspects the R6 object for that method by name (§5).

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

Air's one-time whole-repo format (#1153) is the **last** change before the release
candidate, **and it is optional**. It goes before the tag rather than after because a
candidate that is not the source we ship is not a candidate: the tag exists so people
test what becomes 1.0, and a whole-repo automated rewrite afterwards leaves the tested
tree and the released tree differing by a diff nobody reviewed against the release.
Optionality does not answer that objection. It decides *whether* Air runs, not *when*,
and the decision can be taken when the NEWS reconciliation lands.

Three arguments that look like they belong here do not. Branch conflicts are real but
choose no slot: eight open pull requests touch `R/` today, three of them untouched since
2025, so the cost is whatever happens to be open when Air runs, which is much the same
whenever that is. Whitespace-only determinism makes the change cheap in any slot.
And the worry that a reformatting diff on top of the API removal would hide what broke
does not survive Air being its own pull request, reviewed as whitespace-only with the
suite green — nothing lands on top of anything.

**One check when it runs.** Air reformats `#'` lines like any others, so a reflow that
moves a roxygen tag regenerates `.Rd` and `NAMESPACE` differently and R CMD check will
not notice. Re-run roxygen afterwards and confirm the generated files are unchanged.

Its PR-review action is a separate thing:
additive, conflicting with nothing, and most useful *during* the stages, since
Stages 2 to 4 write a good deal of new code that would otherwise be formatted after
the fact. Check first whether it comments on changed lines or on whole files; if the
latter, it waits for the format.

Jarl (#1172) does not travel with it. Adopting the linter is additive, but acting on
its findings is semantic editing, and that must not land after the candidate — 1.0
would then ship code in a form nobody tested. Those findings are ordinary reviewed
changes, taken whenever, not a sweep.

Neither is folded into Stage 4's own pull requests, where a reformatting or linting
diff carried alongside the API removal would leave a downstream maintainer unable to
see what actually broke. Air's slot after the NEWS reconciliation satisfies that on its
own: the removal is reviewed and merged by then, and Air's diff sits beside that work
rather than inside it.

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

**Absence of evidence is not evidence of absence — twice.** `reported_features`
is tri-state (§1): a feature CmdStan does not report is *unknown*, and treating
unknown as disabled reproduces #765 in a new place. `known_untracked_dependencies`
(§6) is the same shape: an empty list means nothing was *detected*, never that the
record is complete. Both drafts of this document got one of these wrong, so it is
worth checking for deliberately rather than trusting the field names.

Serialization is where it gets lost in practice. `jsonlite` writes `NA` as `null` and
reads it back as `NULL`, so an in-memory tri-state loses its R type on the way through a
file. The states stay recoverable through `names()`, but not through the access anyone
writes: neither `x[["k"]]` nor `!isTRUE(x)` can tell an explicit null from a missing key.
§1 settles this by encoding presence — a key is written only when the state is known —
so the serializer never sees a third value. Reintroducing `NA` for unknown *in the
record* reintroduces the collapse. §8 uses `NA` in the public result for the opposite
reason, and serialization is the whole of the difference: what never reaches a file
keeps its R type.

`cmdstan_version_compare()` is a third instance, in a different costume: it returns
`-1` for a version that is missing, `NA`, or empty (`R/path.R:162-164`), so an absent
version compares as older than everything and every `<` gate fires. That fallback is
correct where it is used, in install-path code where "no CmdStan" genuinely should lose
every comparison. It is wrong for a model.

The guard is also narrower than "unusable", which matters because it removes the
temptation to lean on it: a malformed non-empty string never reaches the `-1` at all.
Both `".."` and `"garbage"` error inside `utils::compareVersion()`, with `missing value
where TRUE/FALSE needed`; `"garbage"` emits `NAs introduced by coercion` first. So the
bad-input behaviour is an error, sometimes with a warning in front of it, and never the
`-1`. Filed as #1260,
separately from this design: the comparison should not answer a question it was not
asked, but fixing it is defence in depth rather than what closes this. That is why §7
makes "an adopted executable always yields a valid version" a *checked* invariant rather
than an observation, and why a model without an executable cannot reach a gate (§8).

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
