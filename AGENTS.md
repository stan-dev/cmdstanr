# Repository Guidelines

## Project Structure & Module Organization
`cmdstanr` is an R package. Core implementation lives in `R/` (R6 classes, model/run logic, utilities). Unit tests are in `tests/testthat/`, with shared helpers in `tests/testthat/helper-*.R` and fixtures under `tests/testthat/resources/` and `tests/testthat/answers/`. Package docs are generated into `man/` from roxygen comments in `R/*.R`. Long-form docs live in `vignettes/`. Installed example Stan programs and data live in `inst/`.

## Build, Test, and Development Commands
Run commands from the repo root:

```r
devtools::install_dev_deps()   # install development dependencies
devtools::load_all()           # load package for interactive development
devtools::test()               # run testthat tests
devtools::check()              # full R CMD check
devtools::document()           # regenerate NAMESPACE + man/ from roxygen
```

CmdStan is required for most tests/checks:

```r
cmdstanr::check_cmdstan_toolchain(fix = TRUE)
cmdstanr::install_cmdstan(cores = 2)
```

Optional coverage workflow:

```r
covr::package_coverage(type = "tests")
```

## Coding Style & Naming Conventions
Follow existing package style and the tidyverse style guide (2-space indentation, readable line breaks, explicit argument names where useful). Keep functions and files lowercase with separators as already used in this repo (for example, `test-model-compile.R`). Use roxygen2 with Markdown for documentation; edit source comments in `R/`, not generated `.Rd` files.

## Testing Guidelines
Tests use `testthat` and are organized as `tests/testthat/test-*.R`. Add or update tests with each behavior change, especially around model compilation, file IO, and platform-specific logic. Keep reusable fixtures in `tests/testthat/resources/`. Before opening a PR, run `devtools::test()` locally and then `devtools::check()`.

## Commit & Pull Request Guidelines
Use short, imperative commit messages (for example, `Fix OpenCL test skip on Windows`). For larger changes, open an issue first to align on scope. PRs should include:
- a clear summary of the change,
- `Fixes #<issue-number>` when applicable,
- updated tests,
- a `NEWS.md` bullet for user-facing changes.

Follow the PR template checklist, including copyright holder declaration and license agreement.
