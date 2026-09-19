# Tests

The files are flat and the prefix says what a file tests.

`test-model-*.R` files test `cmdstan_model()` and the methods of the model
object it returns. `test-fit-*.R` files test a fitted model object, one file
per fitting method plus `test-fit-shared.R` for the methods every fit has.
`test-build-*.R` files test the build record (the hidden JSON file written
beside an executable), the code that writes and reads it, and
`stan_build_info()`, which reports it, using the fixtures in
`helper-build-record.R`; `test-build-record-compile.R` is the one of those
that runs stanc for real, with make mocked; `test-build-info.R` compiles one
model for real. A file with no prefix tests the function or feature
it is named after, so, e.g., `cmdstan_make_local()` is tested in
`test-install.R` because the function is defined in `R/install.R`.

Two files cover how `cmdstan_model()` decides between reusing an existing
executable and compiling a new one. `test-model-rebuild-rules.R` checks each
rule on its own (the Stan file changed, `cpp_options` changed, and so on)
against a mocked CmdStan, so nothing is compiled. `test-model-rebuild.R`
compiles for real and checks the executable that results from a sequence of
calls.

Files that compile call `set_cmdstan_path()` at the top. `with_mocked_cli()`
in `helper-mock-cli.R` stands in for `make` and for the executable's `info`
call, so a test can go through the build code without a compiler.
`mock_cmdstan_model()` wraps it and removes the stand-in executable and its
record when the calling frame ends, so a later real compile of the same
program does not find them. `setup.R` removes everything but the `.stan`
files under `resources/stan/` before and after a run.

`test-install.R` installs CmdStan from source, so it can be convenient to run 
single tests from it with the `desc` argument of `test_file()` rather than the 
whole file.
