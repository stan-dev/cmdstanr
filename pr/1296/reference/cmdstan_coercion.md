# Coercion methods for CmdStan objects

These are generic functions intended to primarily be used by developers
of packages that interface with CmdStanR. Developers can define methods
on top of these generics to coerce objects into CmdStanR's fitted model
objects.

## Usage

``` r
as.CmdStanMCMC(object, ...)

as.CmdStanMLE(object, ...)

as.CmdStanLaplace(object, ...)

as.CmdStanVB(object, ...)

as.CmdStanPathfinder(object, ...)

as.CmdStanGQ(object, ...)

as.CmdStanDiagnose(object, ...)
```

## Arguments

- object:

  The object to be coerced.

- ...:

  Additional arguments to pass to methods.

## Value

An object of the CmdStan fitted-model class corresponding to the
generic, as returned by the dispatched method.

## See also

[`as_cmdstan_fit()`](https://mc-stan.org/cmdstanr/dev/reference/read_cmdstan_csv.md),
[CmdStanMCMC](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMCMC.md),
[CmdStanMLE](https://mc-stan.org/cmdstanr/dev/reference/CmdStanMLE.md),
[CmdStanLaplace](https://mc-stan.org/cmdstanr/dev/reference/CmdStanLaplace.md),
[CmdStanVB](https://mc-stan.org/cmdstanr/dev/reference/CmdStanVB.md),
[CmdStanPathfinder](https://mc-stan.org/cmdstanr/dev/reference/CmdStanPathfinder.md),
[CmdStanGQ](https://mc-stan.org/cmdstanr/dev/reference/CmdStanGQ.md),
and
[CmdStanDiagnose](https://mc-stan.org/cmdstanr/dev/reference/CmdStanDiagnose.md)
