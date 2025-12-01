# Coercion methods for CmdStan objects

These are generic functions intended to primarily be used by developers
of packages that interface with on CmdStanR. Developers can define
methods on top of these generics to coerce objects into CmdStanR's
fitted model objects.

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
