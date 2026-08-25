# Pathfinder init candidates are distinct target parameter vectors

    Code
      process_init.CmdStanPathfinder(pathfinder_fit, 4, model_variables)
    Condition
      Error in `process_init_approx()`:
      ! Not enough distinct draws (4) in Pathfinder fit to create inits. Try running Pathfinder with psis_resample=FALSE.

# VB and Laplace inits error with the right algorithm label

    Code
      process_init.CmdStanVB(make_fit("CmdStanVB"), 2, model_variables)
    Condition
      Error in `process_init_approx()`:
      ! Not enough distinct draws (2) in VB fit to create inits.

---

    Code
      process_init.CmdStanLaplace(make_fit("CmdStanLaplace"), 2, model_variables)
    Condition
      Error in `process_init_approx()`:
      ! Not enough distinct draws (2) in Laplace fit to create inits.

