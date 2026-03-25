# name in STANCFLAGS is set correctly

    Code
      cat(trim_stanc_invocations(out), sep = "\n")
    Output
      bin/stanc --name='bernoulli_model' --o

---

    Code
      cat(trim_stanc_invocations(out), sep = "\n")
    Output
      bin/stanc --name='bernoulli2_model' --o

# STANCFLAGS from get_cmdstan_flags() are included in compile output

    Code
      cat(trim_stanc_invocations(out), sep = "\n")
    Output
      bin/stanc --name='bernoulli_model'  --O1  --warn-pedantic --o

