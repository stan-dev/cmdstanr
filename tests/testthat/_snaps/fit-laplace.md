# summary() and print() methods works after laplace

    Code
      full_print <- fit_laplace$print()
    Output
      variable mean median sd mad q5 q95
      lp__
      lp_approx__
      alpha
      beta[1]
      beta[2]
      beta[3]

---

    Code
      fit_laplace$print(max_rows = 1)
    Output
      variable mean median sd mad q5 q95
      lp__
      
      # showing 1 of 6 rows (change via 'max_rows' argument or 'cmdstanr_max_rows' option)

