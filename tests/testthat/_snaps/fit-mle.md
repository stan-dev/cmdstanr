# print() method works after optimization

    Code
      full_print <- fit_mle$print()
    Output
      variable estimate
      lp__
      alpha
      beta[1]
      beta[2]
      beta[3]

---

    Code
      fit_mle$print(max_rows = 1)
    Output
      variable estimate
      lp__
      
      # showing 1 of 5 rows (change via 'max_rows' argument or 'cmdstanr_max_rows' option)

