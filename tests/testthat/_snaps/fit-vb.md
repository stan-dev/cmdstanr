# print() method works after vb

    Code
      expect_s3_class(fit_vb$print(), "CmdStanVB")
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
      fit_vb$print(max_rows = 1)
    Output
      variable mean median sd mad q5 q95
      lp__
      
      # showing 1 of 6 rows (change via 'max_rows' argument or 'cmdstanr_max_rows' option)

---

    Code
      fit$print()
    Output
      variable mean median sd mad q5 q95
      lp__
      lp_approx__
      mu
      tau
      theta_raw[1]
      theta_raw[2]
      theta_raw[3]
      theta_raw[4]
      theta_raw[5]
      theta_raw[6]
      
      # showing 10 of 20 rows (change via 'max_rows' argument or 'cmdstanr_max_rows' option)

---

    Code
      fit$print(max_rows = 20)
    Output
      variable mean median sd mad q5 q95
      lp__
      lp_approx__
      mu
      tau
      theta_raw[1]
      theta_raw[2]
      theta_raw[3]
      theta_raw[4]
      theta_raw[5]
      theta_raw[6]
      theta_raw[7]
      theta_raw[8]
      theta[1]
      theta[2]
      theta[3]
      theta[4]
      theta[5]
      theta[6]
      theta[7]
      theta[8]

---

    Code
      fit$print(c("theta", "tau", "lp__", "lp_approx__"))
    Output
      variable mean median sd mad q5 q95
      theta[1]
      theta[2]
      theta[3]
      theta[4]
      theta[5]
      theta[6]
      theta[7]
      theta[8]
      tau
      lp__
      
      # showing 10 of 11 rows (change via 'max_rows' argument or 'cmdstanr_max_rows' option)

