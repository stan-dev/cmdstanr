# print() method works after gq

    Code
      expect_s3_class(fit_gq$print(), "CmdStanGQ")
    Output
      variable mean median sd mad q5 q95
      y_rep[1]
      y_rep[2]
      y_rep[3]
      y_rep[4]
      y_rep[5]
      y_rep[6]
      y_rep[7]
      y_rep[8]
      y_rep[9]
      y_rep[10]
      
      # showing 10 of 11 rows (change via 'max_rows' argument or 'cmdstanr_max_rows' option)

---

    Code
      fit_gq$print(max_rows = 1)
    Output
      variable mean median sd mad q5 q95
      y_rep[1]
      
      # showing 1 of 11 rows (change via 'max_rows' argument or 'cmdstanr_max_rows' option)

---

    Code
      fit_gq$print(NULL, c("mad"))
    Output
      variable mad
      y_rep[1]
      y_rep[2]
      y_rep[3]
      y_rep[4]
      y_rep[5]
      y_rep[6]
      y_rep[7]
      y_rep[8]
      y_rep[9]
      y_rep[10]
      
      # showing 10 of 11 rows (change via 'max_rows' argument or 'cmdstanr_max_rows' option)

---

    Code
      fit_gq$print(max_rows = 2)
    Output
      variable mean median sd mad q5 q95
      y_rep[1]
      y_rep[2]
      
      # showing 2 of 11 rows (change via 'max_rows' argument or 'cmdstanr_max_rows' option)

---

    Code
      fit_gq$print(max_rows = 11)
    Output
      variable mean median sd mad q5 q95
      y_rep[1]
      y_rep[2]
      y_rep[3]
      y_rep[4]
      y_rep[5]
      y_rep[6]
      y_rep[7]
      y_rep[8]
      y_rep[9]
      y_rep[10]
      sum_y

---

    Code
      fit_gq$print("y_rep", max_rows = 2)
    Output
      variable mean median sd mad q5 q95
      y_rep[1]
      y_rep[2]
      
      # showing 2 of 10 rows (change via 'max_rows' argument or 'cmdstanr_max_rows' option)

---

    Code
      fit_gq$print("y_rep")
    Output
      variable mean median sd mad q5 q95
      y_rep[1]
      y_rep[2]
      y_rep[3]
      y_rep[4]
      y_rep[5]
      y_rep[6]
      y_rep[7]
      y_rep[8]
      y_rep[9]
      y_rep[10]

---

    Code
      fit_gq$print(c("y_rep[1]", "sum_y", "y_rep[3]"))
    Output
      variable mean median sd mad q5 q95
      y_rep[1]
      sum_y
      y_rep[3]

