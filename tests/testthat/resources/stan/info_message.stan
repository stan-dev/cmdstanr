parameters {
  corr_matrix[10] Omega;
}
model {
  Omega ~ lkj_corr(1);
}
