parameters {
  real a_scalar;
  tuple(tuple(array[2] real, vector[2]), matrix[2, 2]) b_tuple;
  matrix[2, 2] c_matrix;
  complex z;
}
model {
  a_scalar ~ normal(0, 1);
  b_tuple.1.1 ~ normal(0, 1);
  b_tuple.1.2 ~ normal(0, 1);
  to_vector(b_tuple.2) ~ normal(0, 1);
  to_vector(c_matrix) ~ normal(0, 1);
  get_real(z) ~ normal(0, 1);
  get_imag(z) ~ normal(0, 1);
}
generated quantities {
  tuple(real, tuple(tuple(array[2] real, vector[2]), matrix[2, 2]), matrix[2, 2]) d_tuple
    = (a_scalar, b_tuple, c_matrix);

  real mu = normal_rng(a_scalar, 1);
  matrix[2, 3] m = mu * to_matrix(linspaced_vector(6, 5, 11), 2, 3);
  array[4] matrix[2, 3] threeD;
  for (i in 1 : 4) {
    threeD[i] = i * mu * to_matrix(linspaced_vector(6, 5, 11), 2, 3);
  }

  complex_vector[2] zv = to_complex([mu * 3, mu * 5]', [mu * 4, mu * 6]');
  complex_matrix[2, 3] zm = to_complex(m, m + 1);
  array[4] complex_matrix[2, 3] z3D;
  for (i in 1 : 4) {
    z3D[i] = to_complex(threeD[i], threeD[i] + 1);
  }

  real base = normal_rng(0, 1);
  int base_i = to_int(normal_rng(10, 10));

  tuple(real, real) pair = (base, base * 2);
  tuple(real, tuple(int, complex)) nested = (base * 3, (base_i, base * 4.0i));
  array[2] tuple(real, real) arr_pair = {pair, (base * 5, base * 6)};
}
