functions {
  vector f_0_arg(real t, vector z) {
    return z;
  }
}
data {
  int N;
  real rd;
  array[N] real rad;
  vector[N] vd;
}
transformed data {
  array[N] vector[N] zd = ode_rk45(f_0_arg, vd, rd, rad);
}
parameters {
  real r;
  array[N] real ra;
  vector[N] v;
}
transformed parameters {
  array[N] vector[N] z = ode_rk45(f_0_arg, v, r, ra);
}
model {
  r ~ normal(0, 1);
}
