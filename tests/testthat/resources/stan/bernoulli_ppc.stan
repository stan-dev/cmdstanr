data {
  int<lower=0> N;
  array[N] int<lower=0, upper=1> y;
}
parameters {
  real<lower=0, upper=1> theta;
}
model {
  theta ~ beta(1, 1);
  y ~ bernoulli(theta);
}
generated quantities {
  array[N] int y_rep;
  int sum_y;
  for (n in 1 : N) {
    y_rep[n] = bernoulli_rng(theta);
  }
  sum_y = sum(y_rep);
}
