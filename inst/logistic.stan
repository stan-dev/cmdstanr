data {
  int<lower=0> N;
  int<lower=0> K;
  array[N] int<lower=0, upper=1> y;
  matrix[N, K] X;
}
parameters {
  real alpha;
  vector[K] beta;
}
model {
  target += normal_lpdf(alpha | 0, 1);
  target += normal_lpdf(beta | 0, 1);
  target += bernoulli_logit_glm_lpmf(y | X, alpha, beta);
}
generated quantities {
  vector[N] log_lik;
  for (n in 1 : N) {
    log_lik[n] = bernoulli_logit_lpmf(y[n] | alpha + X[n] * beta);
  }
}
