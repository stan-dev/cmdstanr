data {
  int<lower=1> K;
  int<lower=1> N;
  matrix[N,K] x;
  array[N] int y;
  vector[N] outcome_offset;

  real beta_prior_scale;
  real alpha_prior_scale;
}
parameters {
  vector[K] beta;
  real intercept;
}
model {
  y ~ poisson(exp(x * beta + intercept + outcome_offset));
  beta ~ normal(0,beta_prior_scale);
  intercept ~ normal(0,alpha_prior_scale);
}
generated quantities {
  vector[N] log_lik;
  for (n in 1:N)
    log_lik[n] = poisson_lpmf(y[n] | exp(x[n] * beta + intercept + outcome_offset[n]));
}
