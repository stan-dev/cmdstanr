data {
  int<lower=1> k;
  int<lower=0> n;
  matrix[n, k] X;
  int y[n];
}
parameters {
  vector[k] beta;
  real alpha;
}
model {
  # priors
  target += std_normal_log(beta);
  alpha ~ std_normal();
  
  y ~ bernoulli_logit(X * beta + alpha);
}
