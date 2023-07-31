data {
  int<lower=1> k;
  int<lower=0> n;
  matrix[n, k] X;
  array[n] int y;
}
parameters {
  vector[k] beta;
  real alpha;
}
model {
  target += std_normal_lpdf(beta);
  target += std_normal_lpdf(alpha);
  target += bernoulli_logit_glm_lpmf(y | X, alpha, beta);
}
