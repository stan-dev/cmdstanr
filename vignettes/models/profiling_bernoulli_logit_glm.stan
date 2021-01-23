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
  profile("priors") {
    target += std_normal_lpdf(beta);
    target += std_normal_lpdf(alpha);
  }
  profile("likelihood") {
    target += bernoulli_logit_glm_lpmf(y | X, alpha, beta);
  }
}
