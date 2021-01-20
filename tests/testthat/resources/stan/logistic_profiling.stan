functions {
  real glm(int[] y, matrix X, real alpha, vector beta) {
    profile("glm") {
      return bernoulli_logit_glm_lpmf(y | X, alpha, beta);
    }
  }
}
data {
  int<lower=0> N;
  int<lower=0> K;
  int<lower=0,upper=1> y[N];
  matrix[N, K] X;
}
parameters {
  real alpha;
  vector[K] beta;
}
model {
  profile("priors") {
    target += normal_lpdf(alpha | 0, 1);
    target += normal_lpdf(beta | 0, 1);
  }  
  profile("udf") {
    target += glm(y, X, alpha, beta);
  }
}

