data {
  int<lower=0> N;
  array[N] int<lower=0, upper=1> y;
}
parameters {
  real alpha;
}
model {
  target += normal_lpdf(alpha | 0, 1);
  target += bernoulli_logit_lpmf(y | alpha);
}
