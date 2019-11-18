data {
  real<lower=0,upper=1> pr_fail;
}
transformed data {
  int fail = bernoulli_rng(pr_fail);
}
parameters {
  real theta;
}
model {
  if (fail) {
    // this should result in a failed chain
    theta ~ normal(1 / 0.0, 1);
  } else {
    theta ~ normal(0, 1);
  }
}
