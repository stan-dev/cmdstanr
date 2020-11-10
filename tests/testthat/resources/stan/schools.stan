data {
  int<lower=1> J;
  vector<lower=0>[J] sigma;
  vector[J] y;
}
parameters {
  real mu;
  real<lower=0> tau;
  vector[J] theta;
}
model {
  target += normal_lpdf(tau | 0, 10);
  target += normal_lpdf(mu | 0, 10);
  target += normal_lpdf(theta | mu, tau);
  target += normal_lpdf(y | theta, sigma);
}
