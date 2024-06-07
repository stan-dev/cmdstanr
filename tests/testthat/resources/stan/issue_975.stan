data {
  int N;
  vector[N] y;
}

parameters {
  matrix[N, 1] mu;
  vector<lower=0>[N] sigma;
}

model {
  target += normal_lupdf(y | mu[:, 1] , sigma);
}
