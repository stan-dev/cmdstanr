 1: data {
 2:   int<lower=0> N;
 3:   array[N] int<lower=0, upper=1> y;
 4: }
 5: parameters {
 6:   real<lower=0, upper=1> theta;
 7: }
 8: model {
 9:   theta ~ beta(1, 1); // uniform prior on interval 0,1
10:   y ~ bernoulli(theta);
11: }
