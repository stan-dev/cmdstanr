context("bridge_sampler")

set_cmdstan_path()

mu <- 0
tau2 <- 0.5
sigma2 <- 1

n <- 20
theta <- rnorm(n, mu, sqrt(tau2))
y <- rnorm(n, theta, sqrt(sigma2))

mu0 <- 0
tau20 <- 1
alpha <- 1
beta <- 1

dataH0 <- list(y = y, n = n, alpha = alpha, beta = beta, sigma2 = sigma2)
dataH1 <- list(y = y, n = n, mu0 = mu0, tau20 = tau20, alpha = alpha,
               beta = beta, sigma2 = sigma2)

stancodeH0 <- 'data {
  int<lower=1> n; // number of observations
  vector[n] y; // observations
  real<lower=0> alpha;
  real<lower=0> beta;
  real<lower=0> sigma2;
}
parameters {
  real<lower=0> tau2; // group-level variance
  vector[n] theta; // participant effects
}
model {
  target += inv_gamma_lpdf(tau2 | alpha, beta);
  target += normal_lpdf(theta | 0, sqrt(tau2));
  target += normal_lpdf(y | theta, sqrt(sigma2));
}
'
stancodeH1 <- 'data {
  int<lower=1> n; // number of observations
  vector[n] y; // observations
  real mu0;
  real<lower=0> tau20;
  real<lower=0> alpha;
  real<lower=0> beta;
  real<lower=0> sigma2;
}
parameters {
  real mu;
  real<lower=0> tau2; // group-level variance
  vector[n] theta; // participant effects
}
model {
  target += normal_lpdf(mu | mu0, sqrt(tau20));
  target += inv_gamma_lpdf(tau2 | alpha, beta);
  target += normal_lpdf(theta | mu, sqrt(tau2));
  target += normal_lpdf(y | theta, sqrt(sigma2));
}
'
modH0 <- cmdstan_model(write_stan_file(stancodeH0),
                        force_recompile = TRUE)
modH1 <- cmdstan_model(write_stan_file(stancodeH1),
                        force_recompile = TRUE)

fitH0 <- modH0$sample(data = dataH0, iter_warmup = 1000, iter_sampling = 50000,
                      chains = 3, parallel_chains = 3)
fitH1 <- modH1$sample(data = dataH1, iter_warmup = 1000, iter_sampling = 50000,
                      chains = 3, parallel_chains = 3)

test_that("bridge_sampler method can be called", {
  expect_no_error({bridgeH0 <- fitH0$bridge_sampler()})
  expect_no_error({bridgeH1 <- fitH1$bridge_sampler()})

  expect_s3_class(bridgeH0, "bridge")
  expect_s3_class(bridgeH1, "bridge")
})

test_that("bridge_sampler returns usable with bf and logml methods", {
  expect_no_error({ bf_diff <- bridgesampling::bf(bridgeH0, bridgeH1) })
  expect_no_error({ logml_H0 <- bridgesampling::logml(bridgeH0) })
  expect_no_error({ logml_H1 <- bridgesampling::logml(bridgeH1) })
})
