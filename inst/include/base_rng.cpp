#include <Rcpp.h>
#include <stan_rng.hpp>

// [[Rcpp::export]]
SEXP base_rng(boost::uint32_t seed = 1) {
  Rcpp::XPtr<stan::rng_t> rng_ptr(new stan::rng_t(seed));
  return rng_ptr;
}
