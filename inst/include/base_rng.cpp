#include <Rcpp.h>
#include <boost/random/additive_combine.hpp>

// [[Rcpp::export]]
SEXP base_rng(boost::uint32_t seed = 0) {
  Rcpp::XPtr<boost::ecuyer1988> rng_ptr(new boost::ecuyer1988(seed));
  return rng_ptr;
}
