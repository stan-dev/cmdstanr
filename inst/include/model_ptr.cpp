#include <Rcpp.h>
#include <stan/model/log_prob_grad.hpp>
#include <stan/model/log_prob_propto.hpp>
#include <boost/random/additive_combine.hpp>
#ifdef CMDSTAN_JSON
#include <cmdstan/io/json/json_data.hpp>
#else
#include <stan/io/json/json_data.hpp>
#endif

inline std::shared_ptr<stan::io::var_context> var_context(std::string file_path) {
  std::fstream stream(file_path.c_str(), std::fstream::in);
#ifdef CMDSTAN_JSON
using json_data_t = cmdstan::json::json_data;
#else
using json_data_t = stan::json::json_data;
#endif
  json_data_t data_context(stream);
  return std::make_shared<json_data_t>(data_context);
}

// [[Rcpp::export]]
Rcpp::List model_ptr(std::string data_path, boost::uint32_t seed) {
  Rcpp::XPtr<stan_model> ptr(
    new stan_model(*var_context(data_path), seed, &Rcpp::Rcout)
  );
  Rcpp::XPtr<boost::ecuyer1988> base_rng(new boost::ecuyer1988(seed));
  return Rcpp::List::create(
    Rcpp::Named("model_ptr") = ptr,
    Rcpp::Named("base_rng") = base_rng
  );
}
