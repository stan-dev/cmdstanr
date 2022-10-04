#include <RcppEigen.h>
#include <cmdstan/command.hpp>
#include <boost/random/additive_combine.hpp>

// [[Rcpp::depends(RcppEigen)]]

// [[Rcpp::export]]
Rcpp::List model_ptr(std::string data_path, boost::uint32_t seed) {
  Rcpp::XPtr<stan_model> ptr(
    new stan_model(*cmdstan::get_var_context(data_path), seed, &Rcpp::Rcout)
  );
  Rcpp::XPtr<boost::ecuyer1988> base_rng(new boost::ecuyer1988(seed));
  return Rcpp::List::create(
    Rcpp::Named("model_ptr") = ptr,
    Rcpp::Named("base_rng") = base_rng
  );
}

// [[Rcpp::export]]
double log_prob(SEXP ext_model_ptr, Eigen::VectorXd upars) {
  Rcpp::XPtr<stan::model::model_base> ptr(ext_model_ptr);
  return ptr->log_prob<false, true>(upars, &Rcpp::Rcout);
}

// [[Rcpp::export]]
Rcpp::NumericVector grad_log_prob(SEXP ext_model_ptr, Eigen::VectorXd upars) {
  Rcpp::XPtr<stan::model::model_base> ptr(ext_model_ptr);
  Eigen::VectorXd gradients;

  double lp = stan::model::log_prob_grad<false, true>(
    *ptr.get(), upars, gradients);
  Rcpp::NumericVector grad_rtn = Rcpp::wrap(gradients);
  grad_rtn.attr("log_prob") = lp;
  return grad_rtn;
}

// [[Rcpp::export]]
std::vector<double> unconstrain_pars(SEXP ext_model_ptr, std::string init_path) {
  Rcpp::XPtr<stan::model::model_base> ptr(ext_model_ptr);
  std::shared_ptr<stan::io::var_context> init_context
    = cmdstan::get_var_context(init_path);
  std::vector<int> params_i;
  std::vector<double> vars;
  ptr->transform_inits(*init_context.get(), params_i, vars, &Rcpp::Rcout);
  return vars;
}

// [[Rcpp::export]]
std::vector<double> constrain_pars(SEXP ext_model_ptr, SEXP base_rng, std::vector<double> upars) {
  Rcpp::XPtr<stan::model::model_base> ptr(ext_model_ptr);
  Rcpp::XPtr<boost::ecuyer1988> rng(base_rng);
  std::vector<int> params_i;
  std::vector<double> vars;

  ptr->write_array(*rng.get(), upars, params_i, vars, false, false);
  return vars;
}
