#include <Rcpp.h>
#include <stan/model/log_prob_grad.hpp>
#include <stan/model/log_prob_propto.hpp>
#include <boost/random/additive_combine.hpp>
#ifdef CMDSTAN_JSON
#include <cmdstan/io/json/json_data.hpp>
#else
#include <stan/io/json/json_data.hpp>
#endif

std::shared_ptr<stan::io::var_context> var_context(std::string file_path) {
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

// [[Rcpp::export]]
double log_prob(SEXP ext_model_ptr, std::vector<double> upars,
                bool jac_adjust) {
  Rcpp::XPtr<stan::model::model_base> ptr(ext_model_ptr);
  std::vector<int> params_i;
  if (jac_adjust) {
    return stan::model::log_prob_propto<true>(*ptr.get(), upars, params_i, &Rcpp::Rcout);
  } else {
    return stan::model::log_prob_propto<false>(*ptr.get(), upars, params_i, &Rcpp::Rcout);
  }
}

// [[Rcpp::export]]
Rcpp::NumericVector grad_log_prob(SEXP ext_model_ptr, std::vector<double> upars,
                                  bool jac_adjust) {
  Rcpp::XPtr<stan::model::model_base> ptr(ext_model_ptr);
  std::vector<double> gradients;
  std::vector<int> params_i;

  double lp;
  if (jac_adjust) {
    lp = stan::model::log_prob_grad<true, true>(
      *ptr.get(), upars, params_i, gradients);
  } else {
  lp = stan::model::log_prob_grad<true, false>(
      *ptr.get(), upars, params_i, gradients);
  }
  Rcpp::NumericVector grad_rtn = Rcpp::wrap(gradients);
  grad_rtn.attr("log_prob") = lp;
  return grad_rtn;
}

// [[Rcpp::export]]
size_t get_num_upars(SEXP ext_model_ptr) {
  Rcpp::XPtr<stan::model::model_base> ptr(ext_model_ptr);
  return ptr->num_params_r();
}

// [[Rcpp::export]]
Rcpp::List get_param_metadata(SEXP ext_model_ptr) {
  Rcpp::XPtr<stan::model::model_base> ptr(ext_model_ptr);
  std::vector<std::string> param_names;
  std::vector<std::vector<size_t> > param_dims;
  ptr->get_param_names(param_names);
  ptr->get_dims(param_dims);

  Rcpp::List param_metadata = Rcpp::List::create(
    Rcpp::Named(param_names[0]) = param_dims[0]
  );
  for (size_t i = 1; i < param_names.size(); i++) {
    param_metadata.push_back(param_dims[i], param_names[i]);
  }

  return param_metadata;
}

// [[Rcpp::export]]
std::vector<double> unconstrain_variables(SEXP ext_model_ptr, std::string init_path) {
  Rcpp::XPtr<stan::model::model_base> ptr(ext_model_ptr);
  std::vector<int> params_i;
  std::vector<double> vars;
  ptr->transform_inits(*var_context(init_path), params_i, vars, &Rcpp::Rcout);
  return vars;
}

// [[Rcpp::export]]
std::vector<double> constrain_variables(SEXP ext_model_ptr, SEXP base_rng,
                                    std::vector<double> upars,
                                    bool return_trans_pars,
                                    bool return_gen_quants) {
  Rcpp::XPtr<stan::model::model_base> ptr(ext_model_ptr);
  Rcpp::XPtr<boost::ecuyer1988> rng(base_rng);
  std::vector<int> params_i;
  std::vector<double> vars;

  ptr->write_array(*rng.get(), upars, params_i, vars, return_trans_pars, return_gen_quants);
  return vars;
}
