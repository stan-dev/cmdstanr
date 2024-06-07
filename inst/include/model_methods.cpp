#include <Rcpp.h>
#include <rcpp_eigen_interop.hpp>
#include <stan/model/model_base.hpp>
#include <stan/model/log_prob_grad.hpp>
#include <stan/model/log_prob_propto.hpp>
#ifdef CMDSTAN_JSON
#include <cmdstan/io/json/json_data.hpp>
#else
#include <stan/io/json/json_data.hpp>
#include <stan/io/empty_var_context.hpp>
#endif
#include <stan_rng.hpp>

std::shared_ptr<stan::io::var_context> var_context(std::string file_path) {
  if (file_path == "") {
    stan::io::empty_var_context empty_context;
    return std::make_shared<stan::io::empty_var_context>(empty_context);
  }

  std::fstream stream(file_path.c_str(), std::fstream::in);
#ifdef CMDSTAN_JSON
using json_data_t = cmdstan::json::json_data;
#else
using json_data_t = stan::json::json_data;
#endif
  json_data_t data_context(stream);
  return std::make_shared<json_data_t>(data_context);
}

stan::model::model_base&
new_model(stan::io::var_context& data_context, unsigned int seed,
          std::ostream* msg_stream);

// [[Rcpp::export]]
Rcpp::List model_ptr(std::string data_path, boost::uint32_t seed) {
  Rcpp::XPtr<stan::model::model_base> ptr(
    &new_model(*var_context(data_path), seed, &Rcpp::Rcout)
  );
  Rcpp::XPtr<stan::rng_t> base_rng(new stan::rng_t(seed));
  return Rcpp::List::create(
    Rcpp::Named("model_ptr") = ptr,
    Rcpp::Named("base_rng") = base_rng
  );
}

// [[Rcpp::export]]
double log_prob(SEXP ext_model_ptr, Eigen::VectorXd upars, bool jac_adjust) {
  Rcpp::XPtr<stan::model::model_base> ptr(ext_model_ptr);
  if (jac_adjust) {
    return stan::model::log_prob_propto<true>(*ptr.get(), upars, &Rcpp::Rcout);
  } else {
    return stan::model::log_prob_propto<false>(*ptr.get(), upars, &Rcpp::Rcout);
  }
}

// [[Rcpp::export]]
Rcpp::NumericVector grad_log_prob(SEXP ext_model_ptr, Eigen::VectorXd upars,
                                  bool jac_adjust) {
  Rcpp::XPtr<stan::model::model_base> ptr(ext_model_ptr);
  Eigen::VectorXd gradients;

  double lp;
  if (jac_adjust) {
    lp = stan::model::log_prob_grad<true, true>(*ptr.get(), upars, gradients);
  } else {
    lp = stan::model::log_prob_grad<true, false>(*ptr.get(), upars, gradients);
  }
  Rcpp::NumericVector grad_rtn(Rcpp::wrap(std::move(gradients)));
  grad_rtn.attr("log_prob") = lp;
  return grad_rtn;
}

// [[Rcpp::export]]
Rcpp::List hessian(SEXP ext_model_ptr, Eigen::VectorXd upars, bool jacobian) {
  Rcpp::XPtr<stan::model::model_base> ptr(ext_model_ptr);

  auto hessian_functor = [&](auto&& x) {
    if (jacobian) {
      return ptr->log_prob<true, true>(x, 0);
    } else {
      return ptr->log_prob<true, false>(x, 0);
    }
  };

  double log_prob;
  Eigen::VectorXd grad;
  Eigen::MatrixXd hessian;

  stan::math::internal::finite_diff_hessian_auto(hessian_functor, upars, log_prob, grad, hessian);

  return Rcpp::List::create(
    Rcpp::Named("log_prob") = log_prob,
    Rcpp::Named("grad_log_prob") = grad,
    Rcpp::Named("hessian") = hessian);
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
Eigen::VectorXd unconstrain_variables(SEXP ext_model_ptr, Eigen::VectorXd variables) {
  Rcpp::XPtr<stan::model::model_base> ptr(ext_model_ptr);
  Eigen::VectorXd unconstrained_variables;
  ptr->unconstrain_array(variables, unconstrained_variables, &Rcpp::Rcout);
  return unconstrained_variables;
}

// [[Rcpp::export]]
Rcpp::List unconstrain_draws(SEXP ext_model_ptr, Eigen::MatrixXd variables) {
  Rcpp::XPtr<stan::model::model_base> ptr(ext_model_ptr);
  // Need to do this for the first row to get the correct size of the unconstrained draws
  Eigen::VectorXd unconstrained_draw1;
  ptr->unconstrain_array(variables.row(0).transpose(), unconstrained_draw1, &Rcpp::Rcout);
  std::vector<Eigen::VectorXd> unconstrained_draws(unconstrained_draw1.size());
  for (auto&& unconstrained_par : unconstrained_draws) {
    unconstrained_par.resize(variables.rows());
  }
  
  for (int i = 0; i < variables.rows(); i++) {
    Eigen::VectorXd unconstrained_variables;
    ptr->unconstrain_array(variables.transpose().col(i), unconstrained_variables, &Rcpp::Rcout);
    for (int j = 0; j < unconstrained_variables.size(); j++) {
      unconstrained_draws[j](i) = unconstrained_variables(j);
    }
  }
  return Rcpp::wrap(unconstrained_draws);
}

// [[Rcpp::export]]
std::vector<double> constrain_variables(SEXP ext_model_ptr, SEXP base_rng,
                                    std::vector<double> upars,
                                    bool return_trans_pars,
                                    bool return_gen_quants) {
  Rcpp::XPtr<stan::model::model_base> ptr(ext_model_ptr);
  Rcpp::XPtr<stan::rng_t> rng(base_rng);
  std::vector<int> params_i;
  std::vector<double> vars;

  ptr->write_array(*rng.get(), upars, params_i, vars, return_trans_pars, return_gen_quants);
  return vars;
}

// [[Rcpp::export]]
std::vector<std::string> unconstrained_param_names(SEXP ext_model_ptr, bool return_trans_pars, bool return_gen_quants) {
  Rcpp::XPtr<stan::model::model_base> ptr(ext_model_ptr);
  std::vector<std::string> rtn_names;
  ptr->unconstrained_param_names(rtn_names, return_trans_pars, return_gen_quants);
  return rtn_names;
}

// [[Rcpp::export]]
std::vector<std::string> constrained_param_names(SEXP ext_model_ptr,
                                    bool return_trans_pars,
                                    bool return_gen_quants) {
  Rcpp::XPtr<stan::model::model_base> ptr(ext_model_ptr);
  std::vector<std::string> rtn_names;
  ptr->constrained_param_names(rtn_names, return_trans_pars, return_gen_quants);
  return rtn_names;
}
