#include <stan/model/model_base.hpp>
#include <stan/model/log_prob_grad.hpp>
#include <stan/model/log_prob_propto.hpp>
#include <boost/random/additive_combine.hpp>
#ifdef CMDSTAN_JSON
#include <cmdstan/io/json/json_data.hpp>
#else
#include <stan/io/json/json_data.hpp>
#include <stan/io/empty_var_context.hpp>
#endif

#include <Rcpp.h>

stan::model::model_base&
new_model(stan::io::var_context& data_context, unsigned int seed,
          std::ostream* msg_stream);

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

RcppExport SEXP model_ptr_(SEXP data_path_, SEXP seed_) {
  BEGIN_RCPP
  std::string data_path = Rcpp::as<std::string>(data_path_);
  boost::uint32_t seed = Rcpp::as<boost::uint32_t>(seed_);
  Rcpp::XPtr<stan::model::model_base> ptr(
    &new_model(*var_context(data_path), seed, &Rcpp::Rcout)
  );
  Rcpp::XPtr<boost::ecuyer1988> base_rng(new boost::ecuyer1988(seed));
  return Rcpp::List::create(
    Rcpp::Named("model_ptr") = ptr,
    Rcpp::Named("base_rng") = base_rng
  );
  END_RCPP
}

RcppExport SEXP log_prob_(SEXP ext_model_ptr, SEXP upars_, SEXP jacobian_) {
  BEGIN_RCPP
  Rcpp::XPtr<stan::model::model_base> ptr(ext_model_ptr);
  double rtn;
  Eigen::VectorXd upars = Rcpp::as<Eigen::Map<Eigen::VectorXd>>(upars_);
  if (Rcpp::as<bool>(jacobian_)) {
    rtn = stan::model::log_prob_propto<true>(*ptr.get(), upars, &Rcpp::Rcout);
  } else {
    rtn = stan::model::log_prob_propto<false>(*ptr.get(), upars, &Rcpp::Rcout);
  }
  return Rcpp::wrap(rtn);
  END_RCPP
}

RcppExport SEXP grad_log_prob_(SEXP ext_model_ptr, SEXP upars_, SEXP jacobian_) {
  BEGIN_RCPP
  Rcpp::XPtr<stan::model::model_base> ptr(ext_model_ptr);
  Eigen::VectorXd gradients;
  Eigen::VectorXd upars = Rcpp::as<Eigen::VectorXd>(upars_);

  double lp;
  if (Rcpp::as<bool>(jacobian_)) {
    lp = stan::model::log_prob_grad<true, true>(
      *ptr.get(), upars, gradients);
  } else {
  lp = stan::model::log_prob_grad<true, false>(
      *ptr.get(), upars, gradients);
  }
  Rcpp::NumericVector grad_rtn(Rcpp::wrap(gradients));
  grad_rtn.attr("log_prob") = lp;
  return grad_rtn;
  END_RCPP
}

RcppExport SEXP hessian_(SEXP ext_model_ptr, SEXP upars_, SEXP jacobian_) {
  BEGIN_RCPP
  Rcpp::XPtr<stan::model::model_base> ptr(ext_model_ptr);
  Eigen::Map<Eigen::VectorXd> upars = Rcpp::as<Eigen::Map<Eigen::VectorXd>>(upars_);

  auto hessian_functor = [&](auto&& x) {
    if (Rcpp::as<bool>(jacobian_)) {
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
  END_RCPP
}

RcppExport SEXP get_num_upars_(SEXP ext_model_ptr) {
  BEGIN_RCPP
  Rcpp::XPtr<stan::model::model_base> ptr(ext_model_ptr);
  return Rcpp::wrap(ptr->num_params_r());
  END_RCPP
}

RcppExport SEXP get_param_metadata_(SEXP ext_model_ptr) {
  BEGIN_RCPP
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
  END_RCPP
}

RcppExport SEXP unconstrain_variables_(SEXP ext_model_ptr, SEXP init_path_) {
  BEGIN_RCPP
  std::string init_path = Rcpp::as<std::string>(init_path_);
  Rcpp::XPtr<stan::model::model_base> ptr(ext_model_ptr);
  std::vector<int> params_i;
  std::vector<double> vars;
  ptr->transform_inits(*var_context(init_path), params_i, vars, &Rcpp::Rcout);
  return Rcpp::wrap(vars);
  END_RCPP
}

RcppExport SEXP constrain_variables_(SEXP ext_model_ptr, SEXP base_rng,
                                    SEXP upars_,
                                    SEXP return_trans_pars_,
                                    SEXP return_gen_quants_) {
  BEGIN_RCPP
  Rcpp::XPtr<stan::model::model_base> ptr(ext_model_ptr);
  Rcpp::XPtr<boost::ecuyer1988> rng(base_rng);
  std::vector<double> upars = Rcpp::as<std::vector<double>>(upars_);
  bool return_trans_pars = Rcpp::as<bool>(return_trans_pars_);
  bool return_gen_quants = Rcpp::as<bool>(return_gen_quants_);
  std::vector<int> params_i;
  std::vector<double> vars;

  ptr->write_array(*rng.get(), upars, params_i, vars, return_trans_pars, return_gen_quants);
  return Rcpp::wrap(vars);
  END_RCPP
}

RcppExport SEXP unconstrained_param_names_(SEXP ext_model_ptr,
                                            SEXP return_trans_pars_,
                                            SEXP return_gen_quants_) {
  BEGIN_RCPP
  Rcpp::XPtr<stan::model::model_base> ptr(ext_model_ptr);
  bool return_trans_pars = Rcpp::as<bool>(return_trans_pars_);
  bool return_gen_quants = Rcpp::as<bool>(return_gen_quants_);
  std::vector<std::string> rtn_names;
  ptr->unconstrained_param_names(rtn_names, return_trans_pars, return_gen_quants);
  return Rcpp::wrap(rtn_names);
  END_RCPP
}

RcppExport SEXP constrained_param_names_(SEXP ext_model_ptr,
                                          SEXP return_trans_pars_,
                                          SEXP return_gen_quants_) {
  BEGIN_RCPP
  Rcpp::XPtr<stan::model::model_base> ptr(ext_model_ptr);
  bool return_trans_pars = Rcpp::as<bool>(return_trans_pars_);
  bool return_gen_quants = Rcpp::as<bool>(return_gen_quants_);
  std::vector<std::string> rtn_names;
  ptr->constrained_param_names(rtn_names, return_trans_pars, return_gen_quants);
  return Rcpp::wrap(rtn_names);
  END_RCPP
}
