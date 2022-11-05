#include <RcppEigen.h>
#include <stan/math/mix/functor/hessian.hpp>

// [[Rcpp::depends(RcppEigen)]]

// [[Rcpp::export]]
Rcpp::List hessian(SEXP ext_model_ptr, Eigen::VectorXd& upars,
                   bool jac_adjust) {
  Rcpp::XPtr<stan_model> ptr(ext_model_ptr);

  auto log_prob_fun = [](const bool adjust, Rcpp::XPtr<stan_model> model_ptr) {
    return [=](auto& pars) {
      if (adjust) {
        return model_ptr.get()->log_prob<true, true>(pars, &Rcpp::Rcout);
      } else {
        return model_ptr.get()->log_prob<true, false>(pars, &Rcpp::Rcout);
      }
    };
  };

  double log_prob;
  Eigen::VectorXd grad;
  Eigen::MatrixXd hessian;

  stan::math::hessian(log_prob_fun(jac_adjust, ptr), upars, log_prob, grad, hessian);

  return Rcpp::List::create(
    Rcpp::Named("log_prob") = log_prob,
    Rcpp::Named("grad_log_prob") = grad,
    Rcpp::Named("hessian") = hessian);
}
