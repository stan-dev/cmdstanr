#include <RcppEigen.h>

// [[Rcpp::depends(RcppEigen)]]

// [[Rcpp::export]]
Rcpp::List hessian(SEXP ext_model_ptr, Eigen::VectorXd& upars) {
  Rcpp::XPtr<stan_model> ptr(ext_model_ptr);

  double log_prob;
  Eigen::VectorXd grad;
  Eigen::MatrixXd hessian;

  stan::model::hessian(*ptr.get(), upars, log_prob, grad, hessian);

  return Rcpp::List::create(
    Rcpp::Named("log_prob") = log_prob,
    Rcpp::Named("grad_log_prob") = grad,
    Rcpp::Named("hessian") = hessian);
}
