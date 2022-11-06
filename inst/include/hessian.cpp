#include <RcppEigen.h>
#include <stan/model/hessian.hpp>

// [[Rcpp::depends(RcppEigen)]]

template <class M>
struct hessian_wrapper {
  const M& model;
  bool jac_adjust;
  std::ostream* o;

  hessian_wrapper(const M& m, bool adj, std::ostream* out) : model(m), jac_adjust(adj), o(out) {}

  template <typename T>
  T operator()(const Eigen::Matrix<T, Eigen::Dynamic, 1>& x) const {
    if (jac_adjust) {
      // log_prob() requires non-const but doesn't modify its argument
      return model.template log_prob<true, true, T>(
          const_cast<Eigen::Matrix<T, -1, 1>&>(x), o);
    } else {
      // log_prob() requires non-const but doesn't modify its argument
      return model.template log_prob<true, false, T>(
          const_cast<Eigen::Matrix<T, -1, 1>&>(x), o);
    }
  }
};

// [[Rcpp::export]]
Rcpp::List hessian(SEXP ext_model_ptr, Eigen::VectorXd& upars, bool jac_adjust) {
  Rcpp::XPtr<stan_model> ptr(ext_model_ptr);

  double log_prob;
  Eigen::VectorXd grad;
  Eigen::MatrixXd hessian;

  stan::math::hessian(hessian_wrapper<decltype(*ptr.get())>(*ptr.get(), jac_adjust, 0),
                      upars, log_prob, grad, hessian);

  return Rcpp::List::create(
    Rcpp::Named("log_prob") = log_prob,
    Rcpp::Named("grad_log_prob") = grad,
    Rcpp::Named("hessian") = hessian);
}
