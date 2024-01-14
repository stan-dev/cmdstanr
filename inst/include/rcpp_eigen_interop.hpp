#ifndef CMDSTANR_RCPP_EIGEN_INTEROP_HPP
#define CMDSTANR_RCPP_EIGEN_INTEROP_HPP

#include <stan/math/prim/fun/to_array_1d.hpp>
#include <Rcpp.h>

namespace Rcpp {
  namespace traits {
    template <typename T, int R, int C>
    class Exporter<Eigen::Matrix<T, R, C>> {
      private:
        Rcpp::NumericVector vec_x;
      public:
        Exporter(SEXP x) : vec_x(x) { }
        Eigen::Matrix<T, R, C> get() {
          if (R == 1) {
            return Eigen::Map<Eigen::RowVectorXd>(vec_x.begin(), vec_x.size());
          } else if (C == 1) {
            return Eigen::Map<Eigen::VectorXd>(vec_x.begin(), vec_x.size());
          } else {
            Rcpp::Dimension vec_dims(vec_x.attr("dim"));
            return Eigen::Map<Eigen::Matrix<T, R, C>>(vec_x.begin(), vec_dims[0], vec_dims[1]);
          }
        }
    };
  } // traits

  // Rcpp is hardcoded to assume that Eigen types will be wrapped using the
  //   eigen_wrap function in the RcppEigen package
  namespace RcppEigen {
    template <typename T>
    SEXP eigen_wrap(const T& x) {
      Rcpp::NumericVector vec_rtn(Rcpp::wrap(stan::math::to_array_1d(x)));
      if (x.cols() > 1) {
        vec_rtn.attr("dim") = Rcpp::Dimension(x.rows(), x.cols());
      }
      return vec_rtn;
    }
  } // RcppEigen
} // Rcpp

#endif
