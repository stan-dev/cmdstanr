#ifndef CMDSTANR_RCPP_EIGEN_INTEROP_HPP
#define CMDSTANR_RCPP_EIGEN_INTEROP_HPP

#include <stan/math/prim/fun/to_array_1d.hpp>
#include <Rcpp.h>

namespace Rcpp {
  namespace traits {
    template <typename T, int R, int C>
    class Exporter<Eigen::Matrix<T, R, C>> {
    private:
        SEXP object;
    public:
      Exporter(SEXP x) : object(x){}
      ~Exporter(){}

      // Adapted from Rcpp/internal/Exporter.h
      Eigen::Matrix<T, R, C> get() {
        if (R == 1) {
          Eigen::Matrix<T, 1, C> result(Rf_length(object));
          Rcpp::internal::export_indexing<Eigen::Matrix<T, 1, C>, T>(object, result);
          return result;
        } else if (C == 1) {
          Eigen::Matrix<T, R, 1> result(Rf_length(object));
          Rcpp::internal::export_indexing<Eigen::Matrix<T, R, 1>, T>(object, result);
          return result;
        } else {
          Shield<SEXP> dims(Rf_getAttrib(object, R_DimSymbol));
          if (Rf_isNull(dims) || Rf_length(dims) != 2) {
            throw Rcpp::not_a_matrix();
          }
          int* dims_ = INTEGER(dims);
          Eigen::Matrix<T, R, C> result(dims_[0], dims_[1]);
          T* result_data = result.data();
          Rcpp::internal::export_indexing<T*, T>(object, result_data);
          return result;
        }
      }
    };
  } // traits

  // Rcpp is hardcoded to assume that Eigen types will be wrapped using the
  //   eigen_wrap function in the RcppEigen package
  namespace RcppEigen {
    template <typename T>
    SEXP eigen_wrap(const T& x) {
      const static int RTYPE = Rcpp::traits::r_sexptype_traits<stan::scalar_type_t<T>>::rtype;
      Rcpp::Vector<RTYPE> vec_rtn(Rcpp::wrap(stan::math::to_array_1d(x)));
      if (!stan::is_eigen_col_vector<T>::value) {
        vec_rtn.attr("dim") = Rcpp::Dimension(x.rows(), x.cols());
      }
      return vec_rtn;
    }
  } // RcppEigen
} // Rcpp

#endif
