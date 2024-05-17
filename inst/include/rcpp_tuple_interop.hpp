#ifndef CMDSTANR_RCPP_TUPLE_INTEROP_HPP
#define CMDSTANR_RCPP_TUPLE_INTEROP_HPP

#include <stan/math/prim/functor/apply.hpp>
#include <Rcpp.h>

namespace Rcpp {
  namespace traits {
  /**
     * The Rcpp::traits::Exporter class is the implementation used when calling
     * Rcpp::as<T>() to convert an R object (SEXP) to the requested c++ type.
    */
    template <typename... T>
    class Exporter<std::tuple<T...>> {
      private:
        Rcpp::List list_x;

        template<std::size_t... I>
        auto get_impl(std::index_sequence<I...> i) {
          return std::make_tuple(
            Rcpp::as<T>(list_x[I].get())...
          );
        }

      public:
        Exporter(SEXP x) : list_x(x) { }
        std::tuple<T...> get() {
          return get_impl(std::index_sequence_for<T...>{});
        }
    };
  }

  /**
   * The Rcpp::wrap class is used to convert a C++ type to an R object type.
   * Rather than implement anything bespoke for tuples we simply return an R list.
  */
  template <typename... T>
  SEXP wrap(const std::tuple<T...>& x) {
    return stan::math::apply([](const auto&... args) {
      return Rcpp::List::create(Rcpp::wrap(args)...);
    }, x);
  }
}

#endif
