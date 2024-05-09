#include <Rcpp.h>
#include <stan/math.hpp>

template <typename F, typename... TArgs>
inline Rcpp::List function_gradients(const F& func, const TArgs&... args) {
  Eigen::MatrixXd serialised_args = stan::math::serialize_args(args...);

  auto serial_functor = [&](const auto& v) {
    auto v_deserializer = stan::math::to_deserializer(v);
    return stan::math::to_vector(stan::math::serialize<stan::return_type_t<decltype(v)>>(func(v_deserializer.read(args)...)));
  };

  Eigen::VectorXd rtn_value;
  Eigen::MatrixXd grad;
  stan::math::jacobian(serial_functor, serialised_args, rtn_value, grad);

  if (stan::is_stan_scalar<decltype(func(args...))>::value) {
    return Rcpp::List::create(
      Rcpp::Named("function_value") = func(args...),
      Rcpp::Named("gradients") = stan::math::to_vector(grad)
    );
  } else {
    return Rcpp::List::create(
      Rcpp::Named("function_value") = func(args...),
      Rcpp::Named("jacobian") = grad
    );
  }
}
