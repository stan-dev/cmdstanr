#ifndef CMDSTANR_STAN_RNG_HPP
#define CMDSTANR_STAN_RNG_HPP

#include <boost/random/additive_combine.hpp>
#include <stan/version.hpp>

// A consistent rng_t is defined from 2.35 onwards
// so add a fallback for older versions
#if STAN_MAJOR == 2 && STAN_MINOR >= 35
#include <stan/services/util/create_rng.hpp>
#else
namespace stan {
  using rng_t = boost::ecuyer1988;
}
#endif

#endif
