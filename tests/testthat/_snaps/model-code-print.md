# code() and print() methods work

    data {
      int<lower=0> N;
      array[N] int<lower=0, upper=1> y;
    }
    parameters {
      real<lower=0, upper=1> theta;
    }
    model {
      theta ~ beta(1, 1); // uniform prior on interval 0,1
      y ~ bernoulli(theta);
    }

---

    data {
      int<lower=0> N;
      array[N] int<lower=0, upper=1> y;
    }
    parameters {
      real<lower=0, upper=1> theta;
    }
    model {
      theta ~ beta(1, 1); // uniform prior on interval 0,1
      y ~ bernoulli(theta);
    }

# code() warns and print() errors if only exe and no Stan file

    '$code()' will return NULL because the 'CmdStanModel' was not created with a Stan file.

---

    '$print()' cannot be used because the 'CmdStanModel' was not created with a Stan file.

# check_syntax() errors if only exe and no Stan file

    '$check_syntax()' cannot be used because the 'CmdStanModel' was not created with a Stan file.

