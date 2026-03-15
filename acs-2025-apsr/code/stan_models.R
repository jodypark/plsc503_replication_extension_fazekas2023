

## static model

stan.code.static <- "
    data {
    int<lower=1> A; //agencies
    int<lower=1> P; //presidents
    int<lower=1> R; //rule types
    int<lower=1> N; //no. of observations
    int<lower=1, upper=A> a[N]; //agency for observation n
    int<lower=1, upper=P> p[N]; //president for observation n
    int<lower=1, upper=R> r[N]; //rule type for observation n
    int<lower=0, upper=1> y[N]; //audit of observation n
    }
    parameters {
    real xpresident[P];
    real xagency[A];
    real kappa[A]; // <lower=0>
    real lambda[R];  // [R]
    real<lower=0> scale[R];  // [R]
    }
    model {
    xagency ~ normal(0,1);
    lambda ~ normal(0,1);
    scale ~ lognormal(-2,.5);
    kappa ~ normal(0,1);
    xpresident ~ normal(0,1);
    xpresident[1] ~ normal(-.438, .01); // clinton, DW-Nominate
    xpresident[2] ~ normal(.693, .01); //  bush
    xpresident[3] ~ normal(-.358, .01); // obama
    xpresident[4] ~ normal(.403, .01); //   trump
    for (n in 1:N)
    y[n] ~ bernoulli(Phi(kappa[a[n]] + lambda[r[n]] + scale[r[n]] * (xpresident[p[n]] - xagency[a[n]])^2));
    }"


## simple dynamic model (used for variational inference)

stan.code.v2 <- "
    data {
    int<lower=1> A; //agencies
    int<lower=1> P; //presidents
    int<lower=1> R; //rule types
    int<lower=1> N; //no. of observations
    int<lower=1> T; //no. of time periods
    int<lower=1, upper=A> a[N]; //agency for observation n
    int<lower=1, upper=P> p[N]; //president for observation n
    int<lower=1, upper=R> r[N]; //rule type for observation n
    int<lower=0, upper=1> y[N]; //audit of observation n
    int<lower=1, upper=T> t[N]; // time period for obs n
    }
    parameters {
    real xpresident[P];
    // real xagency[A];
    vector[A] xagency[T];
    real kappa[A]; // <lower=0>
    real lambda[R];  // [R]
    real<lower=0> scale[R];  // [R]
    }
    model {
    xagency[1] ~ normal(0,1);
    for (i in 2:T){
    xagency[i] ~ normal(xagency[i - 1], 1); }
    lambda ~ normal(0,1);
    scale ~ lognormal(-2,.5);  //changed from normal
    kappa ~ normal(0,1);
    xpresident ~ normal(0,1);
    xpresident[1] ~ normal(-.438, .01); // clinton, DW-Nominate
    xpresident[2] ~ normal(.693, .01); //  bush
    xpresident[3] ~ normal(-.358, .01); // obama
    xpresident[4] ~ normal(.403, .01); //   trump
    // obama is estimated from audit data
    for (n in 1:N)
    y[n] ~ bernoulli(Phi(kappa[a[n]] + lambda[r[n]] + scale[r[n]] * (xpresident[p[n]] - xagency[t[n], a[n]])^2));
    }"




stan.code.non.centered.kappa <- "

data {
  int<lower=1> A;                 // agencies
  int<lower=1> P;                 // presidents
  int<lower=1> R;                 // rule types
  int<lower=1> N;                 // observations
  int<lower=1> T;                 // time periods
  int<lower=1, upper=A> a[N];     // agency index
  int<lower=1, upper=P> p[N];     // president index
  int<lower=1, upper=R> r[N];     // rule type index
  int<lower=0, upper=1> y[N];     // audit indicator
  int<lower=1, upper=T> t[N];     // time index
}

parameters {
  vector[P] xpresident;
  matrix[T, A] xagency_raw;       // non-centered RW innovations
  real<lower=0> sigma_rw;         // RW step size

  // ---- hierarchical kappa ----
  real mu_kappa;                  // population mean
  real<lower=0> tau_kappa;        // population sd (heterogeneity)
  vector[A] kappa_raw;            // non-centered agency effects

  vector[R] lambda;
  vector<lower=0>[R] scale;
}

transformed parameters {
  matrix[T, A] xagency;
  vector[A] kappa;                // realized agency intercepts

  // build dynamic agency ideal points (non-centered RW)
  xagency[1] = xagency_raw[1];
  for (i in 2:T) {
    for (j in 1:A) {
      xagency[i, j] = xagency[i - 1, j] + sigma_rw * xagency_raw[i, j];
    }
  }

  // non-centered hierarchical kappa
  kappa = mu_kappa + tau_kappa * kappa_raw;
}

model {
  // priors
  to_vector(xagency_raw) ~ normal(0, 1);
  sigma_rw ~ normal(0, 1);

  mu_kappa  ~ normal(0, 1);
  tau_kappa ~ normal(0, 1);        // half-N(0,1) via <lower=0>
  kappa_raw ~ normal(0, 1);

  lambda ~ normal(0, 1);
  scale  ~ lognormal(-2, 0.8);

  xpresident ~ normal(0, 1);
  xpresident[1] ~ normal(-0.438, 0.01);  // Clinton
  xpresident[2] ~ normal( 0.693, 0.01);  // Bush
  xpresident[3] ~ normal(-0.358, 0.01);  // Obama
  xpresident[4] ~ normal( 0.403, 0.01);  // Trump

  // likelihood (probit) with rescaled distance
  {
    real c = 1.5;                          // rescaling constant
    vector[N] eta;
    for (n in 1:N) {
      real d = (xpresident[p[n]] - xagency[t[n], a[n]]) / c;
      eta[n] = kappa[a[n]] + lambda[r[n]] + scale[r[n]] * d * d;
    }
    y ~ bernoulli(Phi(eta));               // vectorized Bernoulli with probit
  }
}
"

