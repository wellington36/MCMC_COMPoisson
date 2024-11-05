functions {
  #include sum_to_threshold.stan
}

data {
  int<lower=1> N;                // Number of unique counts
  array[N] int<lower=0> y;       // Counts (0, 1, 2, ...)
  array[N] int<lower=0> freq;    // Frequencies for each count
  real eps;
  int MAX_ITERS;
}

parameters {
  real mu;              // Mean parameter (mu)
  real<lower=0> nu;              // Dispersion parameter (nu)
}

transformed parameters {
  real logZ;                        // Normalization constant
  real loglamb = nu * log(mu);
  array[2] real infiniteSTTApproach = infiniteSumToThreshold(loglamb, nu, eps, MAX_ITERS);
  
  logZ = infiniteSTTApproach[1];
}

model {
  vector[N] log_p;          // Log probabilities for each count
  // Priors (adjust these based on your knowledge)
  mu ~ gamma(1, 1);            // Prior for mu
  nu ~ gamma(0.0625, 0.25);        // Prior for nu
  //nu ~ normal(0, 1);               // Prior for nu
  
  // Compute log probabilities
  for (j in 1:N) {
    log_p[j] = y[j] * loglamb - nu * lgamma(y[j] + 1) - logZ;
    target += freq[j] * log_p[j];
  }
}

generated quantities{
  real n = infiniteSTTApproach[2];
}
