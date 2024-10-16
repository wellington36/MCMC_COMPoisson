// compoisson_model_mu_nu.stan
data {
  int<lower=1> N;                // Number of unique counts
  array[N] int<lower=0> y;       // Counts (0, 1, 2, ...)
  array[N] int<lower=0> freq;    // Frequencies for each count
  int<lower=1> FIXED;
}

parameters {
  real mu;              // Mean parameter (mu)
  real<lower=0> nu;              // Dispersion parameter (nu)
}

transformed parameters {
  vector[FIXED] Z_terms;
  real logZ;                        // Normalization constant
  real loglamb = nu * log(mu);
  
  // Compute normalization constant Z
  for (i in 1:FIXED) {
    // Compute (mu^n / n!)^nu
    Z_terms[i] = (i-1) * loglamb - nu * lgamma(i);
  }
  
  logZ = log_sum_exp(Z_terms);
}

model {
  vector[N] log_p;          // Log probabilities for each count
  // Priors (adjust these based on your knowledge)
  mu ~ gamma(0.01, 0.01);            // Prior for mu
  //nu ~ gamma(0.0625, 0.25);        // Prior for nu
  nu ~ normal(0, 1);               // Prior for nu
  
  // Compute log probabilities
  for (i in 1:N) {
    log_p[i] = y[i] * loglamb - nu * lgamma(y[i] + 1) - logZ;
    target += freq[i] * log_p[i];
  }
}
