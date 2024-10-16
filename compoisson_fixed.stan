// compoisson_model_mu_nu.stan
data {
  int<lower=1> N;                // Number of unique counts
  array[N] int<lower=0> y;       // Counts (0, 1, 2, ...)
  array[N] int<lower=0> freq;    // Frequencies for each count
  int<lower=1> ITER;
}

parameters {
  real<lower=0> mu;              // Mean parameter (mu)
  real<lower=0> nu;              // Dispersion parameter (nu)
}

transformed parameters {
  vector[ITER] Z_terms;
  real logZ;                        // Normalization constant
  real lambda = pow(mu, nu);
  
  // Compute normalization constant Z
  for (i in 1:ITER) {
    // Compute (mu^n / n!)^nu
    Z_terms[i] = i * log(lambda) - nu * lgamma(i + 1);
  }
  
  logZ = log_sum_exp(Z_terms);
}

model {
  vector[N] log_p;          // Log probabilities for each count
  vector[N] p;               // Probabilities for each count
  // Priors (adjust these based on your knowledge)
  mu ~ gamma(0.1, 0.1);            // Prior for mu
  nu ~ normal(0, 1);            // Prior for nu
  
  // Compute log probabilities
  for (i in 1:N) {
    log_p[i] = y[i] * log(lambda) - nu * lgamma(y[i] + 1) - logZ;
    p[i] = exp(log_p[i]);
    target += freq[i] * p[i];
  }
}
