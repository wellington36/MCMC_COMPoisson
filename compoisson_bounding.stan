// compoisson_model_mu_nu.stan
data {
  int<lower=1> N;                // Number of unique counts
  array[N] int<lower=0> y;       // Counts (0, 1, 2, ...)
  array[N] int<lower=0> freq;    // Frequencies for each count
  real<lower=0> Z_max;           // Maximum y to sum for normalization
}

parameters {
  real<lower=0> mu;              // Mean parameter (mu)
  real<lower=0> nu;              // Dispersion parameter (nu)
}

transformed parameters {
  vector[N] log_p;          // Log probabilities for each count
  vector[N] p;               // Probabilities for each count
  real Z;                        // Normalization constant
  
  // Compute normalization constant Z
  Z = 0;
  for (i in 1:N) {
    // Compute (mu^y) / (y!^nu)
    Z += pow(mu, y[i]) / pow(exp(lgamma(y[i] + 1)), nu);
  }

  // Compute log probabilities
  for (i in 1:N) {
    log_p[i] = y[i] * log(mu) - nu * lgamma(y[i] + 1) - log(Z);
    p[i] = exp(log_p[i]);
  }
}

model {
  // Priors (adjust these based on your knowledge)
  mu ~ gamma(2, 0.1);            // Prior for mu
  nu ~ gamma(2, 0.1);            // Prior for nu

  // Likelihood: Multinomial distribution for frequencies
  freq ~ multinomial(p);
}

generated quantities {
  // Optional: Generate predicted frequencies
  array[N] int freq_rep;
  freq_rep = multinomial_rng(p, sum(freq));
}
