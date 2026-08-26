functions {
  #include sequential.stan
}

data {
  int<lower=1> N;                // Number of unique counts
  array[N] int<lower=0> y;       // Counts (0, 1, 2, ...)
  array[N] int<lower=0> freq;    // Frequencies for each count
  int<lower=1> FIXED;
}

parameters {
  real mu;              // Mean parameter (mu)
  real<lower=0> phi;              // Dispersion parameter (nu)
}

transformed parameters {
  real logZ = sequential(mu, phi, FIXED);
}

model {
  vector[N] log_p;          // Log probabilities for each count
  // Priors (adjust these based on your knowledge)
  mu ~ normal(0, 5);            // Prior for mu
  phi ~ uniform(0, 10);        // Prior for phi
  
  // Compute log probabilities
  for (i in 1:N) {
    log_p[i] = log_kernel(mu, phi, y[i]) - logZ;
    target += freq[i] * log_p[i];
  }
}
