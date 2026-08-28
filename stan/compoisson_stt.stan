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
  real<lower=0> phi;              // Dispersion parameter (phi)
}

transformed parameters {
  real logZ;                        // Normalization constant
  array[2] real infiniteSTTApproach = infiniteSumToThreshold(mu, phi, 0, eps, MAX_ITERS, 0);
  
  logZ = infiniteSTTApproach[1];
}

model {
  vector[N] log_p;          // Log probabilities for each count
  // Priors (adjust these based on your knowledge)
  mu ~ normal(0, 5);            // Prior for mu
  phi ~ uniform(0, 10);        // Prior for phi
  
  // Compute log probabilities
  for (j in 1:N) {
    log_p[j] = log_kernel(mu, phi, y[j]) - logZ;
    target += freq[j] * log_p[j];
  }
}

generated quantities{
  real n = infiniteSTTApproach[2];
}
